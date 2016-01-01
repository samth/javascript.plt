#lang scheme/base

(require scheme/match
         scheme/promise
         "../syntax/ast-core.ss"
         "../syntax/ast-utils.ss"
         "hoist.ss")
(require (for-template scheme/base))

(provide (all-defined-out))

;; =============================================================================
;; UNHYGIENIC IDENTIFIERS
;; =============================================================================

;; The scope chain, only used in dynamic code.
(define scope-chain-id (datum->syntax #f 'scope-chain))

;; The variable object, only used in dynamic code.
(define variable-object-id (datum->syntax #f 'variable-object))

;; The current `this', retrieved from the `current-this' parameter.
(define this-id (datum->syntax #f 'this))

;; =============================================================================
;; COMPILATION PARAMETERS
;; =============================================================================

;; env = (hasheqof symbol Variable)

(define empty-scope #hasheq())

;; TODO: make this (parameterof env) and create a separate current-environment-protocol : (parameterof (union 'dynamic 'static))

;; current-scope : (parameterof (optional env))
(define current-scope (make-parameter empty-scope))

;; (parameterof (union 'module 'script 'scheme 'eval 'interaction))
(define current-compilation-context (make-parameter 'scheme))

;; (parameterof (union 'top 'function))
(define current-lexical-context (make-parameter 'top))

;; (parameterof syntax)
(define current-eval-context (make-parameter #'context.ss))

;; (parameterof (optional syntax))
(define current-source-syntax (make-parameter #f))

;; (parameterof boolean)
(define current-nested? (make-parameter #f))

;; pragma ::= '(lexical scope)

;; (parameterof (hashof pragma #t))
(define current-pragmas (make-parameter '#hash(((lexical scope) . #f))))

(define current-labels (make-parameter null))

;; =============================================================================
;; VARIABLE BINDING
;; =============================================================================

;; Identifier * syntax<identifier>
(define-struct Variable (source compiled))

;; ModuleSpecifier * module-path * boolean
(define-struct (Import Variable) (module-spec module-path eval?))

;; Variable Variable -> boolean
(define (Variable=? v1 v2)
  (Identifier=? (Variable-source v1) (Variable-source v2)))

(define-syntax-rule (with-scope e body ...)
  (parameterize ([current-scope (and (current-scope) e)])
    body ...))

;; (union Variable Identifier symbol) [env] -> (union Variable #f)
(define (resolve x [env (or (current-scope) (error 'resolve "no current environment"))])
  (hash-ref env (name-of x) (lambda () #f)))

;; (union Variable Identifier symbol) [env] -> boolean
(define (bound? x [env (or (current-scope) (error 'bound? "no current environment"))])
  (and (hash-ref env (name-of x) (lambda () #f)) #t))

;; (union Variable Identifier symbol) -> symbol
(define (name-of x)
  (cond
    [(Identifier? x) (Identifier-name x)]
    [(Variable? x) (name-of (Variable-source x))]
    [(symbol? x) x]
    [else (error 'name-of "not a name: ~v~n" x)]))

;; (listof Variable) (optional env) -> (optional env)
(define (bind xs env)
  (if (or (not env) (null? xs))
      env
      (let ([x (car xs)])
        (bind (cdr xs) (hash-set env (name-of x) x)))))

;; =============================================================================
;; DIRECT EVAL ANALYSIS
;; =============================================================================

(define (direct-eval? expr)
  (match expr
    [(struct VarReference (_ (struct Identifier (_ sym))))
     (if (eq? (current-compilation-context) 'module)
         (cond
           [(hash-ref (current-scope) sym (lambda () #f))
            => (lambda (var)
                 (and (Import? var)
                      (force (Import-eval? var))))]
           [else #f])
         (eq? sym 'eval))]
;     (or (not (current-scope))
;         (not (bound? 'eval)))]
    [_ #f]))

;; (listof Statement) -> boolean
(define (contains-direct-eval? body)
  (ormap Statement-contains-direct-eval? body))

;; TODO: tighter analysis-- always #f if we go under a binding of 'eval

(define (Statement-contains-direct-eval? stmt)
  (and stmt
       (match stmt
         [(struct BlockStatement/hoisted (_ stmts _ _)) (ormap Statement-contains-direct-eval? stmts)]
         [(struct EmptyStatement (_)) #f]
         [(struct ExpressionStatement (_ expr)) (Expression-contains-direct-eval? expr)]
         [(struct IfStatement (_ test cons alt)) (or (Expression-contains-direct-eval? test)
                                                     (Statement-contains-direct-eval? cons)
                                                     (Statement-contains-direct-eval? alt))]
         [(struct DoWhileStatement (_ body test)) (or (Statement-contains-direct-eval? body)
                                                      (Expression-contains-direct-eval? test))]
         [(struct WhileStatement (_ test body)) (or (Expression-contains-direct-eval? test)
                                                    (Statement-contains-direct-eval? body))]
         [(struct ForStatement (_ init test incr body)) (or (and (Expression? init) (Expression-contains-direct-eval? init))
                                                            (and incr (Expression-contains-direct-eval? incr))
                                                            (and body (Statement-contains-direct-eval? body)))]
         [(struct ForInStatement (_ lhs rhs body)) (or (and (Expression? lhs) (Expression-contains-direct-eval? lhs))
                                                       (Expression-contains-direct-eval? rhs)
                                                       (Statement-contains-direct-eval? body))]
         [(struct ContinueStatement (_ label)) #f]
         [(struct BreakStatement (_ label)) #f]
         ;; TODO: optimization-- can we ignore direct eval here, since it can't affect anything afterwards? oh... not in case of exceptions...
         [(struct ReturnStatement (_ expr)) (and expr (Expression-contains-direct-eval? expr))]
         [(struct LetStatement (_ head body)) (or (ormap VariableInitializer-contains-direct-eval? head)
                                                  (Statement-contains-direct-eval? body))]
         [(struct WithStatement (_ ctxt body)) (Expression-contains-direct-eval? ctxt)]
         [(struct SwitchStatement (_ expr cases)) (or (Expression-contains-direct-eval? expr)
                                                      (ormap CaseClause-contains-direct-eval? cases))]
         [(struct LabelledStatement (_ label body)) (Statement-contains-direct-eval? body)]
         [(struct ThrowStatement (_ expr)) (Expression-contains-direct-eval? expr)]
         [(struct TryStatement (_ body catch finally)) (or (Statement-contains-direct-eval? body)
                                                           (ormap CatchClause-contains-direct-eval? catch)
                                                           (and finally (Statement-contains-direct-eval? finally)))])))

(define (optional-Expression-contains-direct-eval? expr?)
  (and expr? (Expression-contains-direct-eval? expr?)))

(define (Expression-contains-direct-eval? expr)
  (match expr
    [(or (? StringLiteral?)
         (? NumericLiteral?)
         (? BooleanLiteral?)
         (? RegexpLiteral?)
         (? NullLiteral?))
     #f]
    [(struct ArrayLiteral (_ elts)) (ormap optional-Expression-contains-direct-eval? elts)]
    [(struct ObjectLiteral (_ props)) (ormap (lambda (prop)
                                               (Expression-contains-direct-eval? (cdr prop)))
                                             props)]
    [(struct BlockLiteral (_ args body)) (ExpressionBlock-contains-direct-eval? body)]
    [(struct ThisReference (_)) #f]
    [(struct VarReference (_ id)) #f]
    [(struct BracketReference (_ container key)) (or (Expression-contains-direct-eval? container)
                                                     (Expression-contains-direct-eval? key))]
    [(struct DotReference (_ container id)) (Expression-contains-direct-eval? container)]
    [(struct NewExpression (_ ctor args)) (or (Expression-contains-direct-eval? ctor)
                                              (ormap Expression-contains-direct-eval? args))]
    [(struct PostfixExpression (_ expr op)) (Expression-contains-direct-eval? expr)]
    [(struct PrefixExpression (_ op expr)) (Expression-contains-direct-eval? expr)]
    [(struct InfixExpression (_ left op right)) (or (Expression-contains-direct-eval? left)
                                                    (Expression-contains-direct-eval? right))]
    [(struct ConditionalExpression (_ test cons alt)) (or (Expression-contains-direct-eval? test)
                                                          (Expression-contains-direct-eval? cons)
                                                          (Expression-contains-direct-eval? alt))]
    [(struct AssignmentExpression (_ lhs op rhs)) (or (Expression-contains-direct-eval? lhs)
                                                      (Expression-contains-direct-eval? rhs))]
    [(struct FunctionExpression/hoisted (_ name args body funs vars imports exports)) #f]
    [(struct LetExpression (_ head body)) (or (ormap VariableInitializer-contains-direct-eval? head)
                                              (Expression-contains-direct-eval? body))]
    [(struct CallExpression (_ (struct VarReference (_ (struct Identifier (_ 'eval)))) args)) #t]
    [(struct CallExpression (_ method args)) (or (Expression-contains-direct-eval? method)
                                                 (ormap Expression-contains-direct-eval? args))]
    [(struct ParenExpression (_ expr)) (Expression-contains-direct-eval? expr)]
    [(struct ListExpression (_ exprs)) (ormap Expression-contains-direct-eval? exprs)]
    [(struct DoExpression (_ block)) (ExpressionBlock-contains-direct-eval? block)]))

(define (ExpressionBlock-contains-direct-eval? block)
  (match block
    [(struct ExpressionBlock (_ body tail))
     (or (Statement-contains-direct-eval? body)
         (and tail (Expression-contains-direct-eval? tail)))]))

(define (VariableInitializer-contains-direct-eval? init)
  (match init
    [(struct VariableInitializer (_ id expr))
     (and expr (Expression-contains-direct-eval? expr))]))

(define (CaseClause-contains-direct-eval? clause)
  (match clause
    [(struct CaseClause (_ question answer))
     (or (and question (Expression-contains-direct-eval? question))
         (ormap Statement-contains-direct-eval? answer))]))

(define (CatchClause-contains-direct-eval? clause)
  (match clause
    [(struct CatchClause (_ id body))
     (Statement-contains-direct-eval? body)]))
