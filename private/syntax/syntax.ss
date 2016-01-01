#lang scheme/base

(require (prefix-in set: (planet dherman/set:3:0/seteq))
         (planet cobbe/contract-utils:1/contract-utils)
         scheme/list
         scheme/contract
         scheme/match
         "ast-core.ss"
         "ast-utils.ss"
         "../../private/config.ss"
         ;; TODO: move all imports from here into some more general place (ast-utils, prob)
         "../compiler/helpers.ss")
(require (for-syntax scheme/base))

(define-struct operator (declaration-parser statement-parser expression-parser))

(define current-expansion-context
  (make-parameter 'top))

(define (symbolic-identifier=? id1 id2)
  (eq? (syntax->datum id1) (syntax->datum id2)))

(define (split-javadot id)
  (map string->symbol (regexp-split #rx"\\." (symbol->string id))))

(define ((keyword=? env) id1 id2)
  (let ([s1 (syntax->datum id1)]
        [s2 (syntax->datum id2)])
    (and (eq? s1 s2)
         (eq? (hash-ref env s1 (lambda () #f)); s2))))
              (hash-ref initial-env s2 (lambda () #f))))))

(define (identifiers? stx)
  (andmap identifier? (syntax->list stx)))

(define (syntax-list stxs)
  (if (syntax? stxs)
      (syntax->list stxs)
      stxs))

(define (syntax-map f stxes env)
  (map (lambda (stx)
         (f stx env))
       (syntax-list stxes)))

(define-syntax (transformer x)
  (syntax-case x ()
    [(transformer name type (stx env) clauses ...)
     #'(transformer name type #:keywords () (stx env) clauses ...)]
    [(transformer name type #:keywords (lits ...) (stx env) clauses ...)
     (with-syntax ([operator (string->symbol (format "syntax->~a" (syntax->datum #'type)))])
       #'(lambda (stx env)
           (syntax-case* stx (lits ...) symbolic-identifier=?
             clauses ...
             [_ (raise-syntax-error 'operation (format "invalid ~a" name) stx)])))]))

;; syntax * env -> (optional operator)
(define (lookup-operator stx env)
  (syntax-case* stx (#%keyword) symbolic-identifier=?
    [(#%keyword . op)
     (let ([name (syntax->datum #'op)])
       (cond
         [(hash-ref initial-env name (lambda () #f))]
         [else (raise-syntax-error '#%keyword "unrecognized keyword" stx)]))]
    [_ (and (identifier? stx)
            (let ([operator (hash-ref env (syntax->datum stx) (lambda () #f))])
              (and (operator? operator) operator)))]))

;; ===========================================================================
;; DETERMINING SCOPE
;; ===========================================================================

(define (union-map f stxs)
  (set:unions (map f (syntax-list stxs))))

(define (term-vars* terms env)
  (union-map (lambda (term)
               (term-vars term env))
             terms))

(define (syntax-vars* stxs env)
  (union-map (lambda (stx)
               (syntax-vars stx env))
             stxs))

(define (syntax-clause-vars* stxs env)
  (union-map (lambda (stx)
               (syntax-clause-vars stx env))
             stxs))

(define (term-init-vars init env)
  (match init
    [(struct VariableInitializer (_ (struct Identifier (_ id)) init))
     (set:list->set (list id))]))

(define (syntax-init-vars stx env)
  (syntax-case stx ()
    [id
     (identifier? #'id)
     (set:list->set (list (syntax->datum #'id)))]
    [(id . _)
     (identifier? #'id)
     (set:list->set (list (syntax->datum #'id)))]
    [_ set:empty]))

(define (term-vars term env)
  (match term
    [(struct FunctionDeclaration (_ (struct Identifier (_ name)) args body))
     (set:list->set (list name))]
    [(struct VariableDeclaration (_ inits))
     (union-map (lambda (init)
                  (term-init-vars init env))
                inits)]
    [(struct BlockStatement (_ elts))
     (term-vars* elts env)]
    [(struct IfStatement (_ _ consequent alternate))
     (term-vars* (list consequent alternate) env)]
    [(struct DoWhileStatement (loc body test))
     (term-vars body env)]
    [(struct WhileStatement (loc test body))
     (term-vars body env)]
    [(struct ForStatement (_ (struct VariableDeclaration (_ inits)) test incr body))
     (set:union (union-map (lambda (init)
                             (term-init-vars init env))
                           inits)
                (term-vars body env))]
    [(struct ForStatement (_ _ test incr body))
     (term-vars body env)]
    [(struct ForInStatement (_ (struct VariableDeclaration (_ (list init))) container body))
     (set:union (term-init-vars init env)
                (term-vars body env))]
    [(struct ForInStatement (_ _ container body))
     (term-vars body env)]
    [(struct WithStatement (_ context body))
     (term-vars body env)]
    [(struct SwitchStatement (_ test cases))
     (union-map (lambda (clause)
                  (term-clause-vars clause env))
                cases)]
    [(struct LabelledStatement (_ (struct Identifier (_ l)) stmt))
     (term-vars stmt env)]
    [(struct TryStatement (_ body catch finally))
     (set:union (if finally (term-vars finally env) set:empty)
                (union-map (lambda (clause)
                             (term-clause-vars clause env))
                           catch)
                (term-vars body env))]
    [_ set:empty]))

(define (syntax-vars stx env)
  (syntax-case* stx (function var block if do while for in with switch label try finally) (keyword=? env)
    [(function id (arg ...) body ...)
     (identifier? #'id)
     (set:list->set (list (syntax->datum #'id)))]
    [(function . _)
     set:empty]
    [(var decls ...)
     (union-map (lambda (decl)
                  (syntax-init-vars decl env))
                #'(decls ...))]
    [(block stmts ...)
     (syntax-vars* #'(stmts ...) env)]
    [(if test consequent alternate)
     (syntax-vars* #'(consequent alternate) env)]
    [(if test consequent)
     (syntax-vars #'consequent env)]
    [(do body test)
     (syntax-vars #'body env)]
    [(while test body)
     (syntax-vars #'body env)]
    [(for (var x) in container body)
     (set:union (syntax-init-vars #'x env)
                (syntax-vars #'body env))]
    [(for lhs in container body)
     (syntax-vars #'body env)]
    [(for (var inits ...) test incr body)
     (set:union (union-map (lambda (init)
                             (syntax-init-vars init env))
                           #'(inits ...))
                (syntax-vars #'body env))]
    [(for init test incr body)
     (syntax-vars #'body env)]
    [(with context body)
     (syntax-vars #'body env)]
    [(switch test cases ...)
     (syntax-clause-vars* #'(cases ...) env)]
    [(label id stmt)
     (syntax-vars #'stmt env)]
    [(try body catches ... (finally clause))
     (set:union (syntax-vars #'body env)
                (syntax-clause-vars* #'(catches ...) env)
                (syntax-vars #'clause env))]
    [(try body catches ...)
     (set:union (syntax-vars #'body env)
                (syntax-vars* #'(catches ...) env))]
    [_ set:empty]))

(define (syntax-clause-vars stx env)
  (syntax-case* stx (case catch) (keyword=? env)
    [(case expr stmts ...)
     (syntax-vars* #'(stmts ...) env)]
    [(catch id body)
     (syntax-vars #'body env)]))

(define (term-clause-vars clause env)
  (match clause
    [(struct CatchClause (_ (struct Identifier (_ id)) body))
     (term-vars body env)]
    [(struct CaseClause (loc case stmts))
     (term-vars* stmts env)]))

;; ===========================================================================
;; SYNTAX TRANSFORMERS
;; ===========================================================================

;; expressions

(define #%regexp
  (transformer 'regexp expression (stx env)
    [(_ pattern global? case-insensitive?)
     (make-RegexpLiteral stx (syntax->datum #'pattern) (syntax->datum #'global?) (syntax->datum #'case-insensitive?))]))

(define #%array
  (transformer 'array expression (stx env)
    [(_ elts ...)
     (make-ArrayLiteral stx (->ArrayElements #'(elts ...) env))]))

(define #%call
  (transformer 'call expression (stx env)
    [(method args ...)
     (make-CallExpression stx
                          (->Expression #'method env)
                          (->Expressions #'(args ...) env))]))

(define #%object
  (transformer 'object expression (stx env)
    [(object [ids vals] ...)
     (make-ObjectLiteral stx (map (lambda (id val)
                                    (cons (->Identifier id)
                                          (->Expression val env)))
                                  (syntax->list #'(ids ...))
                                  (syntax->list #'(vals ...))))]))

(define #%field-ref
  (transformer 'field-ref expression (stx env)
    [(field-ref container key)
     (make-BracketReference stx
                            (->Expression #'container env)
                            (->Expression #'key env))]))

(define #%field
  (transformer 'field expression (stx env)
    [(field container id)
     (make-DotReference stx
                        (->Expression #'container env)
                        (->Identifier #'id))]))

(define #%new
  (transformer 'new expression (stx env)
    [(new constructor args ...)
     (make-NewExpression stx
                         (->Expression #'constructor env)
                         (->Expressions #'(args ...) env))]))

(define #%prefix
  (transformer 'prefix expression (stx env)
    [(prefix op expr)
     (make-PrefixExpression stx (syntax->datum #'op) (->Expression #'expr env))]))

(define (#%infix op)
  (transformer op expression (stx env)
    [(_ expr1 expr2 exprs ...)
     (let loop ([accum (make-InfixExpression stx
                                             (->Expression #'expr1 env)
                                             op
                                             (->Expression #'expr2 env))]
                [exprs (syntax->list #'(exprs ...))])
       (if (null? exprs)
           accum
           (loop (make-InfixExpression stx
                                       accum
                                       op
                                       (->Expression (car exprs) env))
                 (cdr exprs))))]))

(define #%postfix
  (transformer 'postfix expression (stx env)
    [(postfix expr op)
     (make-PostfixExpression stx (->Expression #'expr env) (syntax->datum #'op))]))

(define #%if-expression
  (transformer 'if expression (stx env)
    [(if test consequent alternate)
     (make-ConditionalExpression stx
                                 (->Expression #'test env)
                                 (->Expression #'consequent env)
                                 (->Expression #'alternate env))]))

(define (#%assign op)
  (transformer op expression (stx env)
    [(_ left right)
     (make-AssignmentExpression stx
                                (->Expression #'left env)
                                op
                                (->Expression #'right env))]))

(define #%function-expression
  (transformer 'function expression (stx env)
    [(function name (args ...) body ...)
     (identifier? #'name)
     (let ([env* (extend-env (append (map syntax->datum (syntax->list #'(name args ...)))
                                     (set:set->list (syntax-vars* #'(body ...) env)))
                             env)])
       (make-FunctionExpression stx
                                (->Identifier #'name)
                                (->Identifiers #'(args ...))
                                (parameterize ([current-expansion-context 'function])
                                  (->SourceElements #'(body ...) env*))))]
    [(function (args ...) body ...)
     (let ([env* (extend-env (append (map syntax->datum (syntax->list #'(args ...)))
                                     (set:set->list (syntax-vars* #'(body ...) env)))
                             env)])
       (make-FunctionExpression stx
                                #f
                                (->Identifiers #'(args ...))
                                (parameterize ([current-expansion-context 'function])
                                  (->SourceElements #'(body ...) env*))))]))

(define #%begin
  (transformer 'begin expression (stx env)
    [(begin exprs ...)
     (make-ListExpression stx (->Expressions #'(exprs ...) env))]))

(define #%expression
  (transformer '#%expression expression (stx env)
    [(#%expression expr)
     (->Expression #'expr env)]))

;; statements

(define #%block
  (transformer 'block statement (stx env)
    [(block stmts ...)
     (make-BlockStatement stx (->SubStatements #'(stmts ...) env))]))

(define #%if-statement
  (transformer 'if statement (stx env)
    [(if test consequent alternate)
     (make-IfStatement stx
                       (->Expression #'test env)
                       (->SubStatement #'consequent env)
                       (->SubStatement #'alternate env))]
    [(if test consequent)
     (make-IfStatement stx
                       (->Expression #'test env)
                       (->SubStatement #'consequent env)
                       #f)]))

(define #%do
  (transformer 'do statement (stx env)
    [(do body test)
     (make-DoWhileStatement stx
                            (->SubStatement #'body env)
                            (->Expression #'test env))]))

(define #%while
  (transformer 'while statement (stx env)
    [(while test body)
     (make-WhileStatement stx
                          (->Expression #'test env)
                          (->SubStatement #'body env))]))

(define #%for
  (transformer 'for statement #:keywords (var in) (stx env)
    [(for (var x) in container body)
     (make-ForInStatement stx
                          (make-VariableDeclaration #'x (list (->VariableInitializer #'x env)))
                          (->Expression #'container env)
                          (->SubStatement #'body env))]
    [(for lhs in container body)
     (make-ForInStatement stx
                          (->Expression #'lhs env)
                          (->Expression #'container env)
                          (->SubStatement #'body env))]
    [(for (var inits ...) test incr body)
     (make-ForStatement stx
                        (make-VariableDeclaration stx (->VariableInitializers #'(inits ...) env))
                        (->Expression #'test env)
                        (->Expression #'incr env)
                        (->SubStatement #'body env))]
    [(for init test incr body)
     (make-ForStatement stx
                        (->Expression #'init env)
                        (->Expression #'test env)
                        (->Expression #'incr env)
                        (->SubStatement #'body env))]))

(define #%continue
  (transformer 'continue statement (stx env)
    [(continue l)
     (make-ContinueStatement stx (->Identifier #'l))]
    [(continue)
     (make-ContinueStatement stx #f)]))

(define #%break
  (transformer 'break statement (stx env)
    [(break l)
     (make-BreakStatement stx (->Identifier #'l))]
    [(break)
     (make-BreakStatement stx #f)]))

(define #%return
  (transformer 'return statement (stx env)
    [(return expr)
     (make-ReturnStatement stx (->Expression #'expr env))]
    [(return)
     (make-ReturnStatement stx #f)]))

(define #%with
  (transformer 'with statement (stx env)
    [(with context body)
     (make-WithStatement stx (->Expression #'context env) (->SubStatement #'body env))]))

(define #%switch
  (transformer 'switch stateent (stx env)
    [(switch test cases ...)
     (make-SwitchStatement stx
                           (->Expression #'test env)
                           (->CaseClauses #'(cases ...) env))]))

(define #%label
  (transformer 'label statement (stx env)
    [(label l stmt)
     (make-LabelledStatement stx
                             (->Identifier #'l)
                             (->SubStatement #'stmt env))]))

(define #%throw
  (transformer 'throw statement (stx env)
    [(throw expr)
     (make-ThrowStatement stx (->Expression #'expr env))]))

(define #%try
  (transformer 'try statement #:keywords (finally) (stx env)
    [(try body catches ... (finally clause))
     (make-TryStatement stx
                        (->BlockStatement #'body env)
                        (->CatchClauses #'(catches ...) env)
                        (->BlockStatement #'clause env))]
    [(try body catches ...)
     (make-TryStatement stx
                        (->BlockStatement #'body env)
                        (->CatchClauses #'(catches ...) env)
                        #f)]))

(define #%statement
  (transformer '#%statement statement (stx env)
    [(#%statement stmt)
     (->Statement #'stmt env)]))

;; declarations

(define #%var
  (transformer 'var declaration (stx env)
    [(var decls ...)
     (make-VariableDeclaration stx (->VariableInitializers #'(decls ...) env))]))

(define #%function-declaration
  (transformer 'function declaration (stx env)
    [(function name (args ...) body ...)
     (begin
       (when (and (eq? (current-expansion-context) 'block)
                  (not (allow-nested-function-declarations?)))
         (raise-syntax-error 'syntax->statement "illegally nested function definition" stx))
       (let ([env* (extend-env (append (map syntax->datum (syntax->list #'(name args ...)))
                                       (set:set->list (syntax-vars* #'(body ...) env)))
                               env)])
         (make-FunctionDeclaration stx
                                   (->Identifier #'name)
                                   (->Identifiers #'(args ...))
                                   (parameterize ([current-expansion-context 'function])
                                     (->SourceElements #'(body ...) env*)))))]))

;; =============================================================================
;; ENVIRONMENT
;; =============================================================================

(define (alias bindings new-name old-name)
  (cond
    [(assq old-name bindings)
     => (lambda (operator)
          (cons new-name operator))]
    [else #f]))

(define assignment-operator-bindings
  (for/list ([op assignment-operators])
    (cons op (make-operator #f #f (#%assign op)))))

(define infix-operator-bindings
  (for/list ([op infix-operators])
    (cons op (make-operator #f #f (#%infix op)))))

(define convenient-alias-bindings
  (filter-map
   values
   (list (alias infix-operator-bindings 'and         '&&)
         (alias infix-operator-bindings 'or          '\|\|)
         (alias infix-operator-bindings 'bitwise-ior '\|)
         (alias infix-operator-bindings 'bitwise-and '\&)
         (alias infix-operator-bindings 'bitwise-xor '^)
         (alias infix-operator-bindings 'bitwise-not '~))))

(define expression-bindings
  `((array        . ,(make-operator #f #f #%array))
    (regexp       . ,(make-operator #f #f #%regexp))
    (object       . ,(make-operator #f #f #%object))
    (field-ref    . ,(make-operator #f #f #%field-ref))
    (field        . ,(make-operator #f #f #%field))
    (new          . ,(make-operator #f #f #%new))
    (prefix       . ,(make-operator #f #f #%prefix))
    (postfix      . ,(make-operator #f #f #%postfix))
    (begin        . ,(make-operator #f #f #%begin))
    (#%expression . ,(make-operator #f #f #%expression))
    ))

(define statement-bindings
  `((block        . ,(make-operator #f #%block #f))
    (do           . ,(make-operator #f #%do #f))
    (while        . ,(make-operator #f #%while #f))
    (for          . ,(make-operator #f #%for #f))
    (continue     . ,(make-operator #f #%continue #f))
    (break        . ,(make-operator #f #%break #f))
    (return       . ,(make-operator #f #%return #f))
    (with         . ,(make-operator #f #%with #f))
    (switch       . ,(make-operator #f #%switch #f))
    (label        . ,(make-operator #f #%label #f))
    (throw        . ,(make-operator #f #%throw #f))
    (try          . ,(make-operator #f #%try #f))
    (#%statement  . ,(make-operator #f #%statement #f))
    ))

(define declaration-bindings
  `((var         . ,(make-operator #%var #f #f))))

(define shared-bindings
  `((if          . ,(make-operator #f #%if-statement #%if-expression))
    (function    . ,(make-operator #%function-declaration #f #%function-expression))))

(define initial-env
  (make-immutable-hash
   (append assignment-operator-bindings
           infix-operator-bindings
           convenient-alias-bindings
           expression-bindings
           statement-bindings
           declaration-bindings
           shared-bindings)))

(define (extend-env names env)
  (let loop ([names names]
             [env env])
    (if (null? names) env (loop (cdr names) (hash-set env (car names) #t)))))

;; ===========================================================================
;; SYNTAX PARSING
;; ===========================================================================

;; TODO: lots more parse error checking
;;   - check syntax of subforms
;;   - better error messages when falling off end of match
;; TODO: deal with let-bound (block-hoisted) variables

;; sequences:

(define (->Expressions stxs env)
  (syntax-map ->Expression stxs env))

(define (->ArrayElements stxs env)
  (syntax-map ->ArrayElement stxs env))

(define (->Identifiers stxs)
  (map ->Identifier (syntax-list stxs)))

(define (->VariableInitializers stxs env)
  (syntax-map ->VariableInitializer stxs env))

(define (->SourceElements stxs env)
  (syntax-map ->SourceElement stxs env))

(define (->SubStatements stxs env)
  (syntax-map ->SubStatement stxs env))

(define (->CaseClauses stxs env)
  (syntax-map ->CaseClause stxs env))

(define (->CatchClauses stxs env)
  (syntax-map ->CatchClause stxs env))

;; symbols and identifiers:

(define (symbol->Identifier sym stx)
  (check-valid-identifier! 'syntax->expression sym stx)
  (make-Identifier stx sym))

(define (symbol->Expression sym stx env)
  (case (hash-ref env sym (lambda () #t))
    [(this) (make-ThisReference stx)]
    [(null) (make-NullLiteral stx)]
    [else (make-VarReference stx (symbol->Identifier sym stx))]))

(define (->Identifier stx)
  (symbol->Identifier (syntax->datum stx) stx))

;; expressions:

(define (->Expression stx env)
  (syntax-case* stx (null this) symbolic-identifier=?
    [(op . _)
     (lookup-operator #'op env)
     (let ([operator (lookup-operator #'op env)])
       (cond
         [(operator-expression-parser operator)
          => (lambda (parse)
               (parameterize ([current-expansion-context 'expression])
                 (parse stx env)))]
         [else (raise-syntax-error 'syntax->expression "not an expression form" #'op stx)]))]
    [(op . _)
     (#%call stx env)]
    [null (make-NullLiteral stx)]
    [this (make-ThisReference stx)]
    [x
     (identifier? #'x)
     (let ([chain (reverse (split-javadot (syntax->datum #'x)))])
       (let f ([id (car chain)] [prefix (cdr chain)])
         (if (null? prefix)
             (symbol->Expression id stx env)
             (let ([container (f (car prefix) (cdr prefix))])
               (make-DotReference stx container (make-Identifier stx id))))))]
    [datum
     (let ([datum (syntax->datum #'datum)])
       (cond
         [(string? datum) (make-StringLiteral stx datum)]
         [(number? datum) (make-NumericLiteral stx datum)]
         [(boolean? datum) (make-BooleanLiteral stx datum)]
         [else (raise-syntax-error 'syntax->expression "unrecognized literal" stx)]))]))

;(define (->Property stx env)
;  (let ([datum (syntax->datum stx)])
;    (cond
;      [(symbol? datum)
;       (check-valid-identifier! 'syntax->expression datum stx)
;       (make-Identifier stx datum)]
;      [(string? datum)
;       (make-StringLiteral stx datum)]
;      [(number? datum)
;       (make-NumericLiteral stx datum)]
;      [else (raise-syntax-error 'syntax->expression "invalid property name" stx)])))

(define (->ArrayElement stx env)
  (syntax-case stx ()
    [() #f]
    [_ (->Expression stx env)]))

;; statements and declarations:

(define (->Statement stx env)
  (syntax-case stx ()
    [()
     (make-EmptyStatement stx)]
    [(op . _)
     (lookup-operator #'op env)
     (let ([operator (lookup-operator #'op env)])
       (cond
         [(operator-statement-parser operator)
          => (lambda (parser)
               (parameterize ([current-expansion-context 'statement])
                 (parser stx env)))]
         [else (make-ExpressionStatement stx (->Expression stx env))]))]
    [_ (make-ExpressionStatement stx (->Expression stx env))]))

(define (->SourceElement stx env)
  (syntax-case stx ()
    [(op . _)
     (lookup-operator #'op env)
     (let ([operator (lookup-operator #'op env)])
       (cond
         [(operator-declaration-parser operator)
          => (lambda (parser)
               (parser stx env))]
         [else (->Statement stx env)]))]
    [_ (->Statement stx env)]))

(define (->SubStatement stx env)
  (parameterize ([current-expansion-context 'block])
    (->SourceElement stx env)))

(define (->VariableInitializer stx env)
  (syntax-case stx ()
    [id
     (identifier? #'id)
     (let ([id (syntax->datum #'id)])
       (check-valid-identifier! 'syntax->variable-initializer id stx)
       (make-VariableInitializer stx (->Identifier stx) #f))]
    [(id expr)
     (identifier? #'id)
     (make-VariableInitializer stx (->Identifier #'id) (->Expression #'expr env))]
    [_ (raise-syntax-error 'syntax->variable-initializer "invalid variable initializer" stx)]))

(define (->CaseClause stx env)
  (syntax-case* stx (default case) symbolic-identifier=?
    [(default stmts ...)
     (make-CaseClause stx #f (->SubStatements #'(stmts ...) env))]
    [(case expr stmts ...)
     (make-CaseClause stx (->Expression #'expr env) (->SubStatements #'(stmts ...) env))]
    [_ (raise-syntax-error 'syntax->case-clause "invalid case clause" stx)]))

(define (->CatchClause stx env)
  (syntax-case* stx (catch) symbolic-identifier=?
    [(catch id body)
     (let ([env* (extend-env (list (syntax->datum #'id)) env)])
       (make-CatchClause stx (->Identifier #'id) (->BlockStatement #'body env*)))]
    [_ (raise-syntax-error 'syntax->catch-clause "invalid catch clause" stx)]))

(define (->BlockStatement stx env)
  (let ([stmt (->Statement stx env)])
    (if (BlockStatement? stmt)
        stmt
        (make-BlockStatement (Term-location stmt) (list stmt)))))

;; ===========================================================================
;; EXPORTED PARSERS
;; ===========================================================================

(define (syntax->expression stx)
  (->Expression stx initial-env))

(define (sexp->expression sexp)
  (syntax->expression (datum->syntax #f sexp)))

(define (syntax->statement stx)
  (let ([env (extend-env (set:set->list (syntax-vars stx initial-env)) initial-env)])
    (->Statement stx env)))

(define (sexp->statement sexp)
  (syntax->statement (datum->syntax #f sexp)))

(define (syntax->source-element stx)
  (let ([env (extend-env (set:set->list (syntax-vars stx initial-env)) initial-env)])
    (->SourceElement stx env)))

(define (sexp->source-element sexp)
  (syntax->source-element (datum->syntax #f sexp)))

(define (syntax->program-unit stx)
  (let ([env (extend-env (set:set->list (syntax-vars* stx initial-env)) initial-env)])
    (syntax-case stx ()
      [(stmts ...)
       (->SourceElements #'(stmts ...) env)])))

(define (sexp->program-unit sexp)
  (syntax->program-unit (datum->syntax #f sexp)))

;; ===========================================================================
;; SYNTAX GENERATION
;; ===========================================================================

(define (keyword env name [default (string->symbol (format "#%~a" name))])
  (if (eq? (hash-ref env name (lambda () #f)) #t) default name))

(define (ArrayElement-> elt env)
  (if elt (Expression-> elt env) #'()))

(define (ArrayElements-> elts env)
  (for/list ([elt elts])
    (ArrayElement-> elt env)))

(define (Property-> elt)
  (match elt
    [(struct Identifier (_ name)) name]
    [(struct StringLiteral (_ value)) value]
    [(struct NumericLiteral (_ value)) value]))

(define (Properties-> props)
  (for/list ([prop props])
    (Property-> prop)))

(define (Expressions-> exprs env)
  (for/list ([expr exprs])
    (Expression-> expr env)))

(define (Identifiers-> ids env)
  (map Identifier-name ids))

(define (Expression-> expr env)
  (match expr
    [(struct StringLiteral (loc str))
     (datum->syntax #f str (location->syntax loc))]
    [(struct RegexpLiteral (loc pattern global? case-insensitive?))
     (with-syntax ([regexp (keyword env 'regexp)])
       (quasisyntax/loc (location->syntax loc)
         (regexp #,pattern #,global? #,case-insensitive?)))]
    [(struct NumericLiteral (loc n))
     (datum->syntax #f n (location->syntax loc))]
    [(struct BooleanLiteral (loc b))
     (datum->syntax #f b (location->syntax loc))]
    [(struct NullLiteral (loc))
     (datum->syntax #f 'null (location->syntax loc))]
    [(struct ArrayLiteral (loc elts))
     (with-syntax ([array (keyword env 'array)])
       (quasisyntax/loc (location->syntax loc)
         (array #,@(ArrayElements-> elts env))))]
    [(struct ObjectLiteral (loc (list (cons props values) ...)))
     (with-syntax ([object (keyword env 'object)]
                   [(prop ...) (Properties-> props)]
                   [(value ...) (Expressions-> values env)])
       (quasisyntax/loc (location->syntax loc)
         (object [prop value] ...)))]
    [(struct ThisReference (loc))
     (datum->syntax #f 'this (location->syntax loc))]
    [(struct VarReference (loc (struct Identifier (_ id))))
     (datum->syntax #f id (location->syntax loc))]
    [(struct BracketReference (loc container key))
     (with-syntax ([field-ref 'field-ref])
       (quasisyntax/loc (location->syntax loc)
         (field-ref #,(Expression-> container env)
                    #,(Expression-> key env))))]
    [(struct DotReference (loc container (struct Identifier (_ id))))
     (with-syntax ([field (keyword env 'field)])
       (quasisyntax/loc (location->syntax loc)
         (field #,(Expression-> container env) #,id)))]
    [(struct NewExpression (loc constructor args))
     (with-syntax ([new 'new]
                   [constructor (Expression-> constructor env)]
                   [(arg ...) (Expressions-> args env)])
       (quasisyntax/loc (location->syntax loc)
         (new constructor arg ...)))]
    [(struct PrefixExpression (loc op expr))
     (with-syntax ([prefix (keyword env 'prefix)])
       (quasisyntax/loc (location->syntax loc)
         (prefix #,op #,(Expression-> expr env))))]
    [(struct PostfixExpression (loc expr op))
     (with-syntax ([prefix (keyword env 'postfix)])
       (quasisyntax/loc (location->syntax loc)
         (postfix #,(Expression-> expr env) #,op)))]
    [(struct InfixExpression (loc left op right))
     (with-syntax ([op op])
       (quasisyntax/loc (location->syntax loc)
         (op #,(Expression-> left env) #,(Expression-> right env))))]
    [(struct ConditionalExpression (loc test consequent alternate))
     (with-syntax ([if 'if])
       (quasisyntax/loc (location->syntax loc)
         (if #,(Expression-> test env)
             #,(Expression-> consequent env)
             #,(Expression-> alternate env))))]
    [(struct AssignmentExpression (loc lhs op rhs))
     (with-syntax ([op op])
       (quasisyntax/loc (location->syntax loc)
         (op #,(Expression-> lhs env) #,(Expression-> rhs env))))]
    [(struct FunctionExpression (loc #f (list args ...) body))
     (let ([env* (extend-env args env)])
       (with-syntax ([function 'function]
                     [(arg ...) (Identifiers-> args env)]
                     [(body ...) (SourceElements-> body env*)])
         (quasisyntax/loc (location->syntax loc)
           (function (arg ...) body ...))))]
    [(struct FunctionExpression (loc (struct Identifier (_ name)) (list args ...) body))
     (let ([env* (extend-env (cons name args) env)])
       (with-syntax ([function 'function]
                     [name name]
                     [(arg ...) (Identifiers-> args env)]
                     [(body ...) (SourceElements-> body env*)])
         (quasisyntax/loc (location->syntax loc)
           (function name (arg ...) body ...))))]
    [(struct ListExpression (loc exprs))
     (with-syntax ([begin (keyword env 'begin)]
                   [(expr ...) (Expressions-> exprs env)])
       (quasisyntax/loc (location->syntax loc)
         (begin expr ...)))]
    [(struct CallExpression (loc method args))
     (with-syntax ([method (Expression-> method env)]
                   [(arg ...) (Expressions-> args env)])
       (quasisyntax/loc (location->syntax loc)
         (method arg ...)))]
    [(struct ParenExpression (loc expr))
     (Expression-> expr env)]
;     (with-syntax ([#%expression '#%expression]
;                   [e (Expression-> expr env)])
;       (syntax/loc (location->syntax loc)
;         (#%expression e)))]
    ))

(define (SourceElements-> elts env)
  (for/list ([elt elts])
    (SourceElement-> elt env)))

(define (SourceElement-> elt env)
  (match elt
    [(? FunctionDeclaration?)
     (FunctionDeclaration-> elt env)]
    [(struct VariableDeclaration (loc inits))
     (with-syntax ([var 'var]
                   [(init ...) (VariableInitializers-> inits env)])
       (quasisyntax/loc (location->syntax loc)
         (var init ...)))]
    [_ (Statement-> elt env)]))

(define (VariableInitializers-> inits env)
  (for/list ([init inits])
    (VariableInitializer-> init env)))

(define (VariableInitializer-> init env)
  (match init
    [(struct VariableInitializer (loc (struct Identifier (_ id)) #f))
     (datum->syntax #f id (location->syntax loc))]
    [(struct VariableInitializer (loc1 (struct Identifier (loc2 id)) init))
     (with-syntax ([id (datum->syntax #f id (location->syntax loc2))])
       (quasisyntax/loc (location->syntax loc1)
         [id #,(Expression-> init env)]))]))

(define (FunctionDeclaration-> decl env)
  (match decl
    [(struct FunctionDeclaration (loc (struct Identifier (_ name)) (list args ...) body))
     (let ([env* (extend-env (cons name args) env)])
       (with-syntax ([function 'function]
                     [name name]
                     [(arg ...) (Identifiers-> args env)]
                     [(body ...) (SourceElements-> body env*)])
         (quasisyntax/loc (location->syntax loc)
           (function name (arg ...) body ...))))]))

(define (Statement-> stmt env)
  (match stmt
    [(struct BlockStatement (loc elts))
     (with-syntax ([block (keyword env 'block)]
                   [(elt ...) (SubStatements-> elts env)])
       (syntax/loc (location->syntax loc)
         (block elt ...)))]
    [(struct EmptyStatement (loc))
     (syntax/loc (location->syntax loc)
       ())]
    [(struct ExpressionStatement (loc expr))
     (with-syntax ([e (Expression-> expr env)])
       (if (or (FunctionExpression? expr) (ConditionalExpression? expr))
           (with-syntax ([#%expression '#%expression])
             (syntax/loc loc
               (#%expression e)))
           #'e))]
    [(struct IfStatement (loc test consequent alternate))
     (if alternate
         (with-syntax ([if 'if])
           (quasisyntax/loc (location->syntax loc)
             (if #,(Expression-> test env)
                 #,(SubStatement-> consequent env)
                 #,(SubStatement-> alternate env))))
         (with-syntax ([if 'if])
           (quasisyntax/loc (location->syntax loc)
             (if #,(Expression-> test env)
                 #,(SubStatement-> consequent env)))))]
    [(struct DoWhileStatement (loc body test))
     (with-syntax ([do 'do])
       (quasisyntax/loc (location->syntax loc)
         (do #,(SubStatement-> body env) #,(Expression-> test env))))]
    [(struct WhileStatement (loc test body))
     (with-syntax ([while 'while])
       (quasisyntax/loc (location->syntax loc)
         (while #,(Expression-> test env) #,(SubStatement-> body env))))]
    [(struct ForStatement (loc init test incr body))
     (with-syntax ([for 'for])
       (quasisyntax/loc (location->syntax loc)
         (for #,(cond
                  [(not init) #f]
                  [(VariableDeclaration? init) (SourceElement-> init env)]
                  [else (Expression-> init env)])
              #,(if test (Expression-> test env) #t)
              #,(if incr (Expression-> incr env) #f)
              #,(SubStatement-> body env))))]
    [(struct ForInStatement (loc
                             (struct VariableDeclaration (_ (list (struct VariableInitializer (_ (struct Identifier (_ x)) #f)))))
                             container
                             body))
     (with-syntax ([for 'for]
                   [in 'in]
                   [var (keyword env 'var '#%var)])
       (quasisyntax/loc (location->syntax loc)
         (for (var #,x) in #,(Expression-> container env)
           #,(SubStatement-> body env))))]
    [(struct ForInStatement (loc (? Expression? var) container body))
     (with-syntax ([for 'for]
                   [in 'in])
       (quasisyntax/loc (location->syntax loc)
         (for #,(Expression-> var env) in #,(Expression-> container env)
           #,(SubStatement-> body env))))]
    [(struct ContinueStatement (loc #f))
     (with-syntax ([continue 'continue])
       (quasisyntax/loc (location->syntax loc)
         (continue)))]
    [(struct ContinueStatement (loc (struct Identifier (_ id))))
     (with-syntax ([continue 'continue])
       (quasisyntax/loc (location->syntax loc)
         (continue #,id)))]
    [(struct BreakStatement (loc #f))
     (with-syntax ([break 'break])
       (quasisyntax/loc (location->syntax loc)
         (break)))]
    [(struct BreakStatement (loc (struct Identifier (_ id))))
     (with-syntax ([break 'break])
       (quasisyntax/loc (location->syntax loc)
         (break #,id)))]
    [(struct ReturnStatement (loc value))
     (with-syntax ([return 'return])
       (if value
           (quasisyntax/loc (location->syntax loc)
             (return #,(Expression-> value env)))
           (syntax/loc (location->syntax loc)
             (return))))]
    [(struct WithStatement (loc context body))
     (with-syntax ([with 'with])
       (quasisyntax/loc (location->syntax loc)
         (with #,(Expression-> context env)
               #,(SubStatement-> body env))))]
    [(struct SwitchStatement (loc test cases))
     (with-syntax ([switch 'switch]
                   [test (Expression-> test env)]
                   [(clause ...) (CaseClauses-> cases env)])
       (quasisyntax/loc (location->syntax loc)
         (switch test clause ...)))]
    [(struct LabelledStatement (loc (struct Identifier (_ l)) stmt))
     (with-syntax ([label (keyword env 'label)])
       (quasisyntax/loc (location->syntax loc)
         (label #,l #,(SubStatement-> stmt env))))]
    [(struct ThrowStatement (loc value))
     (with-syntax ([throw 'throw])
       (quasisyntax/loc (location->syntax loc)
         (throw #,(Expression-> value env))))]
    [(struct TryStatement (loc body catch finally))
     (with-syntax ([try 'try]
                   [body (Statement-> body env)]
                   [(catch ...) (CatchClauses-> catch env)]
                   [(finally ...) (if finally (list (cons 'finally (Statement-> finally env))) null)])
       (quasisyntax/loc (location->syntax loc)
         (try body catch ... finally ...)))]))

(define (SubStatement-> elt env)
  (SourceElement-> elt env))

(define (SubStatements-> elts env)
  (for/list ([elt elts])
    (SubStatement-> elt env)))

(define (CaseClauses-> clauses env)
  (for/list ([clause clauses])
    (CaseClause-> clause env)))

(define (CaseClause-> clause env)
  (match clause
    [(struct CaseClause (loc #f stmts))
     (with-syntax ([default 'default]
                   [(stmt ...) (SubStatements-> stmts env)])
       (quasisyntax/loc (location->syntax loc)
         (default stmt ...)))]
    [(struct CaseClause (loc expr stmts))
     (with-syntax ([case 'case]
                   [expr (Expression-> expr env)]
                   [(stmt ...) (SubStatements-> stmts env)])
       (quasisyntax/loc (location->syntax loc)
         (case expr stmt ...)))]))

(define (CatchClauses-> clauses env)
  (for/list ([clause clauses])
    (CatchClause-> clause env)))

(define (CatchClause-> clause env)
  (match clause
    [(struct CatchClause (loc (struct Identifier (_ id)) body))
     (with-syntax ([catch 'catch])
       (quasisyntax/loc (location->syntax loc)
         (catch #,id #,(Statement-> body (extend-env (list id) env)))))]))

;; ===========================================================================
;; EXPORTED GENERATORS
;; ===========================================================================

(define (expression->syntax expr)
  (Expression-> expr initial-env))

(define (expression->sexp expr)
  (syntax->datum (expression->syntax expr)))

(define (statement->syntax stmt)
  (let ([env (extend-env (set:set->list (term-vars stmt initial-env)) initial-env)])
    (Statement-> stmt env)))

(define (statement->sexp stmt)
  (syntax->datum (statement->syntax stmt)))

(define (source-element->syntax elt)
  (let ([env (extend-env (set:set->list (term-vars elt initial-env)) initial-env)])
    (SourceElement-> elt env)))

(define (source-element->sexp elt)
  (syntax->datum (source-element->syntax elt)))

(define (program-unit->syntax pgm)
  (let ([env (extend-env (set:set->list (term-vars* pgm initial-env)) initial-env)])
    (SourceElements-> pgm env)))

(define (program-unit->sexp pgm)
  (syntax->datum (program-unit->syntax pgm)))

(provide syntax->expression syntax->statement syntax->source-element syntax->program-unit)
(provide expression->syntax statement->syntax source-element->syntax program-unit->syntax)

(provide sexp->expression sexp->statement sexp->source-element sexp->program-unit)
(provide expression->sexp statement->sexp source-element->sexp program-unit->sexp)

;; --- FOR TESTING ---
(require scheme/port)
(define (mk sexp)
  (with-input-from-string (with-output-to-string (lambda () (write sexp)))
                          read-syntax))
;; --- FOR TESTING ---
