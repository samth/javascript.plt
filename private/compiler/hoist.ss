#lang scheme/base

(require (only-in srfi/1/list lset-difference)
         scheme/match
         scheme/list
         "../syntax/ast-core.ss"
         "../syntax/ast-utils.ss"
         "../syntax/token.ss"
         "hoist-monad.ss")

;; XXX: expressions must be able to hoist too

;; unique-vars : (listof FunctionDeclaration/hoisted) * (listof Identifier) -> (listof Identifier)
(define (unique-vars funs vars)
  (lset-difference Identifier=?
                   (remove-duplicates vars Identifier=?)
                   (map FunctionDeclaration-name funs)))

(define (to-location x)
  (cond
    [(not x) #f]
    [(position? x) x]
    [else (ast-location x)]))

;; optional-statement->statement : (optional Statement) (optional has-location) -> Statement
(define (optional-statement->statement stmt loc)
  (or stmt (make-EmptyStatement (to-location loc))))

;; ===========================================================================
;; TOP-LEVEL HOISTING FUNCTIONS
;; ===========================================================================

;; wrap-in-implicit-block : region * (listof Statement) * (listof FunctionDeclaration/hoisted) * (listof Identifier) -> (listof Statement)
(define (wrap-in-implicit-block loc stmts let-funs let-vars)
  (list (make-BlockStatement/hoisted loc stmts let-funs let-vars)))

;; hoist-function-expression : FunctionExpression -> FunctionExpression/hoisted
(define (hoist-function-expression expr)
  (match expr
    [(struct FunctionExpression (loc name args body))
     (let-values ([(body let-vars let-funs vars funs imports exports)
                   (execute (hoist-source-elements body))])
       (make-FunctionExpression/hoisted loc
                                        name
                                        args
                                        ;; TODO: should FunctionExpressions contain a block instead of a list?
                                        (wrap-in-implicit-block loc body let-funs let-vars)
                                        funs
                                        (unique-vars funs vars)
                                        imports
                                        exports))]))

;; hoist-function-declaration : FunctionDeclaration -> FunctionDeclaration/hoisted
(define (hoist-function-declaration decl)
  (match decl
    [(struct FunctionDeclaration (location name args body))
     (let-values ([(stmts let-vars let-funs vars funs imports exports)
                   (execute (hoist-source-elements body))])
       (make-FunctionDeclaration/hoisted location
                                         name
                                         args
                                         (wrap-in-implicit-block location stmts let-funs let-vars)
                                         funs
                                         (unique-vars funs vars)
                                         imports
                                         exports))]))

;; hoist-program-unit : (listof SourceElement) -> (listof FunctionDeclaration/hoisted)
;;                                                (listof Identifier)
;;                                                (listof ImportDeclaration)
;;                                                (listof ExportDeclaration)
;;                                                (listof Statement)
(define (hoist-program-unit elts)
  (let-values ([(stmts let-vars let-funs vars funs imports exports)
                (execute (hoist-source-elements elts))])
    (values funs
            (unique-vars funs vars)
            imports
            exports
            (wrap-in-implicit-block (@ (first elts) (last elts)) stmts let-funs let-vars))))

;; ===========================================================================
;; COMPOUND HOISTING FUNCTIONS
;; ===========================================================================

;; hoist-source-elements : (listof SourceElement) -> (H (listof Statement))
(define (hoist-source-elements elts)
  (map/m hoist-source-element elts))

;; hoist-optional-expression : (optional Expression) -> (H (optional Expression))
(define (hoist-optional-expression expr)
  (if expr (hoist-expression expr) (return #f)))

;; hoist-substatements : (listof SourceElement) -> (H (listof Statement))
(define (hoist-substatements stmts)
  (hoist-source-elements stmts))

;; hoist-var-initializers : (listof VariableInitializer) (optional region) -> (H Expression)
(define (hoist-var-initializers decls loc)
  (begin-hoist
    (exprs <- (filter-map/m hoist-var-initializer decls))
    (return (if (and (pair? exprs)
                     (null? (cdr exprs)))
                (car exprs)
                (make-ListExpression loc exprs)))))

;; hoist-let-var-initializers : (listof VariableInitializer) (optional region) -> (H Expression)
(define (hoist-let-var-initializers decls loc)
  (begin-hoist
    (exprs <- (filter-map/m hoist-let-var-initializer decls))
    (return (if (and (pair? exprs)
                     (null? (cdr exprs)))
                (car exprs)
                (make-ListExpression loc exprs)))))

;; hoist-case-clauses : (listof CaseClause) -> (H (listof CaseClause))
(define (hoist-case-clauses cases)
  (map/m hoist-case-clause cases))

;; hoist-catch-clauses : (listof CatchClause) -> (H (listof CatchClause))
(define (hoist-catch-clauses catches)
  (map/m hoist-catch-clause catches))

;; ===========================================================================
;; CORE HOISTING FUNCTIONS
;; ===========================================================================

;; hoist-source-element : SourceElement -> (H Statement)
(define (hoist-source-element src)
  (if (Declaration? src)
      (hoist-declaration src)
      (hoist-statement src)))

;; hoist-declaration : Declaration -> (H Statement)
(define (hoist-declaration decl0)
  (cond
    [(FunctionDeclaration? decl0)
     (begin-hoist
       (hoist 'function (hoist-function-declaration decl0))
       (return (make-EmptyStatement (Term-location decl0))))]
    [(VariableDeclaration? decl0)
     (begin-hoist
       (expr <- (hoist-variable-declaration decl0))
       (return (make-ExpressionStatement (Term-location decl0) expr)))]
    [(LetDeclaration? decl0)
     (begin-hoist
       (expr <- (hoist-let-declaration decl0))
       (return (make-ExpressionStatement (Term-location decl0) expr)))]
    [(ImportDeclaration? decl0)
     (begin-hoist
       (hoist 'top decl0)
       (return (make-EmptyStatement (Term-location decl0))))]
    [(ExportDeclaration? decl0)
     (begin-hoist
       (hoist 'top decl0)
       (return (make-EmptyStatement (Term-location decl0))))]
    [else (error 'hoist-declaration (format "unrecognized declaration: ~v" decl0))]))

;; hoist-variable-declaration : VariableDeclaration -> (H Expression)
(define (hoist-variable-declaration decl0)
  (match decl0
    [(struct VariableDeclaration (loc bindings))
     (hoist-var-initializers bindings loc)]))

;; hoist-let-declaration : LetDeclaration -> (H Expression)
(define (hoist-let-declaration decl0)
  (match decl0
    [(struct LetDeclaration (loc bindings))
     (hoist-let-var-initializers bindings loc)]))

;; hoist-substatement : SourceElement -> (H (optional Statement))
(define (hoist-substatement src0)
  (hoist-source-element src0))

;; hoist-var-initializer : VariableInitializer -> (H (optional Expression))
(define (hoist-var-initializer decl)
  (match decl
    [(struct VariableInitializer (loc id #f))
     (begin-hoist
       (hoist 'function id)
       (return #f))]
    [(struct VariableInitializer (loc id init))
     (begin-hoist
       (hoist 'function id)
       (init <- (hoist-expression init))
       (return (make-AssignmentExpression loc (make-VarReference (Term-location id) id) '= init)))]))

;; hoist-let-var-initializer : VariableInitializer -> (H (optional Expression))
(define (hoist-let-var-initializer decl)
  (match decl
    [(struct VariableInitializer (loc id #f))
     (begin-hoist
       (hoist 'block id)
       (return #f))]
    [(struct VariableInitializer (loc id init))
     (begin-hoist
       (hoist 'block id)
       (init <- (hoist-expression init))
       (return (make-AssignmentExpression loc (make-VarReference (Term-location id) id) '= init)))]))

;; hoist-case-clause : CaseClause -> (H CaseClause)
(define (hoist-case-clause case)
  (match case
    [(struct CaseClause (loc #f answer))
     (begin-hoist
       (answer <- (hoist-substatements answer))
       (return (make-CaseClause loc #f answer)))]
    [(struct CaseClause (loc question answer))
     (begin-hoist
       (question <- (hoist-expression question))
       (answer <- (hoist-substatements answer))
       (return (make-CaseClause loc question answer)))]))

;; hoist-catch-clause : CatchClause -> (H CatchClause)
(define (hoist-catch-clause catch)
  (match catch
    [(struct CatchClause (loc id body0))
     (begin-hoist
       (body0 <- (hoist-statement body0))
       (return (make-CatchClause loc id body0)))]))

;; hoist-statement : Statement -> (H Statement)
(define (hoist-statement stmt)
  (match stmt
    [(struct BlockStatement (loc stmts))
     (begin-hoist
       (result <- (capture 'block (hoist-substatements stmts)))
       (match-let ([(list hs stmts) result])
         (let-values ([(vars funs) (partition Identifier? (map hoisted-element hs))])
           (return (make-BlockStatement/hoisted loc stmts funs vars)))))]
    [(struct ExpressionStatement (loc expr))
     (begin-hoist
       (expr <- (hoist-expression expr))
       (return (make-ExpressionStatement loc expr)))]
    [(struct IfStatement (loc test consequent0 #f))
     (begin-hoist
       (test <- (hoist-expression test))
       (consequent <- (hoist-substatement consequent0))
       (return (make-IfStatement loc test (optional-statement->statement consequent consequent0) #f)))]
    [(struct IfStatement (loc test consequent0 alternate))
     (begin-hoist
       (test <- (hoist-expression test))
       (consequent <- (hoist-substatement consequent0))
       (alternate <- (hoist-substatement alternate))
       (return (make-IfStatement loc test (optional-statement->statement consequent consequent0) alternate)))]
    [(struct DoWhileStatement (loc body0 test))
     (begin-hoist
       (body <- (hoist-substatement body0))
       (test <- (hoist-expression test))
       (return (make-DoWhileStatement loc (optional-statement->statement body body0) test)))]
    [(struct WhileStatement (loc test body0))
     (begin-hoist
       (test <- (hoist-expression test))
       (body <- (hoist-substatement body0))
       (return (make-WhileStatement loc test (optional-statement->statement body body0))))]
    [(struct ForStatement (loc (? VariableDeclaration? init) test incr body0))
     (begin-hoist
       (init <- (hoist-variable-declaration init))
       (test <- (hoist-optional-expression test))
       (incr <- (hoist-optional-expression incr))
       (body <- (hoist-substatement body0))
       (return (make-ForStatement loc init test incr (optional-statement->statement body body0))))]
    ;; NOTE: this is a macro-expansion
    [(struct ForStatement (loc (struct LetDeclaration (loc* bindings)) test incr body0))
     (let ([bindings* (for/list ([binding bindings])
                        (struct-copy VariableInitializer binding [init #f]))]
           [inits* (make-ListExpression loc*
                                        (for/list ([binding bindings])
                                          (match binding
                                            [(struct VariableInitializer (loc id init))
                                             (let ([var (make-VarReference (Term-location id) id)])
                                               (make-AssignmentExpression loc var '= init))])))])
       ;; TODO: is it a problem that this indiscriminately makes a for-let into a substatement?
       (hoist-statement (make-LetStatement loc bindings* (make-ForStatement loc inits* test incr body0))))]
    [(struct ForStatement (loc init test incr body0))
     (begin-hoist
       (init <- (hoist-optional-expression init))
       (test <- (hoist-optional-expression test))
       (incr <- (hoist-optional-expression incr))
       (body <- (hoist-substatement body0))
       (return (make-ForStatement loc init test incr (optional-statement->statement body body0))))]
    [(struct ForInStatement (loc (? Expression? lhs) container body0))
     (begin-hoist
       (lhs <- (hoist-expression lhs))
       (container <- (hoist-expression container))
       (body <- (hoist-substatement body0))
       (return (make-ForInStatement loc lhs container (optional-statement->statement body body0))))]
    [(struct ForInStatement (loc (struct VariableDeclaration (_ (list (struct VariableInitializer (v-loc id #f))))) container body0))
     (begin-hoist
       (hoist 'function id)
       (container <- (hoist-expression container))
       (body <- (hoist-substatement body0))
       (return (make-ForInStatement loc
                                    (make-VarReference v-loc id)
                                    container
                                    (optional-statement->statement body body0))))]
    ;; NOTE: this is a macro expansion
    [(struct ForInStatement (loc (struct LetDeclaration (_ (list (struct VariableInitializer (v-loc id #f))))) container body0))
     (hoist-statement (make-LetStatement loc
                                         (list (make-VariableInitializer v-loc id #f))
                                         (make-ForInStatement loc
                                                              (make-VarReference (Term-location id) id)
                                                              container
                                                              body0)))]
    [(struct ReturnStatement (loc expr))
     (begin-hoist
       (expr <- (hoist-optional-expression expr))
       (return (make-ReturnStatement loc expr)))]
    [(struct LetStatement (loc (list (struct VariableInitializer (binding-locs binding-names binding-inits)) ...) body0))
     (begin-hoist
       (binding-inits <- (map/m hoist-optional-expression binding-inits))
       (body <- (hoist-substatement body0))
       (return (make-LetStatement loc
                                  (map make-VariableInitializer binding-locs binding-names binding-inits)
                                  (optional-statement->statement body body0))))]
    [(struct WithStatement (loc obj body0))
     (begin-hoist
       (obj <- (hoist-expression obj))
       (body <- (hoist-substatement body0))
       (return (make-WithStatement loc obj (optional-statement->statement body body0))))]
    [(struct SwitchStatement (loc expr cases))
     (begin-hoist
       (expr <- (hoist-expression expr))
       (cases <- (hoist-case-clauses cases))
       (return (make-SwitchStatement loc expr cases)))]
    [(struct LabelledStatement (loc label stmt0))
     (begin-hoist
       (stmt <- (hoist-substatement stmt0))
       (return (make-LabelledStatement loc label (optional-statement->statement stmt stmt0))))]
    [(struct ThrowStatement (loc expr))
     (begin-hoist
       (expr <- (hoist-expression expr))
       (return (make-ThrowStatement loc expr)))]
    [(struct TryStatement (loc body0 catch0 finally0))
     (begin-hoist
       (body <- (hoist-statement body0))
       (catch <- (hoist-catch-clauses catch0))
       (if finally0
           (begin-hoist
             (finally <- (hoist-statement finally0))
             (return (make-TryStatement loc body catch finally)))
           (return (make-TryStatement loc body catch #f))))]
    [_ (return stmt)]))

;; hoist-expression-block : ExpressionBlock -> (H ExpressionBlock)
(define (hoist-expression-block block)
  (match block
    [(struct ExpressionBlock (loc body tail))
     (begin-hoist
       (body <- (hoist-statement body))
       (tail <- (hoist-optional-expression tail))
       (return (make-ExpressionBlock loc body tail)))]))

;; hoist-expression : Expression -> (H Expression)
(define (hoist-expression expr)
  (match expr
    [(struct ArrayLiteral (loc elts))
     (begin-hoist
       (elts <- (map/m hoist-optional-expression elts))
       (return (make-ArrayLiteral loc elts)))]
    [(struct ObjectLiteral (loc (list (cons props vals) ...)))
     (begin-hoist
       (vals <- (map/m hoist-expression vals))
       (return (make-ObjectLiteral loc (map cons props vals))))]
    [(struct BracketReference (loc container key))
     (begin-hoist
       (container <- (hoist-expression container))
       (key <- (hoist-expression key))
       (return (make-BracketReference loc container key)))]
    [(struct DotReference (loc container id))
     (begin-hoist
       (container <- (hoist-expression container))
       (return (make-DotReference loc container id)))]
    [(struct NewExpression (loc ctor args))
     (begin-hoist
       (ctor <- (hoist-expression ctor))
       (args <- (map/m hoist-expression args))
       (return (make-NewExpression loc ctor args)))]
    [(struct PostfixExpression (loc expr op))
     (begin-hoist
       (expr <- (hoist-expression expr))
       (return (make-PostfixExpression loc expr op)))]
    [(struct PrefixExpression (loc op expr))
     (begin-hoist
       (expr <- (hoist-expression expr))
       (return (make-PrefixExpression loc op expr)))]
    [(struct InfixExpression (loc left op right))
     (begin-hoist
       (left <- (hoist-expression left))
       (right <- (hoist-expression right))
       (return (make-InfixExpression loc left op right)))]
    [(struct ConditionalExpression (loc test consequent alternate))
     (begin-hoist
       (test <- (hoist-expression test))
       (consequent <- (hoist-expression consequent))
       (alternate <- (hoist-expression alternate))
       (return (make-ConditionalExpression loc test consequent alternate)))]
    [(struct AssignmentExpression (loc left op right))
     (begin-hoist
       (left <- (hoist-expression left))
       (right <- (hoist-expression right))
       (return (make-AssignmentExpression loc left op right)))]
    [(struct BlockLiteral (loc args body))
     (begin-hoist
       ;(body <- (hoist-statement body))
       (body <- (hoist-expression-block body))
       (return (make-BlockLiteral loc args body)))]
    [(? FunctionExpression?)
     ;; XXX: capture everything and keep going?
     (return (hoist-function-expression expr))]
    [(struct LetExpression (loc (list (struct VariableInitializer (binding-locs binding-names binding-inits)) ...) body))
     (begin-hoist
       (binding-inits <- (map/m hoist-optional-expression binding-inits))
       (body <- (hoist-expression body))
       (return (make-LetExpression loc (map make-VariableInitializer binding-locs binding-names binding-inits) body)))]
    [(struct CallExpression (loc method args))
     (begin-hoist
       (method <- (hoist-expression method))
       (args <- (map/m hoist-expression args))
       (return (make-CallExpression loc method args)))]
    [(struct ParenExpression (loc expr))
     (begin-hoist
       (expr <- (hoist-expression expr))
       (return (make-ParenExpression loc expr)))]
    [(struct ListExpression (loc exprs))
     (begin-hoist
       (exprs <- (map/m hoist-expression exprs))
       (return (make-ListExpression loc exprs)))]
    [(struct DoExpression (loc block))
     (begin-hoist
       (block <- (hoist-expression-block block))
       (return (make-DoExpression loc block)))]
    [_ (return expr)]))

;; hoist-expression : Expression -> Expression
;(define (hoist-expression expr)
;  (match expr
;    [(struct ArrayLiteral (loc elts))
;     (make-ArrayLiteral loc (map hoist-optional-expression elts))]
;    [(struct ObjectLiteral (loc (list (cons props vals) ...)))
;     (make-ObjectLiteral loc (map cons props (map hoist-expression vals)))]
;    [(struct BracketReference (loc container key))
;     (make-BracketReference loc (hoist-expression container) (hoist-expression key))]
;    [(struct DotReference (loc container id))
;     (make-DotReference loc (hoist-expression container) id)]
;    [(struct NewExpression (loc ctor args))
;     (make-NewExpression loc (hoist-expression ctor) (map hoist-expression args))]
;    [(struct PostfixExpression (loc expr op))
;     (make-PostfixExpression loc (hoist-expression expr) op)]
;    [(struct PrefixExpression (loc op expr))
;     (make-PrefixExpression loc op (hoist-expression expr))]
;    [(struct InfixExpression (loc left op right))
;     (make-InfixExpression loc (hoist-expression left) op (hoist-expression right))]
;    [(struct ConditionalExpression (loc test consequent alternate))
;     (make-ConditionalExpression loc
;                                 (hoist-expression test)
;                                 (hoist-expression consequent)
;                                 (hoist-expression alternate))]
;    [(struct AssignmentExpression (loc left op right))
;     (make-AssignmentExpression loc
;                                (hoist-expression left)
;                                op
;                                (hoist-expression right))]
;;    [(struct BlockLiteral (loc args body))
;;     (hoist-statement body (lambda (h body)
;;                             (
;    [(? FunctionExpression?)
;     (hoist-function-expression expr)]
;    [(struct LetExpression (loc (list (struct VariableInitializer (binding-locs binding-names binding-inits)) ...) body))
;     (make-LetExpression loc
;                         (map make-VariableInitializer binding-locs binding-names (map hoist-optional-expression binding-inits))
;                         (hoist-expression body))]
;    [(struct CallExpression (loc method args))
;     (make-CallExpression loc (hoist-expression method) (map hoist-expression args))]
;    [(struct ParenExpression (loc expr))
;     (make-ParenExpression loc (hoist-expression expr))]
;    [(struct ListExpression (loc exprs))
;     (make-ListExpression loc (map hoist-expression exprs))]
;    [_ expr]))

(provide hoist-program-unit
         hoist-function-expression
         (struct-out FunctionDeclaration/hoisted)
         (struct-out FunctionExpression/hoisted)
         (struct-out BlockStatement/hoisted))
