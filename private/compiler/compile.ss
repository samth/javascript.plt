#lang scheme/base

(require (except-in srfi/1/list any)
         (only-in scheme/list flatten)
         scheme/string
         scheme/match
         scheme/require-transform
         scheme/promise
         "../syntax/ast-core.ss"
         "../syntax/ast-utils.ss"
         "../syntax/token.ss"
         "../syntax/exceptions.ss"
         ;"../../private/config.ss"
         "../runtime/runtime.ss"
         "../../debug.ss"
         "../syntax/parse.ss"
         "../planet.ss"
         "helpers.ss"
         "hoist.ss"
         "context.ss")
(require (for-syntax scheme/require-transform)
         (for-syntax scheme/base))
(require (for-template scheme/base)
         (for-template "../runtime/runtime.ss")
         (for-template "../evector.ss")
         (for-template "../syntax/parse.ss"))

(provide compile-module compile-script compile-global compile-interaction compile-function-expression with-syntax-errors)

;; TODO: optimizations
;;   - don't capture return continuation if it's not used
;;   - remove unnecessary `void' if last statement in function is not an expression statement

(define-syntax-rule (syntax/loc* loc expr)
  (syntax/loc (location->syntax loc)
    expr))

;; loop? : Statement -> boolean
(define (loop? stmt)
  (or (DoWhileStatement? stmt)
      (WhileStatement? stmt)
      (ForStatement? stmt)
      (ForInStatement? stmt)))

(define (with-syntax-errors thunk)
  (with-handlers ([exn:fail:syntax?
                   (lambda (exn)
                     (let* ([loc (exn:fail:syntax-location exn)]
                            [text (format "~a" (exn:fail:syntax-text exn))]
                            [stxloc (build-syntax (string->symbol text) loc)])
                       (raise-syntax-error 'parse (exn-message exn) stxloc stxloc)))])
    (thunk)))

(define (module-declaration-context?)
  (and (eq? (current-compilation-context) 'module)
       (eq? (current-lexical-context) 'top)
       (not (current-nested?))))

;; (listof Variable) -> (listof syntax<expression>)
(define (default-inits vars)
  (for/list ([var vars])
    #'(void)))

;; (listof Variable) syntax [#:inits (listof syntax)] -> syntax
(define (with-bindings vars #:inits [inits (default-inits vars)] stx)
  (cond
    [(null? vars) stx]
    [(and (eq? (current-compilation-context) 'module)
          (eq? (current-lexical-context) 'top)
          (not (current-nested?)))
     (with-module-bindings vars #:inits inits stx)]
    [(current-scope)
     (with-lexical-bindings vars #:inits inits stx)]
    [else (with-dynamic-bindings vars #:inits inits stx)]))

(define (with-module-bindings vars #:inits [inits (default-inits vars)] stx)
  (if (null? vars)
      stx
      (with-syntax ([(x ...) (map Variable-compiled vars)]
                    [(e ...) inits]
                    [body stx])
        (syntax/loc stx
          (begin
            (define x e) ...
            body)))))

(define (with-lexical-bindings vars #:inits [inits (default-inits vars)] stx)
  (if (null? vars)
      stx
      (with-syntax ([(x ...) (map Variable-compiled vars)]
                    [(e ...) (or inits (map (lambda (v) #'(void)) vars))]
                    [body stx])
        (syntax/loc stx
          (let ([x e] ...) body)))))

(define (with-dynamic-bindings vars #:inits [inits (default-inits vars)] stx #:variable-object? [variable-object? #f])
  (if (null? vars)
      stx
      (with-syntax ([scope-chain scope-chain-id]
                    [(prop ...)
                     (for/list ([var vars] [init inits])
                       (with-syntax ([x (Variable-compiled var)]
                                     [e init])
                         #'[x e]))]
                    [frame (if variable-object? variable-object-id (car (generate-temporaries '(frame))))]
                    [body stx])
        (syntax/loc stx
          (let* ([frame (make-frame (object-table prop ...))]
                 [scope-chain (cons frame scope-chain)])
            body)))))

;; ===========================================================================
;; MODULE COMPILATION
;; ===========================================================================

;; Identifier -> Variable
(define (Identifier->Variable id)
  (make-Variable id (Identifier->syntax id #:context (current-source-syntax))))

;; Identifier Identifier ModuleSpecifier module-path -> Import
(define (Identifier->Import internal external module-spec module-path)
  (let ([id (Identifier->syntax internal #:context (current-source-syntax))])
    (make-Import internal id module-spec module-path (delay (eval-import? module-path (Identifier-name external))))))

;; module-path symbol -> identifier
(define (resolve-import path sym)
  (let-values ([(imports sources) (expand-import (with-syntax ([path path]) #'path))])
    (cond
      [(findf (lambda (import) (eq? (import-src-sym import) sym)) imports)
       => (lambda (import)
            (import-local-id import))]
      [else #f])))

;; TODO: any dangers in having this global mutable cell?

;; (promiseof identifier)
(define eval-import
  (delay (resolve-import standard-library-path 'eval)))

;; module-path symbol -> boolean
(define (eval-import? module-path sym)
  (cond
    [(resolve-import module-path sym)
     => (lambda (id)
          (free-identifier=? id (force eval-import)))]
    [else #f]))

;; (listof Variable) -> ExportDeclaration -> syntax<provide>
(define ((compile-export all-defined) export)
  (unless (module-declaration-context?)
    (raise-syntax-error 'compile "illegal context for export statement" (location->syntax (Term-location export))))
  (match export
    [(struct ExportDeclaration (loc export-specs))
     (with-syntax ([(spec ...) (map (compile-export-spec all-defined) export-specs)])
       (syntax/loc* loc
         (provide spec ...)))]))

;; (listof Variable) -> (union ExclusionList ReexportSpecifier ExportBindings Identifier) -> syntax<provide-spec>
(define ((compile-export-spec all-defined) spec)
  (match spec
    [(? Identifier?)
     (let ([symbol (Identifier-name spec)])
       (cond
         [(findf (lambda (variable)
                   (eq? (Identifier-name (Variable-source variable)) symbol))
                 all-defined)
          => Variable-compiled]
         [else
          (raise-syntax-error 'compile (format "unbound export variable: ~a" symbol) (location->syntax (Term-location spec)))]))]
    [(struct ExportBindings (loc bindings))
     (raise-syntax-error 'compile "multiple-identifier export not yet implemented" (location->syntax loc))]
    [(struct ReexportSpecifier (loc module exclusions))
     (raise-syntax-error 'compile "module re-export not yet implemented" (location->syntax loc))]
    [(struct ExclusionList (loc (list)))
     (with-syntax ([(x ...) (map Variable-compiled all-defined)])
       (syntax/loc* loc
         (combine-out x ...)))]
    [(struct ExclusionList (loc (list exclude ...)))
     (raise-syntax-error 'compile "exclusion list not yet implemented" (location->syntax loc))]))

;; ModuleSpecifier * module-path * ImportBinding -> syntax<require-spec>
;;                                                  Import
(define (compile-import-binding module-spec module-path binding)
  (match binding
    [(struct ImportBinding (loc label #f))
     (compile-import-binding module-spec module-path (make-ImportBinding loc label label))]
    [(struct ImportBinding (loc label internal))
     (let ([internal-id (Identifier->syntax internal #:context (current-source-syntax))]
           ;; NOTE: this has to match the lexical context of the `rename-in' identifier
           [external-id (Identifier->syntax label #:context #'here)])
       (values (with-syntax ([module-path module-path]
                             [internal internal-id]
                             [external external-id])
                 (syntax/loc* loc (rename-in module-path [external internal])))
               (make-Import internal internal-id module-spec module-path (delay (eval-import? module-path (Identifier-name label))))))]))

;; (listof (union string symbol number #f)) region -> module-path
(define (parse-planet-path path loc)
  (define ((maybe p?) x)
    (or (p? x) (not x)))
  (match path
    [(list (? string? symbolic-path))
     `(planet ,(string->symbol symbolic-path))]
    [(list (? string? user) (? string? package))
     (parse-planet-path `(,user ,package #f #f "main.ss"))]
    [(list (? string? user) (? string? package) (? (maybe number?) major))
     (parse-planet-path `(,user ,package ,major #f "main.ss"))]
    [(list (? string? user) (? string? package) (? (maybe number?) major) (? (maybe number?) minor) (? (maybe string?) path))
     (let* ([path (reverse (regexp-split #rx"/" (or path "main.ss")))]
            [file (car path)]
            [subpath (reverse (cdr path))]
            [major (or (and major (list major)) '())]
            [minor (or (and minor (list minor)) '())])
       `(planet ,file (,user ,package ,@major ,@minor) ,@subpath))]
    [(list path ...)
     (raise-syntax-error 'compile
                         (format "planet protocol: expects (string) or (string, string[, [number][, [number][, [string]]]]); received ~a" path)
                         (location->syntax loc))]))

;; ModuleSpecifier -> module-path
(define (compile-module-spec spec)
  (match spec
    [(struct ModuleSpecifier (loc 'file (list (? string? path))))
     path]
    [(struct ModuleSpecifier (loc 'planet (list path ...)))
     (parse-planet-path path loc)]
    [(struct ModuleSpecifier (loc 'collect (list 'js)))
     standard-library-path]
    [(struct ModuleSpecifier (loc 'collect (list path ...)))
     (string->symbol (string-join (map symbol->string path) "/"))]))

;; ImportSpecifier -> syntax<require-spec>
;;                    (listof Import)
(define (compile-import-spec spec)
  (match spec
    [(struct ImportSpecifier (loc module bindings))
     (match bindings
       [(? Identifier?)
        (let ([loc (Term-location bindings)])
          (compile-import-spec (make-ImportSpecifier loc module (list (make-ImportBinding loc bindings #f)))))]
       [(struct ExclusionList (loc* (list)))
        ;; NOTE: this has to match the lexical context of the variable references
        (with-syntax ([module-path (datum->syntax (current-source-syntax) (compile-module-spec module))])
          (values #'module-path
                  (let-values ([(imports sources) (expand-import #'module-path)])
                    (for/list ([import imports])
                      (let ([id (make-Identifier loc* (syntax->datum (import-local-id import)))])
                        (Identifier->Import id id module #'module-path))))))]
       [(struct ExclusionList (_ (list excludes ...)))
        (raise-syntax-error 'compile "exclusion list not yet implemented" (location->syntax loc))]
       [(list (? ImportBinding? bindings) ...)
        (let ([module-path (compile-module-spec module)])
          (let-values ([(specs imports) (for/lists (specs imports) ([binding bindings])
                                          (compile-import-binding module module-path binding))])
            (values (with-syntax ([(spec ...) specs])
                      (syntax/loc* loc
                        (combine-in spec ...)))
                    imports)))])]))

;; ImportDeclaration -> syntax<require>
;;                      (listof Import)
(define (compile-import import)
  (unless (module-declaration-context?)
    (raise-syntax-error 'compile "illegal context for import statement" (location->syntax (Term-location import))))
  (match import
    [(struct ImportDeclaration (loc (list (? ImportSpecifier? import-specs) ...)))
     (let-values ([(requires imports) (for/lists (requires imports) ([import-spec import-specs])
                                        (compile-import-spec import-spec))])
       (with-syntax ([(require-spec ...) requires])
         (values #'(require require-spec ...)
                 (flatten imports))))]))

;; (listof ImportDeclaration) -> (listof syntax<require>)
;;                               (listof Import)
(define (compile-imports decls)
  (let-values ([(requires imports) (for/lists (requires imports) ([decl decls])
                                     (compile-import decl))])
    (values requires (flatten imports))))

(define (compile-module-declarations funs vars import-decls export-decls)
  (let-values ([(requires imports) (compile-imports import-decls)])
    (let* ([fun-ids (map FunctionDeclaration-name funs)]
           [all-defined (map Identifier->Variable (append fun-ids vars))]
           [provides (map (compile-export all-defined) export-decls)]
           [new-env (bind (append all-defined imports) (current-scope))]
           [definitions (with-syntax ([(var ...) (map Variable-compiled all-defined)]
                                      [(init-e ...) (append (with-scope new-env
                                                              (map compile-function-declaration funs))
                                                            (map (lambda (var) #'(void)) vars))])
                          (syntax->list #'((define var init-e) ...)))])
      (values requires provides definitions new-env))))

(define (compile-module elts)
  (let-values ([(funs vars imports exports body) (hoist-program-unit elts)])
    (parameterize ([current-compilation-context 'module]
                   [current-lexical-context 'top]
                   [current-nested? #f]
                   [current-pragmas (hash-set (current-pragmas) '(lexical scope) #t)]
                   [current-scope empty-scope])
      (let-values ([(requires provides definitions initial-env) (compile-module-declarations funs vars imports exports)])
        (with-syntax ([this this-id]
                      [(req ...) requires]
                      [(prov ...) provides]
                      [(defn ...) definitions]
                      [(s ...) (with-scope initial-env
                                 (map compile-statement body))])
          #'(begin
              req ...
              defn ...
              (parameterize ([current-this global-object]
                             [previous-completion nothing])
                (define this (current-this))
                s ...
                (void))
              prov ...))))))

;; ===========================================================================
;; SCRIPT COMPILATION
;; ===========================================================================

;; compile-global : (listof SourceElement) -> syntax
(define (compile-global elts)
  (let-values ([(funs vars imports exports body) (hoist-program-unit elts)])
    (when (pair? imports)
      (raise-syntax-error 'compile "illegal context for import statement" (location->syntax (Term-location (car imports)))))
    (when (pair? exports)
      (raise-syntax-error 'compile "illegal context for export statement" (location->syntax (Term-location (car exports)))))
    (parameterize ([current-lexical-context 'top]
                   [current-scope (and (not (contains-direct-eval? body))
                                       (current-scope))])
      ;; XXX: must handle empty body!
      (let ([rev-body (reverse body)])
        (let-values ([(definitions new-env) (compile-script-declarations funs vars)])
          (with-syntax ([this this-id]
                        [(defn ...) definitions]
                        [scope-chain scope-chain-id]
                        [variable-object variable-object-id]
                        [(s ...) (with-scope new-env
                                             (map compile-statement (reverse (cdr rev-body))))]
                        [last-s (with-scope new-env
                                  (compile-statement (car rev-body)))])
            #'(lambda (scope-chain variable-object this)
                (parameterize ([previous-completion nothing])
                  defn ...
                  (complete! s) ...
                  last-s))))))))

(define (compile-script elts)
  (with-syntax ([function (parameterize ([current-compilation-context 'script])
                            (compile-global elts))])
    #'(function (list global-object) global-object global-object)))

;; compile-script-declarations : (listof FunctionDeclaration/hoisted) * (listof Identifier)
;;                            -> (listof syntax)
;;                               environment
(define (compile-script-declarations funs vars)
  ;; XXX: filter out duplicate id's! else might get some weird shadowing?
  (let* ([fun-ids (map FunctionDeclaration-name funs)]
         [all-ids (append fun-ids vars)]
         [all-variables (map Identifier->Variable all-ids)]
         [new-env (bind all-variables (current-scope))]
         [definitions (with-syntax ([variable-object variable-object-id]
                                    [(var ...) (map Variable-compiled all-variables)]
                                    [(var-key ...) (map Identifier->name-syntax all-ids)]
                                    [(init-e ...) (append (with-scope new-env
                                                            (map compile-function-declaration funs))
                                                          (map (lambda (var) #'(void)) vars))])
                        (syntax->list #'((define-syntax var
                                           (syntax-id-rules (set!)
                                             [(set! var expr) (object-put! variable-object var-key expr)]
                                             ;; TODO: does this object-get need to be guarded?
                                             [var (object-get variable-object var-key)]))
                                         ...
                                         (set! var init-e) ...)))])
    (values definitions new-env)))

;; ===========================================================================
;; INTERACTION COMPILATION
;; ===========================================================================

;; TODO: was I wrong that this has to be compiled with a dynamic environment?
;;  - if a future interaction's function calls eval, it doesn't matter
;;  - if a non-function calls eval, it affects the global object only

;; compile-interaction : (listof SourceElement) -> syntax
(define (compile-interaction elt)
  (let*-values ([(funs vars imports exports stmts) (hoist-program-unit elt)]
                [(definitions new-env) (with-scope #f
                                         (compile-script-declarations funs vars))])
    (when (pair? imports)
      (raise-syntax-error 'compile "illegal context for import statement" (location->syntax (Term-location (car imports)))))
    (when (pair? exports)
      (raise-syntax-error 'compile "illegal context for export statement" (location->syntax (Term-location (car exports)))))
    (parameterize ([current-compilation-context 'interaction]
                   [current-lexical-context 'top])
      (with-syntax ([(defn ...) definitions]
                    [scope-chain scope-chain-id]
                    [variable-object variable-object-id]
                    [(s ...) (for/list ([stmt stmts])
                               (dynamic-code (with-scope #f (compile-statement stmt))
                                             (Term-location stmt)))]
                    [(saved-completion) (generate-temporaries '(saved-completion))])
        #'(begin
            (define saved-completion (previous-completion))
            (previous-completion nothing)
            (define scope-chain (list global-object))
            (define variable-object global-object)
            defn ... (complete! s) ...
            (let ([v (previous-completion)])
              (unless (eq? v nothing)
                (object-set! global-object 'it v))
              ;; TODO: wtf? isn't this identical to setting it to `nothing'?
              (previous-completion saved-completion)
              v))))))

;; ===========================================================================
;; LVALUES
;; ===========================================================================

;; reference-expression? : Expression -> boolean
(define (reference-expression? expr)
  (or (VarReference? expr)
      (BracketReference? expr)
      (DotReference? expr)))

;; compile-deletion : Expression -> syntax
(define (compile-deletion expr)
  (match expr
    [(struct VarReference (loc id))
     (cond
       [(hash-ref (current-pragmas) '(lexical scope) (lambda () #f))
        (raise-syntax-error 'compile "cannot delete lexically scoped variables" (location->syntax loc))]
       [(and (current-scope) (not (bound? id)))
        #'(quote #t)]
       [(not (current-scope))
        (with-syntax ([scope-chain scope-chain-id]
                      [key (Identifier->string-expression id)])
          (syntax/loc (location->syntax loc)
            (scope-chain-delete! scope-chain key)))]
       [else
        #'(quote #f)])]
    [(struct BracketReference (loc container key))
     (with-syntax ([container-e (compile-expression container)]
                   [key-e (compile-expression key)]
                   [(obj-val) (generate-temporaries '(obj-val))])
       (syntax/loc (location->syntax loc)
         (let ([obj-val (any->object container-e)])
           (object-delete! obj-val key-e))))]
    [(struct DotReference (loc container id))
     (with-syntax ([container-e (compile-expression container)]
                   [key-e (Identifier->string-expression id)]
                   [(obj-val) (generate-temporaries '(obj-val))])
       (syntax/loc (location->syntax loc)
         (let ([obj-val (any->object container-e)])
           (object-delete! obj-val key-e))))]))

;; compile-assignment : Expression * syntax -> syntax
(define (compile-assignment lhs rhs-stx)
  (match lhs
    [(struct VarReference (loc id))
     (debug 'scope-resolution "looking for ~a in ~v" (Identifier-name id) (current-scope))
     (cond
       [(and (current-scope) (resolve id))
        => (lambda (variable)
             (when (Import? variable)
               (raise-syntax-error 'compile (format "cannot assign to module import ~a" (Identifier-name id)) (location->syntax loc)))
             (with-syntax ([x (with-loc loc (Variable-compiled variable))]
                           [(val) (generate-temporaries '(val))]
                           [rhs-e rhs-stx])
               (syntax/loc (location->syntax loc)
                 (let ([val rhs-e])
                   (set! x val)
                   val))))]
       [(current-scope)
        (debug 'unbound-reference "~a unbound at ~a" (Identifier-name id) (region->string loc))
        (if (hash-ref (current-pragmas) '(lexical scope) (lambda () #f))
            (raise-syntax-error 'compile (format "unbound variable ~a" (Identifier-name id)) (location->syntax loc))
            (with-syntax ([key (Identifier->name-syntax id)]
                          [rhs-e rhs-stx]
                          [(val) (generate-temporaries '(val))])
              (syntax/loc (location->syntax loc)
                (let ([val rhs-e])
                  (object-put! global-object key val)
                  val))))]
       [else
        (with-syntax ([scope-chain scope-chain-id]
                      [key (Identifier->string-expression id)]
                      [rhs-e rhs-stx])
          (syntax/loc (location->syntax loc)
            (scope-chain-set! scope-chain key rhs-e)))])]
    [(struct BracketReference (loc container key))
     (with-syntax ([container-e (compile-expression container)]
                   [key-e (compile-expression key)]
                   [rhs-e rhs-stx]
                   [(container-val key-val) (generate-temporaries '(container-val key-val))])
       (syntax/loc (location->syntax loc)
         (let* ([container-val (any->object container-e)]
                [key-val key-e])
           (object-set! container-val key-val rhs-e))))]
    [(struct DotReference (loc container id))
     (with-syntax ([container-e (compile-expression container)]
                   [rhs-e rhs-stx]
                   [key-val (Identifier->string-expression id)])
       (syntax/loc (location->syntax loc)
         (let ([container-val (any->object container-e)])
           (object-set! container-val key-val rhs-e))))]
    [_ (raise-syntax-error 'compile "invalid assignment left-hand side" (location->syntax (Term-location lhs)))]))

;; compile-lookup : reference-expression -> syntax
(define (compile-lookup expr)
  (match expr
    ;; TODO: binding forms should still introduce binding arrows under with
    ;;  - add name to static environment
    ;;  - compile ref to (if #f x (dynamic-lookup 'x))
    [(struct VarReference (loc id))
     (debug 'scope-resolution "looking for ~a in ~v" (Identifier-name id) (current-scope))
     (cond
       [(and (current-scope) (resolve id))
        => (lambda (variable)
             (with-loc loc (Variable-compiled variable)))]
       [(current-scope)
        (debug 'unbound-reference "~a unbound at ~a" (Identifier-name id) (region->string loc))
        (if (hash-ref (current-pragmas) '(lexical scope) (lambda () #f))
            (raise-syntax-error 'compile (format "unbound variable ~a" (Identifier-name id)) (location->syntax loc))
            (with-syntax ([stxloc (location->syntax loc)]
                          [key (Identifier->name-syntax id)])
              (syntax/loc (location->syntax loc)
;                (or (object-get global-object key)
                (object-get global-object
                            key
                            (lambda ()
                              (raise (make-exn:fail:contract:variable (format "~a is not defined" key)
                                                                      (current-continuation-marks)
                                                                      key)))))))]
       [else
        (with-syntax ([scope-chain scope-chain-id]
                      [key (Identifier->name-syntax id)])
          (syntax/loc (location->syntax loc)
            (scope-chain-get scope-chain
                             key
                             (lambda ()
                               (raise (make-exn:fail:contract:variable (format "~a is not defined" key)
                                                                       (current-continuation-marks)
                                                                       key))))))])]
    [(struct BracketReference (loc container key))
     (with-syntax ([container-e (compile-expression container)]
                   [key-e (compile-expression key)]
                   [(container-val) (generate-temporaries '(container-val))])
       (syntax/loc (location->syntax loc)
         (let ([container-val (any->object container-e)])
           (object-get container-val
                       key-e
                       void))))]
;           (or (object-get container-val key-e) (void)))))]
    [(struct DotReference (loc container id))
     (with-syntax ([container-e (compile-expression container)]
                   [key (Identifier->name-syntax id)]
                   [(container-val) (generate-temporaries '(container-val))])
       (syntax/loc (location->syntax loc)
         (let ([container-val (any->object container-e)])
           (object-get container-val
                       key
                       void))))]))
;           (or (object-get container-val key-val) (void)))))]))

;; ===========================================================================
;; COMPILER CORE
;; ===========================================================================

;; TODO: these two are inconsistent wrt their interfaces; the first hoists, the second expects hoisting to be done already

;; compile-function-expression : FunctionExpression -> syntax
(define (compile-function-expression expr)
  (match (hoist-function-expression expr)
    [(struct FunctionExpression/hoisted (loc name args body funs vars imports exports))
     (compile-function loc name args body funs vars #t)]))

(define (compile-function-declaration decl)
  (match decl
    [(struct FunctionDeclaration/hoisted (loc name args body funs vars imports exports))
     (compile-function loc name args body funs vars #f)]))

(define (compile-expression-block block)
  (match block
    [(struct ExpressionBlock (loc (struct BlockStatement/hoisted (loc* stmts funs vars)) tail))
     (let ([tail (or tail (make-PrefixExpression loc* 'void (make-NullLiteral loc*)))])
       (compile-statement (make-BlockStatement/hoisted loc* (append stmts (list (make-ExpressionStatement (@ tail tail) tail))) funs vars)))]))

(define (compile-block-literal loc args body)
  (let ([arg-bindings (map Identifier->Variable args)])
    (with-syntax ([body (parameterize ([current-scope (bind arg-bindings (current-scope))])
                          (compile-expression-block body))]
                  [arity (length args)])
      (if (current-scope)
          (with-syntax ([(x ...) (map Variable-compiled arg-bindings)])
            (syntax/loc* loc
              (build-function arity (lambda (x ...) body))))
          ;; XXX: implement this for dynamic mode
          (raise-syntax-error 'compile-block-literal "not yet implemented" (location->syntax loc))))))

;; TODO: handle return more gracefully (particularly invalid returns)

;; compiled-function : (optional region) (optional Identifier) (listof Identifier) (listof Statement) (listof FunctionDeclaration/hoisted) (listof Identifier) boolean -> syntax
(define (compile-function loc name args body funs vars expression?)
  (let ([arity (length args)])
    (with-syntax ([(func-object arg-list args-object) (generate-temporaries '(func-object arg-list args-object))])
      (let ([arg-bindings (map Identifier->Variable args)]
            [fun-bindings (map Identifier->Variable (filter-map FunctionDeclaration-name funs))]
            [var-bindings (map Identifier->Variable (cons (make-Identifier loc 'arguments) vars))]
            [name-binding (map Identifier->Variable (if name (list name) null))]
            [dynamic? (or (not (current-scope))
                          (and (not (hash-ref (current-pragmas) '(lexical scope) (lambda () #f)))
                               (contains-direct-eval? body)))])
        (let ([still-lexically-scoped? (and (current-scope) (not dynamic?))]
              [new-static-env (bind arg-bindings
                                (bind fun-bindings
                                  (bind var-bindings
                                    (bind name-binding (current-scope)))))]
              [arg-refs (map (lambda (arg)
                               (make-VarReference (Term-location arg) arg))
                             args)]
              [arguments-ref (make-VarReference loc (make-Identifier loc 'arguments))]
              [all-local-bindings (delete-duplicates (append arg-bindings fun-bindings var-bindings) Variable=?)])
          (with-syntax ([this this-id]
                        [return (datum->syntax #f 'return)]
                        [(s ...) (parameterize ([current-lexical-context 'function]
                                                [current-nested? #f]
                                                [current-scope (and still-lexically-scoped? new-static-env)])
                                   (map compile-statement body))]
                        [set-arguments-object!
                         (parameterize ([current-scope (and still-lexically-scoped? new-static-env)])
                           (with-syntax ([(getter ...) (map (lambda (arg-ref)
                                                              #`(lambda ()
                                                                  #,(compile-lookup arg-ref)))
                                                            arg-refs)]
                                         [(setter ...) (map (lambda (arg-ref)
                                                              (with-syntax ([v (car (generate-temporaries '(v)))])
                                                                #`(lambda (v)
                                                                    #,(compile-assignment arg-ref #'v))))
                                                            arg-refs)])
                             (compile-assignment arguments-ref
                                                 #'(build-arguments-object func-object
                                                                           (list (cons getter setter) ...)
                                                                           arg-list))))]
                        [set-arguments!
                         (parameterize ([current-scope (and still-lexically-scoped? new-static-env)])
                           (with-syntax ([(rhs ...) (map (lambda (arg-ref)
                                                           (with-syntax ([set-undefined! (compile-assignment arg-ref #'(void))]
                                                                         [set-next! (compile-assignment arg-ref #'(car arg-list))])
                                                             #'(if (null? arg-list)
                                                                   (begin set-undefined! '())
                                                                   (begin set-next! (cdr arg-list)))))
                                                         arg-refs)])
                             #'(let* ([arg-list rhs] ...)
                                 (void))))]
                        [(set-nested-func! ...)
                         (parameterize ([current-scope (and still-lexically-scoped? new-static-env)])
                           (map (lambda (fun)
                                  (compile-assignment (make-VarReference (Term-location fun) (FunctionDeclaration-name fun))
                                                      (compile-function-declaration fun)))
                                funs))])
            (let ([block-stx (syntax/loc (location->syntax loc)
                               (let ([this (current-this)])
                                 set-arguments-object!
                                 set-arguments!
                                 set-nested-func! ...
                                 (parameterize ([previous-completion nothing])
                                   (let/ec return
                                     (complete! s) ...
                                     (void)))))])
              (with-syntax ([body (if (or dynamic? (not (current-scope)))
                                      (dynamic-code (with-dynamic-bindings all-local-bindings block-stx #:variable-object? #t) loc)
                                      (with-lexical-bindings all-local-bindings block-stx))]
                            [arity arity])
                (if (and name expression?)
                    (with-syntax ([set-f! (parameterize ([current-scope new-static-env])
                                            (compile-assignment (make-VarReference loc name)
                                                                #'func-object))])
                      (with-bindings name-binding
                       (syntax/loc (location->syntax loc)
                         (letrec ([func-object (build-function arity (lambda arg-list body))])
                           set-f!
                           func-object))))
                    (syntax/loc (location->syntax loc)
                      (letrec ([func-object (build-function arity (lambda arg-list body))])
                        func-object)))))))))))

(define (dynamic-code body-stx [loc #f] [extend-scope-chain (lambda (scope-chain) scope-chain)] [shadow-loc #f])
  (with-syntax ([scope-chain scope-chain-id]
                [body-e body-stx])
    (if (not (current-scope))
        (with-syntax ([scope-e (extend-scope-chain scope-chain-id)])
          (syntax/loc (location->syntax loc)
            (let ([scope-chain scope-e])
              body-e)))
        (let ([variables (for/list ([var (in-hash-values (current-scope))]) var)])
          (with-syntax ([(x ...) (map Variable-compiled variables)]
                        [(setter ...) (for/list ([variable variables])
                                        (with-syntax ([x (Variable-compiled variable)]
                                                      [v (car (generate-temporaries '(v)))])
                                          (if (Import? variable)
                                              #'(lambda (v) v)
                                              #'(lambda (v) (set! x v) v))))]
                        [(x-name ...) (map (compose Identifier-name Variable-source) variables)])
            (with-syntax ([scope-e (extend-scope-chain
                                    #'(list (make-frame
                                             (object-table
                                              [x-name (lambda () x) setter ()]
                                              ...))
                                            global-object))])
              (syntax/loc (location->syntax loc)
                (let ([scope-chain scope-e])
;                  (let-syntax ([shadow-x (syntax-id-rules (set!) ;; XXX: is this code dead?
;                                           [(set! shadow-x expr) (scope-chain-set! scope-chain x-name expr)]
;                                           [shadow-x (or (scope-chain-get scope-chain 'x-name)
;                                                         (raise (make-exn:fail:contract:variable
;                                                                 (format '"~a is not defined" 'x-name)
;                                                                 (current-continuation-marks)
;                                                                 (string->symbol 'x-name))))])]
;                               ...)
                    body-e))))))))

(define (compile-statement stmt)
  (if (BlockStatement/hoisted? stmt)
      (compile-top-level-statement stmt)
      (compile-nested-statement stmt)))

(define (compile-top-level-statement stmt)
  (match stmt
    [(struct BlockStatement/hoisted (loc stmts funs vars))
     (if (null? stmts)
         (syntax/loc (location->syntax loc)
           (previous-completion))
         (let ([rev-stmts (reverse stmts)]
               [var-bindings (map Identifier->Variable vars)]
               [fun-bindings (map (compose Identifier->Variable FunctionDeclaration-name) funs)])
           (let ([new-static-env (bind var-bindings
                                       (bind fun-bindings (current-scope)))])
             (with-syntax ([(f ...) (map Variable-compiled fun-bindings)]
                           [(fe ...) (parameterize ([current-nested? #t])
                                       (with-scope new-static-env
                                         (map compile-function-declaration funs)))]
                           [(s ...) (with-scope new-static-env
                                      (map compile-statement (reverse (cdr rev-stmts))))]
                           [last-s (with-scope new-static-env
                                     (compile-statement (car rev-stmts)))])
               (with-bindings var-bindings
                 (with-bindings fun-bindings
                   (syntax/loc (location->syntax loc)
                     (parameterize ([previous-completion nothing])
                       (set! f fe) ...
                       (complete! s) ...
                       last-s))))))))]))

(define (compile-nested-statement stmt)
  (parameterize ([current-nested? #t])
    (match stmt
      [(struct EmptyStatement (loc))
       (syntax/loc* loc
         (previous-completion))]
      [(struct ExpressionStatement (loc expr))
       (compile-expression expr)]
;       (with-syntax ([e (compile-expression expr)])
;         (syntax/loc* loc
;           (complete! e)))]
      [(struct IfStatement (loc test consequent alternate))
       (with-syntax ([test-e (compile-expression test)]
                     [consequent-s (compile-statement consequent)]
                     [alternate-s (if alternate (compile-statement alternate) #'nothing)])
         (syntax/loc* loc
           (if (any->boolean test-e)
               consequent-s
               alternate-s)))]
      [(? loop?)
       (with-syntax ([(break continue) (generate-temporaries '(break continue))])
         (parameterize ([current-labels (cons (list #f #'break #'continue)
                                              (current-labels))])
           (compile-loop stmt #'break #'continue)))]
      [(struct ContinueStatement (loc #f))
       (cond
         [(ormap (lambda (tuple)
                   (and (pair? (cddr tuple))
                        (caddr tuple)))
                 (current-labels))
          => (lambda (continue-id)
               (with-syntax ([continue continue-id])
                 (syntax/loc* loc
                   (continue (previous-completion)))))]
         [else (let ([stxloc (build-syntax 'continue loc)])
                 (raise-syntax-error 'continue "invalid continue" stxloc stxloc))])]
      [(struct ContinueStatement (loc label))
       (cond
         [(null? (current-labels))
          (raise-syntax-error 'continue "invalid continue" (build-syntax 'continue loc))]
         [(assq (Identifier-name label) (current-labels))
          => (lambda (tuple)
               (if (pair? (cddr tuple))
                   (with-syntax ([continue (caddr tuple)])
                     (syntax/loc* loc
                       (continue (previous-completion))))
                   (raise-syntax-error 'continue "invalid label" (location->syntax (Term-location label)))))]
         [else (raise-syntax-error 'continue "invalid label" (location->syntax (Term-location label)))])]
      [(struct BreakStatement (loc #f))
       (when (null? (current-labels))
         (let ([stxloc (build-syntax 'break loc)])
           (raise-syntax-error 'break "invalid break" stxloc stxloc)))
       (with-syntax ([break (cadar (current-labels))])
         (syntax/loc* loc
           (break (previous-completion))))]
      [(struct BreakStatement (loc label))
       (cond
         [(null? (current-labels))
          (raise-syntax-error 'break "invalid break" (build-syntax 'break loc))]
         [(assq (Identifier-name label) (current-labels))
          => (lambda (tuple)
               (with-syntax ([break (cadr tuple)])
                 (syntax/loc* loc
                   (break (previous-completion)))))]
         [else (raise-syntax-error 'break "invalid label" (location->syntax (Term-location label)))])]
      [(struct ReturnStatement (loc value))
       (unless (eq? (current-lexical-context) 'function)
         (let ([stxloc (build-syntax 'return loc)])
           (raise-syntax-error 'return "invalid return" stxloc stxloc)))
       (with-syntax ([return (datum->syntax #f 'return)]
                     [e (if value
                            (compile-expression value)
                            #'(void))])
         (syntax/loc* loc
           (return e)))]
      [(struct LetStatement (loc bindings body))
       (let ([var-bindings (map (compose Identifier->Variable VariableInitializer-id) bindings)]
             [inits (map (compose compile-optional-expression VariableInitializer-init) bindings)])
         (with-syntax ([body (with-scope (bind var-bindings (current-scope))
                               (compile-statement body))])
           (with-bindings var-bindings #:inits inits
                          (syntax/loc (location->syntax loc) body))))]
      [(struct WithStatement (loc object body))
       (when (hash-ref (current-pragmas) '(lexical scope) (lambda () #f))
         (raise-syntax-error 'compile "illegal context (lexically scoped) for `with' statement" (location->syntax loc)))
       (let* ([body-stx (with-scope #f
                          (compile-statement body))]
              [object-stx (compile-expression object)]
              [extend-scope-chain (lambda (scope-chain-id)
                                    (with-syntax ([scope-chain scope-chain-id]
                                                  [object-e object-stx])
                                      #'(cons object-e scope-chain)))])
         (dynamic-code body-stx loc extend-scope-chain (Term-location object)))]
      [(struct SwitchStatement (loc expr (list (struct CaseClause (_ qs as)) ...)))
       (with-syntax ([e (compile-expression expr)]
                     [(x v break falling-through?) (generate-temporaries '(x v break falling-through?))])
         (with-syntax ([(q ...) (map (lambda (q)
                                       (if q
                                           (with-syntax ([test-e (compile-expression q)])
                                             #'(lambda (x)
                                                 (js:=== x test-e)))
                                           #'(lambda (x) '#t)))
                                     qs)])
           (parameterize ([current-labels (cons (list #f #'break) (current-labels))])
             (with-syntax ([((a ...) ...) (map (lambda (stmts)
                                                 (map compile-statement stmts))
                                               as)])
               (syntax/loc* loc
                 (let ([v e])
                   (let/ec break
                     (let ([falling-through? '#f])
                       (when (or falling-through? (q v))
                         (set! falling-through? '#t)
                         (complete! a) ...)
                       ...
                       (previous-completion)))))))))]
      [(struct LabelledStatement (loc label (? loop? loop)))
       (let ([label-name (Identifier-name label)])
         (with-syntax ([(break continue) (generate-temporaries '(break continue))])
           (parameterize ([current-labels (cons (list label-name #'break #'continue)
                                                (current-labels))])
             (compile-loop loop #'break #'continue))))]
      [(struct LabelledStatement (loc label statement))
       (let ([label-name (Identifier-name label)])
         (with-syntax ([(break) (generate-temporaries '(break))])
           (parameterize ([current-labels (cons (list label-name #'break)
                                                (current-labels))])
             (with-syntax ([s (compile-statement statement)])
               (syntax/loc* loc
                 (let/ec break s))))))]
      [(struct ThrowStatement (loc value))
       (with-syntax ([stxloc (location->syntax loc)]
                     [e (compile-expression value)])
         (syntax/loc* loc
           (raise-runtime-exception stxloc e)))]
      ;; TODO: add conditions to CatchClause ast and compile that too
      ;; TODO: need an error for try with no catch or finally
      ;; TODO: handle try/finally with return in finally
      [(struct TryStatement (loc body catches finally))
       (with-syntax ([body-s (compile-statement body)]
                     [(catch-e ...) (map compile-catch-clause catches)])
         (with-syntax ([try-catch #'(with-handlers ([exn:runtime? catch-e]
                                                    ...)
                                      body-s)])
           (if finally
               (with-syntax ([finally-s (compile-statement finally)])
                 (syntax/loc* loc
                   (dynamic-wind void
                                 (lambda () try-catch)
                                 (lambda () finally-s))))
;                   (begin (dynamic-wind
;                           void
;                           (lambda () try-catch)
;                           (lambda () finally-s))
;                          (previous-completion))))
               (syntax/loc* loc try-catch))))]
      )))

(define (compile-catch-clause clause)
  (match clause
    [(struct CatchClause (loc exn catch))
     (with-syntax ([exn-value (car (generate-temporaries '(exn-value)))])
       (let ([var-bindings (list (Identifier->Variable exn))]
             [inits (list (syntax/loc* loc
                            (exn:runtime-value exn-value)))])
         (with-syntax ([body (with-bindings var-bindings #:inits inits
                               (with-scope (bind var-bindings (current-scope))
                                 (compile-statement catch)))])
           (syntax/loc (location->syntax loc)
             (lambda (exn-value) body)))))]))

(define (compile-loop stmt break-id continue-id)
  (match stmt
    [(struct DoWhileStatement (loc body test))
     (with-syntax ([body-s (compile-statement body)]
                   [test-e (compile-expression test)]
                   [break break-id]
                   [continue continue-id])
       (syntax/loc* loc
         (let/ec break
           (let loop ()
             (let/ec continue body-s)
             (if (any->boolean test-e)
                 (loop)
                 (previous-completion))))))]
    [(struct WhileStatement (loc test body))
     (with-syntax ([test-e (compile-expression test)]
                   [body-s (compile-statement body)]
                   [break break-id]
                   [continue continue-id])
       (syntax/loc* loc
         (let/ec break
           (let loop ()
             (if (any->boolean test-e)
                 (begin (let/ec continue body-s)
                        (loop))
                 (previous-completion))))))]
    [(struct ForStatement (loc init test incr body))
     (with-syntax ([init-e (if init
                               (compile-expression init)
                               #'(void))]
                   [test-e (if test
                               (compile-expression test)
                               #'(quote #t))]
                   [incr-e (if incr
                               (compile-expression incr)
                               #'(void))]
                   [body-s (compile-statement body)]
                   [break break-id]
                   [continue continue-id]
                   [(loop) (generate-temporaries '(loop))])
       (syntax/loc* loc
         (begin
           init-e
           (let/ec break
             (let loop ()
               (if (any->boolean test-e)
                   (begin (let/ec continue body-s)
                          incr-e
                          (loop))
                   (previous-completion)))))))]
    [(struct ForInStatement (loc lhs container body))
     (with-syntax ([(object next-key key) (generate-temporaries '(object next-key key))])
       (with-syntax ([container-e (compile-expression container)]
                     [update (compile-assignment lhs #'key)]
                     [body-s (compile-statement body)]
                     [break break-id]
                     [continue continue-id])
         (syntax/loc (location->syntax loc)
           (let/ec break
             (let* ([object container-e]
                    [next-key (object-keys-stream object)])
               (let loop ()
                 (let ([key (next-key)])
                   (if key
                       (begin
                         update
                         (let/ec continue body-s)
                         (loop))
                       (previous-completion)))))))))]
    ))

(define (field-reference? x)
  (or (BracketReference? x)
      (DotReference? x)))

(define (compile-optional-expression expr [default #'(void)])
  (if expr (compile-expression expr) default))

(define (compile-expression expr)
  (match expr
    [(struct StringLiteral (loc value))
     ;; XXX: does this need the syntax-for-original-property gobbledygook around the quote?
     (with-syntax ([literal (build-syntax value loc)])
       (syntax/loc* loc
         (quote literal)))]
    [(struct NumericLiteral (loc value))
     ;; XXX: does this need the syntax-for-original-property gobbledygook around the quote?
     (with-syntax ([literal (build-syntax value loc)])
       (syntax/loc* loc
         (quote literal)))]
    [(struct BooleanLiteral (loc value))
     (if value
         (syntax/loc* loc '#t)
         (syntax/loc* loc '#f))]
    [(struct NullLiteral (loc))
     (syntax/loc* loc '())]
    [(struct RegexpLiteral (loc pattern global? case-insensitive?))
     (begin (printf "expression not compiled: ~v~n" expr)
            #'"<<regular expression>>")]
    [(struct ArrayLiteral (loc elts))
     (let ([len (length elts)])
       (with-syntax ([(e ...) (for/list ([elt elts])
                                (compile-optional-expression elt #'nothing))]
                     [(i ...) (for/list ([i (in-range len)]) i)]
                     [n len])
         (syntax/loc* loc
           (let ([v (make-evector n nothing)])
             (evector-set! v i e) ...
             (build-array v)))))]
;         (build-array (evector e ...))))]
    [(struct ObjectLiteral (loc properties))
     (let ([names (map (lambda (prop)
                         (let ([name (car prop)])
                           (cond
                             [(NumericLiteral? name) (NumericLiteral-value name)]
                             [(StringLiteral? name) (StringLiteral-value name)]
                             [(Identifier? name) (Identifier-name name)])))
                       properties)]
           [values (map cdr properties)])
       (with-syntax ([(key ...) names]
                     [(e ...) (map compile-expression values)])
         (syntax/loc* loc
           (build-object
            (object-table [key e] ...)))))]
    [(struct BlockLiteral (loc args body))
     (compile-block-literal loc args body)]
    [(struct ThisReference (loc))
     (with-syntax ([this this-id])
       (syntax/loc (location->syntax loc)
         this))]
    [(? reference-expression?)
     (compile-lookup expr)]
    [(struct NewExpression (loc constructor args))
     (with-syntax ([stxloc (location->syntax loc)]
                   [constructor-e (compile-expression constructor)]
                   [(e ...) (map compile-expression args)]
                   [(ctor) (generate-temporaries '(ctor))])
       (syntax/loc* loc
         (let ([ctor constructor-e])
           (unless (function? ctor)
             (raise-runtime-type-error stxloc "constructor" ctor))
           ((function-construct ctor) e ...))))]
;           (unless (object? ctor)
;             (raise-runtime-type-error stxloc "constructor" ctor))
;           ((object-construct ctor) e ...))))]
    [(struct PostfixExpression (loc operand op))
     (with-syntax ([op-e (if (eq? op '++) #'js:+ #'js:-)]
                   [operand-e (compile-expression operand)]
                   [update (compile-expression (make-AssignmentExpression loc
                                                                          operand
                                                                          (if (eq? op '++) '+= '-=)
                                                                          (make-NumericLiteral loc 1)))]
                   [(v) (generate-temporaries '(v))])
       (syntax/loc (location->syntax loc)
         (let ([v (any->number operand-e)])
           update
           v)))]
    [(struct PrefixExpression (loc op operand))
     (cond
       [(memq op '(++ --))
        (let ([op (if (eq? op '++) '+= '-=)])
          (compile-expression
           (make-AssignmentExpression loc operand op (make-NumericLiteral loc 1))))]
       [(eq? op 'delete)
        (compile-deletion operand)]
       [else
        (with-syntax ([op-e (operator->syntax op)]
                      [operand-e (compile-expression operand)])
          (syntax/loc* loc
            (op-e operand-e)))])]
    [(struct InfixExpression (loc left '&& right))
     (with-syntax ([left-e (compile-expression left)]
                   [right-e (compile-expression right)])
       (syntax/loc* loc
         (if (any->boolean left-e) right-e '#f)))]
    [(struct InfixExpression (loc left '\|\| right))
     (with-syntax ([left-e (compile-expression left)]
                   [right-e (compile-expression right)]
                   [(tmp) (generate-temporaries '(tmp))])
       (syntax/loc* loc
         (let ([tmp left-e])
           (if (any->boolean tmp) tmp right-e))))]
    [(struct InfixExpression (loc left op right))
     (with-syntax ([left-e (compile-expression left)]
                   [op-e (operator->syntax op)]
                   [right-e (compile-expression right)])
       (syntax/loc* loc
         (op-e left-e right-e)))]
    [(struct ConditionalExpression (loc test consequent alternate))
     (with-syntax ([test-e (compile-expression test)]
                   [consequent-e (compile-expression consequent)]
                   [alternate-e (compile-expression alternate)])
       (syntax/loc* loc
         (if test-e consequent-e alternate-e)))]
    [(struct AssignmentExpression (loc left '= right))
     (compile-assignment left (compile-expression right))]
    [(struct AssignmentExpression (loc left op right))
     (compile-expression
      (make-AssignmentExpression loc
                                 left
                                 '=
                                 (make-InfixExpression (Term-location right)
                                                       left
                                                       (assignment-operator->infix-operator op)
                                                       right)))]
    [(struct FunctionExpression/hoisted (loc name args body funs vars imports exports))
     (compile-function loc name args body funs vars #t)]
    [(struct LetExpression (loc bindings body))
     (let ([var-bindings (map (compose Identifier->Variable VariableInitializer-id) bindings)]
           [inits (map (compose compile-expression VariableInitializer-init) bindings)])
       (with-syntax ([body (with-scope (bind var-bindings (current-scope))
                             (compile-expression body))])
         (with-bindings var-bindings #:inits inits
           (syntax/loc* loc body))))]
    [(struct CallExpression (loc (and method (struct BracketReference (_ container key))) args))
     (with-syntax ([stxloc (location->syntax loc)]
                   [container-e (compile-expression container)]
                   [key-e (compile-expression key)]
                   [(container-val key-val function-val) (generate-temporaries '(container-val key-val function-val))]
                   [(arg-e ...) (map compile-expression args)]
                   [(arg-val ...) (generate-temporaries args)])
       (syntax/loc (location->syntax loc)
         (let* ([container-val container-e]
                [key-val key-e]
                [function-val (object-get container-val key-val void)]
                [arg-val arg-e] ...)
           (parameterize ([current-this container-val])
             ;; TODO: what if there is no method?!
             (#%app function-val arg-val ...)))))]
;             (call function-val
;                   (list arg-val ...)
;                   (lambda (str1 str2)
;                     (raise-runtime-type-error stxloc str1 str2)))))))]
    [(struct CallExpression (loc (struct DotReference (_ container id)) args))
     (with-syntax ([stxloc (location->syntax loc)]
                   [container-e (compile-expression container)]
                   [key-val (Identifier->name-syntax id)]
                   [(container-val function-val) (generate-temporaries '(container-val function-val))]
                   [(arg-e ...) (map compile-expression args)]
                   [(arg-val ...) (generate-temporaries args)])
       (syntax/loc (location->syntax loc)
         (let* ([container-val container-e]
                [function-val (object-get container-val key-val void)]
                [arg-val arg-e] ...)
           (parameterize ([current-this container-val])
             ;; TODO: what if there is no method?!
             (#%app function-val arg-val ...)))))]
;             (call function-val
;                   (list arg-val ...)
;                   (lambda (str1 str2)
;                     (raise-runtime-type-error stxloc str1 str2)))))))]

    ;; TODO: with explicit `use lexical scope' pragma, lexically scoped `eval' could be mutated

    ;; DIRECT EVAL (10.2.2):
    ;; 
    ;; * lexical scope
    ;;   o dynamic environment
    ;;     + interaction: fresh variable object to protect environment
    ;;     + script:      fresh variable object to protect environment
    ;;     - module:      (modules always use static environment)
    ;;   o static environment
    ;;     + interaction: fresh variable object to protect environment, switch to dynamic environment
    ;;     + script:      fresh variable object to protect environment, switch to dynamic environment
    ;;     + module:      fresh variable object to protect environment, switch to dynamic environment
    ;; ==> CONCLUSION: fresh variable object, ensure dynamic environment
    ;; 
    ;; * script scope
    ;;   o dynamic environment
    ;;     + interaction: use current dynamic environment and variable object
    ;;     + script:      use current dynamic environment and variable object
    ;;     - module:      (modules always use lexical scope)
    ;;   o static environment
    ;;     - interaction: (interaction always uses dynamic environment)
    ;;     - script:      (global and function code use dynamic environment when `contains-direct-eval?')
    ;;     - module:      (modules always use lexical scope)
    ;; ==> CONCLUSION: pass scope-chain and variable-object unchanged

    [(struct CallExpression (loc (? direct-eval? function) args))
     (let ([lexically-scoped? (hash-ref (current-pragmas) '(lexical scope) (lambda () #f))]
           [stxloc (location->syntax loc)])
       (with-syntax ([this this-id]
                     [(arg-e ...) (map compile-expression args)]
                     [scope-chain scope-chain-id]
                     [variable-object variable-object-id]
                     [eval-begin (datum->syntax (current-eval-context) 'eval-begin stxloc)]
                     [stxloc stxloc]
                     [(function-val arg-vals arg-val ...) (generate-temporaries (append '(function-val arg-vals) args))])
         (with-syntax ([invoke-script (if lexically-scoped?
                                          #'(let ([variable-object (build-object (object-table))])
                                              (function-val (cons variable-object scope-chain) variable-object this))
                                          #'(function-val scope-chain variable-object this))])
           (with-syntax ([direct-eval #'(let ([arg-vals (list arg-e ...)])
                                          (if (null? arg-vals)
                                              (void)
                                              (let ([function-val (with-handlers ([exn? (lambda (exn)
                                                                                          (raise-runtime-exception stxloc (exn-message exn)))])
                                                                    (eval-syntax #`(eval-begin #,@(parse-program-unit (any->string (car arg-vals))))))])
                                                invoke-script)))])
             (if lexically-scoped?
                 (dynamic-code (syntax/loc* loc direct-eval) loc)
                 (syntax/loc* loc
                   (if (original-eval?)
                       direct-eval
                       (let ([function-val (object-get global-object 'eval)]
                             [arg-val arg-e] ...)
                         (parameterize ([current-this global-object])
                           (#%app function-val arg-val ...))))))))))]
;                           (call function-val
;                                 (list arg-val ...)
;                                 (lambda (str1 str2)
;                                   (raise-runtime-type-error stxloc str1 str2))))))))))))]
    [(struct CallExpression (loc function args))
     (with-syntax ([stxloc (location->syntax loc)]
                   [function-e (compile-expression function)]
                   [(arg-e ...) (map compile-expression args)]
                   [(function-val arg-val ...) (generate-temporaries (cons 'function-val args))])
       (syntax/loc (location->syntax loc)
         (let ([function-val function-e]
               [arg-val arg-e] ...)
           (parameterize ([current-this global-object])
             ;; XXX: should really wrap the function-val with a guard
             (#%app function-val arg-val ...)))))]
;             (call function-val
;                   (list arg-val ...)
;                   (lambda (str1 str2)
;                     (raise-runtime-type-error stxloc str1 str2)))))))]
    [(struct ParenExpression (loc expr))
     (compile-expression expr)]
    [(struct ListExpression (loc (list)))
     #'(void)]
    [(struct ListExpression (loc exprs))
     (with-syntax ([(e ...) (map compile-expression exprs)])
       (syntax/loc* loc
         (begin e ...)))]
    [(struct DoExpression (loc block))
     (with-syntax ([e (compile-expression-block block)])
       (syntax/loc* loc
         (begin e)))]
    ))
