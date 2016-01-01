#lang scheme/base

(require scheme/list
         scheme/match
         scheme/class
         "ast-core.ss"
         "ast-utils.ss"
         "../../private/config.ss"
         "input.ss"
         "lex.ss"
         "token.ss"
         "exceptions.ss")

(provide parser<%> parser%
         input-source? input-source->input-port input-source->parser
         parse-program-unit parse-expression parse-function-constructor parse-source-element)

;; TODO:
;;   - disallow newlines before postfix operators (7.9.1)
;;   - Mozilla extensions:
;;      * catch guards
;;      * multiple catch clauses
;;      * const
;;   - let function
;;   - destructuring assignment

;; ===========================================================================
;; INHERITED ATTRIBUTES
;; ===========================================================================

;; Is the binary `in' operator disabled? (i.e., within `for' initialization)
(define disable-in? (make-parameter #f))

;; Is the binary `,' operator disabled? (i.e., within argument lists)
(define disable-comma? (make-parameter #f))

;; ===========================================================================
;; OPERATORS AND PRECEDENCE
;; ===========================================================================

(define operatorTypePrecedenceList
  '((\;)
    (\,)
    (ASSIGN)
    (? :)
    (\|\|)
    (&&)
    (\|)
    (^)
    (&)
    (== != === !==)
    (< <= >= > in instanceof)
    (<< >> >>>)
    (+ -)
    (* / %)
    (delete void typeof ! ~ UNARY do)
    (++ --)
    (new)))

(define operatorTypePrecedence
  (foldl (lambda (ops index result)
           (append (map (lambda (op)
                          (cons op index))
                        ops)
                   result))
         null
         operatorTypePrecedenceList
         (for/list ([i (in-range (length operatorTypePrecedenceList))]) i)))

;; operator-precedence : token -> nat
(define (operator-precedence op)
  (cdr (assq (token-type op) operatorTypePrecedence)))

;; infix-operator-token? : token -> boolean
(define (infix-operator-token? token)
  (let ([tt (token-type token)])
    (and (not (and (disable-in?) (eq? tt 'in)))
         (or (eq? tt 'ASSIGN)
             (eq? tt '?)
             (and (eq? tt '\,) (not (disable-comma?)))
             (infix-operator? tt)))))

;; postfix-operator-token? : token -> boolean
(define (postfix-operator-token? token)
  (postfix-operator? (token-type token)))

;; binds-tighter? : token token -> boolean
(define (binds-tighter? op1 op2)
  (> (operator-precedence op1)
     (operator-precedence op2)))

;; ===========================================================================
;; PARSING UTILITIES
;; ===========================================================================

(define abstract-parser%
  (class object%
    (init lexer)
    (define t lexer)

    ;; while/same-line : symbol ... -> condition
    (define/public (while/same-line . delimiters)
      (lambda (first?)
        (let ([token (send t peek-token/same-line)])
          (and (memq (token-type token) delimiters)
               token))))

    ;; while : symbol ... -> condition
    (define/public (while . delimiters)
      (lambda (first?)
        (let ([token (send t peek-token)])
          (and (memq (token-type token) delimiters)
               token))))

    ;; until/same-line : symbol ... -> condition
    (define/public (until/same-line . delimiters)
      (lambda (first?)
        (let ([token (send t peek-token/same-line)])
          (and (not (memq (token-type token) delimiters))
               token))))

    ;; until : symbol ... -> condition
    (define/public (until . delimiters)
      (lambda (first?)
        (let ([token (send t peek-token)])
          (and (not (memq (token-type token) delimiters))
               token))))

    ;; sep/same-line : symbol condition -> condition
    (define/public (sep/same-line sym condition)
      (lambda (first?)
        (cond
          [first? (condition #t)]
          [(eq? (token-type (send t peek-token/same-line)) sym)
           (let ([token (send t read-token/same-line)])
             (or (condition #f)
                 (begin (send t unread-token) #f)))]
          [else #f])))

    ;; sep : symbol condition -> condition
    (define/public (sep sym condition)
      (lambda (first?)
        (cond
          [first? (condition #t)]
          [(eq? (token-type (send t peek-token)) sym)
           (let ([token (send t read-token)])
             (or (condition #f)
                 (begin (send t unread-token) #f)))]
          [else #f])))

    ;; map-tokens : condition (token -> a) -> (listof a)
    (define/public (map-tokens condition proc)
      (if (not (condition #t))
          null
          (let loop ([acc (list (proc))])
            (if (not (condition #f))
                (reverse acc)
                (loop (cons (proc) acc))))))

    (super-make-object)))

;; token->Identifier : token -> Identifier
(define (token->Identifier token)
  (make-Identifier (token-location token) (token-contents token)))

;; string-token->Identifier : token -> Identifier
(define (string-token->Identifier token)
  (make-Identifier (@ token token) (string->symbol (token-contents token))))

;; ===========================================================================
;; PARSER
;; ===========================================================================

(define parser<%>
  (interface ()
    parse-source-element
    parse-source-elements
    parse-expression
    parse-formal-parameters
    skip-empty-tokens
    ))

(define parser%
  (class* abstract-parser% (parser<%>)
    (init lexer)
    (define t lexer)

    (inherit while while/same-line until until/same-line sep sep/same-line map-tokens)

    (define (expression-on-same-line?)
      (not (memq (token-type (send t peek-token/same-line))
                 '(END NEWLINE \; \} =>))))

    ;; skip-empty-tokens : -> any
    (define/public (skip-empty-tokens)
      (send t skip-whitespace)
      (let ([token (send t peek-token/same-line)])
        (when (eq? (token-type token) 'NEWLINE)
          (send t read-token/same-line)
          (skip-empty-tokens))))

    ;; fail : string any ... -> <never>
    (define (fail fmt . args)
      (send/apply t fail fmt args))

    ;; fail : region string any ... -> <never>
    (define (fail/loc loc text fmt . args)
      (send/apply t fail/loc loc text fmt args))

    ;; TODO: unit tests for REPL parsing

    ;; parse-source-element : -> SourceElement
    (define/public (parse-source-element)
      (case (token-type (send t peek-token))
        [(import)
         (parse-import)]
        [(export)
         (parse-export)]
        [(function)
         (parse-function-definition #f)]
        [(\{)
         (parse-block)]
        [(if)
         (send t must-match 'if)
         (let ([test (parse-paren-expression)]
               [consequent (parse-sub-statement)]
               [alternate (and (send t match 'else) (parse-sub-statement))])
           (make-IfStatement (@ test (or alternate consequent))
                             test consequent alternate))]
        [(switch)
         (let ([switch-token (send t must-match 'switch)]
               [test (parse-paren-expression)])
           (send t must-match '\{)
           (let ([cases (map-tokens (until '\})
                          (lambda ()
                            (let* ([token (send t read-token)]
                                   [condition (case (token-type token)
                                                [(default) #f]
                                                [(case) (parse-list-expression)]
                                                [else
                                                 (fail/loc (token-location token)
                                                           (token-contents token)
                                                           "expected case or default")])]
                                   [colon-token (send t must-match ':)]
                                   [action (map-tokens (until 'case 'default '\})
                                             (lambda ()
                                               (parse-sub-statement)))])
                              (make-CaseClause (@ token (if (pair? action) (last action) colon-token))
                                               condition action))))])
             (let ([close-curly (send t must-match '\})])
               (make-SwitchStatement (@ switch-token close-curly)
                                     test cases))))]
        [(for)
         (let ([for-token (send t must-match 'for)])
           (send t must-match '\()
           ;; The `in' operator is disabled in init expressions since
           ;; it's ambiguous with `for..in' expressions.
           (let ([init (parameterize ([disable-in? #t])
                         (let ([next (send t peek-token)])
                           (and (not (eq? (token-type next) '\;))
                                (let ([type (token-type next)])
                                  (case type
                                    [(let var)
                                     (let ([bindings (begin (send t read-token)
                                                            (parse-variable-bindings))]
                                           [ctor (case type
                                                   [(let) make-LetDeclaration]
                                                   [(var) make-VariableDeclaration])])
                                       (ctor (@ (first bindings) (last bindings)) bindings))]
                                  [else (parse-list-expression)])))))])
             (if (and init (send t match 'in))
                 ;; ForInStatement
                 (let ([lhs (match init
                              [(? ListExpression?)
                               (fail/loc (token-location for-token)
                                         "for..."
                                         "invalid for..in left-hand side")]
                              [(? Expression?) init]
                              [(or (struct LetDeclaration (_ (list (struct VariableInitializer (_ _ #f)))))
                                   (struct VariableDeclaration (_ (list (struct VariableInitializer (_ _ #f))))))
                               init]
                              [(or (struct LetDeclaration (loc (list (struct VariableInitializer (_ _ _)))))
                                   (struct LetDeclaration (loc (list _ _ ...)))
                                   (struct VariableDeclaration (loc (list (struct VariableInitializer (_ _ _)))))
                                   (struct VariableDeclaration (loc (list _ _ ...))))
                               (fail/loc loc
                                         "for..."
                                         "invalid for..in left-hand side")]
                              [_ (fail/loc (token-location for-token)
                                           "for..."
                                           "invalid for..in left-hand side")])])
                   (let ([expression (parse-list-expression)])
                     (send t must-match '\))
                     (let ([body (parse-sub-statement)])
                       (make-ForInStatement (@ for-token body)
                                            lhs expression body))))
                 ;; ForStatement
                 (begin (send t must-match '\;)
                        (let ([test (and (not (eq? (token-type (send t peek-token)) '\;))
                                         (parse-list-expression))]
                              [semicolon-token (send t must-match '\;)]
                              [incr (and (not (eq? (token-type (send t peek-token)) '\)))
                                         (parse-list-expression))]
                              [close-paren (send t must-match '\))]
                              [body (parse-sub-statement)])
                          (make-ForStatement (@ for-token body)
                                             init test incr body))))))]
        [(while)
         (let ([while-token (send t must-match 'while)])
           (let ([test (parse-paren-expression)]
                 [body (parse-sub-statement)])
             (make-WhileStatement (@ while-token body)
                                  test body)))]
        [(do)
         (let ([do-token (send t must-match 'do)])
           (let ([body (parse-sub-statement)]
                 [test (begin (send t must-match 'while)
                              (parse-paren-expression))]
                 [semicolon-token (match-semicolon (infer-do-while-semicolon?))])
             (make-DoWhileStatement (@ do-token (or semicolon-token test))
                                    body test)))]
        [(break continue)
         (let* ([token (send t read-token)]
                [constructor (if (eq? (token-type token) 'break)
                                 make-BreakStatement
                                 make-ContinueStatement)]
                ;; 7.9.1
                [label (and (expression-on-same-line?)
                            (token->Identifier (send t must-match 'ID)))])
           ;; 12.12
           (let ([semicolon-token (match-semicolon #t)])
             (constructor (@ token (or semicolon-token label token))
                          label)))]
        [(try)
         (let ([try-token (send t must-match 'try)]
               [body (parse-block)]
               [catches (map-tokens (while 'catch)
                          (lambda ()
                            (let ([catch-token (send t must-match 'catch)])
                              (send t must-match '\()
                              (let ([id (token->Identifier (send t must-match 'ID))])
                                (send t must-match '\))
                                (let ([body (parse-block)])
                                  (make-CatchClause (@ catch-token body)
                                                    id body))))))]
               [finally (and (send t match 'finally)
                             (parse-block))])
           (unless (or (enable-extended-catch-statements?) (<= (length catches) 1))
             (fail/loc (Term-location (cadr catches)) "catch..." "catch without preceding try"))
           (make-TryStatement (@ try-token (or finally (and (pair? catches) (last catches)) body))
                              body catches finally))]
        [(catch finally)
         (let ([token (send t read-token)])
           (fail/loc (token-location token)
                     (token-contents token)
                     "~a without preceding try" (token-type token)))]
        [(throw)
         (let ([throw-token (send t must-match 'throw)])
           ;; 7.9.1
           (unless (expression-on-same-line?)
             (fail/loc (token-location (send t peek-token/same-line))
                       "throw..."
                       "expected expression on same line"))
           (let ([expr (parse-list-expression)]
                 [semicolon-token (match-semicolon #t)])
             (make-ThrowStatement (@ throw-token (or semicolon-token expr))
                                  expr)))]
        [(return)
         (let ([return-token (send t must-match 'return)]
               ;; 7.9.1
               [value (and (expression-on-same-line?) (parse-list-expression))]
               [semicolon-token (match-semicolon #t)])
           (make-ReturnStatement (@ return-token (or semicolon-token value return-token))
                                 value))]
        [(with)
         (let ([with-token (send t must-match 'with)]
               [object (parse-paren-expression)]
               [body (parse-sub-statement)])
           (make-WithStatement (@ with-token body)
                               object body))]
        [(var)
         (let ([var-token (send t must-match 'var)]
               [variables (parse-variable-bindings)]
               [semicolon-token (match-semicolon #t)])
           (make-VariableDeclaration (@ var-token (or semicolon-token (last variables)))
                                     variables))]
        [(let)
         (if (eq? (token-type (send t peek-token 1)) '\()
             (let ([let-expr-or-stmt (parse-let #f)])
               (if (Expression? let-expr-or-stmt)
                   (make-ExpressionStatement (Term-location let-expr-or-stmt) let-expr-or-stmt)
                   let-expr-or-stmt))
             ;               (parse-let #f)
             (let ([new-operator (send t read-token)]
                   [bindings (parse-variable-bindings)]
                   [semicolon-token (match-semicolon #t)])
               (make-LetDeclaration (@ new-operator (or semicolon-token (last bindings)))
                                    bindings)))]
        [(\;)
         (make-EmptyStatement (token-location (send t must-match '\;)))]
        [else
         (if (and (eq? (token-type (send t peek-token)) 'ID)
                  (eq? (token-type (send t peek-token/infix-operator 1)) ':))
             (let ([label (token->Identifier (send t read-token))])
               (send t read-token/infix-operator)
               (let ([body (parse-sub-statement)])
                 (make-LabelledStatement (@ label body) label body)))
             ;; NOTE: can never be a FunctionExpression (12.4)
             (let ([expr (parse-list-expression)]
                   [semicolon-token (match-semicolon #t)])
               (make-ExpressionStatement (@ expr (or semicolon-token expr))
                                         expr)))]))

    ;; parse-import : -> ImportDeclaration
    (define (parse-import)
      (let ([import (send t must-match 'import)]
            [spec (parse-import-spec)]
            [more (map-tokens (while '\,)
                    (lambda ()
                      (begin (send t must-match '\,)
                             (parse-import-spec))))]
            [semicolon-token (match-semicolon #t)])
        (make-ImportDeclaration (@ import (or semicolon-token spec)) (cons spec more))))

    ;; parse-module-spec : -> ModuleSpecifier
    (define (parse-module-spec)
      (cond
        [(send t match 'ID 'file)
         => (lambda (file)
              (let ([left (send t must-match '\()]
                    [contents (send t must-match 'STRING)]
                    [right (send t must-match '\))])
                (make-ModuleSpecifier (@ file right)
                                      'file
                                      (list (token-contents contents)))))]
        [(send t match 'ID 'planet)
         => (lambda (planet)
              (define (parse-spec-literal)
                (cond
                  [(or (send t match 'STRING)
                       (send t match 'NUMBER)
                       (send t match 'ID))
                   => token-contents]
                  [(memq (token-type (send t peek-token)) '(\, \))) #f]
                  [else (let ([token (send t read-token)])
                          (fail/loc (token-location token)
                                    (token-contents token)
                                    "expected string literal, number literal, identifier, ',', or ')'"))]))
              (let ([left (send t must-match '\()]
                    [elements (cons (parse-spec-literal)
                                    (map-tokens (while '\,)
                                      (lambda ()
                                        (send t must-match '\,)
                                        (parse-spec-literal))))]
                    [right (send t must-match '\))])
                (make-ModuleSpecifier (@ planet right)
                                      'planet
                                      elements)))]
        [(send t match 'ID)
         => (lambda (collect1)
              (let ([collects (cons collect1
                                    (map-tokens (while '/)
                                      (lambda ()
                                        (let ([slash (send t read-token)])
                                          (send t must-match 'ID)))))])
                (make-ModuleSpecifier (@ (first collects) (last collects))
                                      'collect
                                      (map token-contents collects))))]
        [else (let ([token (send t peek-token)])
                (fail/loc (token-location token)
                          (token-contents token)
                          "expected 'file', 'planet', or identifier"))]))

    ;; parse-import-binding : -> ImportBinding
    (define (parse-import-binding)
      (cond
        [(send t match 'ID)
         => (lambda (external)
              (cond
                [(send t match ':)
                 => (lambda (colon)
                      (let ([internal (send t must-match 'ID)])
                        (make-ImportBinding (@ external internal)
                                            (token->Identifier external)
                                            (token->Identifier internal))))]
                [else (make-ImportBinding (@ external external)
                                          (token->Identifier external)
                                          #f)]))]
        [(send t match 'STRING)
         => (lambda (external)
              (let ([colon (send t must-match ':)]
                    [internal (send t must-match 'ID)])
                (make-ImportBinding (@ external internal)
                                    (string-token->Identifier external)
                                    (token->Identifier internal))))]
        [else (let ([token (send t peek-token)])
                (fail/loc (token-location token)
                          (token-contents token)
                          "expected identifier or string literal"))]))

    ;; parse-import-spec : -> ModuleSpecifier
    (define (parse-import-spec)
      (let ([module (parse-module-spec)]
            [dot (send t must-match '\.)]
            [bindings (parse-import-bindings)])
        (make-ImportSpecifier (@ module (cond
                                          [(pair? bindings) (last bindings)]
                                          [(null? bindings) module]
                                          [else bindings]))
                              module
                              bindings)))

    ;; parse-import-bindings : -> (union ExclusionList (listof ImportBinding) Identifier)
    (define (parse-import-bindings)
      (cond
        [(send t match '*)
         => (lambda (star)
              (cond
                [(send t match '-)
                 => (lambda (minus)
                      (fail/loc (token-location minus)
                                (token-contents minus)
                                "exclusion list not yet implemented"))]
                [else (make-ExclusionList (@ star star) null)]))]
        [(send t match '\{)
         => (lambda (left)
              (cond
                [(send t match '\}) null]
                [else (let ([binding1 (parse-import-binding)]
                            [bindings (map-tokens (while '\,)
                                        (lambda ()
                                          (let ([comma (send t read-token)])
                                            (parse-import-binding))))]
                            [right (send t must-match '\})])
                        (cons binding1 bindings))]))]
        [else (token->Identifier (send t must-match 'ID))]))
;      (let ([module1 (send t match 'ID)]
;            [more (map-tokens (while '\[ '\.)
;                    (lambda ()
;                      (let ([sep (send t read-token)])
;                        (if (eq? (token-type sep) '\[)
;                            (begin0 (send t must-match 'STRING)
;                                    (send t must-match '\]))
;                            (let ([next (send t read-token)])
;                              (cond
;                                [(eq? (token-contents next) '*)
;                                 (case (send t peek-token)
;                                   [(\. \[)
;                                    (fail/loc (token-location next)
;                                              (token-contents next)
;                                              "expected identifier")]
;                                   [else next])]
;                                [(eq? (token-type next) 'ID) next]
;                                [else
;                                 (fail/loc (token-location next)
;                                           (token-contents next)
;                                           "expected identifier or '*'")]))))))])
;        (when (null? more)
;          (fail/loc (token-location (send t peek-token))
;                    (token-contents (send t peek-token))
;                    "expected '.' or '['"))
;        (let* ([erom (reverse more)]
;               [path (for/list ([collect (cons module1 (reverse (cdr erom)))])
;                       (if (eq? (token-type collect) 'STRING)
;                           (string-token->Identifier collect)
;                           (token->Identifier collect)))]
;               [import (car erom)])
;          (cond
;            [(eq? (token-contents import) '*)
;             (if (send t match '-)
;                 (error 'parse-import-spec "haven't implemented excludes-list yet")
;                 (make-ListModuleSpecifier (@ module1 import) path null))]
;            [(eq? (token-type import) 'STRING)
;             (send t must-match 'ID 'as)
;             (let ([rename (send t must-match 'ID)])
;               (make-SingleModuleSpecifier (@ module1 rename)
;                                           path
;                                           (string-token->Identifier import)
;                                           (token->Identifier rename)))]
;            [(send t match 'ID 'as)
;             (let ([rename (send t must-match 'ID)])
;               (make-SingleModuleSpecifier (@ module1 rename)
;                                           path
;                                           (token->Identifier import)
;                                           (token->Identifier rename)))]
;            [else (make-SingleModuleSpecifier (@ module1 import)
;                                              path
;                                              (token->Identifier import)
;                                              #f)]))))

    (define (parse-export-spec)
      (cond
        [(send t match '\{)
         => (lambda (left)
              (fail/loc (token-location left)
                        (token-contents left)
                        "renamed exports not yet implemented"))]
        ;; TODO: get rid of this when you implement private
        [(send t match '*)
         => (lambda (star)
              (cond
                [(send t match '-)
                 => (lambda (minus)
                      (fail/loc (token-location minus)
                                (token-contents minus)
                                "excludes list not yet implemented"))]
                [else (make-ExclusionList (@ star star) null)]))]
        [else (let ([module (parse-module-spec)]
                    [dot (send t must-match '\.)]
                    [star (send t must-match '*)])
                (cond
                  [(send t match '-)
                   => (lambda (minus)
                        (fail/loc (token-location minus)
                                  (token-contents minus)
                                  "excludes list not yet implemented"))]
                  [else (make-ReexportSpecifier (@ module star)
                                                module
                                                (make-ExclusionList (@ star star) null))]))]))
;      (let ([internal (cond
;                        [(send t match 'ID) => token->Identifier]
;                        [(send t match 'STRING) => string-token->Identifier]
;                        [else (let ([token (send t peek-token)])
;                                (fail/loc (token-location token)
;                                          (token-contents token)
;                                          "expected identifier or string literal"))])]
;            [external (and (send t match 'ID 'as)
;                           (cond
;                             [(send t match 'ID) => token->Identifier]
;                             [(send t match 'STRING) => string-token->Identifier]
;                             [else (let ([token (send t peek-token)])
;                                     (fail/loc (token-location token)
;                                               (token-contents token)
;                                               "expected identifier or string literal"))]))])
;        (make-SingleModuleSpecifier (@ internal (or external internal)) #f internal external)))

    ;; parse-export : -> ExportDeclaration
    (define (parse-export)
      (let ([export (send t must-match 'export)]
            [spec (parse-export-spec)]
            [more (map-tokens (while '\,)
                    (lambda ()
                      (begin (send t must-match '\,)
                             (parse-export-spec))))]
            [semicolon-token (match-semicolon #t)])
        (make-ExportDeclaration (@ export (or semicolon-token spec)) (cons spec more))))
;      (let ([export (send t must-match 'export)]
;            [spec1
;            [specs (cond
;                     [(send t match '*)
;                      => (lambda (star)
;                           ;; XXX: implement excludes-list
;                           (list (make-ListModuleSpecifier (@ star star) #f null)))]
;                     [else (cons (parse-export-spec)
;                                 (map-tokens (while '\,) parse-export-spec))])])
;        (make-ExportDeclaration (@ export (last specs)) specs)))

    ;; parse-sub-statement : -> SubStatement
    (define (parse-sub-statement)
      (let ([element (parse-source-element)])
        (when (and (FunctionDeclaration? element)
                   (not (allow-nested-function-declarations?)))
          (fail/loc (Term-location element)
                    "function..."
                    "illegally nested function declaration"))
        element))

    ;; parse-sub-statements : -> (listof SubStatement)
    (define (parse-sub-statements)
      (map-tokens (until '\} 'END '=>)
        (lambda ()
          (parse-sub-statement))))

    ;; match-semicolon : boolean -> (optional token)
    (define (match-semicolon insert-semicolon?)
      (if (not insert-semicolon?)
          (send t must-match '\;)
          (send t match '\;)))

    ;; parse-block : -> BlockStatement
    (define (parse-block)
      (let ([open-curly (send t must-match '\{)]
            [body (parse-sub-statements)]
            [close-curly (send t must-match '\})])
        (make-BlockStatement (@ open-curly close-curly)
                             body)))

    ;; parse-source-elements : -> (listof SourceElement)
    (define/public (parse-source-elements)
      (map-tokens (until '\} 'END)
        (lambda ()
          (parse-source-element))))

    ;; parse-variable-bindings : -> (nelistof VariableInitializer)
    (define (parse-variable-bindings)
      (let ([result (map-tokens (sep '\, (while 'ID))
                      (lambda ()
                        (let ([var (send t must-match 'ID)]
                              [init (let ([token (send t match 'ASSIGN)])
                                      (when (and token (not (eq? (token-contents token) '=)))
                                        (fail/loc (token-location token)
                                                  (token-contents token)
                                                  "invalid variable intitialization"))
                                      (and token (parse-single-expression)))])
                          (make-VariableInitializer (@ var (or init var))
                                                    (token->Identifier var)
                                                    init))))])
        (when (null? result)
          (let ([bad-token (send t peek-token)])
            (fail/loc (token-location bad-token)
                      (token-contents bad-token)
                      "expected variable name")))
        result))

    ;; parse-let-expression : -> LetExpression
    (define (parse-let-expression)
      (parse-let #t))

    ;; parse-let : boolean -> (union LetExpression LetStatement)
    (define (parse-let expression-only?)
      (let ([new-operator (send t read-token)]
            [bindings (begin (send t match '\()
                             (begin0 (map-tokens (sep '\, (until '\)))
                                       (lambda ()
                                         (let ([id (send t must-match 'ID)]
                                               [binding (cond
                                                          [(send t match 'ASSIGN)
                                                           => (lambda (tok)
                                                                (and (eq? (token-contents tok) '=)
                                                                     (parse-single-expression)))]
                                                          [else #f])])
                                           (make-VariableInitializer (@ id (or binding id))
                                                                     (token->Identifier id)
                                                                     binding))))
                                     (send t must-match '\))))])
        (let ([next-token (send t peek-token)])
          (if (eq? (token-type next-token) '\{)
              (begin
                (when expression-only?
                  (fail/loc (token-location next-token)
                            (token-contents next-token)
                            "expected expression"))
                (let ([body (parse-block)])
                  (make-LetStatement (@ new-operator body) bindings body)))
              (let ([body (parse-list-expression)])
                (make-LetExpression (@ new-operator body) bindings body))))))

;    ;; parse-function-expression : -> Expression
;    (define/public (parse-function-expression)
;      (parse-function-definition #t))

    ;; -> (listof Identifier)
    (define/public (parse-formal-parameters [stop '\)])
      (map-tokens (sep '\, (until stop 'END))
        (lambda ()
          (let ([arg (send t read-token)])
            (unless (eq? (token-type arg) 'ID)
              (fail/loc (token-location arg)
                        (token-contents arg)
                        "expected formal parameter"))
            (token->Identifier arg)))))

    ;; parse-function-definition : boolean -> Term
    (define (parse-function-definition expression-context?)
      (let ([function-token (send t must-match 'function)]
            [name (let ([token (send t match 'ID)])
                    (and token (token->Identifier token)))])
        (when (and (not name)
                   (not expression-context?)
                   (not (allow-anonymous-function-source-elements?)))
          (let ([bad-token (send t peek-token)])
            (fail/loc (token-location bad-token)
                      (token-contents bad-token)
                      "missing required function identifier")))
        (send t must-match '\()
        (let ([args (parse-formal-parameters)])
          (send t must-match '\))
          (send t must-match '\{)
          (let* ([body (parse-source-elements)]
                 [close-curly (send t must-match '\})]
                 [loc (@ function-token close-curly)])
              (cond
                [expression-context?
                 (make-FunctionExpression loc name args body)]
                [(not name)
                 (make-ExpressionStatement loc
                                           (make-FunctionExpression loc name args body))]
                [else
                 (make-FunctionDeclaration loc name args body)])))))

    ;; parse-paren-expression : -> Expression
    (define (parse-paren-expression)
      (let ([open-paren (send t must-match '\()]
            [expr (parse-list-expression)]
            [close-paren (send t must-match '\))])
        (make-ParenExpression (@ open-paren close-paren)
                              expr)))

    ;; parse-expression : -> Expression
    (define/public (parse-expression)
      (parse-list-expression))

    ;; parse-single-expression : -> Expression
    (define (parse-single-expression)
      (parameterize ([disable-comma? #t])
        (parse-operand #f)))

    ;; parse-list-expression : -> Expression
    (define (parse-list-expression)
      (parameterize ([disable-comma? #f])
        (parse-operand #f)))

    ;; parse-argument-list : symbol -> (listof Expression)
    (define (parse-argument-list stop)
      (map-tokens (sep '\, (until stop))
        (lambda ()
          (parse-single-expression))))

    ;; parse-optional-argument-list : symbol -> (listof (optional Expression))
    (define (parse-optional-argument-list stop)
      (map-tokens (sep '\, (until stop))
        (lambda ()
          (and (not (memq (token-type (send t peek-token)) (list '\, stop)))
               (parse-single-expression)))))

    ;; shift? : token (optional token) -> boolean
    (define (shift? operator context-operator)
      (or (not context-operator)
          (binds-tighter? operator context-operator)))

    ;; parse-core-operand : (optional token) -> Expression
    (define (parse-core-operand current-operator)
      (let* ([first-token (send t peek-token)]
             [left (case (token-type first-token)
                     [(function)
                      (parse-function-definition #t)]
                     [(null this true false ID NUMBER STRING REGEXP)
                      (send t read-token)
                      (case (token-type first-token)
                        [(null) (make-NullLiteral (token-location first-token))]
                        [(this) (make-ThisReference (token-location first-token))]
                        [(true) (make-BooleanLiteral (token-location first-token) #t)]
                        [(false) (make-BooleanLiteral (token-location first-token) #f)]
                        [(ID) (make-VarReference (token-location first-token) (token->Identifier first-token))]
                        [(NUMBER) (make-NumericLiteral (token-location first-token) (token-contents first-token))]
                        [(STRING) (make-StringLiteral (token-location first-token) (token-contents first-token))]
                        [(REGEXP)
                         (let ([contents (token-contents first-token)])
                           (make-RegexpLiteral (token-location first-token)
                                               (regexp-contents-pattern contents)
                                               (regexp-contents-global? contents)
                                               (regexp-contents-case-insensitive? contents)))])]
                     [(delete void typeof ++ -- ! ~ UNARY)
                      (let* ([new-operator (send t read-token)]
                             [operand (parse-operand new-operator)])
                        (make-PrefixExpression (@ new-operator operand)
                                               (token-contents first-token)
                                               operand))]
                     [(do)
                      (let* ([do-operator (send t read-token)]
                             [open-curly (send t must-match '\{)]
                             [block (parse-expression-block open-curly)])
                        (make-DoExpression (@ block block) block))]
                     [(new)
                      (let* ([new-operator (send t read-token)]
                             [constructor (parse-operand new-operator)])
                        (if (send t match '\()
                            (let ([args (parse-argument-list '\))]
                                  [close-paren (send t must-match '\))])
                              (make-NewExpression (@ new-operator close-paren)
                                                  constructor
                                                  args))
                            (make-NewExpression (@ new-operator constructor)
                                                constructor
                                                null)))]
                     [(let)
                      (parse-let-expression)]
                     [(\[)
                      (let ([new-operator (send t read-token)]
                            [elts (parse-optional-argument-list '\])]
                            [close-bracket (send t must-match '\])])
                        (make-ArrayLiteral (@ new-operator close-bracket)
                                           elts))]
                     [(\{)
                      (case (token-type (send t peek-token 1))
                        [(\| \|\|) (parse-block-literal)]
                        [else (parse-object-literal)])]
                     [(\()
                      (parse-paren-expression)]
                     [else (fail/loc (token-location first-token)
                                     (token-contents first-token)
                                     (format "unexpected expression token: ~a" (token-contents first-token)))])])
        (let loop ([left left])
          (case (token-type (send t peek-token/infix-operator))
            [(\.)
             (send t read-token/infix-operator)
             (let ([id (send t must-match 'ID)])
               (loop (make-DotReference (@ left id)
                                        left
                                        (token->Identifier id))))]
            [(\[)
             (send t read-token/infix-operator)
             (let ([subscript (parse-list-expression)]
                   [close-bracket (send t must-match '\])])
               (loop (make-BracketReference (@ left close-bracket)
                                            left
                                            subscript)))]
            [(\()
             ;; NOTE: new expressions take precedence over method calls
             (if (and current-operator (eq? (token-type current-operator) 'new))
                 left
                 (begin (send t read-token/infix-operator)
                        (let ([arguments (parse-argument-list '\))]
                              [close-paren (send t must-match '\))])
                          (loop (make-CallExpression (@ left close-paren)
                                                     left
                                                     arguments)))))]
            [else left]))))

    ;; TODO: error if we're not in Harmony mode

    ;; parse-expression-block : token -> ExpressionBlock
    (define (parse-expression-block open-curly)
      (let* ([body (parse-sub-statements)]
             [tail (and (eq? (token-type (send t peek-token)) '=>)
                        (begin (send t read-token)
                               (parse-expression)))]
             [close-curly (send t must-match '\})]
             [loc (@ open-curly close-curly)])
        (make-ExpressionBlock loc (make-BlockStatement loc body) tail)))

    ;; parse-block-literal : -> BlockLiteral
    (define (parse-block-literal)
      (let ([open-curly (send t must-match '\{)]
            [open-pipe (send t read-token)])
        (case (token-type open-pipe)
          [(\|\|)
           (let ([body (parse-expression-block open-curly)])
             (make-BlockLiteral (@ body body) null body))]
          [(\|)
           (let ([args (parse-formal-parameters '\|)]
                 [close-pipe (send t must-match '\|)]
                 [body (parse-expression-block open-curly)])
             (make-BlockLiteral (@ body body) args body))]
          [else
           (fail/loc (token-location open-pipe)
                     (token-contents open-pipe)
                     "expected |")])))

    ;; parse-object-literal : -> ObjectLiteral
    (define (parse-object-literal)
      (let ([open-curly (send t must-match '\{)]
            [elts (map-tokens (sep '\, (until '\}))
                              (lambda ()
                                (let ([property (send t read-token)])
                                  (send t must-match ':)
                                  (let ([binding (parse-single-expression)])
                                    (cons (case (token-type property)
                                            [(ID) (token->Identifier property)]
                                            [(STRING) (make-StringLiteral (token-location property)
                                                                          (token-contents property))]
                                            [(NUMBER) (make-NumericLiteral (token-location property)
                                                                           (token-contents property))]
                                            [else (fail/loc (token-location property)
                                                            (token-contents property)
                                                            "invalid property id")])
                                          binding)))))]
            [close-curly (send t must-match '\})])
        (make-ObjectLiteral (@ open-curly close-curly)
                            elts)))

    ;; parse-operand : (optional token) -> Expression
    (define (parse-operand current-operator)
      (let loop ([left (parse-core-operand current-operator)])
        (let ([next-token (send t peek-token/infix-operator)])
          (cond
            [(postfix-operator-token? next-token)
             (send t read-token)
             (loop (make-PostfixExpression (@ left next-token)
                                           left (token-contents next-token)))]
            [(and (infix-operator-token? next-token)
                  (shift? next-token current-operator))
             (let ([new-operator (send t read-token/infix-operator)])
               (case (token-type next-token)
                 [(?)
                  (let ([consequent (parse-single-expression)]
                        [alternate (begin (send t must-match ':)
                                          (parameterize ([disable-comma? #t])
                                            (parse-operand new-operator)))])
                    (loop (make-ConditionalExpression (@ left alternate)
                                                      left consequent alternate)))]
                 [(\,)
                  (let ([right (parse-operand new-operator)])
                    (loop (make-ListExpression (@ left right) (list left right))))]
                 [(ASSIGN)
                  (let ([value (parse-operand new-operator)])
                    (loop (make-AssignmentExpression (@ left value)
                                                     left
                                                     (token-contents next-token)
                                                     value)))]
                 [else
                  (let ([right-operand (parse-operand new-operator)])
                    (loop (make-InfixExpression (@ left right-operand)
                                                left
                                                (token-contents next-token)
                                                right-operand)))]))]
            [else left]))))

    (super-make-object lexer)))

;; input-source->parser : input-source -> parser
(define (input-source->parser in)
  (make-object parser%
    (make-object lexer%
      (input-source->input-port in))))

(define (dump-exn exn)
  (let ([t (exn:fail:syntax-source exn)])
    (send t show-state "*** FAILURE (~a)" (exn-message exn))
    (raise exn)))

;; parse-program-unit : input-source -> (listof SourceElement)
(define (parse-program-unit src)
  (let ([parser (input-source->parser src)])
    (begin0 (send parser parse-source-elements)
            (send parser skip-empty-tokens))))

;; parse-expression : input-source -> Expression
(define (parse-expression src)
  (let ([parser (input-source->parser src)])
    (begin0 (send parser parse-expression)
            (send parser skip-empty-tokens))))

;; parse-function-constructor : string string -> FunctionExpression
(define (parse-function-constructor args body)
  (let* ([formals-parser (input-source->parser args)]
         [formals (send formals-parser parse-formal-parameters)])
    ;; XXX: check for EOF
    (let* ([body-parser (input-source->parser body)]
           [body (send body-parser parse-source-elements)])
      ;; XXX: check for EOF
      ;; TODO: these are pretty bogus source locations (two different strings!)
      (let ([left (cond
                    [(pair? formals) (first formals)]
                    [(pair? body) (first body)]
                    [else #f])]
            [right (and (pair? body) (last body))])
        (make-FunctionExpression (@ left right) #f formals body)))))

;;; parse-function-expression : input-source -> FunctionExpression
;(define (parse-function-expression src)
;  (let ([parser (input-source->parser src)])
;    (begin0 (send parser parse-function-expression)
;            (send parser skip-empty-tokens))))

;; parse-source-element : input-source -> SourceElement
(define (parse-source-element src)
  (let ([parser (input-source->parser src)])
    (begin0 (send parser parse-source-element)
            (send parser skip-empty-tokens))))
