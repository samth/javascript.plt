#lang scheme/base

(require (planet cobbe/contract-utils:1/contract-utils)
         scheme/contract
         "ast-core.ss"
         "../../private/config.ss"
         "token.ss")

(define (Property? x)
  (or (Identifier? x)
      (StringLiteral? x)
      (NumericLiteral? x)))

(define (SourceElement? x)
  (or (Statement/X? x)
      (Declaration? x)))

(define (SubStatement? x)
  (or (Statement? x)
      (and (Declaration? x)
           (not (FunctionDeclaration? x)))
      (and (allow-nested-function-declarations?)
           (FunctionDeclaration? x))))

(define (extensible-predicate base param)
  (let ([base? (if (flat-contract? base)
                   (flat-contract-predicate base)
                   base)])
    (lambda (x)
      (or (base? x)
          (let matches-any? ([predicates (param)])
            (and (pair? predicates)
                 (or ((car predicates) x)
                     (matches-any? (cdr predicates)))))))))

;; TODO: make custom "with-X-predicate" forms instead of exposing these
(define Expression-predicates (make-parameter null))
(define Statement-predicates (make-parameter null))
(define ExpressionList-predicates (make-parameter null))
(define StatementList-predicates (make-parameter null))
;; TODO:
;;   - Declaration-predicates
;;   - SourceElementList-predicates (?)
;;   - InitializerList-predicates

(define Expression/X?
  (extensible-predicate Expression? Expression-predicates))

(define Statement/X?
  (extensible-predicate Statement? Statement-predicates))

(define SubStatement/X?
  (extensible-predicate SubStatement? Statement-predicates))

(define ExpressionList/X?
  (extensible-predicate (listof Expression/X?) ExpressionList-predicates))

(define StatementList/X?
  (extensible-predicate (listof Statement/X?) StatementList-predicates))

(define SubStatementList/X?
  (extensible-predicate (listof SubStatement/X?) StatementList-predicates))

(define (Term/X? x)
  (or (Declaration? x)
      (Statement/X? x)
      (Expression/X? x)))

(define (prefab->sexp p)
  (cons (prefab-struct-key p)
        (cdr (vector->list (struct->vector p)))))

(define (sexp->prefab s)
  (apply make-prefab-struct (car s) (cdr s)))

(define (with-location loc term)
  (let ([key (prefab-struct-key term)]
        [elts (cdr (vector->list (struct->vector term)))])
    (apply make-prefab-struct key loc (cdr elts))))

(define (Term=? t1 t2)
  (cond
    [(and (Term? t1) (Term? t2))
     (let ([s1 (prefab->sexp t1)]
           [s2 (prefab->sexp t2)])
       (andmap Term=? (cddr s1) (cddr s2)))]
    [else (equal? t1 t2)]))

(define (Identifier=? id1 id2)
  (eq? (Identifier-name id1)
       (Identifier-name id2)))

(define (valid-identifier-start? ch)
  (or (memq ch '(#\$ #\_))
      (char-alphabetic? ch)))

(define (valid-identifier-part? ch)
  (or (valid-identifier-start? ch)
      (char-numeric? ch)))

;; symbol -> boolean
(define (valid-identifier? id)
  (let ([chars (string->list (symbol->string id))])
    (and (pair? chars)
         (valid-identifier-start? (car chars))
         (andmap valid-identifier-part? (cdr chars))
         (not (memq id (lexical-keywords))))))

;; symbol * symbol * syntax -> symbol
(define (check-valid-identifier! name id ctx)
  (let ([chars (string->list (symbol->string id))])
    (cond
      [(null? chars)
       (raise-syntax-error name "invalid JavaScript identifier (empty string)" ctx)]
      [(not (valid-identifier-start? (car chars)))
       (raise-syntax-error name (format "invalid identifier ~a (contains illegal start character '~a')" id (car chars)) ctx)]
      [(findf (compose not valid-identifier-part?) (cdr chars))
       => (lambda (invalid)
            (raise-syntax-error name (format "invalid identifier ~a (contains illegal character '~a')" id invalid) ctx))]
      [(memq id (lexical-keywords))
       (raise-syntax-error name (format "invalid identifier ~a (reserved word)" id) ctx)]
      [else id])))

(define postfix-operators '(++ --))
(define prefix-operators '(delete void typeof ++ -- + - ~ !))
(define infix-operators '(* / % + -
                          << >> >>> < > <= >=
                          instanceof in
                          == != === !==
                          & ^ \|
                          && \|\|))
(define assignment-operators '(= *= /= %= += -= <<= >>= >>>= &= ^= \|=))

;; assignment-operator->infix-operator : assignment-operator -> (optional infix-operator)
(define (assignment-operator->infix-operator aop)
  (and (not (eq? aop '=))
       (let* ([aop-str (symbol->string aop)]
              [op-str (substring aop-str 0 (sub1 (string-length aop-str)))])
         (string->symbol op-str))))

(define (postfix-operator? x) (and (memq x postfix-operators) #t))
(define (prefix-operator? x) (and (memq x prefix-operators) #t))
(define (infix-operator? x) (and (memq x infix-operators) #t))
(define (assignment-operator? x) (and (memq x assignment-operators) #t))

(define PostfixOperator/c (apply symbols postfix-operators))
(define PrefixOperator/c (apply symbols prefix-operators))
(define InfixOperator/c (apply symbols infix-operators))
(define AssignmentOperator/c (apply symbols assignment-operators))

;; ===========================================================================
;; LOCATION INFORMATION
;; ===========================================================================

(define (source-location? x)
  (or (region? x)
      (syntax? x)
      #f))

(define source-location/c
  (or/c region? syntax? #f))

(define (source-location-source location)
  (cond
    [(region? location) (region-source location)]
    [(syntax? location) (syntax-source location)]
    [else #f]))

(define (source-location-start location)
  (cond
    [(region? location) (region-start location)]
    [(syntax? location) (make-posn (syntax-position location)
                                   (syntax-line location)
                                   (syntax-column location))]
    [else #f]))

(define (source-location-end location)
  (cond
    [(region? location) (region-end location)]
    [(syntax? location)
     (let ([start-offset (syntax-position location)]
           [start-line (syntax-line location)]
           [start-column (syntax-column location)]
           [span (syntax-span location)])
       (make-posn (+ start-offset span)
                  ;; TODO: these next two are bogus; perhaps we should change regions not to track the ending line/column
                  start-line
                  (+ start-column span)))]
    [else #f]))

;; has-location? : any -> boolean
(define (has-location? x)
  (or (token? x) (Term? x)))

;; ast-location : has-location -> source-location
(define (ast-location ast)
  (cond
    [(token? ast) (token-location ast)]
    [(Term? ast) (Term-location ast)]
    [else (error 'ast-location "not an ast node")]))

;; ast-source : has-location -> (optional any)
(define (ast-source t)
  (cond
    [(ast-location t) => source-location-source]
    [else #f]))

;; ast-start : has-location -> (optional posn)
(define (ast-start t)
  (cond
    [(ast-location t) => source-location-start]
    [else #f]))

;; ast-end : has-location -> (optional posn)
(define (ast-end t)
  (cond
    [(ast-location t) => source-location-end]
    [else #f]))

;; @ : (optional has-location) (optional has-location) -> (optional region)
(define (@ start end)
  (and start
       end
       (let ([source (ast-source start)]
             [start (ast-start start)]
             [end (ast-end end)])
         (and start end (make-region source start end)))))

(provide/contract
 [source-location? predicate/c]
 [source-location/c flat-contract?])

(provide/contract
 [has-location? predicate/c]
 [ast-location (has-location? . -> . source-location/c)]
 [ast-source (has-location? . -> . (optional/c any/c))]
 [ast-start (has-location? . -> . (optional/c posn?))]
 [ast-end (has-location? . -> . (optional/c posn?))]
 [@ ((optional/c has-location?) (optional/c has-location?) . -> . source-location/c)]
 [with-location (source-location/c Term? . -> . Term?)])

(provide/contract
 [Property? predicate/c]
 [SubStatement? predicate/c]
 [SourceElement? predicate/c]
 [Identifier=? (Identifier? Identifier? . -> . boolean?)]
 [Term=? (Term/X? Term/X? . -> . boolean?)])

(provide/contract
 [valid-identifier? (symbol? . -> . boolean?)]
 [check-valid-identifier! (symbol? symbol? syntax? . -> . symbol?)])

(provide/contract
 [Expression-predicates parameter?]
 [Statement-predicates parameter?]
 [ExpressionList-predicates parameter?]
 [StatementList-predicates parameter?])

(provide/contract
 [Expression/X? predicate/c]
 [Statement/X? predicate/c]
 [SubStatement/X? predicate/c]
 [ExpressionList/X? predicate/c]
 [StatementList/X? predicate/c]
 [SubStatementList/X? predicate/c])

(provide/contract
 [Term/X? predicate/c])

(provide/contract
 [postfix-operators (listof symbol?)]
 [prefix-operators (listof symbol?)]
 [infix-operators (listof symbol?)]
 [assignment-operators (listof symbol?)]
 [assignment-operator->infix-operator (assignment-operator? . -> . (optional/c infix-operator?))]
 [postfix-operator? predicate/c]
 [prefix-operator? predicate/c]
 [infix-operator? predicate/c]
 [assignment-operator? predicate/c]
 [PostfixOperator/c flat-contract?]
 [PrefixOperator/c flat-contract?]
 [InfixOperator/c flat-contract?]
 [AssignmentOperator/c flat-contract?])
