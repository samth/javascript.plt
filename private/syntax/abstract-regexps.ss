#lang scheme/base

(require (for-syntax scheme/base scheme/match srfi/13/string)
         parser-tools/lex)

(provide define-abstract-regexps make-rx make-lex)

(define-for-syntax regexp-keywords
  '(union sequence kleene* kleene+ maybe complement range any save))

(define-syntax (define-abstract-regexps stx)
  (define (interp x env)
    (define (interp* xs)
      (map (lambda (x)
             (interp x env))
           xs))
    (syntax-case x ()
      [(op are ...)
       (and (identifier? #'op)
            (memq (syntax->datum #'op) regexp-keywords))
       `(,(syntax->datum #'op) ,@(interp* (syntax->list #'(are ...))))]
      [datum
       (let ([v (syntax->datum #'datum)])
         (or (string? v) (char? v)))
       (syntax->datum #'datum)]
      [id
       (identifier? #'id)
       (cond
         [(assf (λ (i) (free-identifier=? #'id i)) env) => cdr]
         [else (raise-syntax-error 'define-abstract-regexps "unbound regexp variable" x x)])]))
  (syntax-case stx ()
    [(_ [name binding] ...)
     (let loop ([names (syntax->list #'(name ...))]
                [bindings (syntax->list #'(binding ...))]
                [env '()])
       (if (null? names)
           (with-syntax ([(b ...) (reverse (map cdr env))])
             #'(begin (define-syntax name 'b) ...))
           (loop (cdr names)
                 (cdr bindings)
                 (cons (cons (car names) (interp (car bindings) env))
                       env))))]))

(define-for-syntax (string-append-map c->s s)
  (string-fold-right (lambda (c rest)
                       (string-append (c->s c) rest))
                     ""
                     s))

(define-for-syntax rx-special-chars
  '(#\? #\| #\^ #\$ #\& #\( #\) #\{ #\} #\[ #\] #\+ #\- #\* #\. #\\))

(define-for-syntax (escape-rx c)
  (if (memq c rx-special-chars)
      (string #\\ c)
      (string c)))

(define-for-syntax (single-char? x)
  (or (char? x)
      (and (string? x) (= (string-length x) 1))))

(define-syntax (make-rx stx)
  ;; char-are? : are -> boolean
  (define (char-are? are)
    (match are
      [(list 'range c1 c2) #t]
      [(list 'union (? char-are?) ...) #t]
      [(list 'sequence (? char-are?)) #t]
      [(list 'any) #t]
      [(? char?) #t]
      [(? string?) (= (string-length are) 1)]
      [_ #f]))
  
  ;; char-are->rx : char-are -> string
  (define (char-are->rx char-are)
    (match char-are
      [(list 'range c1 c2) (format "~a-~a" (escape-rx c1) (escape-rx c2))]
      [(list 'union char-ares ...) (string-append-map escape-rx char-ares)]
      [(list 'sequence char-are) (char-are->rx char-are)]
      [(list 'any) "."]
      [(? char?) (escape-rx char-are)]
      [(? string?) (string-append-map escape-rx char-are)]
      [_ (error 'are->rx "must match exactly one character")]))
  
  ;; simple-are? : are -> boolean
  (define (simple-are? are)
    (match are
      [(? char-are?) #t]
      [(list 'range c1 c2) #t]
      [(list 'union (? simple-are?)) #t]
      [(list 'sequence (? simple-are?)) #t]
      [(list 'any) #t]
      [(list 'complement char-ares ...) #t]
      [(list 'save ares ...) #t]
      [(? char?) #t]
      [(? string?) (= (string-length are) 1)]
      [_ #f]))
  
  ;; are->rx : are -> string
  (define (are->rx are)
    (match are
      [(list 'range char-are1 char-are2)
       (format "[~a-~a]" (escape-rx char-are1) (escape-rx char-are2))]
      [(list 'kleene* are)
       (format "~a*" (are->sub-rx are))]
      [(list 'kleene* ares ...)
       (format "(?:~a)*" (apply string-append (map are->rx ares)))]
      [(list 'kleene+ are)
       (format "~a+" (are->sub-rx are))]
      [(list 'kleene+ ares ...)
       (format "(?:~a)+" (apply string-append (map are->rx ares)))]
      [(list 'union are)
       (are->rx are)]
      [(list 'union (? char-are? ares) ...)
       (apply string-append (cons "[" (append (map char-are->rx ares) (list "]"))))]
      [(list 'union ares ...)
       (string-join (map are->sub-rx ares) "|" 'infix)]
      [(list 'sequence are)
       (are->rx are)]
      [(list 'sequence ares ...)
       (apply string-append (map are->sub-rx ares))]
      [(list 'any)
       "."]
      [(list 'maybe are)
       (format "~a?" (are->sub-rx are))]
      [(list 'maybe ares ...)
       (format "(?:~a)?" (apply string-append (map are->rx ares)))]
      [(list 'complement char-ares ...)
       (format "[^~a]" (apply string-append (map char-are->rx char-ares)))]
      [(list 'save are)
       (format "(~a)" (are->rx are))]
      [(? string?) (string-append-map escape-rx are)]
      [(? char?) (escape-rx are)]))
  
  (define (are->sub-rx are)
    (if (simple-are? are)
        (are->rx are)
        (format "(?:~a)" (are->rx are))))
  
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (with-syntax ([rx (string-append "^" (are->sub-rx (syntax-local-value #'id)))])
       #'(regexp rx))]))

(define-lex-trans make-lex
  (letrec ([are->sre (lambda (are)
                       (match are
                         [(list 'range char-are1 char-are2)
                          (with-syntax ([c1 char-are1]
                                        [c2 char-are2])
                            #'(char-range c1 c2))]
                         [(list 'kleene* ares ...)
                          (with-syntax ([(ares* ...) (map are->sre ares)])
                            #'(repetition 0 +inf.0 (concatenation ares* ...)))]
                         [(list 'kleene+ ares ...)
                          (with-syntax ([(ares* ...) (map are->sre ares)])
                            #'(repetition 1 +inf.0 (concatenation ares* ...)))]
                         [(list 'union ares ...)
                          (with-syntax ([(ares* ...) (map are->sre ares)])
                            #'(union ares* ...))]
                         [(list 'sequence ares ...)
                          (with-syntax ([(ares* ...) (map are->sre ares)])
                            #'(concatenation ares* ...))]
                         [(list 'any)
                          #'(any-char)]
                         [(list 'maybe ares ...)
                          (with-syntax ([(ares* ...) (map are->sre ares)])
                            #'(repetition 0 1 (concatenation ares* ...)))]
                         [(list 'complement char-ares ...)
                          (with-syntax ([re (apply string-append
                                                   (map (lambda (x)
                                                          (cond
                                                            [(string? x) x]
                                                            [(char? x) (string x)]
                                                            [else (raise-syntax-error
                                                                   'make-lex
                                                                   "complement can only contain chars or strings")]))
                                                        char-ares))])
                            #'(char-complement (char-set re)))]
                         [(list 'save are)
                          (are->sre are)]
                         [(? string?)
                          (with-syntax ([s are])
                            #'s)]
                         [(? char?)
                          (with-syntax ([c are])
                            #'c)]))])
    (lambda (stx)
      (syntax-case stx ()
        [(_ id)
         (identifier? #'id)
         (are->sre (syntax-local-value #'id))]))))

(define-syntax define-rx
  (syntax-rules ()
    [(_ name binding)
     (begin
       (define-abstract-regexps [t binding])
       (define name (make-rx t)))]))
