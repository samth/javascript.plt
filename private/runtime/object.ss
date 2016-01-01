#lang scheme/base

(require (for-syntax scheme/base))
(require "exceptions.ss")
(provide (all-defined-out))

;; empty-bit-field : bit-field
(define empty-bit-field 0)

;; empty-bit-field? : bit-field -> boolean
(define empty-bit-field? zero?)

;; bit-field : bit-flag ... -> bit-field
(define (bit-field . flags)
  (if (null? flags) 0 (apply bitwise-ior flags)))

;; make-bit-field : (cons bit-flag boolean) ... -> bit-field
(define (make-bit-field . pairs)
  (apply bit-field (map (lambda (pair)
                          (if (cdr pair) (car pair) 0))
                        pairs)))

(define-syntax (define-bit-flags stx)
  (syntax-case stx ()
    [(_ (x1 x2 ...))
     (with-syntax ([(i1 i2 ...) (for/list ([i (in-range (length (syntax->list #'(x1 x2 ...))))])
                                  (expt 2 i))])
       #'(begin (define x1 i1)
                (define x2 i2)
                ...))]))

;; bit-flag-set? : bit-field bit-flag -> boolean
(define (bit-flag-set? bf flag)
  (not (zero? (bitwise-and bf flag))))

(define current-this (make-parameter #f))

(define-struct attributed ([value #:mutable] attributes) #:transparent)

(define-syntax (object-table stx)
  (syntax-case stx ()
    [(_ [key . value-info] ...)
     (with-syntax ([(name ...)
                    (map (lambda (stx)
                           (let ([x (syntax->datum stx)])
                             (cond
                               [(string? x) (with-syntax ([symbol (string->symbol x)])
                                              #'(quote symbol))]
                               [(number? x) (with-syntax ([symbol (string->symbol (number->string x))])
                                              #'(quote symbol))]
                               [(symbol? x) (with-syntax ([symbol x])
                                              #'(quote symbol))]
                               [else (raise-syntax-error 'object-table "expected symbol, string literal, or number literal" stx)])))
                         (syntax->list #'(key ...)))]
                   [(value ...)
                    (map (lambda (stx)
                           (syntax-case stx ()
                             [(value) #'value]
                             [(value (attributes ...))
                              #'(make-attributed value (bit-field attributes ...))]
                             [(getter setter (attributes ...))
                              #'(make-attributed (make-ref getter setter (lambda () #f))
                                                 (bit-field DONT-DELETE? attributes ...))]))
                         (syntax->list #'(value-info ...)))])
       #'(let ([table (make-hasheq)])
           (hash-set! table name value) ...
;           (for ([n '(key ...)]
;                 [v (list value ...)])
;             (hash-set! table n v))
           table))]))

;; CONVENTION: Never allow a closure to close over a binding to an object's
;;             property table; this may prevent garbage collection of an object's
;;             properties if its table is replaced...?
(define-struct object (proto [properties #:mutable])
;  #:property
;  prop:procedure
;  (struct-field-index call)
  #:property
  prop:custom-write
  (lambda (o out write?)
    (if write?
        (invoke-method o 'write (list out) (lambda ()
                                             (invoke-method o 'display (list out) (lambda ()
                                                                                    (display "[object Object]" out)))))
        (invoke-method o 'display (list out) (lambda ()
                                               (invoke-method o 'write (list out) (lambda ()
                                                                                    (display "[object Object]" out))))))))

(define-struct (function object) (call construct)
  #:property
  prop:procedure
  (struct-field-index call))

(define-struct (name object) ())

;; symbol * any
(define-struct (wrapper object) (class value))

(define-struct (array object) (vector))

(define (object-class o)
  (cond
    [(array? o) 'Array]
    [(function? o) 'Function]
    [(wrapper? o) (wrapper-class o)]
    [(name? o) 'Name]
    [else 'Object]))

(define (invoke-method obj method-name args [fk (lambda ()
                                                  (raise-runtime-type-error here (format "object with ~a method" method-name) "object"))])
  (parameterize ([current-this obj])
    ((let/ec return
       (let ([method (hash-ref (object-properties obj) method-name (lambda () (return fk)))])
         (if (or (function? method) (procedure? method))
             (lambda () (apply method args))
             fk))))))

(define-struct ref (get set! delete!))

;(define (function? x)
;  (if (object? x)
;      (and (object-call x) #t)
;      (procedure? x)))

(define-bit-flags (READ-ONLY? DONT-ENUM? DONT-DELETE?))
