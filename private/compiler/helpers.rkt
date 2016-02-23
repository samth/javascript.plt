#lang scheme/base

(require "../syntax/ast-core.ss"
         "../syntax/ast-utils.ss"
         "../syntax/token.ss"
         "../runtime/runtime.ss")
(require (for-template scheme/base)
         (for-template "../runtime/runtime.ss"))

(provide (all-defined-out))

;; This syntax object will have the syntax-original? property. It can be used
;; with datum->syntax-object to give subsequent syntax objects this property.
(define stx-for-original-property (read-syntax #f (open-input-string "original")))

;; Identifier->syntax : Identifier -> syntax
(define (Identifier->syntax id [loc (Term-location id)] #:context [context #f])
  (build-syntax (Identifier-name id) loc #t context))

;; location identifier -> identifier
(define (with-loc loc id)
  (datum->syntax id (syntax->datum id) (location->syntax loc)))

;; operator->syntax : (union infix-operator prefix-operator) -> syntax
(define (operator->syntax op)
  (if (or (infix-operator? op) (prefix-operator? op))
      (datum->syntax #'js:in (string->symbol (format "js:~a" op)))
      (error 'operator->syntax "bad operator")))

;; Identifier->name-syntax : Identifier -> syntax
(define (Identifier->name-syntax id)
  (with-syntax ([symbol-literal (Identifier-name id)])
    #'(quote symbol-literal)))

;; Identifier->string-syntax : Identifier -> syntax
(define (Identifier->string-syntax id)
  (build-syntax (symbol->string (Identifier-name id))
                (Term-location id)))

;; Identifier->string-expression : Identifier -> syntax
(define (Identifier->string-expression id)
  (datum->syntax #'here
                 `(quote ,(symbol->string (Identifier-name id)))
                 #f
                 stx-for-original-property))

;; build-syntax : any [(optional location) boolean (optional syntax)] -> syntax
(define (build-syntax expr [location #f] [original? #t] [context #f])
  (datum->syntax context
                 expr
                 (and location (location->syntax location original?))
                 (and original? stx-for-original-property)))

;; location->syntax : source-location [boolean] -> syntax
(define (location->syntax loc [original? #t])
  (cond
    [(and (syntax? loc) (syntax-source loc) (syntax-position loc)) loc]
    [(syntax? loc) (region->syntax #f original?)]
    [else (region->syntax loc original?)]))

;; region->syntax : (optional region) [boolean] -> syntax
(define (region->syntax region [original? #t])
  (if (not region)
      (datum->syntax #f 'source-location #f (and original? stx-for-original-property))
      (let ([start (region-start region)]
            [end (region-end region)])
        (datum->syntax #f
                       'source-location
                       (list
                        (region-source region)
                        (posn-line start)
                        (posn-col start)
                        (posn-offset start)
                        (- (posn-offset end) (posn-offset start)))
                       (and original? stx-for-original-property)))))
