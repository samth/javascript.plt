#lang scheme/base

(require (for-syntax scheme/base))
(provide (all-defined-out))

(define-struct (exn:runtime exn) (value) #:transparent)

;; TODO: create a current-location syntax-parameter

(define-syntax (raise-runtime-exception stx)
  (syntax-case stx ()
    [(_ loc e)
     (with-syntax ([c-c-m (syntax/loc #'loc (current-continuation-marks))])
       #'(raise (make-exn:runtime "uncaught exception" c-c-m e)))]))

(define-syntax-rule (raise-reference-error loc e)
  (let ([message (format "~a is not defined" e)])
    (raise-runtime-exception loc message)))

(define-syntax-rule (raise-runtime-type-error loc expected actual)
  (let ([message (format "expected ~a, received ~a" expected actual)])
    (raise-runtime-exception loc message)))

;; TODO: make an object of the appropriate type
;; TODO: include the source code
(define-syntax-rule (raise-assignment-error loc)
  (let ([message "invalid assignment left-hand-side"])
    (raise-runtime-exception loc message)))
