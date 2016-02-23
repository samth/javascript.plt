#lang scheme

(require rackunit
         scheme/string
         (only-in srfi/13/string string-trim-both)
         "../../runtime.ss"
         "../../eval.ss")

(provide (rename-out [check-output* check-output])
         (rename-out [check-result* check-result])
         run test-ns)

(define test-ns (make-js-namespace))

(define (run . lines)
  (reset-js-namespace! test-ns)
  (eval-script (string-join lines "\n") test-ns))

(define-binary-check (check-output actual expected)
  (andmap string=? actual expected))

(define-syntax-rule (check-output* expected lines ...)
  (check-output (regexp-split #rx"[\r\n]+" (string-trim-both (with-output-to-string (lambda () (run lines ...))))) expected))

;(define-simple-check (check-output* expected lines)
;  (let ([actual (with-output-to-string (apply run lines))])
;    (andmap string=?
;            expected
;            (regexp-split #rx"[\r\n]+" (string-trim-both actual)))))

(define /dev/null
  (make-output-port
   'null
   always-evt
   (lambda (s start end non-block? breakable?) (- end start))
   void
   (lambda (special non-block? breakable?) #t)
   (lambda (s start end) (wrap-evt
                          always-evt
                          (lambda (x)
                            (- end start))))
   (lambda (special) always-evt)))

(define-binary-check (check-result actual expected)
  (case expected
    [(object) (object? actual)]
    [(array) (array? actual)]
    [else (equal? expected actual)]))

(define-syntax-rule (check-result* expected lines ...)
  (check-result (parameterize ([current-output-port /dev/null])
                  (run lines ...))
                expected))

;(define-simple-check (check-result* expected lines)
;  (let ([actual (parameterize ([current-output-port /dev/null])
;                  (apply run lines))])
;    (case expected
;      [(object) (object? actual)]
;      [(array) (array? actual)]
;      [else (equal? expected actual)])))
;
;(define-syntax-rule (check-result expected lines ...)
;  (check-result* expected (list lines ...)))
