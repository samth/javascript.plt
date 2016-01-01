#lang scheme/base

(require scheme/match
         "private/config.ss")

(provide debug)

(define (observing? topic)
  (case topic
    [(scope-resolution) (debug-scope-resolution?)]
    [(unbound-reference) (debug-unbound-references?)]
    [else (error 'debug (format "unknown topic: ~a" topic))]))

(define (debug . args)
  (match args
    [(list (? symbol? topic) (? string? fmt) rest-args ...)
     (when (observing? topic)
       (parameterize ([print-struct #t])
         (apply fprintf (current-debug-port) (string-append "~a: " fmt "~n") topic rest-args)))]
    [(list (? string? fmt) rest-args ...)
     (apply fprintf (current-debug-port) (string-append "DEBUG: " fmt "~n") rest-args)]))

;  (define (debug topic fmt . args)
;    (when (observing? topic)
;      (parameterize ([print-struct #t])
;        (apply fprintf (current-debug-port) (string-append "~a: " fmt "~n") topic args))))
