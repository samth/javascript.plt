#lang scheme/base

(require (planet "evector.scm" ("soegaard" "evector.plt" 1))
         (only-in scheme/gui/base message-box)
         srfi/13/string
         "exceptions.ss"
         "value.ss"
         "standard-library.ss")

(provide install-gui-library!)

(define js:alert
  (build-function 1
    (lambda args
      (when (null? args)
        (raise-runtime-exception here "not enough arguments"))
      (let* ([msg (any->string (car args))]
             [msg-padded (if (< (string-length msg) 20)
                             (string-pad-right msg 20 #\space)
                             msg)])
        (message-box "JavaScript" msg-padded #f '(ok)))
      (void))))

(define global-methods
  `((alert ,js:alert)))

(define (install-gui-library! global)
  (install-properties! global global-methods)
  global)
