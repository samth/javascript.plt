#lang scheme/base

(require scheme/runtime-path
         scheme/gui/dynamic
         ;; These are required "in spirit" (i.e., dynamically):
         #;"runtime.ss"
         #;"standard-library.ss"
         #;"../../debug.ss"
         #;"../../lang/lang.ss")
(require (for-syntax scheme/base))

(provide make-js-namespace reset-js-namespace!)

(define-runtime-path home-directory (build-path 'up 'up))

(define (require-spec module-name . path)
  `(file ,(path->string (apply build-path home-directory (append path (list module-name))))))

(define runtime (require-spec "runtime.ss" "private" "runtime"))
(define standard-library (require-spec "standard-library.ss" "private" "runtime"))
(define gui-library (require-spec "gui-library.ss" "private" "runtime"))
(define debug (require-spec "debug.ss"))
(define lang (require-spec "lang.ss" "lang"))

;; make-js-namespace : -> javascript-namespace
(define (make-js-namespace)
  (let ([ns (if (gui-available?)
                ((gui-dynamic-require 'make-gui-namespace))
                (make-base-namespace))])
    (parameterize ([current-namespace ns])
      (dynamic-require runtime #f)
      (dynamic-require standard-library #f)
      (dynamic-require debug #f)
      (namespace-require 'javascript/lang/lang)
      (let ([path1 ((current-module-name-resolver) runtime #f #f)]
            [path2 ((current-module-name-resolver) standard-library #f #f)]
            [path3 ((current-module-name-resolver) debug #f #f)]
            [path4 ((current-module-name-resolver) lang #f #f)])
        (namespace-attach-module ns path1)
        (namespace-attach-module ns path2)
        (namespace-attach-module ns path3)
        (namespace-attach-module ns path4)))
    (reset-js-namespace! ns)
    ns))

;; reset-javascript-namespace! : javascript-namespace -> any
(define (reset-js-namespace! ns)
  (parameterize ([current-namespace ns])
    (let ([global-object (dynamic-require runtime 'global-object)]
          [install-standard-library! (dynamic-require standard-library 'install-standard-library!)])
      (install-standard-library! global-object)
      (when (gui-available?)
        ((dynamic-require gui-library 'install-gui-library!) global-object))))
  (void))
