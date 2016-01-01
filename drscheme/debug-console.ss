#lang scheme/base

(require (planet dherman/widgets:2/text)
         scheme/gui/base
         framework
         scheme/class)

(provide create-debug-console)

(define debug-frame%
  (class (frame:basic-mixin frame%)
    (init label (parent #f) (width 640) (height 480))
    (inherit show get-area-container)
    (super-new (label label)
               (parent parent)
               (width width)
               (height height))

    (define-values (input-port output-port)
      (make-pipe #f 'debug 'debug))

    (define read-thread
      (thread (lambda ()
                (let loop ()
                  (let ([in (read-line input-port)])
                    (unless (eof-object? in)
                      (append (format "~a~n" in))
                      (loop)))))))

    (define/public (append str)
      (send editor insert/programmatic str (send editor last-position)))
    (define/public (get-debug-port) output-port)

    (define contents (instantiate editor-canvas% ((get-area-container))))
    (define editor (instantiate read-only-text% ()))

    (define/public (kill)
      (show #f)
      (kill-thread read-thread))

    (send contents set-editor editor)))

(define (create-debug-console)
  (instantiate debug-frame% ("JavaScript Debug Console")))
