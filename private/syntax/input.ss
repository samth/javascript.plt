#lang scheme/base

(provide (all-defined-out))

;; input-source? : any -> boolean
(define (input-source? x)
  (or (string? x)
      (path? x)
      (input-port? x)))

;; input-source->input-port : input-source -> input-port
(define (input-source->input-port in)
  (cond
    [(string? in) (open-input-string in)]
    [(path? in) (open-input-file in)]
    [(input-port? in) in]))
