#lang scheme/base

(require (for-syntax scheme/base)
         scribble/manual
         scribble/eval
         scribble/basic
         (only-in srfi/13/string string-pad)
         scheme/string
         scheme/runtime-path)

(provide (all-defined-out))

(define-runtime-path home (build-path 'up))

(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require javascript/main))
    the-eval))

(define (bugref num [text (format "issue #~a" num)])
  (link (format "http://planet.plt-scheme.org/trac/ticket/~a" num) text))

(define (hist-item version year month day . text)
  (define (pad x n)
    (string-pad (format "~a" x) n #\0))
  (apply item (bold (format "Version ~a" version)) (format " (~a-~a-~a) - " year (pad month 2) (pad day 2)) text))

(define (jsver . parts)
  (string-join (for/list ([part parts]) (format "~a" part)) "."))

