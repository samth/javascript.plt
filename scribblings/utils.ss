#lang scheme/base

(require (for-syntax scheme/base)
         (for-syntax planet/util)
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
    (parameterize ([current-directory home])
      (the-eval '(require "main.ss")))
    the-eval))

(define (bugref num [text (format "issue #~a" num)])
  (link (format "http://planet.plt-scheme.org/trac/ticket/~a" num) text))

(define (hist-item version year month day . text)
  (define (pad x n)
    (string-pad (format "~a" x) n #\0))
  (apply item (bold (format "Version ~a" version)) (format " (~a-~a-~a) - " year (pad month 2) (pad day 2)) text))

(define (jsver . parts)
  (string-join (for/list ([part parts]) (format "~a" part)) "."))

(define-for-syntax (build-planet-string suffix)
  (format "~a/~a:~a:~a~a"
          (this-package-version-owner)
          (regexp-replace "\\.plt$" (this-package-version-name) "")
          (this-package-version-maj)
          (this-package-version-min)
          suffix))

(define-for-syntax (build-planet-id stx id)
  (datum->syntax
   stx
   (string->symbol
    (build-planet-string
     (if id (format "/~a" (syntax-e id)) "")))))

(define-syntax (this-package-version-id stx)
  (syntax-case stx ()
    [(tpvi) (build-planet-id stx #f)]
    [(tpvi name)
     (identifier? #'name)
     (build-planet-id stx #'name)]))

(define-syntax (scheme/this-package stx)
  (syntax-case stx ()
    [(sp)
     (quasisyntax/loc stx
       (scheme (planet #,(build-planet-id stx #f))))]
    [(sp name)
     (identifier? #'name)
     (quasisyntax/loc stx
       (scheme (planet #,(build-planet-id stx #'name))))]))

(define-syntax (defmodule/this-package stx)
  (syntax-case stx ()
    [(dmp)
     (quasisyntax/loc stx
       (defmodule (planet #,(build-planet-id stx #f))))]
    [(dmp name)
     (identifier? #'name)
     (quasisyntax/loc stx
       (defmodule (planet #,(build-planet-id stx #'name))))]))

(define-syntax (lang/this-package stx)
  (syntax-case stx ()
    [(sp)
     (quasisyntax/loc stx
       (SCHEME (UNSYNTAX (hash-lang)) planet #,(build-planet-id stx #f)))]
    [(sp name)
     (identifier? #'name)
     (quasisyntax/loc stx
       (SCHEME (UNSYNTAX (hash-lang)) planet #,(build-planet-id stx #'name)))]))
