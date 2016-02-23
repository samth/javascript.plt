#lang scheme/base

(provide (struct-out exn:fail:syntax))

(define-struct (exn:fail:syntax exn:fail) (source location text) #:transparent)
