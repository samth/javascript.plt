#lang scheme/base

(require "private/runtime/exceptions.ss"
         "private/runtime/runtime.ss"
         "private/runtime/standard-library.ss"
         "private/runtime/namespace.ss")

(provide (all-from-out "private/runtime/exceptions.ss"))
(provide global-object install-standard-library!)
(provide make-js-namespace reset-js-namespace!)

(provide object? ;(struct-out object)
         array? ;(struct-out array)
         function?
         wrapper?
         (struct-out attributed)
         ref? set-ref! delete-ref! deref)
(provide bit-flag-set? READ-ONLY? DONT-ENUM? DONT-DELETE?)
(provide has-property? has-own-property? has-attribute?
         object-get object-set! object-delete! object-keys-stream)
