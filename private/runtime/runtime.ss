#lang scheme/base

(require "exceptions.ss" "operator.ss" "value.ss")

(define original-eval? (make-parameter #t))

;; make-frame : hash-table -> object
(define (make-frame table)
  (build-object table))

(provide original-eval?
         make-frame
         (all-from-out "exceptions.ss")
         (all-from-out "operator.ss"))

;; from exceptions.ss:
(provide (struct-out exn:runtime))

;; from value.ss:
(provide current-this)
(provide bit-field make-bit-field bit-flag-set?)
(provide READ-ONLY? DONT-ENUM? DONT-DELETE?)
(provide ref? deref set-ref! delete-ref!)
(provide (struct-out object)
         (struct-out function)
         (struct-out wrapper)
         (struct-out array)
         (struct-out attributed))
(provide object-get-attributes has-property? has-own-property? has-attribute?
         object-table build-object0 object-get object-set! object-put! object-delete! object-keys object-keys* object-keys-stream
         object-class)
(provide scope-chain-get scope-chain-set! scope-chain-delete!)
(provide #;call)
(provide completion->value completion->string
         any->boolean any->string any->object any->primitive native->primitive
         any->property-name
         any->string/debug object->string/debug
         any->number native->number any->integer any->int32 any->uint32 any->uint16)
;(provide true-value?)
(provide previous-completion complete! nothing)
(provide build-object build-array build-function build-arguments-object)
(provide list->array)
;(provide make-arguments-object)
(provide global-object)
(provide current-Function-context)
