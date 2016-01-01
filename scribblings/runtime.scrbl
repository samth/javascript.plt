#lang scribble/doc

@begin[(require scribble/manual)
       (require scribble/eval)
       (require scribble/basic)
       (require (for-label (except-in scheme/base exn:fail:syntax struct:exn:fail:syntax make-exn:fail:syntax exn:fail:syntax?)))
       (require (for-label scheme/contract))
       (require (for-label "../runtime.ss"))
       (require "utils.ss")]

@title[#:tag "runtime"]{Runtime System}

This library implements the JavaScript runtime system. It can be required via:

@defmodule[javascript/runtime]

@section[#:tag "namespaces"]{Namespaces}

@defproc[(make-js-namespace) namespace?]{
Creates a PLT namespace containing the standard JavaScript top-level bindings.}

@defproc[(reset-js-namespace! (ns namespace?)) any]{
Resets the global object in JavaScript namespace @scheme[ns].}

@section[#:tag "values"]{Values}

All Scheme values are legal JavaScript values. A subset of Scheme values are designated as
@deftech{native} JavaScript values. A native value is one of:

@itemize[
  @item{@scheme[void?]: the JavaScript @tt{undefined} value}
  @item{@scheme[null?]: the JavaScript @tt{null} value}
  @item{@scheme[boolean?]: a JavaScript primitive boolean value}
  @item{@scheme[string?]: a JavaScript primitive string value}
  @item{@scheme[number?]: a JavaScript primitive number value}
  @item{@scheme[object?]: a JavaScript object}
  @item{@scheme[function?]: a JavaScript function}
  @item{@scheme[array?]: a JavaScript primitive array object}
  @item{@scheme[wrapper?]: a JavaScript object wrapping a primitive value}]

@section[#:tag "objects"]{Objects}

The type @scheme[object?] is an opaque structure type. The types @scheme[function?], @scheme[array?] and @scheme[wrapper?] are
structure subtypes of @scheme[object?]. All objects carry an internal, mutable @deftech{property table}.

@;defstruct[object ([call (or/c function? #f)] [construct (or/c function? #f)] [proto (or/c object? #f)] [class string?] [properties (hash-of string? property?)])]{
@;A JavaScript object.}

A @deftech{property} is one of:

@itemize[
  @item{@scheme[property-value?]}
  @item{@scheme[attributed?]}]

Plain value properties are assumed to have no attributes. An @deftech{attributed} value
may have any combination of the attributes @scheme[DONT-ENUM?], @scheme[READ-ONLY?], and @scheme[DONT-DELETE?].

@defstruct[attributed ([value value?] [attributes bit-set?])]{}

A @deftech{bit-set} is an efficient representation of a vector of booleans.

@defproc[(bit-flag-set? (x bit-field?) (bit bit?)) boolean?]{Checks for a bit in a bit set.}

@defthing[READ-ONLY? bit?]{A bit representing the ECMAScript @tt{ReadOnly} attribute.}
@defthing[DONT-ENUM? bit?]{A bit representing the ECMAScript @tt{DontEnum} attribute.}
@defthing[DONT-DELETE? bit?]{A bit representing the ECMAScript @tt{DontDelete} attribute.}

A @deftech{property-value} is one of:

@itemize[
  @item{@scheme[ref?]}
  @item{@scheme[value?]}]

A @deftech{ref} is a special property with custom get, set, and delete behavior.

@defproc[(ref? (x any)) boolean?]{Determines whether @scheme[x] is a @tech{ref}.}
@defproc[(set-ref! (x ref?) (v value?)) any]{Invoke @scheme[x]'s setter.}
@defproc[(delete-ref! (x ref?)) any]{Invoke @scheme[x]'s deleter.}
@defproc[(deref (x ref?)) any]{Invoke @scheme[x]'s getter.}

@;JavaScript array values are represented specially for faster access of numeric properties:

@;defstruct[(array object) ([vector (evector-of property?)])]{}

@;defproc[(function? (x any)) boolean?]{Determines whether a value is a callable object.}

@defproc[(has-property? (x object?) (key string?)) boolean?]{Checks for the presence of property @scheme[key] in @scheme[x], searching the prototype chain if necessary.}
@defproc[(has-own-property? (x object?) (key string?)) boolean?]{Checks for the presence of property @scheme[key] in @scheme[x] without searching the prototype chain.}
@defproc[(has-attribute? (x property?) (bit bit?)) boolean?]{Checks for attribute @scheme[bit] in property @scheme[x].}

@; XXX: update this contract
@defproc[(object-get (x object?) (key string?)) (optional/c value?)]{Attempts to lookup property @scheme[key] in @scheme[x].}
@defproc[(object-set! (x object?) (key string?) (v value?)) any]{Sets property @scheme[key] in @scheme[x].}
@defproc[(object-delete! (x object?) (key string?)) any]{Attempts to delete property @scheme[key] from @scheme[x].}
@defproc[(object-keys-stream (x object?)) (-> string?)]{Produces an enumeration stream consistent with JavaScript's @tt{for..in} semantics.}

@section[#:tag "library"]{JavaScript Library}

@defthing[global-object object?]{The global object.}

@defproc[(install-standard-library! (global object?)) any]{Installs the properties of the standard library in @scheme[global].}
