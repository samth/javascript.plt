#lang scribble/doc

@begin[(require scribble/manual)
       (require scribble/eval)
       (require scribble/basic)
       (require (for-label (except-in scheme/base exn:fail:syntax struct:exn:fail:syntax make-exn:fail:syntax exn:fail:syntax?)))
       (require (for-label scheme/contract))
       (require (for-label "../eval.ss"))
       (require (for-label "../parse.ss"))
       (require (for-label "../runtime.ss"))
       (require "utils.ss")]

@title[#:tag "eval"]{Evaluation}

This library provides facilities for evaluating JavaScript. It can be
required via:

@defmodule/this-package[eval]

@defproc[(eval-script (src (or/c input-source? syntax?)) (ns namespace? (make-js-namespace))) any]{
Evaluates a JavaScript script from source @scheme[src] in namespace @scheme[ns].}

@defproc[(eval-expression (src (or/c input-source? syntax?)) (ns namespace? (make-js-namespace))) any]{
Evaluates a JavaScript expression from source @scheme[src] in namespace @scheme[ns].}
