#lang scribble/doc

@begin[(require scribble/manual)
       (require scribble/eval)
       (require scribble/basic)
       (require (for-label (except-in scheme/base exn:fail:syntax struct:exn:fail:syntax make-exn:fail:syntax exn:fail:syntax?)))
       (require (for-label scheme/contract))
       (require (for-label "../ast.ss"))
       (require (for-label "../compile.ss"))
       (require "utils.ss")]

@title[#:tag "compile"]{Compiling to Scheme}

This library implements a JavaScript-to-Scheme compiler. It be can required via:

@defmodule[javascript/compile]

@defproc[(compile-script (elts (listof SourceElement?))) syntax?]{
Compiles a JavaScript script to Scheme.}

@defproc[(compile-interaction (elts (listof SourceElement?))) syntax?]{
Compiles a JavaScript REPL interaction to Scheme.}

@defproc[(compile-module (elts (listof SourceElement?))) syntax?]{
Compiles a JavaScript module to Scheme.}

@defproc[(compile-expression (expr Expression/X?)) syntax?]{
Compiles a JavaScript script to Scheme.}
