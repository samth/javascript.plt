#lang scribble/doc

@begin[(require scribble/manual)
       (require scribble/eval)
       (require scribble/basic)
       (require (for-label (except-in scheme/base exn:fail:syntax struct:exn:fail:syntax make-exn:fail:syntax exn:fail:syntax?)))
       (require (for-label "../main.ss"))
       (require "utils.ss")]

@title[#:tag "top"]{JavaScript for PLT Scheme}

by Dave Herman (@tt{dherman at ccs dot neu dot edu})

This package is an implementation of the ECMAScript language specified by
@link["http://www.ecma-international.org/publications/standards/Ecma-262.htm"]{ECMA-262 Edition 3},
better known as JavaScript.

For license information, please see the file @tt{COPYING.LIB}.


@table-of-contents[]

@include-section["intro.scrbl"]

@include-section["parse.scrbl"]

@include-section["ast.scrbl"]

@include-section["print.scrbl"]

@include-section["pjs.scrbl"]

@include-section["compile.scrbl"]

@include-section["runtime.scrbl"]

@include-section["eval.scrbl"]

@include-section["config.scrbl"]

@index-section[]
