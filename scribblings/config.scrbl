#lang scribble/doc

@begin[(require scribble/manual)
       (require scribble/eval)
       (require scribble/basic)
       (require (for-label (except-in scheme/base exn:fail:syntax struct:exn:fail:syntax make-exn:fail:syntax exn:fail:syntax?)))
       (require (for-label scheme/contract))
       (require (for-label "../config.ss"))
       (require "utils.ss")]

@title[#:tag "config"]{Configuration Parameters}

This library provides configuration parameters for the other libraries. It be can
required via:

@defmodule/this-package[config]

@defthing[allow-anonymous-function-source-elements? (parameter/c boolean?)]{Allow anonymous function expressions to appear in source-element contexts (non-standard)?}
@defthing[infer-do-while-semicolon? (parameter/c boolean?)]{Allow @tt{do..while} statements to omit the final semicolon (non-standard)?}
@defthing[enable-extended-catch-statements? (parameter/c boolean?)]{Allow Mozilla-style multiple catch clauses (non-standard)?}
@defthing[allow-nested-function-declarations? (parameter/c boolean?)]{Allow function declarations to appear nested within statements (non-standard)?}
@defthing[proper-tail-recursion? (parameter/c boolean?)]{Enable proper tail recursion? (Not implemented.)}
@defthing[stack-limit (parameter/c (optional/c natural-number/c))]{Impose an artificial stack depth limit.}
@defthing[allow-eval-aliasing? (parameter/c boolean?)]{Allow treating @tt{eval} as a general value?}
@defthing[code-representation (parameter/c (one-of/c 'standard 'sexp))]{Used for the DrScheme tool to choose between the standard JavaScript syntax and an S-expression syntax.}
@defthing[lexical-keywords (parameter/c (listof symbol?))]{Recognized lexical keywords.}
@defthing[enable-let-expressions? (parameter/c boolean?)]{Enable non-standard @tt{let} expressions?}
@defthing[debug-destination (parameter/c (one-of/c 'error-port 'debug-console))]{}
@defthing[debug-scope-resolution? (parameter/c boolean?)]{}
@defthing[debug-unbound-references? (parameter/c boolean?)]{}

@defthing[ecma-strict? (pseudo-parameter/c boolean?)]{A pseudo-parameter for setting parameters above to adhere strictly to the Ecma standard.}
@defthing[current-debug-port (parameter/c output-port?)]{The output port used for displaying debugging information during evaluation.}
