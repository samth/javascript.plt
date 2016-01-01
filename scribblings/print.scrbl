#lang scribble/doc

@begin[(require scribble/manual)
       (require scribble/eval)
       (require scribble/basic)
       (require (for-label (except-in scheme/base exn:fail:syntax struct:exn:fail:syntax make-exn:fail:syntax exn:fail:syntax?)))
       (require (for-label scheme/contract))
       (require (for-label "../ast.ss"))
       (require (for-label "../print.ss"))
       (require "utils.ss")]

@title[#:tag "print"]{Pretty-Printing}

This library provides facilities for pretty-printing JavaScript source. It can be
required via:

@defmodule[javascript/print]

This library depends on the @tt{pprint} PLaneT package, which can be required via:

@schemeblock[(require (planet dherman/pprint:4))]

See the documentation for @tt{pprint} for information on how to use it.

@section[#:tag "format"]{Formatting Terms}

@defproc[(format-term (term Term/X?)) doc?]{
Formats any JavaScript term as a @tt{doc} for pretty-printing.}

@defproc[(format-source-element (element SourceElement?)) doc?]{
Formats a JavaScript source element as a @tt{doc} for pretty-printing.}

@defproc[(format-variable-initializer (init VariableInitializer?)) doc?]{
Formats a variable initializer as a @tt{doc} for pretty-printing.}

@defproc[(format-declaration (decl Declaration?)) doc?]{
Formats a declaration as a @tt{doc} for pretty-printing.}

@defproc[(format-expression (expr Expression/X?)) doc?]{
Formats an expression as a @tt{doc} for pretty-printing.}

@defproc[(format-subexpression (expr Expression?) (parent Expression?)) doc?]{
Formats an expression @scheme[expr] that occurs as an immediate subexpression of @scheme[parent]
as a @tt{doc} for pretty-printing. The parenthesization is determined based on the precedence
of the two expressions.}

@defproc[(format-statement (stmt Statement/X?)) doc?]{
Formats a statement as a @tt{doc} for pretty-printing.
@italic{Post-conditions:} The statement output includes its own semicolon if appropriate,
and statement output is not newline-terminated.}

@defproc[(format-nested-substatement (stmt SubStatement/X?)) doc?]{
Formats a statement that occurs as an indented substatement as a @tt{doc} for pretty-printing.
@italic{Post-condition:} The indentation level is returned to its previous level after the
substatement.}

@defproc[(format-substatement (stmt SubStatement/X?)) doc?]{
Formats a statement that occurs as a substatement as a @tt{doc} for pretty-printing.}

@defproc[(format-case-clause (clause CaseClause?)) doc?]{
Formats a @tt{case} clause as a @tt{doc} for pretty-printing.}

@defproc[(format-property (property Property?)) doc?]{
Formats an object property as a @tt{doc} for pretty-printing.}

@defproc[(format-identifier (id Identifier?)) doc?]{
Formats an identifier as a @tt{doc} for pretty-printing.}

@section[#:tag "format-config"]{Configuration Parameters}

@defthing[current-indentation-width (parameter/c natural-number/c)]{The number of spaces to indent.}
@defthing[collapse-lines? (parameter/c boolean?)]{(Currently ignored.)}
@defthing[collapse-simple-substatements? (parameter/c boolean?)]{Non-block substatements stay on same line?}

@section[#:tag "format-extensions"]{Extending the Pretty-Printer}

As described in @secref["extensions"], it is possible to extend the language with custom forms.
The pretty-printer can be extended with extra matchers that recognize just their form types and
produce pretty-printed @tt{doc}s. Matchers should simple fail to match (raising the same error
as @scheme[(match)] would for anything other than the forms they recognize.

@deftogether[
[@defthing[formatters/Expression (parameter/c (listof (any -> doc?)))]{}
 @defthing[formatters/Statement (parameter/c (listof (any -> doc?)))]{}
 @defthing[formatters/ExpressionList (parameter/c (listof (any -> doc?)))]{}
 @defthing[formatters/StatementList (parameter/c (listof (any -> doc?)))]{}]]

@defproc[(format-map (f (_a -> _c)) (elts (or (listof _a) _b)) (param (parameter/c (_b -> _c)))) _c]{
Applies a formatter to an extensible list of terms (such as an @scheme[ExpressionList/X] or @scheme[StatementList/X]).
If the formatter fails to match, the current value of the @scheme[param] parameter is used to format the elements
instead.}
