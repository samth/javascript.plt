#lang scribble/doc

@begin[(require scribble/manual)
       (require scribble/eval)
       (require scribble/basic)
       (require (for-label (except-in scheme/base exn:fail:syntax struct:exn:fail:syntax make-exn:fail:syntax exn:fail:syntax?)))
       (require (for-label scheme/contract))
       (require (for-label "../parse.ss"))
       (require (for-label "../print.ss"))
       (require "utils.ss")]

@title[#:tag "pjs"]{Parenthetical JavaScript}

This package recognizes an alternative, Scheme-like front-end syntax for JavaScript, dubbed
@italic{Parenthetical JavaScript} (or PJS for short). If you pronounce that "pee-jays" you will
make me happy.

The PJS library can be required via:

@defmodule[javascript/pjs]

@section[#:tag "pjs_grammar"]{Syntax}

The syntax of PJS is as follows:

@schemegrammar*[
    #:literals (#%expression #%statement #%keyword #%variable
                function var default case catch finally block
                if do while for for-in continue break return with switch label throw try
                regexp array object field-ref field new prefix postfix begin
                null this
                in)
    [program-unit (source-elt ...)]
    [source-elt decl stmt]
    [decl (function id (id ...) source-elt ...)
          (var var-init ...+)]
    [stmt (#%statement stmt)
          block-stmt
          (if expr stmt)
          (if expr stmt stmt)
          (do stmt expr)
          (while expr stmt)
          (for (var var-init ...+) expr expr stmt)
          (for expr expr expr stmt)
          (for (var id) in expr stmt)
          (for id in expr stmt)
          (continue maybe-id)
          (break maybe-id)
          (return maybe-expr)
          (with expr stmt)
          (switch expr case-clause ...)
          (label id stmt)
          (throw expr)
          (try stmt catch-clause finally-clause)
          (try stmt catch-clause)
          (try stmt finally-clause)
          ()
          expr]
    [block-stmt (block source-elt ...)]
    [expr (#%expression expr)
          (regexp string boolean boolean)
          (array array-elt ...)
          (object [id expr] ...)
          (field-ref expr expr)
          (field expr id)
          (new expr expr ...)
          (prefix prefix-op expr)
          (postfix expr postfix-op)
          (infix-op expr expr expr ...)
          (assign-op expr expr)
          (if expr expr expr)
          (function id (id ...) source-elt ...)
          (function (id ...) source-elt ...)
          (begin expr ...+)
          string
          number
          boolean
          null
          this
          javadot-id]
    [javadot-id id.id...]
    @;[prop-name id string number]
    [array-elt expr ()]
    [var-init id [id expr]]
    [case-clause (default stmt ...)
                 (case expr stmt ...)]
    [catch-clause (catch id stmt)]
    [finally-clause (finally stmt)]
]

@subsection[#:tag "operators"]{Operators}

The set of recognized operators is:

@itemize[
@item{@svar[prefix-op] : 
      @schemeid[++], @schemeid[--], @schemeid[+], @schemeid[-], @schemeid[~], @schemeid[!]}
@item{@svar[infix-op] : 
      @schemeid[*], @schemeid[/], @schemeid[%], @schemeid[+], @schemeid[-],
      @schemeid[<], @schemeid[<<], @schemeid[>], @schemeid[>>], @schemeid[>>>],
      @schemeid[<=], @schemeid[>=],
      @schemeid[==], @schemeid[!=], @schemeid[===], @schemeid[!==],
      @schemeid[&], @schemeid[^], @schemeid[\|],
      @schemeid[&&], @schemeid[\|\|]}
@item{@svar[postfix-op] : 
      @schemeid[++], @schemeid[--]}
@item{@svar[assign-op] : 
      @schemeid[=], @schemeid[*=], @schemeid[/=], @schemeid[%=], @schemeid[+=], @schemeid[-=],
      @schemeid[<<=], @schemeid[>>=], @schemeid[>>>=], @schemeid[&=], @schemeid[^=], @schemeid[\|=]}
]

For aesthetics and consistency with Scheme, some of the JavaScript operators with awkward or
obscure operator tokens are given convenient synonyms in PJS:

@itemize[
@item{@schemeid[or] = @schemeid[\|\|]}
@item{@schemeid[and] = @schemeid[&&]}
@item{@schemeid[bitwise-and] = @schemeid[&]}
@item{@schemeid[bitwise-ior] = @schemeid[\|]}
@item{@schemeid[bitwise-not] = @schemeid[~]}
@item{@schemeid[bitwise-xor] = @schemeid[^]}
]

@subsection[#:tag "identifiers"]{Identifiers}

Identifiers in PJS (i.e., the @svar[id] non-terminal in the grammar) are restricted to
@link["http://bclary.com/2004/11/07/#a-7.6"]{valid JavaScript identifiers}.
This ensures that, when generating actual JavaScript syntax, the exact name of a variable
or object property is preserved, with no name mangling. (Since variable names are sometimes observable
in JavaScript, this prevents subtle bugs where a program may change its behavior based on a
particular mangling.)

@section[#:tag "ambiguities"]{Resolving Ambiguities}

The grammar as presented contains several ambiguities.

@subsection[#:tag "disambiguating_with_keyword"]{Disambiguating operators with @tt{#%keyword}}

The following primitive operators have names that are valid JavaScript identifiers:

@itemize[
@item{@schemeid[and]}
@item{@schemeid[array]}
@item{@schemeid[begin]}
@item{@schemeid[block]}
@item{@schemeid[field]}
@item{@schemeid[label]}
@item{@schemeid[object]}
@item{@schemeid[or]}
@item{@schemeid[prefix]}
@item{@schemeid[postfix]}
@item{@schemeid[regexp]}
]

The parsing and code generation functions maintain an environment for lexically bound variables.
JavaScript variable bindings can therefore shadow these operators. For example, in the expression

@schemeblock[(function (array)
               (return (array 1 2 3)))]

the inner occurrence of @schemeid[array] is parsed as a variable reference rather than an array constructor.

Operators may be disambiguated with the use of the special form @schemeid[#%keyword]. For example, 
@scheme[(#%keyword . array)] unconditionally refers to the initial binding of @schemeid[array],
regardless of the current environment. The above example can then be rewritten as

@schemeblock[(function (array)
               (return ((#%keyword . array) 1 2 3)))]

Note that if JavaScript code would dynamically bind one of these names (e.g., via @schemeid[with] or
@schemeid[eval]), the operator is not shadowed, since the static environment only tracks lexical
bindings.

@subsection[#:tag "disambiguating_with_expression"]{Disambiguating expressions with @tt{#%expression}}

Expression statements introduce a few minor ambiguities into the PJS syntax.
These include @schemeid[function] declarations vs. named @schemeid[function]
expressions and @schemeid[if] statements vs. @schemeid[if] expressions.
These are always resolved in favor of the declaration or statement form rather than
the expression form, since in both cases the expression form is less likely to occur.

The expression form operator @schemeid[#%expression] forces its argument to be parsed
as an expression. For example, the expression @scheme[(#%expression (if x y z))] is
parsed as a conditional expression rather than a conditional statement.

@section[#:tag "syntactic_extensions"]{Syntactic Conveniences}

The PJS syntax provides several additional syntactic conveniences.

@subsection[#:tag "javadot"]{Java Dot Notation}

Similar to the 
@link["http://jscheme.sourceforge.net/jscheme/doc/javadot.html"]{Java Dot Notation} 
originally introduced by @link["http://jscheme.sourceforge.net"]{JScheme},
PJS permits the use of dotted identifiers as a
short-hand for the obvious corresponding chains of object field references. Unlike in
JScheme, it is impossible to bind a variable with a dot in its name, so
dotted identifiers are always unambiguously decoded as field references.

For example, the identifier @scheme[document.body.innerHTML] is equivalent to the
expression @scheme[(field (field document body) innerHTML)].

@subsection[#:tag "multiary"]{Multiary Operators}

Binary JavaScript operators are generalized in PJS to @italic{n}-ary
operators and expanded left-associatively into nested applications of the binary operator.

For example, the expression @scheme[(- 4 3 2 1)] is equivalent to @scheme[(- (- (- 4 3) 2) 1)].

@section[#:tag "pjs_api"]{Library Procedures}

The PJS library provides procedures that operate on either syntax objects or S-expressions.
The syntax object procedures maintain source location information.

@subsection[#:tag "from_syntax"]{S-expression Parsers}

@defproc[(syntax->expression (stx syntax?)) Expression?]{Parses the PJS expression @scheme[stx].}
@defproc[(syntax->statement (stx syntax?)) Statement?]{Parses the PJS statement @scheme[stx].}
@defproc[(syntax->source-element (stx syntax?)) SourceElement?]{Parses the PJS source element @scheme[stx].}
@defproc[(syntax->program-unit (stx syntax?)) (listof SourceElement?)]{Parses the PJS program unit @scheme[stx].}

@defproc[(sexp->expression (sexp sexp?)) Expression?]{Parses the PJS expression @scheme[sexp].}
@defproc[(sexp->statement (sexp sexp?)) Statement?]{Parses the PJS statement @scheme[sexp].}
@defproc[(sexp->source-element (sexp sexp?)) SourceElement?]{Parses the PJS source element @scheme[sexp].}
@defproc[(sexp->program-unit (sexp (listof sexp?))) (listof SourceElement?)]{Parses the PJS program unit @scheme[sexp].}

@subsection[#:tag "to_syntax"]{S-expression Generators}

@defproc[(expression->syntax (expr Expression?)) syntax?]{Generates a PJS representation of @scheme[expr].}
@defproc[(statement->syntax (stmt Statement?)) syntax?]{Generates a PJS representation of @scheme[stmt].}
@defproc[(source-element->syntax (elt SourceElement?)) syntax?]{Generates a PJS representation of @scheme[elt].}
@defproc[(program-unit->syntax (elts (listof SourceElement?))) syntax?]{Generates a PJS representation of @scheme[elts].}

@defproc[(expression->sexp (expr Expression?)) sexp?]{Generates a PJS representation of @scheme[expr].}
@defproc[(statement->sexp (stmt Statement?)) sexp?]{Generates a PJS representation of @scheme[stmt].}
@defproc[(source-element->sexp (elt SourceElement?)) sexp?]{Generates a PJS representation of @scheme[elt].}
@defproc[(program-unit->sexp (elts (listof SourceElement?))) sexp?]{Generates a PJS representation of @scheme[elts].}
