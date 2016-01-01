#lang scribble/doc

@begin[(require scribble/manual)
       (require scribble/eval)
       (require scribble/basic)
       (require (for-label (except-in scheme/base exn:fail:syntax struct:exn:fail:syntax make-exn:fail:syntax exn:fail:syntax?)))
       (require (for-label scheme/contract))
       (require (for-label "../parse.ss"))
       (require (for-label "../ast.ss"))
       (require (for-label (except-in scheme/class object? make-object)))
       (require "utils.ss")
       #;(provide parse)]

@title[#:tag "parse"]{Lexing and Parsing}

This library provides facilities for lexing and parsing JavaScript source. It can be
required via:

@defmodule[javascript/parse]

@section[#:tag "input"]{Input Sources}

An @deftech{input-source} is either a string, a path, or an input port.

@defproc[(input-source? (x any)) boolean?]{Determines whether @scheme[x] is an @tech{input-source}.}

@defproc[(input-source->input-port (in input-source?)) input-port?]{
Produces an input port for reading from @scheme[in].}

@section[#:tag "source"]{Source Representation}

@defstruct[position ([offset exact-positive-integer?] [line exact-positive-integer?] [col exact-nonnegative-integer?])]{
Re-exported from the PLT Scheme collection @scheme[parser-tools/lex].}

@defstruct[region ([source any] [start position?] [end position?])]{
Represents a region of text from @scheme[start] (inclusive) to @scheme[end] (exclusive).}

@defstruct[regexp-contents ([pattern string] [global? boolean?] [case-insensitive? boolean?])]{
The source to a regular expression token, with regular expression pattern @scheme[pattern] and
flags @scheme[global?], representing the @tt{/g} option, and @scheme[case-insensitive?],
representing the @tt{/i} option.}

@defstruct[token ([type symbol?] [contents (optional/c
                                            (or/c string? number? symbol? regexp-contents?))] [location region?])]{
A single token of input.}

@defproc[(region->string (rgn region?)) string?]{
Produces a string representation of a region, convenient for debugging and error reporting.}

@section[#:tag "syntax-errors"]{Syntax Errors}

@defstruct[(exn:fail:syntax exn:fail) ([source (implementation?/c lexer<%>)] [location region?] [text any])]{}

@section[#:tag "lexer"]{Lexer Objects}

@definterface[lexer<%> ()]{
A JavaScript lexical scanner.
@defmethod[(fail (fmt string?) (arg any) ...) any]{Raises an @scheme[exn:fail:syntax] exception with error message @scheme[(format fmt arg ...)].}
@defmethod[(fail/loc (loc region?) (text any) (fmt string?) (arg any) ...) any]{Raises an @scheme[exn:fail:syntax] exception with source location @scheme[loc] and error message @scheme[(format fmt arg ...)].}
@defmethod[(done?) boolean?]{Determines whether the end of input has been reached.}
@defmethod[(current-token) token?]{Produces the current token in the lexer's input stream.}
@defmethod[(match (type symbol?)) (optional token?)]{Attempts to read a token of type @scheme[type], producing the token on success and @scheme[#f] on failure.}
@defmethod[(must-match (type symbol?)) token?]{Attempts to read a token of type @scheme[type], produce the token on success and raising an @scheme[exn:fail:syntax] exception on failure.}
@defmethod[(peek-token (skip natural-number/c 0)) token?]{Produces the next token (after skipping @scheme[skip] tokens) in the input stream without changing the current position in the input stream.}
@defmethod[(peek-token/infix-operator (skip natural-number/c 0)) token?]{Similar to @scheme[peek-token], but assumes the lexer is being used in a parsing state that expects an infix operator.}
@defmethod[(peek-token/same-line) token?]{Similar to @scheme[peek-token], but does not lex past end-of-line sequences.}
@defmethod[(read-token (skip natural-number/c 0)) token?]{Produces the next token (after skipping @scheme[skip] tokens) in the input stream and updates the current position in the input stream.}
@defmethod[(read-token/same-line) token?]{Similar to @scheme[read-token], but does not lex past end-of-line sequences.}
@defmethod[(unread-token) any]{Rewinds the current position in the input stream by one token.}
@defmethod[(skip-whitespace) any]{Skips past any whitespace in the underlying input port.}}

@defclass[lexer% object% (lexer<%>)]{
  An implementation of @scheme[lexer<%>], a JavaScript lexical scanner.
  @defconstructor[([port input-port?] [name any (object-name port)])]{
    Constructs a new @scheme[lexer%] which reads tokens from @scheme[port].
    The optional @scheme[name] argument is used for source location information and error reporting.}}

@section[#:tag "lexing"]{Lexing Functions}

@defproc[(lex (in input-source?)) (-> token?)]{
Convenience function for producing a functional lexer from an input source.}

@section[#:tag "parser"]{Parser Objects}

@definterface[parser<%> ()]{
A JavaScript parser.
@defmethod[(parse-source-element) SourceElement?]{Parses a single source element.}
@defmethod[(parse-source-elements) (listof SourceElement?)]{Parses a sequence of source elements.}
@defmethod[(parse-expression) Expression?]{Parses a single expression.}
@defmethod[(skip-empty-tokens) any]{Skips past meaningless whitespace tokens.}
}

@defclass[parser% object% (parser<%>)]{
  An implementation of @scheme[parser<%>], a JavaScript parser.
  @defconstructor[([lexer (implementation/c lexer<%>)])]{
    Constructs a new @scheme[parser%] which receives tokens from @scheme[lexer].}}

@defproc[(input-source->parser (in input-source?)) (is-a?/c parser<%>)]{
Produces a JavaScript parser for the input from @scheme[in].}

@section[#:tag "parsing"]{Parsing Functions}

@defproc[(parse-program-unit (in input-source?)) (listof SourceElement?)]{
Parses a JavaScript program unit from @scheme[in].}

@defproc[(parse-expression (in input-source?)) Expression?]{
Parses a JavaScript expression from @scheme[in].}

@defproc[(parse-function-constructor (args string?) (body string?)) FunctionExpression?]{
Uses the arguments constructed from the JavaScript @tt{Function} constructor to parse a function expression.
The @scheme[args] string represents the comma-separated formal parameter list, and the @scheme[body] string
represents the function body (not including the surrounding braces).}

@defproc[(parse-source-element (in input-source?)) SourceElement?]{
Parses a JavaScript source element from @scheme[in].}
