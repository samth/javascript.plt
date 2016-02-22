#lang scribble/doc

@begin[(require scribble/manual)
       (require scribble/eval)
       (require scribble/basic)
       (require (for-label (except-in scheme/base exn:fail:syntax struct:exn:fail:syntax make-exn:fail:syntax exn:fail:syntax?)))
       (require (for-label "../main.ss"))
       (require "utils.ss")]

@title[#:tag "intro"]{JavaScript for PLT Scheme}

This package is an implementation of the ECMAScript language specified by
@link["http://www.ecma-international.org/publications/standards/Ecma-262.htm"]{ECMA-262 Edition 3},
better known as JavaScript.

@section[#:tag "started"]{Getting Started}

The easiest way to get started using JavaScript for PLT Scheme is with the main module:

@defmodule[javascript]

This module provides everything in the entire package. Subsequent sections of this
manual describe the functionality of the individual libraries included, which can also be
required individually.

@examples[#:eval the-eval
          (eval-script "print(40 + 2)")
          (require pprint)
          (pretty-print
           (format-term
            (parse-source-element
             "while (true) { print('break!'); break }")))]

@section[#:tag "libraries"]{Libraries Provided by this Package}

This package includes:

@itemize[
  @item{A library for lexing and parsing JavaScript--see @secref["parse"]}
  @item{An extensible hierarchy of structure definitions for representing JavaScript abstract syntax--see @secref["ast"]}
  @item{A library for pretty-printing JavaScript to text--see @secref["print"]}
  @item{A library implementing @italic{Parenthetical JavaScript}, an S-expression based alternative syntax for JavaScript--see @secref["pjs"]}
  @item{A library for compiling JavaScript to PLT Scheme--see @secref["compile"]}
  @item{A library implementing the JavaScript runtime, including a representation of JavaScript values in Scheme as well as the standard JavaScript library--see @secref["runtime"]}
  @item{A library for evaluating JavaScript programs--see @secref["eval"]}
  @item{A library of configuration parameters--see @secref["config"]}
]

@section[#:tag "language"]{JavaScript Language for DrScheme}

Installing this PLaneT package also automatically registers a DrScheme language called @tt{JavaScript},
filed under @tt{Experimental Languages}. Selecting the @tt{JavaScript} language from DrScheme's
@tt{Language} menu allows you to create and load JavaScript programs and interact with JavaScript at
the REPL.

As of version 0.17 (released as PLaneT version 8:0), JavaScript is also available as
a module language. This can be used by beginning a JavaScript source file with the line:

@defmodulelang[javascript]

You can omit the PLaneT version numbers if you prefer. Programs without the version number
do not need to be updated when this PLaneT package is upgraded. However, it is then the
responsibility of the programmer to address any backwards-incompatible changes to the
JavaScript semantics.

@section[#:tag "design"]{Design Choices}

@itemize[
  @item{@bold{Ref type}: no expressions ever evaluate to refs at runtime. The standard
        allows for this possibility, but does not require it. This allows for a more
        efficient model of variable reference and assignment, one which matches Scheme's
        more closely.}
  @item{@bold{Bindings, @tt{with}, and @tt{eval}}: code that does not occur in the syntactic context
        of a @tt{with} statement is compiled more efficiently, using Scheme's lexical scope.
        Compilation of @tt{with} still complies with the standard, but is compiled to a much
        more inefficient form that creates a runtime representation of the environment as
        a linked list of objects. Similarly, in order to provide access to the caller's environment,
        @tt{eval} code is (dynamically) compiled to the more inefficient form.}
  @item{@bold{Indirect @tt{eval}}: only a direct call to a lexical variable with the name @tt{eval}
        is considered a legal call to the @tt{eval} function. Attempts to call @tt{eval}
        indirectly (e.g., via method calls or from variables with names other than @tt{eval})
        raise a dynamic error. This is permitted by the spec and allows for a more efficient
        implementation of variable reference and assignment.}
  @item{@bold{Proto}: I have not exposed the internal @tt{[[Prototype]]} property of objects,
        e.g. via a @tt{__proto__} property. I may choose to do so in a read-only form later.
        See @bugref[41].}
  @item{@bold{Interoperation with Scheme}: There is preliminary support for interoperation with
        Scheme. All Scheme values are JavaScript values and vice versa. Moreover, JavaScript functions
        are callable in Scheme as procedures and vice versa.}
]

@section[#:tag "limitations"]{Known Limitations}

Some limitations that are likely to be addressed eventually:

@itemize[
  @item{The @tt{toString} method for functions doesn't produce their source (see @bugref[34]).}
  @item{There may be some semantic issues with exceptions (see @bugref[35]):
        @itemize[
          @item{@tt{return} from @tt{finally} blocks}
          @item{suspicious uses of @scheme[dynamic-wind]}]}
  @item{The standard JavaScript @tt{Error} objects are not implemented, so library errors currently just throw strings.}
  @item{The JavaScript standard library is only about half-implemented (see @bugref[36]).}
  @;item{Proper tail recursion is not implemented (see @bugref[37]).}
  ]

Some limitations that are less likely to be fixed any time soon:

@itemize[
  @item{Numbers are not faithful to the spec;
        they are just represented as Scheme bignums (see @bugref[38]).}
  @item{Regular expressions don't work at all (see @bugref[39]).}
  @item{I haven't done any profiling or optimizing.}
  ]

@section[#:tag "feedback"]{Feedback and Bug Reports}

Before sending feedback or bug reports, please consult the
@link["http://planet.plt-scheme.org/trac/query?status=accepted&status=assigned&status=needinfo&status=new&status=reopened&component=dherman%2Fjavascript.plt&order=priority&col=id&col=summary&col=status&col=type&col=priority&col=milestone&col=component"
      "current set of registered issues"].
If you cannot find your issue there, feel free to
@link["http://planet.plt-scheme.org/trac/newticket?component=dherman/javascript.plt&owner=dherman"]{file a new bug report}
in the online issue database.

Any other feedback may be emailed to me, Dave Herman, at @link["mailto:dherman@ccs.neu.edu" "dherman@ccs.neu.edu"].

@section[#:tag "acknowledgments"]{Acknowledgments}

This package was developed by me, Dave Herman. I thank
Michael Greenberg, Dave Gurnell, Leo Meyerovich, and Jay McCarthy
for their helpful feedback. I also thank Richard Cobbe, Ryan
Culpepper, Carl Eastlund, Matthew Flatt, and Sam Tobin-Hochstadt
for helping me with my many technical questions.

@include-section["history.scrbl"]
