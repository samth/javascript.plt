#lang scribble/doc

@begin[(require scribble/manual)
       (require scribble/eval)
       (require scribble/basic)
       (require (for-label (except-in scheme/base exn:fail:syntax struct:exn:fail:syntax make-exn:fail:syntax exn:fail:syntax?)))
       (require (for-label "../main.ss"))
       (require "utils.ss")]

@title[#:tag "history"]{History}

@itemize[
@hist-item[(jsver 0 18) 2009 01 02]{New features including Scheme interoperability and runtime enhancements, Parenthetical JavaScript, and Harmony extensions.
                                    @itemize[
                                     @item{Changed runtime representation to interoperate with Scheme (notably booleans and procedures).}
                                     @item{Experimental Harmony features: tail-recursive block literals and @tt{do}-expressions.}
                                     @item{Continuation marks via a global @tt{Trace} constructor.}
                                     @item{Improvements to internal representation of objects.}
                                     @item{New @tt{Name} constructor and support for @tt{Name} objects as property names.}
                                     @item{@secref["pjs"]: renewed support for S-expression syntax, now with improved syntax and support for source-location tracking.}
                                    ]}
@hist-item[(jsver 0 17) 2008 11 11]{Implemented modules and more of the standard library.
                                    @itemize[
                                     @item{Implemented @tt{Array} library.}
                                     @item{Implemented other miscellaneous standard libraries.}
                                     @item{Fixed a bug with treating primitives as objects.}
                                     @item{Implemented a module language, allowing interoperability with Scheme modules.}
                                     @item{Fixed some prototype chain relationships in the standard library objects.}
                                     @item{Implemented a @hash-lang{} reader, enabling use with the Module language.}
                                     @item{Implemented @tt{import} and @tt{export} declarations.}
                                     @item{Major reimplementation of variable binding.}
                                     @item{Correct implementation of @tt{eval} that has access to the lexical environment.}
                                     @item{API changes in @tt{eval.ss}, @tt{parse.ss}, and @tt{compile.ss}.}
                                    ]}
@hist-item[(jsver 0 16) 2008 10 20]{Major bugfix release:
                                    @itemize[
                                     @item{Significant re-implementation of internal environment structure.}
                                     @item{Fixed a bug in lookup of non-string property names.}
                                     @item{Implemented @tt{eval} with proper inheritance of the environment and variable object 
                                           (see @link["http://bclary.com/2004/11/07/#a-10.1.3"]{10.1.3}).}
                                     @item{Mutating @tt{with}-bound variables now persists correctly.}
                                     @item{Fixed bugs in the implementation of variable assignment in dynamic code (i.e., code under @tt{with} or @tt{eval}).}
                                     @item{Indirect @tt{eval} now raises a runtime exception when called.}
                                   ]}
@hist-item[(jsver 0 15) 2008 9 30]{Bugfix release:
                                   @itemize[
                                     @item{Exported some internals from @tt{print.ss}.}
                                     @item{Exported the utilities for the S-expression representation of JavaScript.}
                                     @item{Changed the AST structure types to be @scheme[#:prefab].}
                                     @item{Added a generic @scheme[Term=?] predicate to @tt{ast.ss}.}
                                   ]}
@hist-item[(jsver 0 14) 2008 9 8]{Major overhaul for PLT v4, including:
                                  @itemize[
                                    @item{New required methods for DrScheme language interface.}
                                    @item{Scribble documentation.}
                                    @item{Rearranged API's for better consistency and organization (still needs more cleanup, though).}
                                    @item{Convenience @tt{main.ss} module for easy PLaneT require.}
                                    @item{Moved all known bugs into PLaneT Trac database.}
                                    @item{Pretty-printer uses new @tt{pprint:4} library.}
                                    @item{Directory structure reorganization--more internal libraries under @tt{private/}.}
                                  ]}
@hist-item[(jsver 0 13) 2006 8 8]{Expressions are extensible: leaf nodes are checked with custom predicates.
                                  Pretty-printing is extensible: extended nodes are printed with custom printers.}
@hist-item[(jsver 0 12) 2006 7 18]{Fixed a bug related to nested @tt{with} statements.}
@hist-item[(jsver 0 11) 2006 7 17]{Stubbed lots of standard libraries.
                                   Some more documentation.
                                   Added @scheme[parse-function-expression] to @scheme[parser<%>] interface.
                                   Implemented reflective @tt{Function} constructor.
                                   Implemented primitive constructors (except @tt{Date} and @tt{RegExp}).
                                   Moved language level files into subdirectory.}
@hist-item[(jsver 0 10) 2006 7 13]{Implemented JavaScript `eval' library function.}
@hist-item[(jsver 0 9) 2006 7 12]{Created @tt{eval-}* functions for interpreting JavaScript.
                                  Should soon be able to implement JavaScript @tt{eval}.
                                  Improved tests.}
@hist-item[(jsver 0 8) 2006 7 12]{Fixed bug involving nested @tt{with} forms.
                                  Corrected implementation of binding of @tt{catch} variables.
                                  Implemented all @tt{let} forms.}
@hist-item[(jsver 0 7) 2006 7 11]{Fixed some small type-related bugs with hoisting and compilation.}
@hist-item[(jsver 0 6) 2006 7 11]{Fixed nested @tt{with} bug.}
@hist-item[(jsver 0 5) 2006 7 11]{New implementation of binding: no more evaluating to @tech{ref}s!
                                  Nested @tt{with} forms are broken.
                                  Binding arrows are broken for top-level.}
@hist-item[(jsver 0 4) 2006 7 6]{Got rid of unnecessary @tt{PLTCOLLECTS} hack for testing.
                                 Fixed bugs in @tt{sexp} and @tt{pretty-print} due to changed AST types.
                                 Hoisting for ES4 @tt{let} corrected.}
@hist-item[(jsver 0 3) 2006 6 28]{Drastically improved binding implementation.
                                  Addressed serious bugs in implementation of functions.
                                  Began implementation for debug console (still disabled).}
@hist-item[(jsver 0 2) 2006 6 22]{Added newly required @scheme[capability-value] to language level interface.
                                  Bug fixed for pretty-printer (thanks to Jay).
                                  Serialized language level settings.
                                  Broke @tt{let}.
                                  (Internally: hoisting for ES4 @tt{let} in place.)}
@hist-item[(jsver 0 1) 2006 2 23]{Initial release.
                                  Implements most of ECMA-262 Edition 3.
                                  Primary limitations:
                                  @itemize[
                                    @item{Lack of regular expressions}
                                    @item{Lack of standard library}
                                    @item{No reflection or function reification}]}
]