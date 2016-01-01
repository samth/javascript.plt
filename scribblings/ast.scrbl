#lang scribble/doc

@begin[(require scribble/manual)
       (require scribble/eval)
       (require scribble/basic)
       (require (for-label (except-in scheme/base exn:fail:syntax struct:exn:fail:syntax make-exn:fail:syntax exn:fail:syntax? )))
       (require (for-label scheme/contract unstable/contract))
       (require (for-label "../ast.ss"))
       (require "utils.ss")]

@title[#:tag "ast"]{Abstract Syntax}

This library provides an extensible hierarchy of structure types representing abstract
JavaScript syntax. It can be required via:

@defmodule[javascript/ast]

@defstruct[Term ([location (maybe/c region?)])]{
The base structure type of all abstract syntax.}

@section[#:tag "declarations"]{Declarations}

@defstruct[(Declaration Term) ([location (maybe/c region?)])]
@defstruct[(FunctionDeclaration Declaration) ([location (maybe/c region?)]
                                              [name Identifier?]
                                              [args (listof Identifier?)]
                                              [body (listof SourceElement?)])]
@defstruct[(ImportDeclaration Declaration) ([location (maybe/c region?)]
                                            [specifiers (listof ImportSpecifier?)])]
@defstruct[(ExportDeclaration Declaration) ([location (maybe/c region?)]
                                            [specifiers (listof (or/c ExclusionList? ReexportSpecifier? ExportBindings? Identifier?))])]
@defstruct[(VariableDeclaration Declaration) ([location (maybe/c region?)]
                                              [bindings (nelistof/c VariableInitializer?)])]
@defstruct[(LetDeclaration Declaration) ([location (maybe/c region?)]
                                         [bindings (or/c (nelistof/c VariableInitializer?)
                                                         (nelistof/c FunctionDeclaration?))])]
@section[#:tag "expressions"]{Expressions}

@defstruct[(Expression Term) ([location (maybe/c region?)])]

@defstruct[(StringLiteral Expression) ([location (maybe/c region?)]
                                       [value string?])]
@defstruct[(RegexpLiteral Expression) ([location (maybe/c region?)]
                                       [pattern string?]
                                       [global? boolean?]
                                       [case-insensitive? boolean?])]
@defstruct[(NumericLiteral Expression) ([location (maybe/c region?)]
                                        [value number?])]
@defstruct[(BooleanLiteral Expression) ([location (maybe/c region?)]
                                        [value boolean?])]
@defstruct[(NullLiteral Expression) ([location (maybe/c region?)])]
@defstruct[(ArrayLiteral Expression) ([location (maybe/c region?)]
                                      [elements (listof (maybe/c Expression/X?))])]
@defstruct[(ObjectLiteral Expression) ([location (maybe/c region?)]
                                       [properties (listof (cons/c Property? Expression/X?))])]
@defstruct[(ThisReference Expression) ([location (maybe/c region?)])]
@defstruct[(VarReference Expression) ([location (maybe/c region?)]
                                      [id Identifier?])]
@defstruct[(BracketReference Expression) ([location (maybe/c region?)]
                                          [container Expression/X?]
                                          [key Expression/X?])]
@defstruct[(DotReference Expression) ([location (maybe/c region?)]
                                      [container Expression/X?]
                                      [id Identifier?])]
@defstruct[(NewExpression Expression) ([location (maybe/c region?)]
                                       [constructor Expression/X?]
                                       [arguments ExpressionList/X?])]
@defstruct[(PostfixExpression Expression) ([location (maybe/c region?)]
                                           [expression Expression/X?]
                                           [operator PostfixOperator/c])]
@defstruct[(PrefixExpression Expression) ([location (maybe/c region?)]
                                          [operator PrefixOperator/c]
                                          [expression Expression/X?])]
@defstruct[(InfixExpression Expression) ([location (maybe/c region?)]
                                         [left Expression/X?]
                                         [operator InfixOperator/c]
                                         [right Expression/X?])]
@defstruct[(ConditionalExpression Expression) ([location (maybe/c region?)]
                                               [test Expression/X?]
                                               [consequent Expression/X?]
                                               [alternate Expression/X?])]
@defstruct[(AssignmentExpression Expression) ([location (maybe/c region?)]
                                              [lhs Expression/X?]
                                              [operator AssignmentOperator/c]
                                              [rhs Expression/X?])]
@defstruct[(FunctionExpression Expression) ([location (maybe/c region?)]
                                            [name (maybe/c Identifier?)]
                                            [args (listof Identifier?)]
                                            [body (listof SourceElement?)])]
@defstruct[(LetExpression Expression) ([location (maybe/c region?)]
                                       [bindings (listof VariableInitializer?)]
                                       [body Expression/X?])]
@defstruct[(CallExpression Expression) ([location (maybe/c region?)]
                                        [method Expression/X?]
                                        [args ExpressionList/X?])]
@defstruct[(ParenExpression Expression) ([location (maybe/c region?)]
                                         [expression Expression/X?])]
@defstruct[(ListExpression Expression) ([location (maybe/c region?)]
                                        [expressions ExpressionList/X?])]

@section[#:tag "statements"]{Statements}

@defstruct[(Statement Term) ([location (maybe/c region?)])]

@defstruct[(BlockStatement Statement) ([location (maybe/c region?)]
                                       [statements SubStatementList/X?])]
@defstruct[(EmptyStatement Statement) ([location (maybe/c region?)])]
@defstruct[(ExpressionStatement Statement) ([location (maybe/c region?)]
                                            [expression Expression/X?])]
@defstruct[(IfStatement Statement) ([location (maybe/c region?)]
                                    [test Expression/X?]
                                    [consequent SubStatement/X?]
                                    [alternate (maybe/c SubStatement/X?)])]
@defstruct[(DoWhileStatement Statement) ([location (maybe/c region?)]
                                         [body SubStatement/X?]
                                         [test Expression/X?])]
@defstruct[(WhileStatement Statement) ([location (maybe/c region?)]
                                       [test Expression/X?]
                                       [body SubStatement/X?])]
@defstruct[(ForStatement Statement) ([location (maybe/c region?)]
                                     [init (or/c (maybe/c Expression/X?) VariableDeclaration? LetDeclaration?)]
                                     [test (maybe/c Expression/X?)]
                                     [incr (maybe/c Expression/X?)]
                                     [body SubStatement/X?])]
@defstruct[(ForInStatement Statement) ([location (maybe/c region?)]
                                       [lhs (or/c Expression/X? VariableDeclaration? LetDeclaration?)]
                                       [container Expression/X?]
                                       [body SubStatement/X?])]
@defstruct[(ContinueStatement Statement) ([location (maybe/c region?)]
                                          [label (maybe/c Identifier?)])]
@defstruct[(BreakStatement Statement) ([location (maybe/c region?)]
                                       [label (maybe/c Identifier?)])]
@defstruct[(ReturnStatement Statement) ([location (maybe/c region?)]
                                        [value (maybe/c Expression/X?)])]
@defstruct[(LetStatement Statement) ([location (maybe/c region?)]
                                     [bindings (listof VariableInitializer?)]
                                     [body SubStatement/X?])]
@defstruct[(WithStatement Statement) ([location (maybe/c region?)]
                                      [context Expression/X?]
                                      [body SubStatement/X?])]
@defstruct[(SwitchStatement Statement) ([location (maybe/c region?)]
                                        [expression Expression/X?]
                                        [cases (listof CaseClause?)])]
@defstruct[(LabelledStatement Statement) ([location (maybe/c region?)]
                                          [label Identifier?]
                                          [statement SubStatement/X?])]
@defstruct[(ThrowStatement Statement) ([location (maybe/c region?)]
                                       [value Expression/X?])]
@defstruct[(TryStatement Statement) ([location (maybe/c region?)]
                                     [body Statement/X?]
                                     [catch (listof CatchClause?)]
                                     [finally (maybe/c Statement/X?)])]

@section[#:tag "miscterms"]{Miscellaneous Terms}

@defstruct[(Identifier Term) ([location (maybe/c region?)]
                              [name symbol?])]
@defstruct[(CaseClause Term) ([location (maybe/c region?)]
                              [question (maybe/c Expression/X?)]
                              [answer SubStatementList/X?])]
@defstruct[(CatchClause Term) ([location (maybe/c region?)]
                               [id Identifier?]
                               [body Statement/X?])]
@defstruct[(VariableInitializer Term) ([location (maybe/c region?)]
                                       [id Identifier?]
                                       [init (maybe/c Expression/X?)])]
@defstruct[(ImportSpecifier Term) ([location (maybe/c region?)]
                                   [module ModuleSpecifier?]
                                   [bindings (or/c Identifier? (listof ImportBinding?) ExclusionList?)])]
@defstruct[(ImportBinding Term) ([location (maybe/c region?)]
                                 [label Identifier?]
                                 [binding (maybe/c Identifier?)])]
@defstruct[(ExclusionList Term) ([location (maybe/c region?)]
                                 [ids (listof Identifier?)])]
@defstruct[(ModuleSpecifier Term) ([location (maybe/c region?)]
                                   [protocol (or/c 'file 'planet 'collect)]
                                   [elements (listof (or/c string? integer? symbol? #f))])]
@defstruct[(ReexportSpecifier Term) ([location (maybe/c region?)]
                                     [module ModuleSpecifier?]
                                     [exclusions ExclusionList?])]
@defstruct[(ExportBindings Term) ([location (maybe/c region?)]
                                  [bindings (listof (cons/c Identifier? (maybe/c Expression/X?)))])]

@section[#:tag "utilities"]{Utility Functions}

@defproc[(has-location? (x Term?)) boolean?]{Determines whether @scheme[x] has source location information.}
@defproc[(ast-location (x (and/c Term? has-location?))) region?]{}
@defproc[(ast-source (x (and/c Term? has-location?))) any]{}
@defproc[(ast-start (x (and/c Term? has-location?))) position?]{}
@defproc[(ast-end (x (and/c Term? has-location?))) position?]{}
@defproc[(|@| (start-term Term?) (end-term Term?)) region?]{}
@defproc[(with-location (loc (maybe/c region?)) (x Term?)) Term?]{}

@defproc[(Property? (x Term?)) boolean?]{Determines whether @scheme[x] can be used as a property name in an object literal.}
@defproc[(SubStatement? (x any)) boolean?]{Determines whether @scheme[x] is a valid sub-statement, i.e., a statement not in the top-level position of a function or program.}
@defproc[(SourceElement? (x any)) boolean?]{Determines whether @scheme[x] is a @deftech{source element}, i.e., a statement or declaration.}
@defproc[(Identifier=? (x Identifier?) (y Identifier?)) boolean?]{Compares @scheme[x] and @scheme[y] for name equality.}
@defproc[(Term=? (x Term?) (y Term?)) boolean?]{
Recursively compares two terms, ignoring all source location information and comparing all non-@tt{Term} components
with @scheme[equal?].}

@defthing[postfix-operators (listof symbol?)]{The postfix operators of JavaScript.}
@defthing[prefix-operators (listof symbol?)]{The prefix operators of JavaScript.}
@defthing[infix-operators (listof symbol?)]{The infix operators of JavaScript.}
@defthing[assignment-operators (listof symbol?)]{The assignment operators of JavaScript.}
@defproc[(assignment-operator->infix-operator (op assignment-operator?)) (maybe/c infix-operator?)]{Produces the binary infix operator corresponding to @scheme[op].}
@defproc[(postfix-operator? (x any)) boolean?]{Determines whether @scheme[x] is a JavaScript postfix operator.}
@defproc[(prefix-operator? (x any)) boolean?]{Determines whether @scheme[x] is a JavaScript prefix operator.}
@defproc[(infix-operator? (x any)) boolean?]{Determines whether @scheme[x] is a JavaScript infix operator.}
@defproc[(assignment-operator? (x any)) boolean?]{Determines whether @scheme[x] is a JavaScript assignment operator.}
@defthing[PostfixOperator/c flat-contract?]{Contract form of @scheme[postfix-operator?].}
@defthing[PrefixOperator/c flat-contract?]{Contract form of @scheme[prefix-operator?].}
@defthing[InfixOperator/c flat-contract?]{Contract form of @scheme[infix-operator?].}
@defthing[AssignmentOperator/c flat-contract?]{Contract form of @scheme[assignment-operator?].}

@section[#:tag "extensions"]{Extending the Language}

Some libraries in this package support extending the language. The notions of expression,
statement, expression list, and statement list can be extended by adding custom predicates
to the following parameters:

@deftogether[
[@defthing[Expression-predicates (parameter/c (listof (any -> boolean?)))]{}
 @defthing[Statement-predicates (parameter/c (listof (any -> boolean?)))]{}
 @defthing[ExpressionList-predicates (parameter/c (listof (any -> boolean?)))]{}
 @defthing[StatementList-predicates (parameter/c (listof (any -> boolean?)))]{}]]

The following predicates represent extensible terms by checking an argument against the
standard predicates followed by the corresponding custom predicates:

@deftogether[
[@defproc[(Expression/X? (x any)) boolean?]{}
 @defproc[(Statement/X? (x any)) boolean?]{}
 @defproc[(SubStatement/X? (x any)) boolean?]{}
 @defproc[(ExpressionList/X? (x any)) boolean?]{}
 @defproc[(StatementList/X? (x any)) boolean?]{}
 @defproc[(SubStatementList/X? (x any)) boolean?]{}
 @defproc[(Term/X? (x any)) boolean?]{}]]
