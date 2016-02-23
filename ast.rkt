#lang scheme/base

(require scheme/contract
         unstable/contract
         "private/syntax/ast-core.ss"
         "private/syntax/ast-utils.ss"
         "private/syntax/token.ss")

(provide source-location/c)

(provide/contract
 [Term? predicate/c]
 [Term-location (Term? . -> . source-location/c)])

(provide/contract
 (struct (Declaration Term) ([location source-location/c]))
 (struct (Expression Term) ([location source-location/c]))
 (struct (Statement Term) ([location source-location/c]))
 (struct (Identifier Term) ([location source-location/c]
                            [name symbol?]))
 (struct (CaseClause Term) ([location source-location/c]
                            [question (maybe/c Expression/X?)]
                            [answer SubStatementList/X?]))
 (struct (CatchClause Term) ([location source-location/c]
                             [id Identifier?]
                             [body Statement/X?]))
 (struct (VariableInitializer Term) ([location source-location/c]
                                     [id Identifier?]
                                     [init (maybe/c Expression/X?)]))
 (struct (ImportSpecifier Term) ([location source-location/c]
                                 [module ModuleSpecifier?]
                                 [bindings (or/c Identifier? (listof ImportBinding?) ExclusionList?)]))
 (struct (ImportBinding Term) ([location source-location/c]
                               [label Identifier?]
                               [binding (maybe/c Identifier?)]))
 (struct (ExclusionList Term) ([location source-location/c]
                               [ids (listof Identifier?)]))
 (struct (ModuleSpecifier Term) ([location source-location/c]
                                 [protocol (or/c 'file 'planet 'collect)]
                                 [elements (listof (or/c string? integer? symbol? #f))]))
 (struct (ReexportSpecifier Term) ([location source-location/c]
                                   [module ModuleSpecifier?]
                                   [exclusions ExclusionList?]))
 (struct (ExportBindings Term) ([location source-location/c]
                                [bindings (listof (cons/c Identifier? (maybe/c Expression/X?)))])))

(provide/contract
 (struct (FunctionDeclaration Declaration) ([location source-location/c]
                                            [name Identifier?]
                                            [args (listof Identifier?)]
                                            [body (listof SourceElement?)]))
 (struct (ImportDeclaration Declaration) ([location source-location/c]
                                          [specifiers (listof ImportSpecifier?)]))
 (struct (ExportDeclaration Declaration) ([location source-location/c]
                                          [specifiers (listof (or/c ExclusionList? ReexportSpecifier? ExportBindings? Identifier?))]))
 (struct (VariableDeclaration Declaration) ([location source-location/c]
                                            [bindings (listof VariableInitializer?)]))
 (struct (LetDeclaration Declaration) ([location source-location/c]
                                       [bindings (or/c (listof VariableInitializer?)
                                                       (listof FunctionDeclaration?))])))

(provide/contract
 (struct (StringLiteral Expression) ([location source-location/c]
                                     [value string?]))
 (struct (RegexpLiteral Expression) ([location source-location/c]
                                     [pattern string?]
                                     [global? boolean?]
                                     [case-insensitive? boolean?]))
 (struct (NumericLiteral Expression) ([location source-location/c]
                                      [value number?]))
 (struct (BooleanLiteral Expression) ([location source-location/c]
                                      [value boolean?]))
 (struct (NullLiteral Expression) ([location source-location/c]))
 (struct (ArrayLiteral Expression) ([location source-location/c]
                                    [elements (listof (maybe/c Expression/X?))]))
 (struct (ObjectLiteral Expression) ([location source-location/c]
                                     [properties (listof (cons/c Property? Expression/X?))]))
 (struct (ThisReference Expression) ([location source-location/c]))
 (struct (VarReference Expression) ([location source-location/c]
                                    [id Identifier?]))
 (struct (BracketReference Expression) ([location source-location/c]
                                        [container Expression/X?]
                                        [key Expression/X?]))
 (struct (DotReference Expression) ([location source-location/c]
                                    [container Expression/X?]
                                    [id Identifier?]))
 (struct (NewExpression Expression) ([location source-location/c]
                                     [constructor Expression/X?]
                                     [arguments ExpressionList/X?]))
 (struct (PostfixExpression Expression) ([location source-location/c]
                                         [expression Expression/X?]
                                         [operator PostfixOperator/c]))
 (struct (PrefixExpression Expression) ([location source-location/c]
                                        [operator PrefixOperator/c]
                                        [expression Expression/X?]))
 (struct (InfixExpression Expression) ([location source-location/c]
                                       [left Expression/X?]
                                       [operator InfixOperator/c]
                                       [right Expression/X?]))
 (struct (ConditionalExpression Expression) ([location source-location/c]
                                             [test Expression/X?]
                                             [consequent Expression/X?]
                                             [alternate Expression/X?]))
 (struct (AssignmentExpression Expression) ([location source-location/c]
                                            [lhs Expression/X?]
                                            [operator AssignmentOperator/c]
                                            [rhs Expression/X?]))
 (struct (FunctionExpression Expression) ([location source-location/c]
                                          [name (maybe/c Identifier?)]
                                          [args (listof Identifier?)]
                                          [body (listof SourceElement?)]))
 (struct (LetExpression Expression) ([location source-location/c]
                                     [bindings (listof VariableInitializer?)]
                                     [body Expression/X?]))
 (struct (CallExpression Expression) ([location source-location/c]
                                      [method Expression/X?]
                                      [args ExpressionList/X?]))
 (struct (ParenExpression Expression) ([location source-location/c]
                                       [expression Expression/X?]))
 (struct (ListExpression Expression) ([location source-location/c]
                                      [expressions ExpressionList/X?])))

(provide/contract
 (struct (BlockStatement Statement) ([location source-location/c]
                                     [statements SubStatementList/X?]))
 (struct (EmptyStatement Statement) ([location source-location/c]))
 (struct (ExpressionStatement Statement) ([location source-location/c]
                                          [expression Expression/X?]))
 (struct (IfStatement Statement) ([location source-location/c]
                                  [test Expression/X?]
                                  [consequent SubStatement/X?]
                                  [alternate (maybe/c SubStatement/X?)]))
 (struct (DoWhileStatement Statement) ([location source-location/c]
                                       [body SubStatement/X?]
                                       [test Expression/X?]))
 (struct (WhileStatement Statement) ([location source-location/c]
                                     [test Expression/X?]
                                     [body SubStatement/X?]))
 (struct (ForStatement Statement) ([location source-location/c]
                                   [init (or/c (maybe/c Expression/X?) VariableDeclaration? LetDeclaration?)]
                                   [test (maybe/c Expression/X?)]
                                   [incr (maybe/c Expression/X?)]
                                   [body SubStatement/X?]))
 (struct (ForInStatement Statement) ([location source-location/c]
                                     [lhs (or/c Expression/X? VariableDeclaration? LetDeclaration?)]
                                     [container Expression/X?]
                                     [body SubStatement/X?]))
 (struct (ContinueStatement Statement) ([location source-location/c]
                                        [label (maybe/c Identifier?)]))
 (struct (BreakStatement Statement) ([location source-location/c]
                                     [label (maybe/c Identifier?)]))
 (struct (ReturnStatement Statement) ([location source-location/c]
                                      [value (maybe/c Expression/X?)]))
 (struct (LetStatement Statement) ([location source-location/c]
                                   [bindings (listof VariableInitializer?)]
                                   [body SubStatement/X?]))
 (struct (WithStatement Statement) ([location source-location/c]
                                    [context Expression/X?]
                                    [body SubStatement/X?]))
 (struct (SwitchStatement Statement) ([location source-location/c]
                                      [expression Expression/X?]
                                      [cases (listof CaseClause?)]))
 (struct (LabelledStatement Statement) ([location source-location/c]
                                        [label Identifier?]
                                        [statement SubStatement/X?]))
 (struct (ThrowStatement Statement) ([location source-location/c]
                                     [value Expression/X?]))
 (struct (TryStatement Statement) ([location source-location/c]
                                   [body Statement/X?]
                                   [catch (listof CatchClause?)]
                                   [finally (maybe/c Statement/X?)])))

(provide (all-from-out "private/syntax/ast-utils.ss"))
