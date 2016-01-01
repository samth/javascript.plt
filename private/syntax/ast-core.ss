#lang scheme/base

(provide (all-defined-out))

;; TODO: save location information for operators?

;; ===========================================================================
;; TERMS
;; ===========================================================================

;; (union region syntax)
(define-struct Term (location) #:prefab)

(define-struct (Declaration Term) () #:prefab)
(define-struct (Statement Term) () #:prefab)
(define-struct (Expression Term) () #:prefab)

;; ===========================================================================
;; DECLARATIONS
;; ===========================================================================

;; Identifier * (listof Identifier) * (listof SourceElement)
(define-struct (FunctionDeclaration Declaration) (name args body) #:prefab)

;; (nelistof VariableInitializer)
(define-struct (VariableDeclaration Declaration) (bindings) #:prefab)

;; (nelistof Identifier)
;(define-struct (PrivateDeclaration Declaration) (names) #:prefab)

;; Identifier * (optional Expression)
(define-struct (VariableInitializer Term) (id init) #:prefab)

;; TODO: should `let-ness' be a property of FunctionDeclarations, rather than
;;       wrapping FunctionDeclarations inside LetDeclarations?
;; (union (nelistof VariableInitializer) (nelistof FunctionDeclaration))
(define-struct (LetDeclaration Declaration) (bindings) #:prefab)

;; (listof ImportSpecifier)
(define-struct (ImportDeclaration Declaration) (specifiers) #:prefab)

;; ModuleSpecifier * (union Identifier (listof ImportBinding) ExclusionList)
(define-struct (ImportSpecifier Term) (module bindings) #:prefab)

;; Identifier * (optional Identifier)
(define-struct (ImportBinding Term) (label binding) #:prefab)

;; (listof Identifier)
(define-struct (ExclusionList Term) (ids) #:prefab)

;; (union 'file 'planet 'collect) * (listof (union string integer symbol #f))
(define-struct (ModuleSpecifier Term) (protocol elements) #:prefab)

;; (listof (union ExclusionList ReexportSpecifier ExportBindings Identifier))
(define-struct (ExportDeclaration Declaration) (specifiers) #:prefab)

;; ModuleSpecifier * ExclusionList
(define-struct (ReexportSpecifier Term) (module exclusions) #:prefab)

;; (listof (cons Identifier (optional Expression)))
(define-struct (ExportBindings Term) (bindings) #:prefab)

;; ===========================================================================
;; EXPRESSIONS
;; ===========================================================================

;; string
(define-struct (StringLiteral Expression) (value) #:prefab)

;; number
(define-struct (NumericLiteral Expression) (value) #:prefab)

;; boolean
(define-struct (BooleanLiteral Expression) (value) #:prefab)

;;
(define-struct (NullLiteral Expression) () #:prefab)

;; string * boolean * boolean
(define-struct (RegexpLiteral Expression) (pattern global? case-insensitive?) #:prefab)

;; (listof (optional Expression))
(define-struct (ArrayLiteral Expression) (elements) #:prefab)

;; (listof (cons Property Expression))
(define-struct (ObjectLiteral Expression) (properties) #:prefab)

;; (listof Identifier) * ExpressionBlock
(define-struct (BlockLiteral Expression) (args body) #:prefab)

;; 
(define-struct (ThisReference Expression) () #:prefab)

;; Identifier
(define-struct (VarReference Expression) (id) #:prefab)

;; Expression * Expression
(define-struct (BracketReference Expression) (container key) #:prefab)

;; Expression * Identifier
(define-struct (DotReference Expression) (container id) #:prefab)

;; Expression * (listof Expression)
(define-struct (NewExpression Expression) (constructor arguments) #:prefab)

;; Expression * PostfixOperator
(define-struct (PostfixExpression Expression) (expression operator) #:prefab)

;; PrefixOperator * Expression
(define-struct (PrefixExpression Expression) (operator expression) #:prefab)

;; Expression * InfixOperator * Expression
(define-struct (InfixExpression Expression) (left operator right) #:prefab)

;; Expression * Expression * Expression
(define-struct (ConditionalExpression Expression) (test consequent alternate) #:prefab)

;; Expression * AssignmentOperator * Expression
(define-struct (AssignmentExpression Expression) (lhs operator rhs) #:prefab)

;; (optional Identifier) * (listof Identifier) * (listof SourceElement)
(define-struct (FunctionExpression Expression) (name args body) #:prefab)

;; (listof VariableInitializer) Expression
(define-struct (LetExpression Expression) (bindings body) #:prefab)

;; Expression * (listof Expression)
(define-struct (CallExpression Expression) (method args) #:prefab)

;; Expression
(define-struct (ParenExpression Expression) (expression) #:prefab)

;; (listof Expression)
(define-struct (ListExpression Expression) (expressions) #:prefab)

;; ExpressionBlock
(define-struct (DoExpression Expression) (block) #:prefab)

;; ===========================================================================
;; STATEMENTS
;; ===========================================================================

;; (listof SubStatement)
(define-struct (BlockStatement Statement) (statements) #:prefab)

;; 
(define-struct (EmptyStatement Statement) () #:prefab)

;; Expression
(define-struct (ExpressionStatement Statement) (expression) #:prefab)

;; Expression * SubStatement * (optional SubStatement)
(define-struct (IfStatement Statement) (test consequent alternate) #:prefab)

;; SubStatement * Expression
(define-struct (DoWhileStatement Statement) (body test) #:prefab)

;; Expression * SubStatement
(define-struct (WhileStatement Statement) (test body) #:prefab)

;; (union (optional Expression) VariableDeclaration) * (optional Expression) * (optional Expression) * SubStatement
(define-struct (ForStatement Statement) (init test incr body) #:prefab)

;; (union Expression VariableDeclaration) * Expression * SubStatement
(define-struct (ForInStatement Statement) (lhs container body) #:prefab)

;; (optional Identifier)
(define-struct (ContinueStatement Statement) (label) #:prefab)

;; (optional Identifier)
(define-struct (BreakStatement Statement) (label) #:prefab)

;; (optional Expression)
(define-struct (ReturnStatement Statement) (value) #:prefab)

;; (listof VariableInitializer) SubStatement
(define-struct (LetStatement Statement) (bindings body) #:prefab)

;; Expression * SubStatement
(define-struct (WithStatement Statement) (context body) #:prefab)

;; Expression * (listof CaseClause)
(define-struct (SwitchStatement Statement) (expression cases) #:prefab)

;; Identifier * SubStatement
(define-struct (LabelledStatement Statement) (label statement) #:prefab)

;; Expression
(define-struct (ThrowStatement Statement) (value) #:prefab)

;; Statement * (listof CatchClause) * (optional Statement)
(define-struct (TryStatement Statement) (body catch finally) #:prefab)

;; symbol
(define-struct (Identifier Term) (name) #:prefab)

;; (optional Expression) * (listof SubStatement)
(define-struct (CaseClause Term) (question answer) #:prefab)

;; Identifier * Statement
(define-struct (CatchClause Term) (id body) #:prefab)

;; BlockStatement * (optional Expression)
(define-struct (ExpressionBlock Term) (block tail) #:prefab)
