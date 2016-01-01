#lang scheme/base

(require (planet cobbe/contract-utils:1/contract-utils)
         (planet dherman/pprint:4)
         scheme/contract
         scheme/match
         "private/syntax/ast-core.ss"
         "private/syntax/ast-utils.ss")

(provide/contract
 [format-term (Term/X? . -> . doc?)]
 [format-source-element (SourceElement? . -> . doc?)]
 [format-variable-initializer (VariableInitializer? . -> . doc?)]
 [format-declaration (Declaration? . -> . doc?)]
 [format-expression (Expression/X? . -> . doc?)]
 [format-subexpression (Expression? Expression? . -> . doc?)]
 [format-statement (Statement/X? . -> . doc?)]
 [format-nested-substatement (SubStatement/X? . -> . doc?)]
 [format-substatement (SubStatement/X? . -> . doc?)]
 [format-case-clause (CaseClause? . -> . doc?)]
 [format-property (Property? . -> . doc?)]
 [format-identifier (Identifier? . -> . doc?)])

(provide format-map)
(provide formatters/Expression formatters/Statement formatters/ExpressionList formatters/StatementList)
(provide current-indentation-width collapse-lines? collapse-simple-substatements?)

(define collapse-lines? (make-parameter #f))
(define collapse-simple-substatements? (make-parameter #f))
(define current-indentation-width (make-parameter 4))

(define formatters/Expression (make-parameter null))
(define formatters/Statement (make-parameter null))
(define formatters/ExpressionList (make-parameter null))
(define formatters/StatementList (make-parameter null))

;; TODO:
;;   - formatters/Declaration
;;   - formatters/SourceElementList (?)
;;   - formatters/InitializerList

;; ===========================================================================
;; PRECEDENCES
;; ===========================================================================

;; TODO: shouldn't `new' bind tighter than call? make a test case

(define expression-precedences
  '(;; binds loosest
    (AssignmentExpression)
    (ConditionalExpression)
    (InfixExpression)
    (PrefixExpression)
    (PostfixExpression)
    (FunctionExpression)
    (CallExpression BracketReference DotReference)
    (NewExpression)
    (StringLiteral NumericLiteral BooleanLiteral NullLiteral RegexpLiteral
                   ThisReference VarReference
                   ArrayLiteral ObjectLiteral)
    (ParenExpression)
    ;; binds tightest
    ))

;; expression-precedence : (union struct-type Expression) -> natural-number
(define (expression-precedence e)
  (let* ([type (cond
                 [(Expression? e) (let-values ([(type _) (struct-info e)]) type)]
                 [(struct-type? e) e])]
         [type-name (let-values ([(name _2 _3 _4 _5 _6 _7 _8) (struct-type-info type)]) name)])
    (let loop ([precedences expression-precedences] [n 0])
      (when (null? precedences)
        (error 'expression-precedence "no precedence for ~a" type))
      (if (memq type-name (car precedences))
          n
          (loop (cdr precedences) (add1 n))))))

(define infix-expression-precedence (expression-precedence struct:InfixExpression))

;; TODO: could make this overrideable, but maybe not important

;; binds-tighter? : Expression * Expression -> boolean
(define (binds-tighter? e1 e2)
  (cond
    [(not (Expression? e1)) #f]
    [(not (Expression? e2)) #t]
    [else  (let ([p1 (expression-precedence e1)]
                 [p2 (expression-precedence e2)])
             (or (> p1 p2)
                 (and (= p1 p2 infix-expression-precedence)
                      (infix-binds-tighter? e1 e2))))]))

(define operator-precedences
  '(;; binds loosest
    (\|\|)
    (&&)
    (\|)
    (^)
    (&)
    (== != === !==)
    (< > <= >= instanceof in)
    (<< >> >>>)
    (+ -)
    (* / %)
    ;; binds tightest
    ))

;; operator-precedence : (union InfixExpression symbol) -> natural-number
(define (operator-precedence e)
  (let ([operator (cond
                    [(InfixExpression? e) (InfixExpression-operator e)]
                    [(symbol? e) e])])
    (let loop ([precedences operator-precedences] [n 0])
      (when (null? precedences)
        (error 'operator-precedence "No precedence for ~S" operator))
      (if (memq operator (car precedences))
          n
          (loop (cdr precedences) (add1 n))))))

;; infix-binds-tighter? : InfixExpression * InfixExpression -> boolean
(define (infix-binds-tighter? e1 e2)
  (> (operator-precedence e1)
     (operator-precedence e2)))

;; ===========================================================================
;; EXTENSION
;; ===========================================================================

(define (fail-match x)
  (match x
    [(? (lambda _ #f)) #f]))

;; format-map : (a -> c) (union (listof a) b) (parameterof (b -> c)) -> c
(define (format-map proc elts [param #f])
  (cond
    [(list? elts) (h-concat (map proc elts))]
    [param ((param) elts)]
    [else (error 'format-map "not a list")]))

(define (format/extensions x extensions)
  (if (null? extensions)
      (fail-match x)
      (with-handlers ([exn:misc:match? (lambda (exn)
                                         (format/extensions x (cdr extensions)))])
        ((car extensions) x))))

;; ===========================================================================
;; FORMATTING
;; ===========================================================================

;; format-source-element : SourceElement -> doc
(define (format-source-element elt)
  (h-append line (format-term elt)))

;; format-variable-initializer : VariableInitializer -> doc
(define (format-variable-initializer init)
  (match init
    [(struct VariableInitializer (_ id init))
     (h-append (format-identifier id)
               (if init
                   (h-append (text " = ")
                             (format-expression init))
                   empty))]))

;; format-declaration : Declaration -> doc
(define (format-declaration decl)
  (match decl
    [(struct FunctionDeclaration (_ name args body))
     (h-append (text "function ")
               (format-identifier name)
               (text "(")
               (h-concat (apply-infix (text ", ") (map format-identifier args)))
               (text ") {")
               (nest (current-indentation-width)
                     (format-map format-source-element body formatters/StatementList))
               line
               (text "}"))]
    ;; TODO: LetDeclaration
    [(struct VariableDeclaration (_ bindings))
     (h-append (text "var ")
               (h-concat (apply-infix (text ", ") (map format-variable-initializer bindings)))
               (text ";"))]))

;; format-expression : Expression -> any
(define (format-expression expr)
  (with-handlers ([exn:misc:match? (lambda (exn)
                                     (format/extensions expr (formatters/Expression)))])
    (match expr
      [(struct StringLiteral (_ value))
       (text (format "~v" value))] ;; TODO: use the real lexical definition
      [(struct NumericLiteral (_ value))
       (text (format "~a" value))] ;; TODO: use the real lexical definition
      [(struct BooleanLiteral (_ value))
       (text (if value "true" "false"))]
      [(struct NullLiteral (_))
       (text "null")]
      [(struct RegexpLiteral (_ pattern g? i?))
       (text (format "/~a/~a~a" pattern (if g? "g" "") (if i? "i" "")))]
      [(struct ArrayLiteral (_ elements))
       (if (null? elements)
           (text "[]")
           (h-append (text "[ ")
                     (if (car elements)
                         (format-expression (car elements))
                         empty)
                     (format-map (lambda (element)
                                   (h-append (text ",")
                                             (if element
                                                 (h-append (text " ")
                                                           (format-expression element))
                                                 empty)))
                                 (cdr elements)
                                 formatters/ExpressionList)
                     (text " ]")))]
      [(struct ObjectLiteral (_ properties))
       (if (null? properties)
           (text "{}")
           (h-append (text "{")
                     (nest (current-indentation-width)
                           (h-append line
                                     (format-property (car properties))
                                     (format-map (lambda (property)
                                                   (h-append (text ",")
                                                             line
                                                             (format-property property)))
                                                 (cdr properties))))
                     line
                     (text "}")))]
      [(struct ThisReference (_))
       (text "this")]
      [(struct VarReference (_ id))
       (format-identifier id)]
      [(struct BracketReference (_ container key))
       (h-append (format-subexpression container expr)
                 (text "[")
                 (format-expression key)
                 (text "]"))]
      [(struct DotReference (_ container id))
       ;; TODO: if (not (valid-identifier? id)) then (make-BracketReference loc container (make-StringLiteral (Term-location id) (symbol->string (Identifier-name id))))
       (h-append (format-subexpression container expr)
                 (text ".")
                 (format-identifier id))]
      [(struct NewExpression (_ constructor arguments))
       (h-append (text "new ")
                 (format-subexpression constructor expr)
                 (text "(")
                 (if (list? arguments)
                     (h-concat (apply-infix (text ", ") (map format-expression arguments)))
                     (format/extensions arguments (formatters/ExpressionList)))
                 (text ")"))]
      [(struct PostfixExpression (_ expression operator))
       (h-append (format-subexpression expression expr)
                 (text (symbol->string operator)))]
      [(struct PrefixExpression (_ operator expression))
       (h-append (text (symbol->string operator))
                 (format-subexpression expression expr))]
      [(struct InfixExpression (_ left operator right))
       (h-append (if (InfixExpression? left)
                     (if (infix-binds-tighter? expr left)
                         (h-append (text "(")
                                   (format-expression left)
                                   (text ")"))
                         (format-expression left))
                     (format-subexpression left expr))
                 (text " ")
                 (text (symbol->string operator))
                 (text " ")
                 ;; We don't reassociate because of e.g. overloading of the + operator.
                 ;; We could potentially reassociate some operators, but this is enough.
                 (if (binds-tighter? right expr)
                     (format-expression right)
                     (h-append (text "(")
                               (format-expression right)
                               (text ")"))))]
      [(struct ConditionalExpression (_ test consequent alternate))
       (h-append (format-subexpression test expr)
                 (text " ? ")
                 (format-subexpression consequent expr)
                 (text " : ")
                 (format-subexpression alternate expr))]
      [(struct AssignmentExpression (_ lhs operator rhs))
       (h-append (format-subexpression lhs expr)
                 (text " ")
                 (text (symbol->string operator))
                 (text " ")
                 (format-subexpression rhs expr))]
      [(struct FunctionExpression (_ name args body))
       (align
        (h-append (text "function")
                  (if name
                      (h-append (text " ")
                                (format-identifier name))
                      empty)
                  (text "(")
                  (h-concat (apply-infix (text ", ") (map format-identifier args)))
                  (text ") {")
                  (nest (current-indentation-width)
                        (format-map format-source-element body formatters/StatementList))
                  line
                  (text "}")))]
      [(struct LetExpression (_ bindings body))
       (h-append (text "let (")
                 (h-concat (apply-infix (text ", ") (map format-variable-initializer bindings)))
                 (text ")")
                 (nest (current-indentation-width)
                       (h-append line (format-expression body))))]
      [(struct CallExpression (_ method args))
       (h-append (format-subexpression method expr)
                 (text "(")
                 (if (list? args)
                     (h-concat (apply-infix (text ", ") (map format-expression args)))
                     (format/extensions args (formatters/ExpressionList)))
                 (text ")"))]
      [(struct ParenExpression (_ expr))
       (h-append (text "(")
                 (format-expression expr)
                 (text ")"))]
      [(struct ListExpression (_ exprs))
       (if (list? exprs)
           (h-concat (apply-infix (text ", ") (map format-expression exprs)))
           (format/extensions exprs (formatters/ExpressionList)))]
      )))

;; format-subexpression : Expression * Expression -> any
(define (format-subexpression expr parent)
  (if (binds-tighter? parent expr)
      (h-append (text "(")
                (format-expression expr)
                (text ")"))
      (format-expression expr)))

;; format-statement : Statement -> any
;; POSTCONDITIONS:
;;   - statement output includes its own semicolon if appropriate
;;   - statement output is not newline-terminated
(define (format-statement stmt)
  (with-handlers ([exn:misc:match? (lambda (exn)
                                     (format/extensions stmt (formatters/Statement)))])
    (match stmt
      [(struct BlockStatement (_ statements))
       (if (null? statements)
           (text "{}")
           (h-append (nest (current-indentation-width)
                           (h-append (text "{")
                                     line
                                     (format-substatement (car statements))
                                     (format-map (lambda (statement)
                                                   (h-append line
                                                             (format-substatement statement)))
                                                 (cdr statements)
                                                 formatters/StatementList)))
                     line
                     (text "}")))]
      [(struct EmptyStatement (_))
       (text ";")]
      [(struct ExpressionStatement (_ expression))
       (h-append (format-expression expression)
                 (text ";"))]
      [(struct IfStatement (_ test consequent alternate))
       (h-append (text "if (")
                 (format-expression test)
                 (text ")")
                 (format-nested-substatement consequent)
                 line
                 (cond
                   [(IfStatement? alternate)
                    (h-append (text "else ")
                              (format-statement alternate))]
                   [alternate
                    (h-append (text "else")
                              (format-nested-substatement alternate))]
                   [else empty]))]
      [(struct DoWhileStatement (_ body test))
       (h-append (text "do")
                 (if (BlockStatement? body)
                     (h-append (text " ")
                               (format-substatement body)
                               (text " "))
                     (h-append (nest (current-indentation-width)
                                     (h-append line
                                               (format-substatement body)))
                               line))
                 (text "while (")
                 (format-expression test)
                 (text ");"))]
      [(struct WhileStatement (_ test body))
       (h-append (text "while (")
                 (format-expression test)
                 (text ")")
                 (format-nested-substatement body))]
      [(struct ForStatement (_ init test incr body))
       (h-append (text "for (")
                 (cond
                   [(Expression? init)
                    (format-expression init)]
                   [(VariableDeclaration? init)
                    (h-append (text "var ")
                              (h-concat (apply-infix (text ", ") (map format-variable-initializer (VariableDeclaration-bindings init)))))])
                 (text ";")
                 (if test
                     (h-append (text " ")
                               (format-expression test))
                     empty)
                 (text ";")
                 (if incr
                     (h-append (text " ")
                               (format-expression incr))
                     empty)
                 (text ")")
                 (format-nested-substatement body))]
      [(struct ForInStatement (_ lhs container body))
       (h-append (text "for (")
                 (if (Expression? lhs)
                     (format-expression lhs)
                     (h-append (text "var ")
                               (format-variable-initializer (car (VariableDeclaration-bindings lhs)))))
                 (text " in ")
                 (format-expression container)
                 (text ")")
                 (format-nested-substatement body))]
      [(struct ContinueStatement (_ label))
       (h-append (text "continue")
                 (if label
                     (h-append (text " ")
                               (format-identifier label))
                     empty)
                 (text ";"))]
      [(struct BreakStatement (_ label))
       (h-append (text "break")
                 (if label
                     (h-append (text " ")
                               (format-identifier label))
                     empty)
                 (text ";"))]
      [(struct ReturnStatement (_ value))
       (h-append (text "return")
                 (if value
                     (h-append (text " ")
                               (format-expression value))
                     empty)
                 (text ";"))]
      [(struct WithStatement (_ context body))
       (h-append (text "with (")
                 (format-expression context)
                 (text ")")
                 (nest (current-indentation-width)
                       (h-append line
                                 (format-substatement body))))]
      [(struct SwitchStatement (_ expression cases))
       (h-append (text "switch (")
                 (format-expression expression)
                 (text ") {")
                 (nest (current-indentation-width)
                       (h-append line
                                 (format-case-clause (car cases))
                                 (format-map (lambda (case)
                                               (h-append line
                                                         (format-case-clause case)))
                                             (cdr cases))))
                 line
                 (text "}"))]
      [(struct LabelledStatement (_ label statement))
       (h-append (format-identifier label)
                 (text ":")
                 (nest (current-indentation-width)
                       (h-append line
                                 (format-substatement statement))))]
      [(struct ThrowStatement (_ value))
       (h-append (text "throw ")
                 (format-expression value)
                 (text ";"))]
      [(struct TryStatement (_ body catches finally))
       (h-append (text "try")
                 (format-nested-substatement body)
                 (format-map (lambda (catch)
                               (h-append line
                                         (match-let ([(struct CatchClause (_ id body)) catch])
                                           (h-append (text "catch (")
                                                     (format-identifier id)
                                                     (text ")")
                                                     (format-nested-substatement body)))))
                             catches)
                 (if finally
                     (h-append line
                               (text "finally")
                               (format-nested-substatement finally))
                     empty))]
      )))

;; format-nested-substatement : SubStatement -> any
;; PRECONDITION:
;;   - starts on the same line as its containing statement
(define (format-nested-substatement body)
  (cond
    [(EmptyStatement? body)
     (text ";")]
    [(or (BlockStatement? body)
         (collapse-simple-substatements?))
     (h-append (text " ")
               (format-substatement body))]
    [else
     (nest (current-indentation-width)
           (h-append line
                     (format-substatement body)))]))

;; format-substatement : SubStatement -> any
(define (format-substatement statement)
  (if (Declaration? statement)
      (format-declaration statement)
      (format-statement statement)))

;; format-case-clause : CaseClause -> any
(define (format-case-clause case)
  (let ([question (CaseClause-question case)]
        [answer (CaseClause-answer case)])
    (h-append
     (if question
         (h-append (text "case ") (format-expression question))
         (text "default"))
     (text ":")
     (if (= (length answer) 1)
         (format-nested-substatement (car answer))
         (nest (current-indentation-width)
               (h-append line
                         (format-map format-substatement answer formatters/StatementList)))))))

;; format-property : Property -> doc
(define (format-property pair)
  (let ([property (car pair)]
        [value (cdr pair)])
    (h-append (if (Identifier? property)
                  (format-identifier property)
                  ;; TODO: check the type here
                  (format-term property))
              (text ": ")
              (format-expression value))))

;; format-identifier : Identifier -> doc
(define (format-identifier id)
  (text (symbol->string (Identifier-name id))))

;; format-term : Term/X -> doc
(define (format-term term)
  (cond
    [(Declaration? term) (format-declaration term)]
    [(Statement/X? term) (format-statement term)]
    [(Expression/X? term) (format-expression term)]))
