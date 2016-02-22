#lang scheme/base

(require rackunit
         pprint
         "../../parse.ss"
         "../syntax/ast-core.ss"
         "../../print.ss"
         "../syntax/syntax.ss")

(provide pretty-print-tests)

(define (pretty-test term)
  (printf "~v~n" (pretty-format (format-term term))))

;; TODO: test expression statement never generates immediate function subexpression

(define (check-expression sexp)
  (check-equal? sexp
                (expression->sexp (parse-expression (pretty-format (format-term (sexp->expression sexp)))))))

(define (check-source-element sexp)
  (check-equal? sexp
                (source-element->sexp (parse-source-element (pretty-format (format-term (sexp->source-element sexp)))))))

(define grungy-tests
  (test-suite "some grungy tests with an expected regexp pattern"
    (test-case "function-literal binds looser than call"
      (check-regexp-match #px"^\\(function\\(\\)\\s*\\{\\s*\\}\\)\\(\\)$"
                          (pretty-format (format-term (make-CallExpression #f (make-FunctionExpression #f #f '() '()) '())))))
    (test-case "function-literal binds looser than new"
      (check-regexp-match #px"^new \\(function\\(\\)\\s*\\{\\s*\\}\\)\\(\\)$"
                          (pretty-format (format-term (make-NewExpression #f (make-FunctionExpression #f #f '() '()) '())))))
    (test-case "new binds tighter than call 1"
      (check-regexp-match #px"^new\\s*\\(id\\(Number\\)\\)\\(\\\"42\\\"\\)$"
                          (pretty-format
                           (format-term
                            (make-NewExpression #f
                                                (make-CallExpression #f
                                                                     (make-VarReference #f (make-Identifier #f 'id))
                                                                     (list (make-VarReference #f (make-Identifier #f 'Number))))
                                                (list (make-StringLiteral #f "42")))))))
    (test-case "new binds tighter than call 2"
      (check-regexp-match #px"^new\\s*Function\\(\\)\\(42\\)$"
                          (pretty-format
                           (format-term
                            (make-CallExpression #f
                                                 (make-NewExpression #f
                                                                     (make-VarReference #f (make-Identifier #f 'Function))
                                                                     '())
                                                 (list (make-NumericLiteral #f 42)))))))
    (test-case "call expression and dot reference bind the same"
      (check-equal? (pretty-format
                     (format-term
                      (make-DotReference
                       #f (make-CallExpression
                           #f (make-DotReference
                               #f (make-CallExpression
                                   #f (make-VarReference
                                       #f (make-Identifier #f 'a))
                                   (list (make-NumericLiteral #f 1)))
                               (make-Identifier #f 'b))
                           (list (make-NumericLiteral #f 2)))
                       (make-Identifier #f 'c))))
                    "a(1).b(2).c"))
    (test-case "bracket reference and dot reference bind the same"
      (check-equal? (pretty-format
                     (format-term
                      (make-DotReference
                       #f (make-BracketReference
                           #f (make-DotReference
                               #f (make-BracketReference
                                   #f (make-VarReference
                                       #f (make-Identifier #f 'a))
                                   (make-NumericLiteral #f 1))
                               (make-Identifier #f 'b))
                           (make-NumericLiteral #f 2))
                       (make-Identifier #f 'c))))
                    "a[1].b[2].c"))
    (test-case "function expression and dot reference bind the same"
      (check-regexp-match #px"\\(function\\(\\) \\{\\s+\\(function\\(\\) \\{\\s+\\}\\)\\.a;\\s+\\}\\)\\.b"
                          (pretty-format
                           (format-term
                            (make-DotReference
                             #f (make-FunctionExpression
                                 #f #f null 
                                 (list (make-ExpressionStatement
                                        #f (make-DotReference
                                            #f (make-FunctionExpression #f #f null null)
                                            (make-Identifier #f 'a)))))
                             (make-Identifier #f 'b))))))
    (test-case "function expression and bracket reference bind the same"
      (check-regexp-match #px"\\(function\\(\\) \\{\\s+\\(function\\(\\) \\{\\s+\\}\\)\\[1\\];\\s+\\}\\)\\[2\\]"
                          (pretty-format
                           (format-term
                            (make-BracketReference
                             #f (make-FunctionExpression
                                 #f #f null 
                                 (list (make-ExpressionStatement
                                        #f (make-BracketReference
                                            #f (make-FunctionExpression #f #f null null)
                                            (make-NumericLiteral #f 1)))))
                             (make-NumericLiteral #f 2))))))
    ))

(define invertibility-tests
  (test-suite "tests of invertibility of pretty-printing"
    (test-case "array literal"
      (check-expression '(array 1 2 3 4 5)))
    (test-case "sparse array literal"
      (check-expression '(array 1 () 2 () 3)))
    (test-case "function"
      (check-source-element '(function f (a b c)
                               (return a))))
    (test-case "object literal"
      (check-expression '(object [a "aaa"]
                                 [b 12]
                                 [c 42])))
    (test-case "regexp literal"
      (check-expression '(regexp "^\\d+$" #f #f)))
    (test-case "block statement"
      (check-source-element '(block "foo" (print 6))))
    (test-case "one-armed if"
      (check-source-element '(if #f (print 12))))
    (test-case "nested ifs"
      (check-source-element '(if #f
                                 (block (print 2))
                                 (if #f
                                     (print 3)
                                     (if #t
                                         (print)
                                         (print null))))))
    (test-case "empty do body"
      (check-source-element '(do () #f)))
    (test-case "do with block body"
      (check-source-element '(do (block (break)) #f)))
    (test-case "for-in with var declaration"
      (check-source-element '(for (var x) in (array 0 1 2)
                               (block (print x)))))
    (test-case "application of complex expression"
      (check-expression '((new Function "print('hi!');"))))
    (test-case "nested addition"
      (check-expression '(+ (+ 2 3) 4)))
    (test-case "order of operations 1"
      (check-expression '(* (+ 2 3) 4)))
    (test-case "order of operations 2"
      (check-expression '(+ (* 2 3) 4)))
    (test-case "order of operations 3"
      (check-expression '(+ 2 (* 3 4))))
    (test-case "nested addition with different kinds of operands"
      (check-expression '(+ "foo" (+ 2 3))))
    (test-case "order of operations with overloaded addition"
      (check-expression '(+ "foo" (* 2 3))))
    (test-case "nested functions"
      (check-expression '(function ()
                           (function foo () (return))
                           (foo))))
    (test-case "empty for init"
      (check-source-element '(for #f #t #f (break))))
    (test-case "standard for loop"
      (check-source-element '(for (= i 0) (< i 10) (postfix i ++)
                               (print i))))
    (test-case "complex for loop"
      (check-source-element '(for (var [i 0] [j 10]) (< i 10) (begin (postfix i ++) (postfix j --))
                               (print (+ i (+ ", " j))))))
    ))

(define pretty-print-tests
  (test-suite "pretty-print tests"
    invertibility-tests
    grungy-tests
    ))
