#lang scheme/base

(require rackunit
         file/in-new-directory
         scheme/class
         "../syntax/lex.ss"
         "../syntax/parse.ss"
         "../syntax/ast-core.ss"
         "../syntax/syntax.ss"
         "../syntax/token.ss")

(provide parse-tests)

;; TODO: unit tests for:
;;   - empty case clause
;;   - one-armed if
;;   - label definedness check

(define (string-lexer s)
  (make-object lexer% (open-input-string s)))

(define (extract-tokens l n)
  (for/list ([i (in-range n)])
    (send l read-token)))

(define (string->tokens s)
  (let ([l (string-lexer s)])
    (let loop ([acc '()])
      (let ([token (send l read-token)])
        (if (eq? (token-type token) 'END)
            (reverse acc)
            (loop (cons token acc)))))))

(define-simple-check (check-tokenized str actual expected)
  (and (= (length actual) (length expected))
       (andmap (lambda (actual expected)
                 (if (symbol? expected)
                     (eq? (token-type actual) expected)
                     (and (eq? (token-type actual) (car expected))
                          (equal? (token-contents actual) (cdr expected)))))
               actual
               expected)))

(define-check (check-tokens str expected)
  (check-tokenized str (string->tokens str) expected))

(define-simple-check (check-parsed str actual expected)
  (equal? actual expected))

(define-check (check-expression str expected)
  (check-parsed str (expression->sexp (parse-expression str)) expected))

(define-check (check-source-element str expected)
  (check-parsed str (source-element->sexp (parse-source-element str)) expected))

(define lexer-tests
  (test-suite "lexer tests"
    (test-case "identifiers"
      (check-tokens "foo bar baz"
                    '((ID . foo) (ID . bar) (ID . baz))))
    (test-case "for loop"
      (check-tokens "for (var i = 0; i < 10; i+=1) {\n    print(i)\n}"
                    '(for \( var ID ASSIGN NUMBER \; ID < NUMBER \; ID ASSIGN NUMBER \) \{ ID \( ID \) \})))
    (test-case "regexp with quoted forward slash"
      (check-tokens "foo /ab\\/cd/ bar"
                    '((ID . foo) REGEXP (ID . bar))))
    (test-case "regexp with character classes and escapes"
      (check-tokens "foo /^[ \\t]+/ bar"
                    '((ID . foo) REGEXP (ID . bar))))
    (test-case "regexp with escape"
      (check-tokens "foo /^\\s+/ bar"
                    '((ID . foo) REGEXP (ID . bar))))
    (test-case "regexp stops at first forward slash"
      (check-tokens "/abc/ : /def/"
                    '(REGEXP : REGEXP)))
    (test-case "bigger example of regexp stopping at first forward slash"
      (check-tokens "(this.scanNewlines ? /^[ \\t]+/ : /^\\s+/)"
                    '(\( this \. ID ? REGEXP : REGEXP \))))
    (test-case "string literal (single quotes)"
      (check-tokens "'foo'"
                    '((STRING . "foo"))))
    (test-case "string literal (double quotes)"
      (check-tokens "\"foo\""
                    '((STRING . "foo"))))
    (test-case "string single-char escape 1"
      (check-tokens "'foo\\nbar'"
                    '((STRING . "foo\nbar"))))
    (test-case "string single-char escape 2"
      (check-tokens "'foo\\'bar'"
                    '((STRING . "foo'bar"))))
    (test-case "string single-char escape 3"
      (check-tokens "'foo\\\"bar'"
                    '((STRING . "foo\"bar"))))
    (test-case "string hex escape"
      (check-tokens "'foo\\x51bar'"
                    '((STRING . "fooQbar"))))
    (test-case "string unicode escape"
      (check-tokens "'foo\\u0051bar'"
                    '((STRING . "fooQbar"))))
    (test-case "string hex non-escape 1"
      (check-tokens "'foo\\x5qbar'"
                    '((STRING . "foox5qbar"))))
    (test-case "string hex non-escape 2"
      (check-tokens "'foo\\xqqbar'"
                    '((STRING . "fooxqqbar"))))
    (test-case "string unicode non-escape 1"
      (check-tokens "'foo\\u555qbar'"
                    '((STRING . "foou555qbar"))))
    (test-case "string unicode non-escape 2"
      (check-tokens "'foo\\u55qqbar'"
                    '((STRING . "foou55qqbar"))))
    (test-case "string unicode non-escape 3"
      (check-tokens "'foo\\u5qqqbar'"
                    '((STRING . "foou5qqqbar"))))
    (test-case "string unicode non-escape 4"
      (check-tokens "'foo\\uqqqqbar'"
                    '((STRING . "foouqqqqbar"))))
    (test-case "string octal escape 1"
      (check-tokens "'foo\\121bar'"
                    '((STRING . "fooQbar"))))
    (test-case "string octal escape 2"
      (check-tokens "'foo\\00bar'"
                    '((STRING . "foo\0bar"))))
    (test-case "string null escape"
      (check-tokens "'foo\0bar'"
                    '((STRING . "foo\0bar"))))
    ))

(define precedence-tests
  (test-suite "precedence tests"
    (test-case "higher precedence between lower"
      (check-expression "x - y * z + w"
                        '(+ (- x (* y z)) w)))
    (test-case "low, high, middle"
      (check-expression "x < y * z + w"
                        '(< x (+ (* y z) w))))
    (test-case "big example"
      (check-expression "x + y * z / 3 - 21 + a.b.c * 6"
                        '(+ (- (+ x (/ (* y z) 3)) 21) (* (field (field a b) c) 6))))
    (test-case "low followed by two high"
      (check-expression "x + y * z * n"
                        '(+ x (* (* y z) n))))
    (test-case "two of same precedence"
      (check-expression "y * z / 3"
                        '(/ (* y z) 3)))
    (test-case "new with arguments"
      (check-expression "new C(2, 3)"
                        '(new C 2 3)))
    (test-case "new with arguments then called"
      (check-expression "new Function('return')()"
                        '((new Function "return"))))
    ))

(define for-tests
  (test-suite "for loop tests"
    (test-case "empty for loop"
      (check-source-element "for (;;) break;"
                            '(for #f #t #f (break))))
    ;; TODO: test all combinations
    (test-case "for-in loop"
      (check-source-element "for (var x in e) break;"
                            '(for (var x) in e
                               (break))))
    ))

(define misc-tests
  (test-suite "miscellaneous parse tests"
    (test-case "nullary function expression"
      (check-expression "function() { return }"
                        '(function () (return))))
    (test-case "unary function expression"
      (check-expression "function(x) { return }"
                        '(function (x) (return))))
    (test-case "binary function expression"
      (check-expression "function(x,y) { return }"
                        '(function (x y) (return))))
    (test-case "ternary function expression"
      (check-expression "function(x,y,z) { return }"
                        '(function (x y z) (return))))
    (test-case "empty object expression"
      (check-expression "{ }"
                        '(object)))
    (test-case "unary object expression"
      (check-expression "{ a: 2 }"
                        '(object [a 2])))
    (test-case "binary object expression"
      (check-expression "{ a: 2, b: 3 }"
                        '(object [a 2] [b 3])))
    (test-case "ternary object expression"
      (check-expression "{ a: 2, b: 3, c: 4 }"
                        '(object [a 2] [b 3] [c 4])))
    (test-case "function literal in object"
      (check-expression "{ f: function() { return }, a: 3 }"
                        '(object [f (function () (return))]
                                 [a 3])))
    (test-case "nested braces"
      (check-expression "function() { var s = {}; return }"
                        '(function ()
                           (var [s (object)])
                           (return))))
    (test-case "nested brackets"
      (check-expression "[ [1, 2, 3], [4, 5, 6], [7, 8] ]"
                        '(array (array 1 2 3)
                                (array 4 5 6)
                                (array 7 8))))
    (test-case "brackets don't throw off tokenizer state"
      (check-expression "function() { var s = []; return }"
                        '(function ()
                           (var [s (array)])
                           (return))))
    (test-case "var with empty array literal"
      (check-source-element "var x = [];"
                            '(var [x (array)])))
    (test-case "assignment expression"
      (check-expression "x = foo(3)"
                        '(x . = . (foo 3))))
    (test-case "empty switch"
      (check-source-element "switch(x) { }"
                            '(switch x)))
    (test-case "case clause with multiple statements"
      (check-source-element "switch (x) { case 1: foo(); bar(); break; case 2: break; }"
                            '(switch x
                               (case 1
                                 (foo)
                                 (bar)
                                 (break))
                               (case 2
                                 (break)))))
    (test-case "do-while loop"
      (check-source-element "do { foo() } while (true);"
                            '(do (block (foo))
                               #t)))
    (test-case "infix operators don't include unary operators 1"
      (check-expression "2 ~ 3" 2))
    (test-case "infix operators don't include unary operators 2"
      (check-expression "2 ! 3" 2))
    (test-case "ternary ? : is an `infix-operator-token?' 1"
      (check-expression "x ? y : z"
                        '(if x y z)))
    (test-case "ternary ? : is an `infix-operator-token?' 2"
      (check-source-element "{ s = x ? y : z }"
                            '(block (= s (if x y z)))))
    ;; TODO: test block literals with no spaces between pipes
    ))

(define big-tests
  (test-suite "big example files"
    (test-case "example1"
      (check-not-false (in-this-directory
                        (with-input-from-file "example1.js"
                          (lambda ()
                            (parse-program-unit (current-input-port)))))))
    (test-case "example2"
      (check-not-false (in-this-directory
                        (with-input-from-file "example2.js"
                          (lambda ()
                            (parse-program-unit (current-input-port)))))))
    (test-case "example3"
      (check-not-false (in-this-directory
                        (with-input-from-file "example3.js"
                          (lambda ()
                            (parse-program-unit (current-input-port)))))))
    ))

(define parse-tests
  (test-suite "parse tests"
    lexer-tests
    precedence-tests
    misc-tests
    for-tests
    big-tests
    ))
