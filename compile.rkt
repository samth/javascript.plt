#lang scheme/base

(require scheme/contract
         "private/syntax/ast-core.ss"
         "private/syntax/ast-utils.ss"
         "lang/lang.ss")

(define (compile-script elts)
  (with-syntax ([(ast ...) elts])
    (eval #'(script-compile ast ...))))

(define (compile-expression expr)
  (compile-script (list (make-ExpressionStatement (Term-location expr) expr))))

(define (compile-module elts)
  (with-syntax ([(ast ...) elts])
    (eval #'(module-compile ast ...))))

(define (compile-interaction elts)
  (with-syntax ([(ast ...) elts])
    (eval #'(interaction-compile ast ...))))

(provide/contract [compile-script ((listof SourceElement?) . -> . syntax?)]
                  [compile-expression (Expression/X? . -> . syntax?)]
                  [compile-interaction ((listof SourceElement?) . -> . syntax?)]
                  [compile-module ((listof SourceElement?) . -> . syntax?)])
