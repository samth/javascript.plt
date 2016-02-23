#lang scheme/base

(require scheme/contract
         "private/syntax/parse.ss"
         "private/syntax/ast-core.ss"
         "private/compiler/compile.ss"
         "lang/lang.ss"
         "private/runtime/namespace.ss")

;; (union input-source sytax) [namespace] -> any
(define (eval-script src [ns (make-js-namespace)])
  (if (syntax? src)
      (eval src ns)
      (with-syntax ([(ast ...) (with-syntax-errors
                                (lambda ()
                                  (parse-program-unit src)))])
        (eval #'(script-begin ast ...) ns))))

;; (union input-source sytax) [namespace] -> any
(define (eval-expression src [ns (make-js-namespace)])
  (if (syntax? src)
      (eval src ns)
      (let ([ast (with-syntax-errors
                  (lambda ()
                    (parse-expression src)))])
        (with-syntax ([ast (make-ExpressionStatement (Term-location ast) ast)])
          (eval #'(script-begin ast) ns)))))

(provide/contract [eval-script (((or/c input-source? syntax?)) (namespace?) . ->* . any)]
                  [eval-expression (((or/c input-source? syntax?)) (namespace?) . ->* . any)])
