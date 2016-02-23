#lang scheme/base

(require scheme/contract
         unstable/contract
         parameter
         "private/config.ss")

(provide/contract [allow-anonymous-function-source-elements? (parameter/c boolean?)]
                  [infer-do-while-semicolon? (parameter/c boolean?)]
                  [enable-extended-catch-statements? (parameter/c boolean?)]
                  [allow-nested-function-declarations? (parameter/c boolean?)]
                  [proper-tail-recursion? (parameter/c boolean?)]
                  [stack-limit (parameter/c (maybe/c natural-number/c))]
                  [allow-eval-aliasing? (parameter/c boolean?)]
                  [code-representation (parameter/c (one-of/c 'standard 'sexp))]
                  [lexical-keywords (parameter/c (listof symbol?))]
                  [enable-let-expressions? (parameter/c boolean?)]
                  [debug-destination (parameter/c (one-of/c 'error-port 'debug-console))]
                  [debug-scope-resolution? (parameter/c boolean?)]
                  [debug-unbound-references? (parameter/c boolean?)])

(provide/contract [current-debug-port (-> output-port?)]
                  [ecma-strict? (pseudo-parameter/c boolean?)])
