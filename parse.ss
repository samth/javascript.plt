#lang scheme/base

(require (planet cobbe/contract-utils:1/contract-utils)
         scheme/contract
         scheme/class
         "private/syntax/ast-core.ss"
         "private/syntax/ast-utils.ss"
         "private/syntax/parse.ss"
         "private/syntax/exceptions.ss"
         "private/syntax/token.ss"
         "private/syntax/lex.ss"
         "private/syntax/input.ss")

(provide/contract
 [input-source? predicate/c]
 [input-source->input-port (input-source? . -> . input-port?)])

(provide (struct-out exn:fail:syntax))

(provide (struct-out token)
         (struct-out position)
         (struct-out regexp-contents)
         (struct-out region))

(provide/contract
 [lexer<%> interface?]
 [lexer% (implementation?/c lexer<%>)]
 [lex (input-source? . -> . (-> token?))]
 [region->string (region? . -> . string?)])

(provide/contract
 [parser<%> interface?]
 [parser% (implementation?/c parser<%>)]
 [input-source->parser (input-source? . -> . (is-a?/c parser<%>))]
 [parse-program-unit (input-source? . -> . (listof SourceElement?))]
 [parse-expression (input-source? . -> . Expression?)]
 [parse-function-constructor (string? string? . -> . FunctionExpression?)]
 [parse-source-element (input-source? . -> . SourceElement?)])
