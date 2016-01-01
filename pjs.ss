#lang scheme/base

(require scheme/contract
         "private/syntax/ast-core.ss"
         "private/syntax/ast-utils.ss"
         "private/syntax/syntax.ss")

(provide/contract
 [syntax->expression (syntax? . -> . Expression?)]
 [syntax->statement (syntax? . -> . Statement?)]
 [syntax->source-element (syntax? . -> . SourceElement?)]
 [syntax->program-unit (syntax? . -> . (listof SourceElement?))])

(provide/contract
 [sexp->expression (any/c . -> . Expression?)]
 [sexp->statement (any/c . -> . Statement?)]
 [sexp->source-element (any/c . -> . SourceElement?)]
 [sexp->program-unit (any/c . -> . (listof SourceElement?))])

(provide/contract
 [expression->syntax (Expression? . -> . syntax?)]
 [statement->syntax (Statement? . -> . syntax?)]
 [source-element->syntax (SourceElement? . -> . syntax?)]
 [program-unit->syntax ((listof SourceElement?) . -> . syntax?)])

(provide/contract
 [expression->sexp (Expression? . -> . any)]
 [statement->sexp (Statement? . -> . any)]
 [source-element->sexp (SourceElement? . -> . any)]
 [program-unit->sexp ((listof SourceElement?) . -> . any)])
