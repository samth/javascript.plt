#lang scheme/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "../private/syntax/regexps.ss"
         "../private/config.ss")

(provide get-syntax-token)

;; Unfortunately, tokenizing JavaScript is context-sensitive. The syntax of
;; regular expressions (similar to that of Perl) is ambiguous with division,
;; as the following example demonstrates:

;;     var g = 2;
;;     function f(re) { return "3".match(re); }
;;
;;     4  /3/g    /* evaluates to 0.66666666 */
;;     f( /3/g )  /* evaluates to "3"        */
;;        ^^^^

;; The language definition states that regular expression literals should only
;; be allowed in expression contexts, and division operators should only be
;; allowed in binary infix operator position.

;; However, for syntax coloring, we tokenize without parsing, so we cannot
;; no whether we are in an expression context or an operator context. So there
;; is no way to know the correct way to tokenize.

;; Moreover, in the PLT Framework manual, section 7.1, under the start-colorer
;; method of color:text<%>, the documented invariant for a syntax coloring
;; tokenizer is that "the tokenization of some part of the input cannot depend
;; on earlier parts of the input." So we cannot even perform a heuristic guess
;; based on the previous token.

;; We choose to prefer regular expression literals when it is possible to lex
;; them, and otherwise back off to the division operator. In practice, the
;; ambiguity should not arise often.

(define (syn-val lex a b c d)
  (values lex a b (position-offset c) (position-offset d)))

(define (colorize-string delimiter my-start-pos)
  (define lxr
    (lexer
     [(:or #\' #\")
      (if (string=? lexeme delimiter)
          (syn-val "" 'string #f my-start-pos end-pos)
          (lxr input-port))]
     [(eof) (syn-val "" 'error #f my-start-pos end-pos)]
     [(:seq #\\ (:or #\' #\")) (lxr input-port)]
     [any-char (lxr input-port)]))
  lxr)

(define (colorize-block-comment my-start-pos)
  (define lxr
    (lexer
     [(:seq #\* #\/)
      (syn-val "" 'comment #f my-start-pos end-pos)]
     [(eof) (syn-val "" 'error #f my-start-pos end-pos)]
     [any-char (lxr input-port)]))
  lxr)

;; TODO: fix the taxonomy a little
(define get-syntax-token
  (lexer
   [(:or "true" "false" "null")
    (syn-val lexeme 'literal #f start-pos end-pos)]     
   [lex:integer
    (syn-val lexeme 'literal #f start-pos end-pos)]
   [lex:float
    (syn-val lexeme 'literal #f start-pos end-pos)]
   [(:or "[" "]" "{" "}" "(" ")")
    (syn-val lexeme 'parenthesis (string->symbol lexeme) start-pos end-pos)]
   [(:or "," ":" ";" "=" ".")
    (syn-val lexeme 'default #f start-pos end-pos)]
;  [lex:keyword
;   (syn-val lexeme 'keyword #f start-pos end-pos)]
   [(:seq #\/ #\*)
    ((colorize-block-comment start-pos) input-port)]
   [lex:line-comment
    (syn-val lexeme 'comment #f start-pos end-pos)]
   [lex:assignment-operator
    (syn-val lexeme 'keyword #f start-pos end-pos)]
   [lex:operator
    (syn-val lexeme 'keyword #f start-pos end-pos)]
   [lex:identifier
    (if (memq (string->symbol lexeme) (lexical-keywords))
        (syn-val lexeme 'keyword #f start-pos end-pos)
        (syn-val lexeme 'identifier #f start-pos end-pos))]
   [(:or #\' #\")
    ((colorize-string lexeme start-pos) input-port)]
   [(:+ lex:whitespace)
    (syn-val lexeme 'whitespace #f start-pos end-pos)]
   [(eof)
    (syn-val lexeme 'eof #f start-pos end-pos)]
   [any-char
    (syn-val lexeme 'error #f start-pos end-pos)]
   ))
