#lang scheme/base

(require (planet dherman/parameter:1:3)
         (only-in srfi/1/list lset-adjoin))

(provide (except-out (all-defined-out) default-keywords keyword-guard add-keyword remove-keyword (struct-out config)))

;; ===========================================================================
;; LEXICAL KEYWORD UTILITIES
;; ===========================================================================

(define default-keywords
  (let ([keywords '(break case catch const continue default delete do
                    else finally for function if instanceof in
                    new return switch this throw try typeof
                    var void while with)]
        [null-literal '(null)]
        [boolean-literals '(true false)]
        [future-keywords '(abstract boolean byte char class const debugger double
                           enum export extends final float goto implements import
                           int interface long native package private protected public
                           short static super synchronized throws transient volatile)]
        [extended-keywords '(let)])
    (append keywords null-literal boolean-literals future-keywords extended-keywords)))

;; keyword-guard : symbol -> boolean -> any
(define (keyword-guard kw)
  (lambda (v)
    (if v (add-keyword kw) (remove-keyword kw))))

;; add-keyword : symbol -> any
(define (add-keyword kw)
  (lexical-keywords (lset-adjoin eq? (lexical-keywords) kw)))

;; remove-keyword : symbol -> any
(define (remove-keyword kw)
  (lexical-keywords (filter (lambda (k)
                              (not (eq? k kw)))
                            (lexical-keywords))))

(define current-debug-port (make-parameter (current-error-port)))

;; ===========================================================================
;; JAVASCRIPT SETTINGS
;; ===========================================================================

(define-parameter-set config current-config
  ;; Turn on this flag to allow anonymous function expressions to appear in
  ;; SourceElement contexts. Named functions in SourceElement contexts are
  ;; always considered declarations. (Not ECMA-compliant).
  (allow-anonymous-function-source-elements? #t)

  ;; Allow do-while statements to omit the semicolon (not ECMA-compliant)?
  (infer-do-while-semicolon? #f)

  ;; Allow Mozilla-style extended catch statements with guard expressions
  ;; (not ECMA-compliant)?
  (enable-extended-catch-statements? #f)

  ;; Allow function declarations to appear nested inside of statements
  ;; (not ECMA-compliant)?
  (allow-nested-function-declarations? #f)

  ;; Turn on this flag to enable proper tail recursion.
  (proper-tail-recursion? #f)

  ;; Set this to a positive integer to set an artificial limit to the number
  ;; of allowed nested function calls.
  (stack-limit #f)

  ;; Turn on this flag to allow `eval' to be used in contexts other than as
  ;; a function call. Uses of `eval' in any of these other contexts will
  ;; not inherit the lexical environment of their application site.
  (allow-eval-aliasing? #f)

  ;; Choose from either 'standard (i.e., the standard syntax for JavaScript)
  ;; or 'sexp (an S-expression syntax).
  (code-representation 'standard)

  ;; The current set of lexical keywords.
  (lexical-keywords default-keywords)

  ;; Enable `let' expressions, which introduce a new lexical scope block
  ;; (as opposed to implicitly hoisted variables)?
  (enable-let-expressions? #t (keyword-guard 'let))

  ;; Debugging options:
  (debug-destination 'error-port)
  (debug-scope-resolution? #f)
  (debug-unbound-references? #f))

(define default-config (current-config))

(define (default-config? c)
  (equal? c default-config))

(define ecma-strict?
  (make-pseudo-parameter (lambda ()
                           (not (or (allow-anonymous-function-source-elements?)
                                    (infer-do-while-semicolon?)
                                    (enable-extended-catch-statements?)
                                    (allow-nested-function-declarations?))))
                         (lambda (b)
                           (let ([non-strict? (not b)])
                             (allow-anonymous-function-source-elements? non-strict?)
                             (infer-do-while-semicolon? non-strict?)
                             (enable-extended-catch-statements? non-strict?)
                             (allow-nested-function-declarations? non-strict?)))))
