#lang scheme/base

(require (for-syntax scheme/base)
         (for-syntax "../private/compiler/context.ss")
         (for-syntax "../private/compiler/compile.ss")
         "../private/runtime/runtime.ss"
         "../private/runtime/standard-library.ss")

(define-syntax (script-compile stx)
  (syntax-case stx ()
    [(script-compile ast ...)
     (parameterize ([current-eval-context #'here]
                    [current-compilation-context 'script]
                    [current-source-syntax stx])
       (with-syntax ([e (compile-script (syntax->datum #'(ast ...)))])
         #'(quote-syntax (begin (current-Function-context #'here)
                                e))))]))

(define-syntax (script-begin stx)
  (syntax-case stx ()
    [(script-begin ast ...)
     (parameterize ([current-eval-context #'here]
                    [current-compilation-context 'script]
                    [current-source-syntax stx])
       (with-syntax ([e (compile-script (syntax->datum #'(ast ...)))])
         #'(begin (current-Function-context #'here)
                  e)))]))

(define-syntax (interaction-begin stx)
  (syntax-case stx ()
    [(interaction-begin ast ...)
     (parameterize ([current-eval-context #'here]
                    [current-compilation-context 'interaction]
                    [current-source-syntax stx])
       (with-syntax ([e (compile-interaction (syntax->datum #'(ast ...)))])
         #'(begin (current-Function-context #'here)
                  e)))]))

(define-syntax (interaction-compile stx)
  (syntax-case stx ()
    [(interaction-compile ast ...)
     (with-syntax ([e (parameterize ([current-eval-context #'here]
                                     [current-compilation-context 'interaction]
                                     [current-source-syntax stx])
                        (compile-interaction (syntax->datum #'(ast ...))))])
       #'(quote-syntax (begin (current-Function-context #'here)
                              e)))]))

(define-syntax (eval-begin stx)
  (syntax-case stx ()
    [(eval-begin ast ...)
     (parameterize ([current-pragmas (hash-set (current-pragmas) '(lexical scope) #t)]
                    [current-eval-context #'here]
                    [current-compilation-context 'eval]
                    [current-source-syntax stx])
       (with-syntax ([body (with-scope #f
                             (compile-global (syntax->datum #'(ast ...))))])
         #'(begin (current-Function-context #'here)
                  body)))]))

(define-syntax (module-compile stx)
  (syntax-case stx ()
    [(module-compile ast ...)
     (with-syntax ([body (parameterize ([current-eval-context #'here]
                                        [current-source-syntax stx])
                           (compile-module (syntax->datum #'(ast ...))))])
       (with-syntax ([module #'(#%plain-module-begin
                                (install-standard-library-once! global-object)
                                (current-Function-context #'here)
                                body)])
         #'(quote-syntax module)))]))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(module-begin)
     #'(#%plain-module-begin (begin #f))]
    [(module-begin ast ...)
     (with-syntax ([body (parameterize ([current-eval-context #'here]
                                        [current-source-syntax stx])
                           (compile-module (syntax->datum #'(ast ...))))])
       #'(#%plain-module-begin
          (install-standard-library-once! global-object)
          (current-Function-context #'here)
          body))]))

(define-syntax (function-begin stx)
  (syntax-case stx ()
    [(function-begin ast)
     (parameterize ([current-eval-context #'here]
                    [current-compilation-context 'eval]
                    [current-source-syntax stx])
       (compile-function-expression (syntax->datum #'ast)))]))

(provide module-begin module-compile eval-begin script-begin script-compile interaction-begin interaction-compile function-begin)
