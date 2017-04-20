#lang scheme/base

(require scheme/gui/base
         framework
         scheme/runtime-path
         drscheme/tool
         scheme/match
         scheme/unit
         scheme/class
         string-constants
         "../private/syntax/parse.ss"
         "../private/compiler/compile.ss"
         "../private/compiler/context.ss"
         (except-in "../private/runtime/runtime.ss" object? make-object)
         "../private/runtime/namespace.ss"
         "../lang/lang.ss"
         "syntax-color.ss"
         "../private/config.ss"
         "debug-console.ss")
(require (for-syntax scheme/base)
         (for-template "../lang/lang.ss"))

(provide tool@)

(define-runtime-path home-directory (build-path 'up))

(define (require-spec module-name . path)
  `(file ,(path->string (apply build-path home-directory (append path (list module-name))))))

(define (prefab->list x)
  (cons (prefab-struct-key x)
        (cdr (vector->list (struct->vector x)))))

(define (list->prefab x)
  (apply make-prefab-struct x))

(define-unit tool@
  (import drscheme:tool^)
  (export drscheme:tool-exports^)

  (define (phase1) (void))
  (define (phase2) 
    (drscheme:language-configuration:add-language
     (make-object (macro-stepper-mixin
                   ((drscheme:language:get-default-mixin)
                    (javascript-lang-mixin 'ecmascript))))))

  (define macro-stepper-mixin
    (mixin () ()
      (super-make-object)
      (define/override (enable-macro-stepper?) #t)))
  
  (define (javascript-lang-mixin level)
    (class* object% (drscheme:language:language<%>)
      (define/public (default-settings) default-config)
      (define/public (default-settings? x)
        (equal? x default-config))
      (define/public (marshall-settings x)
        (with-handlers ([void (lambda (exn)
                                (prefab->list default-config))])
          (prefab->list x)))
      (define/public (unmarshall-settings x)
        (with-handlers ([void (lambda (exn) default-config)])
          (list->prefab x)))
;     (define debug-console #f)
      (define/public (get-reader-module) #f)
      (define/public (get-metadata a b) #f)
      (define/public (metadata->settings m) #f)
      (define/public (get-metadata-lines) #f)
      (define/public (capability-value capability)
        (case capability
          [(drscheme:language-menu-title) "JavaScript"]
          [(drscheme:define-popup) #f]
          [(drscheme:special:insert-fraction) #f]
          [(drscheme:special:insert-lambda) #f]
          [(drscheme:special:insert-large-letters) #f]
          [(drscheme:special:insert-image) #f]
          [(drscheme:special:insert-comment-box) #f]
          [(drscheme:special:insert-gui-tool) #f]
          [(drscheme:special:slideshow-menu-item) #f]
          [(drscheme:special:insert-text-box) #f]
          [(drscheme:special:xml-menus) #f]
          [else (drscheme:language:get-capability-default capability)]))
      (define/public (first-opened) (void))
      (define/public (get-comment-character) (values "//" #\*))
      (define/public (config-panel parent)
        (letrec ([top (instantiate vertical-panel% ()
                        (parent parent)
                        (alignment '(center center))
                        (stretchable-height #f)
                        (stretchable-width #f))]
                 [syntax-group (instantiate group-box-panel% ()
                                 (label "Syntactic extensions (non-ECMA)")
                                 (parent top)
                                 (alignment '(left center)))]
                 [do-while (make-object check-box%
                             "Infer semicolons for do-while loops?"
                             syntax-group)]
                 [anonymous-function-statements (make-object check-box%
                                                  "Allow anonymous function statements?"
                                                  syntax-group)]
                 [nested-functions (make-object check-box%
                                     "Allow nested function declarations?"
                                     syntax-group)]
                 [special-forms-group (instantiate group-box-panel% ()
                                        (label "Special forms")
                                        (parent top)
                                        (alignment '(left center)))]
                 [let-expressions (instantiate check-box% ()
                                    (label "Enable let-expressions")
                                    (parent special-forms-group)
                                    (enabled #t))]
                 [catch-guards (instantiate check-box% ()
                                 (label "Enable catch guards")
                                 (parent special-forms-group)
                                 (enabled #f))]
                 [semantics-group (instantiate group-box-panel% ()
                                    (label "Behavioral extensions")
                                    (parent top)
                                    (alignment '(left center)))]
                 [tail-calls (instantiate check-box% ()
                               (label "Enable proper tail calls?")
                               (parent semantics-group)
                               (enabled #f))]
                 [eval-value (instantiate check-box% ()
                               (label "Treat eval as a value?")
                               (parent semantics-group)
                               (enabled #f))]
                 [source-representation (instantiate radio-box% ()
                                          (label "Source code representation")
                                          (choices '("Standard" "S-expressions"))
                                          (parent semantics-group)
                                          (enabled #f))]
                 [debugging-group (instantiate group-box-panel% ()
                                    (label "Debugging")
                                    (parent top)
                                    (alignment '(left center)))]
                 [debug-port (instantiate radio-box% ()
                               (label "Debugging output destination")
                               (choices '("Standard error port" "Debug window"))
                               (parent debugging-group)
                               (enabled #f))]
                 [debug-scope (instantiate check-box% ()
                                (label "Monitor scope resolution?")
                                (parent debugging-group)
                                (enabled #t))]
                 [debug-unbound (instantiate check-box% ()
                                  (label "Warn on unbound variables?")
                                  (parent debugging-group)
                                  (enabled #t))])
          (case-lambda
            [()
             (infer-do-while-semicolon? (send do-while get-value))
             (allow-anonymous-function-source-elements? (send anonymous-function-statements get-value))
             (allow-nested-function-declarations? (send nested-functions get-value))
             (enable-let-expressions? (send let-expressions get-value))
             (enable-extended-catch-statements? (send catch-guards get-value))
             (proper-tail-recursion? (send tail-calls get-value))
             (allow-eval-aliasing? (send eval-value get-value))
             (case (send source-representation get-selection)
               [(0) (code-representation 'standard)]
               [(1) (code-representation 'sexp)])
             (case (send debug-port get-selection)
               [(0) (debug-destination 'error-port)]
               [(1) (debug-destination 'debug-console)])
             (debug-scope-resolution? (send debug-scope get-value))
             (debug-unbound-references? (send debug-unbound get-value))
             (current-config)]
            [(settings)
             (current-config settings)
             (send do-while set-value (infer-do-while-semicolon?))
             (send anonymous-function-statements set-value (allow-anonymous-function-source-elements?))
             (send nested-functions set-value (allow-nested-function-declarations?))
             (send let-expressions set-value (enable-let-expressions?))
             (send catch-guards set-value (enable-extended-catch-statements?))
             (send tail-calls set-value (proper-tail-recursion?))
             (send eval-value set-value (allow-eval-aliasing?))
             (send source-representation set-selection (case (code-representation)
                                                         [(standard) 0]
                                                         [(sexp) 1]))
             (send debug-port set-selection (case (debug-destination)
                                              [(error-port) 0]
                                              [(debug-console) 1]))
             (send debug-scope set-value (debug-scope-resolution?))
             (send debug-unbound set-value (debug-unbound-references?))
             ])))
      (define/public (front-end/complete-program port settings)
        (lambda ()
          (if (eof-object? (peek-char-or-special port))
              eof
              (with-syntax ([(ast ...) (with-syntax-errors (lambda ()
                                                       (parse-program-unit port)))])
                #'(script-begin ast ...)))))
      (define/public (extra-repl-information settings port) (void))
      (define/public (front-end/finished-complete-program settings) (void))
      (define/public (front-end/interaction port settings)
        (lambda ()
          (if (eof-object? (peek-char-or-special port))
              eof
              (with-syntax ([(ast ...) (with-syntax-errors (lambda ()
                                                       (parse-program-unit port)))])
                #'(interaction-begin ast ...)))))
      (define/public (get-style-delta) #f)
      (define/public (get-language-position)
        (list (string-constant experimental-languages)
              "JavaScript"))
      ;; TODO: this is copied from honu -- is it right?
      (define/public (order-manuals x)
        (values
         (list #"drscheme" #"tour" #"help")
         #f))
      (define/public (get-language-name) "JavaScript")
      (define/public (get-language-url) "http://www.ecma-international.org/publications/standards/Ecma-262.htm")
      (define/public (get-language-numbers) (list 1000 12))
      (define/public (get-teachpack-names) null)
      (define (LOG! fmt . args)
        (with-output-to-file "C:\\Documents and Settings\\dherman\\Desktop\\log.txt"
          (lambda ()
            (apply fprintf (current-output-port) fmt args))
          'append))
      (define/public (on-execute settings run-in-user-thread)
        (let ([module-forms (require-spec "module-forms.ss" "drscheme")]
              [runtime (require-spec "runtime.ss" "private" "runtime")]
              [standard-library (require-spec "standard-library.ss" "private" "runtime")]
              [lang (require-spec "lang.ss" "lang")]
              [debug (require-spec "debug.ss")])
          (print-struct #t)
          (dynamic-require module-forms #f)
          (dynamic-require runtime #f)
          (dynamic-require standard-library #f)
          (dynamic-require debug #f)
          (dynamic-require lang #f)
          (let ([path1 ((current-module-name-resolver) module-forms #f #f #t)]
                [path2 ((current-module-name-resolver) runtime #f #f #t)]
                [path3 ((current-module-name-resolver) standard-library #f #f #t)]
                [path4 ((current-module-name-resolver) debug #f #f #t)]
                [path5 ((current-module-name-resolver) lang #f #f #t)]
                [n (current-namespace)])
            (run-in-user-thread
             (lambda ()
               (current-debug-port (current-error-port))
;               (if (eq? (debug-destination) 'debug-console)
;                   (begin
;                     (when debug-console
;                       (send debug-console kill))
;                     (set! debug-console (create-debug-console))
;                     (send debug-console show #t)
;                     (current-debug-port (send debug-console get-debug-port)))
;                   (current-debug-port (current-error-port)))
               (let ([previous-error-display-handler
                      (drscheme:debug:make-debug-error-display-handler
                       (error-display-handler))])
                 (error-display-handler
                  (lambda (msg exn)
                    (if (exn:runtime? exn)
                        (let* ([value (exn:runtime-value exn)]
                               [msg (format "uncaught exception: ~a" (any->string value))])
                          (previous-error-display-handler msg exn))
                        (previous-error-display-handler msg exn)))))
               (let ([previous-eval (drscheme:debug:make-debug-eval-handler (current-eval))])
                 (current-eval
                  (lambda (exp)
                    (previous-eval (if (syntax? exp)
                                       (namespace-syntax-introduce exp)
                                       exp)))))
               (namespace-attach-module n path1)
               (namespace-require path1)
               (namespace-attach-module n path2)
               (namespace-attach-module n path3)
               (namespace-attach-module n path4)
               (namespace-attach-module n path5)
               (reset-js-namespace! n)
               )))))
      (define/public (render-value value settings port)
        (display (completion->string value) port))
      (define/public (render-value/format value settings port width)
        (display (completion->string value) port))
      (define/public (create-executable fn parent . args)
        (message-box "Unsupported"
                     "Sorry - executables are not supported for JavaScript at this time"
                     parent))
      (define/public (get-one-line-summary) "ECMA-262 Edition 3 (JavaScript)")
      (super-make-object)))

  ;; short-sym->pref-name : symbol -> symbol
  ;; returns the preference name for the color prefs
  (define (short-sym->pref-name sym) (string->symbol (short-sym->style-name sym)))

  ;; short-sym->style-name : symbol->string
  ;; converts the short name (from the table above) into a name in the editor list
  ;; (they are added in by `color-prefs:register-color-pref', called below)
  (define (short-sym->style-name sym) (format "javascript:syntax-colors:scheme:~a" sym))

  ;; TODO: fix the taxonomy a little
  
  ;; JavaScript editing colors
  (define color-prefs-table
    `((keyword     ,(make-object color% "purple")      "keyword")
      (parenthesis ,(make-object color% 132 60 36)     "parenthesis")
      (string      ,(make-object color% "forestgreen") "string")
      (literal     ,(make-object color% "forestgreen") "literal")
      (comment     ,(make-object color% 194 116 31)    "comment")
      (error       ,(make-object color% "red")         "error")
      (identifier  ,(make-object color% 38 38 128)     "identifer")
      (default     ,(make-object color% "black")       "default")))
  
  ;; extend-preferences-panel : vertical-panel -> void
  ;; adds in the configuration for the Honu colors to the prefs panel
  (define (extend-preferences-panel parent)
    (for-each
     (lambda (line)
       (let ([sym (car line)])
         (color-prefs:build-color-selection-panel 
          parent
          (short-sym->pref-name sym)
          (short-sym->style-name sym)
          (format "~a" sym))))
     color-prefs-table))
  
  ;; JavaScript editing mode
  (define javascript:surrogate-text%
    (class mode:surrogate-text%
      (define/override (put-file text sup directory default-name)
        (parameterize ([finder:default-filters
                        (list (list "JavaScript (.js)" "*.js")
                              (list "Any" "*.*"))]
                       [finder:default-extension "js"])
          (sup directory default-name)))
      (super-make-object)))
  
  (define javascript:surrogate-text-mode%
    (color:text-mode-mixin javascript:surrogate-text%))
  
  (define mode-surrogate
    (new javascript:surrogate-text-mode% ; color:text-mode%
         (matches (list (list '|{| '|}|)
                        (list '|(| '|)|)
                        (list '|[| '|]|)))
         (get-token get-syntax-token)
         (token-sym->style short-sym->style-name)))
  
  (define (matches-language? l)
    (match l
      [(list _ "JavaScript" _ ...) #t]
      [_ #f]))
  
  (define (delimiter-pair? x y)
    (or (and (char=? x #\() (char=? y #\)))
        (and (char=? x #\[) (char=? y #\]))
        (and (char=? x #\{) (char=? y #\}))))
  
  ;; repl-submit? : drscheme:rep:text<%> nat -> boolean
  (define (repl-submit? text prompt-position)
    (let loop ([i prompt-position]
               [blank? #t]
               [string-char #f]
               [delimiter-stack null])
      (let ([c (send text get-character i)])
        (case c
          [(#\nul)
           (and (not blank?)
                (not string-char)
                (null? delimiter-stack))]
          [(#\( #\[ #\{)
           (if string-char
               (loop (add1 i) #f string-char delimiter-stack)
               (loop (add1 i) #f #f (cons c delimiter-stack)))]
          [(#\) #\] #\})
           (cond
             [string-char
              (loop (add1 i) #f string-char delimiter-stack)]
             [(and (pair? delimiter-stack)
                   (delimiter-pair? (car delimiter-stack) c))
              (loop (add1 i) #f #f (cdr delimiter-stack))]
             [else
              (loop (add1 i) #f #f delimiter-stack)])]
          [(#\' #\")
           (cond
             [(and string-char (char=? c string-char))
              (loop (add1 i) #f #f delimiter-stack)]
             [string-char
              (loop (add1 i) #f string-char delimiter-stack)]
             [else
              (loop (add1 i) #f c delimiter-stack)])]
          [(#\\)
           (if string-char
               (loop (+ i 2) #f string-char delimiter-stack)
               (loop (add1 i) #f string-char delimiter-stack))]
          [else
           (loop (add1 i)
                 (and blank? (char-whitespace? c))
                 string-char
                 delimiter-stack)]))))
  
  ;; Wire up to DrScheme.
  
  (drscheme:modes:add-mode "JavaScript mode" mode-surrogate repl-submit? matches-language?)
  (color-prefs:add-to-preferences-panel "JavaScript" extend-preferences-panel)

  (for ([line color-prefs-table])
       (let ([sym (car line)]
             [color (cadr line)])
         (color-prefs:register-color-preference (short-sym->pref-name sym)
                                                (short-sym->style-name sym)
                                                color))))
