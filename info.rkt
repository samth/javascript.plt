#lang info
(define collection "javascript")
(define name "JavaScript")
(define blurb
  (list "An implementation of JavaScript as a Racket language."))
(define scribblings '(("scribblings/javascript.scrbl" (multi-page))))
(define categories '(devtools))
(define version "0.18")
(define deps '("base"
               "compatibility-lib"
               "drracket-plugin-lib"
               "gui-lib"
               "parameter"
               "parser-tools-lib"
               "planet-lib"
               "scheme-lib"
               "set"
               "srfi-lite-lib"
               "string-constants-lib"
               "unstable-contract-lib"
               "pprint"))
(define build-deps '("in-new-directory"
                     "parser-tools-doc"
                     "racket-doc"
                     "rackunit-lib"
                     "scribble-lib"
                     "unstable-doc"))
