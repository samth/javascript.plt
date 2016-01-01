#lang setup/infotab
(define name "JavaScript")
(define blurb
  (list "An implementation of JavaScript as a PLT Scheme language."))
(define scribblings '(("scribblings/javascript.scrbl" (multi-page))))
(define categories '(devtools))
(define version "0.18")
(define primary-file "main.ss")
(define release-notes
  (list '(p "Bugfix: Incorrect rendering of case clauses (" (a ([href "http://planet.plt-scheme.org/trac/ticket/155"]) "#155") "). "
            "Thanks to Dave Gurnell for the report and fix.")
        '(p "Bugfix: missing environment argument to recursive calls inside PJS. "
            "Thanks to Jay McCarthy for the report and fix.")))
(define required-core-version "4.1.3.0")
(define repositories '("4.x"))
