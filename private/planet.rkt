#lang scheme/base

(require planet/util)

(provide (all-defined-out))

(define major (this-package-version-maj))
(define minor (this-package-version-min))
(define (path-of file)
  (let-values ([(dir file must-be-dir?) (split-path file)])
    (resolve-planet-path `(planet ,(path->string file)
                                  ("dherman" "javascript.plt" ,major ,minor)
                                  ,(path->string dir)))))
(define standard-library-path
  `(planet "js.js" ("dherman" "javascript.plt" ,major ,minor) "collects"))
