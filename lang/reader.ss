(module reader syntax/module-reader
  #:language 'javascript/lang/module
  #:read (lambda ([in (current-input-port)])
           (let ([ast (with-syntax-errors (lambda ()
                                            (parse-program-unit in)))])
             (list `(#%module-begin ,@ast))))
  #:read-syntax (lambda ([source-name #f] [in (current-input-port)])
                  (let ([ast (with-syntax-errors (lambda ()
                                                   (parse-program-unit in)))])
                    (list `(#%module-begin ,@ast))))
  #:whole-body-readers? #t
  (require "../private/planet.ss"
           "../private/syntax/parse.ss"
           "../private/compiler/compile.ss"))
