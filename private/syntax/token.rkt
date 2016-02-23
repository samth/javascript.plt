#lang scheme/base

;; We use the position struct from the parser-tools collection in order to
;; interoperate with the DrScheme syntax coloring mechanism, but we use our
;; own custom token structure since the syntax colorer doesn't make use of
;; the parser-tools token structure.

(require (only-in parser-tools/lex
                  position struct:position make-position position?
                  position-offset position-line position-col
                  set-position-offset! set-position-line! set-position-col!))
(provide (all-defined-out)
         (struct-out position)
         (struct-out posn)
         posn->position position->posn)

(define-struct region (source start end) #:prefab)
;  #:property prop:custom-write (lambda (r port write?)
;                                 (fprintf port
;                                          "#<region:~a:~a-~a:~a>"
;                                          (position-line (region-start r))
;                                          (position-col (region-start r))
;                                          (position-line (region-end r))
;                                          (position-col (region-end r)))))

;; isomorphic to position, but prefab
(define-struct posn (offset line col) #:prefab)

(define (posn->position posn)
  (make-position (posn-offset posn)
                 (posn-line posn)
                 (posn-col posn)))

(define (position->posn position)
  (make-posn (position-offset position)
             (position-line position)
             (position-col position)))

;; region->string : region -> string
(define (region->string r)
  (format "~a:~a:~a-~a:~a"
          (object-name (region-source r))
          (posn-line (region-start r))
          (posn-col (region-start r))
          (posn-line (region-end r))
          (posn-col (region-end r))))

;; string * boolean * boolean
(define-struct regexp-contents (pattern global? case-insensitive?)
  #:transparent)

;; symbol * (optional (union string number symbol regexp-contents)) * region
(define-struct token (type contents location)
  #:transparent)
