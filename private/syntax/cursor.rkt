#lang scheme/base

(require srfi/13/string
         scheme/list
         "token.ss")

(provide (struct-out token)
         cursor? cursor-advance cursor-rewind cursor-current cursor-position
         (rename-out [build-cursor make-cursor]))

;; TODO: limit size of history buffer

;; (optional nat) * (listof token) * (listof token) * posn
(define-struct cursor (max-backtrack history future position)
  #:property
  prop:custom-write
  (lambda (c port write?)
    (let ([history (cursor-history c)]
          [max-backtrack (cursor-max-backtrack c)])
      (fprintf port "#<cursor:~a:(~a)>"
               (posn-line (cursor-position c))
               (string-join
                (append (map (lambda (token)
                               (format "~v" (token-type token)))
                             (reverse
                              (if max-backtrack
                                  (take history (min (length history) max-backtrack))
                                  history)))
                        '(".")
                        (map (lambda (t)
                               (format "~v" (token-type t)))
                             (cursor-future c)))
                " "
                'infix)))))

;; cursor-advance : cursor (-> token) -> cursor
(define (cursor-advance c get-next)
  (let* ([max-backtrack (cursor-max-backtrack c)]
         [history (cursor-history c)]
         [future (cursor-future c)]
         [position (cursor-position c)]
         [next (if (null? future)
                   (get-next)
                   (car future))])
    (make-cursor max-backtrack
                 (cons next history)
                 (if (null? future) null (cdr future))
                 (region-end (token-location next)))))

;; length-without-newlines : (listof token) -> nat
(define (length-without-newlines ls)
  (cond
    [(null? ls) 0]
    [(eq? (token-type (car ls)) 'NEWLINE)
     (length-without-newlines (cdr ls))]
    [else (add1 (length-without-newlines (cdr ls)))]))

;; cursor-rewind : cursor -> cursor
(define (cursor-rewind c)
  (let ([max-backtrack (cursor-max-backtrack c)]
        [history (cursor-history c)]
        [future (cursor-future c)]
        [position (cursor-position c)])
    (when (null? history)
      (error 'cursor-rewind "cannot rewind initial cursor"))
    (when (= (length-without-newlines future) max-backtrack)
      (error 'cursor-rewind "exceeded backtracking limit"))
    (let* ([last (car history)]
           [newlines (if (eq? (token-type last) 'NEWLINE)
                         (token-contents last)
                         0)])
      (make-cursor max-backtrack
                   (cdr history)
                   (cons last future)
                   (region-start (token-location last))))))

;; cursor-current : cursor -> (optional token)
(define (cursor-current c)
  (let ([history (cursor-history c)])
    (and (pair? history) (car history))))

;; build-cursor : nat -> cursor
(define (build-cursor max-backtrack)
  (make-cursor max-backtrack null null (make-posn 1 1 0)))
