#lang scheme/base

(require srfi/13/string
         "../syntax/ast-core.ss"
         "../syntax/ast-utils.ss"
         "exceptions.ss"
         "value.ss")

(provide (except-out (all-defined-out) less-than? same-type? == ===))

;; 11.4.2
(define (js:void v)
  (void))

;; 11.4.1
(define (js:delete ref)
  (if (ref? ref)
      (delete-ref! ref)
      #t))

;; 11.4.3
(define (js:typeof v)
  (cond
    [(void? v) "undefined"]
    [(null? v) "object"]
    [(boolean? v) "boolean"]
    [(string? v) "string"]
    [(function? v) "function"]
    [(object? v) "object"]))

;; 11.8.6
(define (js:instanceof x y)
  (descendant-of? x y))

;; 11.5.1
(define (js:* x y)
  (* (any->number x)
     (any->number y)))

;; 11.5.2
(define (js:/ x y)
  ;; try to lose some of Scheme's precision :)
  (let ([result (exact->inexact (/ (any->number x)
                                   (any->number y)))])
    (if (integer? result)
        (inexact->exact result)
        result)))

;; 11.6.1
(define js:+
  (case-lambda
    [(x) (any->number x)]
    [(x y)
     (let ([x (any->primitive x object->number)]
           [y (any->primitive y object->number)])
       (if (or (string? x) (string? y))
           (string-append (any->string x)
                          (any->string y))
           (+ (any->number x)
              (any->number y))))]))

;; 11.6.2
(define js:-
  (case-lambda
    [(x) (- (any->number x))]
    [(x y) (- (any->number x)
              (any->number y))]))

;; 11.5.3
(define (js:% x y)
  (let ([dividend (any->number x)]
        [divisor (any->number y)])
    ;; TODO: there's probably an easier way to do this with the Scheme library
    (cond
      [(or (NaN? dividend) (NaN? divisor) (infinite? dividend) (zero? divisor))
       NaN]
      [(and (not (infinite? dividend)) (infinite? divisor))
       x]
      [else
       (let* ([quotient (exact->inexact (/ dividend divisor))]
              [sign (if (negative? quotient) - +)]
              [magnitude (inexact->exact (floor (abs quotient)))])
         (- dividend (* divisor (sign magnitude))))])))

;; 11.7.1
(define (js:<< x y)
  (arithmetic-shift (any->int32 x)
                    (bitwise-and (any->uint32 y) #x1F)))

;; 11.7.2
(define (js:>> x y)
  (arithmetic-shift (any->int32 x)
                    (- (bitwise-and (any->uint32 y) #x1F))))

;; 11.7.3
(define (js:>>> x y)
  (arithmetic-shift (any->uint32 x)
                    (- (bitwise-and (any->uint32 y) #x1F))))

;; 11.10
(define (js:& x y)
  (bitwise-and (any->int32 x)
               (any->int32 y)))

;; 11.10
(define (js:^ x y)
  (bitwise-xor (any->int32 x)
               (any->int32 y)))

;; 11.10
(define (js:\| x y)
  (bitwise-ior (any->int32 x)
               (any->int32 y)))

;; 11.4.8
(define (js:~ x)
  (bitwise-not (any->int32 x)))

;; 11.4.9
(define (js:! x)
  (not (any->boolean x)))

;; 11.8.5
(define (less-than? x y)
  (let ([x (any->primitive x object->number)]
        [y (any->primitive y object->number)])
    (if (and (string? x) (string? y))
        (and (not (string-prefix? y x))
             (string-prefix? x y))
        (< (any->number x)
           (any->number y)))))

;; 11.8.1
(define (js:< x y)
  (less-than? x y))

;; 11.8.2
(define (js:> x y)
  (less-than? y x))

;; 11.8.3
(define (js:<= x y)
  (not (less-than? y x)))

;; 11.8.4
(define (js:>= x y)
  (not (less-than? x y)))

;; 11.8.7
(define (js:in x y)
  (unless (object? y)
    (raise-runtime-type-error here "object" (any->string y)))
  (has-property? y (any->string x)))

;; same-type? : any any -> boolean
(define (same-type? x y)
  (or (and (void? x) (void? y))
      (and (null? x) (null? y))
      (and (boolean? x) (boolean? y))
      (and (number? x) (number? y))
      (and (string? x) (string? y))
      ;(and (function? x) (function? y))
      (and (object? x) (object? y))))

;; 11.9.3
(define (== x y)
  (cond
    [(not (same-type? x y))
     (or (and (null? x) (void? y))
         (and (void? x) (null? y))
         (and (number? x) (string? y) (== x (any->number y)))
         (and (string? x) (number? y) (== (any->number x) y))
         (and (boolean? x) (== (any->number x) y))
         (and (boolean? y) (== x (any->number y)))
         ;; TODO: are these four rules right?
         (and (string? x) (object? y) (== x (any->string y)))
         (and (number? x) (object? y) (== x (any->number y)))
         (and (object? x) (string? y) (== (any->string x) y))
         (and (object? x) (number? y) (== (any->number x) y))
         ;; These couldn't have been right (value->primitive takes two arguments):
;         (and (or (string? x) (number? x)) (object? y) (== x (value->primitive y)))
;         (and (object? x) (or (string? y) (number? y)) (== (value->primitive x) y))
     )]
    [(void? x) #t]
    [(null? x) #t]
    [(number? x) (= x y)]
    [(string? x) (string=? x y)]
    [(boolean? x) (eq? x y)]
    [(object? x) (eq? x y)]
    [else (raise-runtime-type-error here "native values" (format "~a and ~a" (any->string x) (any->string y)))]))

;; 11.9.1
(define (js:== x y)
  (== x y))

;; 11.9.2
(define (js:!= x y)
  (not (== x y)))

;; 11.9.6
(define (=== x y)
  (and (same-type? x y)
       (or (void? x)
           (null? x)
           (and (number? x) (= x y))
           (and (string? x) (string=? x y))
           (and (boolean? x) (eq? x y))
           (and (object? x) (eq? x y))
           (eq? x y))))

;; 11.9.4
(define (js:=== x y)
  (=== x y))

;; 11.9.5
(define (js:!== x y)
  (not (=== x y)))
