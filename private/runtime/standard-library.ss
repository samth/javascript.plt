#lang scheme/base

(require (planet "evector.scm" ("soegaard" "evector.plt" 1))
         srfi/13/string
         (except-in scheme/list empty)
         scheme/match
         scheme/math
         "../../debug.ss"
         "../syntax/regexps.ss"
         "exceptions.ss"
         "value.ss"
         "runtime.ss")

;; TODO: abstract out the conveniences like install-properties! into a separate module
(provide install-standard-library! install-standard-library-once! install-properties! reset-global-object! reset-primitive-constructors!)

(define (object-descriptor object)
  (format "[object ~a]" (object-class object)))

(define js:print
  (build-function 1
    (lambda args
      (let ([print1 (lambda (x)
                      (display (any->string x)))])
        (unless (null? args)
          (print1 (car args))
          (for-each (lambda (arg)
                      (display " ")
                      (print1 arg))
                    (cdr args)))
        (newline)))))

(define js:parseInt
  (build-function 2
    (lambda ([string (void)] [radix (void)] . _)
      (let* ([s (string-trim (any->string string) char-whitespace?)]
             [r (any->int32 radix)]
             [sign (if (char=? (string-ref s 0) #\-)
                       (begin (set! s (substring s 1)) -1)
                       1)])
        (if (or (and (not (zero? r)) (< r 2))
                (> r 36))
            +nan.0
            (let ([r (cond
                       [(or (string-prefix? "0x" s) (string-prefix? "0X" s))
                        (set! s (substring s 2))
                        16]
                       [(string-prefix? "0" s)
                        (set! s (substring s 1))
                        8]
                       [(zero? r)
                        10]
                       [else r])])
              (cond
                [(regexp-match (build-integer-regexp r) s)
                 => (lambda (match)
                      (let sum ([factor 1]
                                [total 0]
                                [digits (map char->digit (reverse (string->list (car match))))])
                        (if (null? digits)
                            total
                            (sum (* factor r)
                                 (+ total (* (car digits) factor))
                                 (cdr digits)))))]
                [else +nan.0])))))))

(define (char->digit ch)
  (cond
    [(memv ch (string->list "0123456789"))
     (- (char->integer ch) (char->integer #\0))]
    [(memv ch (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
     (- (char->integer ch) (char->integer #\A))]
    [(memv ch (string->list "abcdefghijklmnopqrstuvwxyz"))
     (- (char->integer ch) (char->integer #\a))]
    [else
     (error 'char->digit "bad digit: ~a" ch)]))

(define (build-integer-regexp base)
  (regexp
   (cond
     [(<= base 10)
      (format "^[0-~a]+" (sub1 base))]
     [(= base 11)
      "^[0-9Aa]+"]
     [else
      (let ([last-char-index (- base 11)])
        (format "^[0-9A-~aa-~a]+"
                (string-ref "ABCDEFGHIJKLMNOPQRSTUVWXYZ" last-char-index)
                (string-ref "abcdefghijklmnopqrstuvwxyz" last-char-index)))])))

;; 15.1.2.3
(define js:parseFloat
  (build-function 1
    (lambda ([arg (void)] . _)
      (let ([s (string-trim (any->string arg)
                            char-whitespace?)])
        (cond
          [(regexp-match rx:float s)
           => (lambda (match)
                ;; TODO: calculate the MV according to 9.3.1
                (string->number (car match)))]
          [else +nan.0])))))

(define js:isNaN
  (build-function 1
    (lambda ([arg (void)] . _)
      (NaN? (any->number arg)))))

(define js:isFinite
  (build-function 1
    (lambda ([arg (void)] . _)
      (let ([x (any->number arg)])
        (and (not (NaN? x))
             (not (infinite? x)))))))

(define js:eval
  (build-function 1
    (lambda args
      (raise-runtime-exception here "indirect eval"))))

(define (tmp:stub arity name)
  (build-function arity
    (lambda args
      (error name "not yet implemented"))))

(define js:decodeURI (tmp:stub 1 'decodeURI))
(define js:decodeURIComponent (tmp:stub 1 'decodeURIComponent))
(define js:encodeURI (tmp:stub 1 'encodeURI))
(define js:encodeURIComponent (tmp:stub 1 'encodeURIComponent))

(define (reset-object! object)
  (set-object-properties! object (object-table))
  (set-ref! eval-ref js:eval))

(define (reset-global-object! global)
  (reset-object! global)
  (reset-object! proto:global) ;; TODO: get this from (object-proto global) instead?
  (reset-object! proto:proto)

  (object-put! proto:proto 'write (build-function 1
                                    (lambda args
                                      (write (object-descriptor (current-this)) (if (null? args) (current-output-port) (car args))))))
  (object-put! proto:proto 'display (build-function 1
                                      (lambda args
                                        (display (object-descriptor (current-this)) (if (null? args) (current-output-port) (car args))))))
  (object-put! proto:global 'toString (build-function 0
                                        (lambda args
                                          (object-descriptor (current-this)))))
  (object-put! proto:global 'hasOwnProperty (build-function 1
                                              (lambda args
                                                (has-own-property? (current-this)
                                                                   (any->property-name (get-arg args 0)))))))

(define (reset-primitive-constructors! global)
  (for ([ctor (list Object Function Array String Boolean Number Trace Name)]
        [proto (list proto:Object proto:Function proto:Array proto:String proto:Boolean proto:Number proto:Trace proto:Name)]
        [name '(Object Function Array String Boolean Number Trace Name)])
    (reset-object! proto)
    (reset-object! ctor)
    ;; 15.2.3.1, 15.3.3.1, 15.4.3.1, 15.5.3.1, 15.6.3.1, 15.7.3.1
    (object-put! ctor 'prototype proto (bit-field DONT-ENUM? DONT-DELETE? READ-ONLY?))
    ;; 15, 15.2.3, 15.3.3, 15.4.3, 15.5.3, 15.6.3, 15.7.3
    (object-put! ctor 'length 1 (bit-field DONT-ENUM? DONT-DELETE? READ-ONLY?))                
    ;; 15?
    (object-put! global name ctor (bit-field DONT-ENUM? DONT-DELETE?))
    ;; 15.2.4.1, 15.3.4.1, 15.4.4.1, 15.5.4.1, 15.6.4.1, 15.7.4.1
    (object-put! proto 'constructor ctor))
  (reset-object! Math)
  (object-put! global 'Math Math (bit-field DONT-ENUM? DONT-DELETE?)))

(define Object-methods
  `(;; 15.2.4.2
    (toString           ,(build-function 0
                           (lambda args
                             (object-descriptor (current-this)))))
    ;; 15.2.4.3
    (toLocaleString     ,(build-function 0
                           (lambda args
                             (let ([toString (object-get (current-this) 'toString (lambda ()
                                                                                    (raise-runtime-type-error here "function" "undefined")))])
                               (apply toString args)))))
    ;; 14.2.4.4
    (valueOf            ,(build-function 0
                           (lambda args
                             (current-this))))
    ;; 15.2.4.5
    (hasOwnProperty     ,(build-function 0
                           (lambda args
                             (has-own-property? (current-this)
                                                (any->property-name (get-arg args 0))))))
    ;; 15.2.4.6
    (isPrototypeOf      ,(build-function 1
                           (lambda args
                             (let ([O (current-this)]
                                   [V (if (null? args) (void) (car args))])
                               (and (object? V)
                                    (let loop ([V (object-proto V)])
                                      (and V (or (eq? O V)
                                                 (loop (object-proto V))))))))))
    ;; 15.2.4.7
    (propertyIsEnumerable ,(build-function 1
                             (lambda args
                               (let ([O (current-this)]
                                     [V (any->property-name (if (null? args) (void) (car args)))])
                                 (and (has-own-property? O V)
                                      (not (bit-flag-set? (object-get-attributes O V) DONT-ENUM?)))))))
    ))

(define Function-methods
  `(;; 15.3.4.2
    (toString           ,(build-function 0
                           (lambda args
                             (unless (descendant-of? (current-this) proto:Function)
                               (raise-runtime-type-error here "function" "object"))
                             ;; TODO: show function source
                             "[object Function]")))
    ;; 15.3.4.3
    (apply              ,(tmp:stub 2 "apply"))
    ;; 15.3.4.4
    (call               ,(tmp:stub 1 "call"))
    ))

;; NOTE: according to Brendan this is a bug in the spec and no one adheres to it
(define (as-if-by-new-Array)
  (new-Array))
;  (let* ([obj (object-get global-object "Array" (lambda ()
;                                                  (raise-runtime-exception here "Array undefined")))] ;; TODO: can't happen? (DONT-DELETE)
;         [ctor (or (and (function? obj) (function-construct obj))
;                   (and (object? obj) (raise-runtime-type-error here "constructor" "object"))
;                   (raise-runtime-type-error here "constructor" "primitive"))])
;    (ctor)))

(define Array-methods
  `((write              ,(build-function 1
                           (lambda args
                             (let ([this (current-this)]
                                   [out (if (null? args) (current-output-port) (car args))])
                               (for ([i (in-range (object-get this 'length (lambda () 0) any->uint32))])
                                 (when (> i 0)
                                   (display "," out))
                                 (object-get this (any->string i) void (lambda (x) (write x out))))))))
    (display            ,(build-function 1
                           (lambda args
                             (let ([this (current-this)]
                                   [out (if (null? args) (current-output-port) (car args))])
                               (for ([i (in-range (object-get this 'length (lambda () 0) any->uint32))])
                                 (when (> i 0)
                                   (display "," out))
                                 (object-get this (any->property-name i) void (lambda (x) (display x out))))))))
    ;; 15.4.4.2
    (toString           ,(build-function 0
                           (lambda args
                             (let ([this (current-this)])
                               (string-join (for/list ([i (in-range (object-get this 'length (lambda () 0) any->uint32))])
                                              (object-get this (any->property-name i) (lambda () "") any->string))
                                            ","
                                            'infix)))))
    ;; 15.4.4.3
    (toLocaleString     ,(build-function 0
                           (lambda args
                             (let ([this (current-this)])
                               (unless (descendant-of? this proto:Array)
                                 (raise-runtime-type-error here "array" "object"))
                               (string-join (for/list ([i (in-range (object-get this 'length (lambda () 0) any->uint32))])
                                              (object-get this
                                                          (any->string i)
                                                          (lambda () "")
                                                          (lambda (v)
                                                            (invoke (any->object v)
                                                                    'toLocaleString
                                                                    '()
                                                                    (lambda (s1 s2)
                                                                      (raise-runtime-type-error here s1 s2))))))
;                                              (let ([v (object-get this (any->string i))])
;                                                (if v
;                                                    (invoke (any->object v)
;                                                            "toLocaleString"
;                                                            '()
;                                                            (lambda (s1 s2)
;                                                              (raise-runtime-type-error here s1 s2)))
;                                                    "")))
                                            ;; TODO: use locale info to choose separator
                                            ","
                                            'infix)))))
    ;; 15.4.4.4
    (concat             ,(build-function 1
                           (lambda args
                             (let ([this (current-this)]
                                   [A (as-if-by-new-Array)])
                               ;; (listof value) * nat -> nat
                               (define (copy-arrays arrays n)
                                 (if (pair? arrays)
                                     (let ([E (car arrays)])
                                       ;; nat * nat * nat -> nat
                                       (define (copy-array Result6 n k)
                                         (if (= k Result6)
                                             (copy-arrays (cdr arrays) n)
                                             (let ([Result8 (any->property-name k)])
                                               (when (has-property? E Result8)
                                                 (object-put! A (any->string n) (object-get E Result8)))
                                               (copy-array Result6 (add1 n) (add1 k)))))
                                       (if (array? E)
                                           (copy-array (object-get E 'length) n 0)
                                           (begin (object-put! A n E)
                                                  (copy-arrays (cdr arrays) (add1 n)))))
                                     n))
                               (object-put! A "length" (copy-arrays (cons this args) 0))
                               A))))
    ;; 15.4.4.5
    (join               ,(build-function 1
                           (lambda args
                             (let ([this (current-this)])
                               (string-join (for/list ([i (in-range (object-get this 'length (lambda () 0) any->uint32))])
                                              (object-get this
                                                          (any->property-name i)
                                                          (lambda () "")
                                                          (lambda (v)
                                                            (if (or (void? v) (null? v)) "" (any->string v)))))
                                            (or (and (pair? args) (any->string (car args)))
                                                ",")
                                            'infix)))))
    ;; 15.4.4.6
    (pop                ,(build-function 0
                           (lambda args
                             (let ([this (current-this)])
                               (let ([len (object-get this 'length (lambda () 0) any->uint32)])
                                 (if (zero? len)
                                     (begin (object-put! this 'length len) (void))
                                     (let* ([key (any->property-name (sub1 len))]
                                            [val (object-get this key void)])
                                       (object-delete! this key)
                                       (object-put! this "length" (sub1 len))
                                       val)))))))
    ;; 15.4.4.7
    (push               ,(build-function 1
                           (lambda args
                             (let ([this (current-this)])
                               (let* ([len (object-get this 'length (lambda () 0) any->uint32)]
                                      [new-len (+ len (length args))])
                                 (for ([arg args]
                                       [n (in-range len new-len)])
                                   (object-put! this (any->property-name n) arg))
                                 (object-put! this 'length new-len)
                                 new-len)))))
    ;; 15.4.4.8
    (reverse            ,(build-function 0
                           (lambda args
                             (let ([this (current-this)])
                               (let* ([len (object-get this 'length (lambda () 0) any->uint32)]
                                      [half (floor (/ len 2))])
                                 (define (loop left)
                                   (if (= left half)
                                       this
                                       (let* ([right (sub1 (- len left))]
                                              [left-key (any->property-name left)]
                                              [right-key (any->property-name right)])
                                         (object-get this
                                                     left-key
                                                     (lambda ()
                                                       (object-get this
                                                                   right-key
                                                                   (lambda ()
                                                                     (object-delete! this left-key)
                                                                     (object-delete! this right-key))
                                                                   (lambda (right-val)
                                                                     (object-delete! this right-key)
                                                                     (object-put! this left-key right-val))))
                                                     (lambda (left-val)
                                                       (object-get this
                                                                   right-key
                                                                   (lambda ()
                                                                     (object-put! this right-key left-val)
                                                                     (object-delete! this right-key))
                                                                   (lambda (right-val)
                                                                     (object-put! this left-key right-val)
                                                                     (object-put! this right-key left-val)))))
;                                         (cond
;                                           [(object-get this left-key)
;                                            => (lambda (left-val)
;                                                 (cond
;                                                   [(object-get this right-key)
;                                                    => (lambda (right-val)
;                                                         (object-put! this left-key right-val)
;                                                         (object-put! this right-key left-val))]
;                                                   [else
;                                                    (object-put! this right-key left-val)
;                                                    (object-delete! this right-key)]))]
;                                           [(object-get this right-key)
;                                            => (lambda (right-val)
;                                                 (object-delete! this right-key)
;                                                 (object-put! this left-key right-val))]
;                                           [else
;                                            (object-delete! this left-key)
;                                            (object-delete! this right-key)])
                                         (loop (add1 left)))))
                                 (loop 0))))))
    ;; 15.4.4.9
    (shift              ,(build-function 0
                           (lambda args
                             (let ([this (current-this)])
                               (let ([len (object-get this 'length (lambda () 0) any->uint32)])
                                 (if (zero? len)
                                     (begin (object-put! this 'length len) (void))
                                     (let ([removed (object-get this "0")])
                                       (define (loop k)
                                         (if (= k len)
                                             (begin (object-delete! this (any->property-name (sub1 len)))
                                                    (object-put! this 'length (sub1 len))
                                                    removed)
                                             (let ([k-key (any->property-name k)]
                                                   [k-1-key (any->property-name (sub1 k))])
                                               (object-get this
                                                           k-key
                                                           (lambda ()
                                                             (object-delete! this k-1-key))
                                                           (lambda (val)
                                                             (object-put! this k-1-key val)))
;                                               (cond
;                                                 [(object-get this k-key)
;                                                  => (lambda (val)
;                                                       (object-put! this k-1-key val))]
;                                                 [else (object-delete! this k-1-key)])
                                               (loop (add1 k)))))
                                       (loop 0))))))))
    ;; 15.4.4.10
    (slice              ,(build-function 2
                           (lambda args
                             (let* ([this (current-this)]
                                    [len (object-get this 'length (lambda () 0) any->uint32)]
                                    [A (as-if-by-new-Array)])
                               (define (any->index x)
                                 (let ([int (any->integer x)])
                                   (if (negative? int)
                                       (max (+ len int) 0)
                                       (min int len))))
                               (let-values ([(start end) (match args
                                                           [(list) (values (any->index (void))
                                                                           (any->index (void)))]
                                                           [(list start) (values (any->index start)
                                                                                 (any->index (void)))]
                                                           [(list start end _ ...) (values (any->index start)
                                                                                           (any->index end))])])
                                 (define (loop k n)
                                   (if (>= k end)
                                       (object-put! A 'length n)
                                       (let ([k-key (any->property-name k)])
                                         (when (has-property? this k-key)
                                           (object-put! A (any->property-name n) (object-get this k-key)))
                                         (loop (add1 k) (add1 n)))))
                                 (loop start 0)
                                 A)))))
    ;; 15.4.4.11
    (sort               ,(build-function 1
                           (lambda args
                             (let ([this (current-this)]
                                   [comparefn (if (null? args) (void) (car args))])
                               (define (SortCompare j k)
                                 (let ([j-key (any->property-name j)]
                                       [k-key (any->property-name k)])
                                   (let ([has-j? (has-property? this j-key)]
                                         [has-k? (has-property? this k-key)])
                                     (cond
                                       [(and (not has-j?) (not has-k?)) 0]
                                       [(not has-j?) 1]
                                       [(not has-k?) -1]
                                       [else
                                        (let ([x (object-get this j-key)]
                                              [y (object-get this k-key)])
                                          (cond
                                            [(and (void? x) (void? y)) 0]
                                            [(void? x) 1]
                                            [(void? y) -1]
                                            [(void? comparefn)
                                             (let ([x-str (any->string x)]
                                                   [y-str (any->string y)])
                                               (cond
                                                 [(string<? x-str y-str) -1]
                                                 [(string<? x-str y-str) 1]
                                                 [else 0]))]
                                            ;; XXX: #%app it
                                            [else (comparefn x y)
;                                                  (call comparefn
;                                                        (list x y)
;                                                        (lambda (s1 s2)
;                                                          (raise-runtime-type-error here s1 s2)))
                                                  ]))]))))
                               (define (quicksort! p r)
                                 ;(fprintf (current-error-port) "quicksort! ~a ~a~n" p r)
                                 (when (< p r)
                                   (let ([q (partition! p r)])
                                     (quicksort! p q)
                                     (quicksort! (add1 q) r))))
                               ;; TODO: randomized-partition!
                               (define (partition! p r)
                                 (define (loop i j)
                                   (let ([j (let drop-top ([j j])
                                              (let ([cmp (any->integer (SortCompare j p))])
                                                (if (or (zero? cmp) (negative? cmp)) j (drop-top (sub1 j)))))]
                                         [i (let raise-bottom ([i i])
                                              (let ([cmp (any->integer (SortCompare i p))])
                                                (if (or (zero? cmp) (positive? cmp)) i (raise-bottom (add1 i)))))])
                                     (if (< i j)
                                         (begin (swap! i j)
                                                (loop i j))
                                         j)))
                                 (loop (sub1 p) (add1 r)))
                               (define (swap! i j)
                                 (let ([i-key (any->property-name i)]
                                       [j-key (any->property-name j)])
                                   (let ([has-i? (has-property? this i-key)]
                                         [has-j? (has-property? this j-key)])
                                     (cond
                                       [(and (not has-i?) (not has-j?)) (void)]
                                       [(not has-i?)
                                        (let ([j-val (object-get this j-key)])
                                          (object-delete! this j-key)
                                          (object-put! this i-key j-val))]
                                       [(not has-j?)
                                        (let ([i-val (object-get this i-key)])
                                          (object-delete! this i-key)
                                          (object-put! this j-key i-val))]
                                       [else
                                        (let ([i-val (object-get this i-key)]
                                              [j-val (object-get this j-key)])
                                          (object-put! this i-key j-val)
                                          (object-put! this j-key i-val))]))))
                               (let ([len (object-get this 'length (lambda () 0) any->uint32)])
                                 (quicksort! 0 (sub1 len))
                                 this)))))
    ;; 15.4.4.12
    (splice             ,(build-function 2
                           (lambda args
                             (let* ([this (current-this)]
                                    [len (object-get this 'length (lambda () 0) any->uint32)]
                                    [A (as-if-by-new-Array)])
                               (let-values ([(start deleteCount items)
                                             (match args
                                               [(list) (values 0 0 null)]
                                               [(list start) (values (any->integer start) 0 null)]
                                               [(list start deleteCount items ...)
                                                (values (any->integer start)
                                                        (any->integer deleteCount)
                                                        items)])])
                                 (let* ([start (if (negative? start) ;; 5
                                                   (max (+ len start) 0)
                                                   (min start len))]
                                        [deleteCount (min (max deleteCount 0) (- len start))]) ;; 6
                                   ;; 7 - 15
                                   (for ([k (in-range 0 deleteCount)])
                                     (let ([key (any->property-name (+ start k))])
                                       (when (has-property? this key)
                                         (object-put! A (any->property-name k) (object-get this key)))))
                                   ;; 16
                                   (object-put! A 'length deleteCount)
                                   (let* ([insertCount (length items)] ;; 17
                                          [newLength (+ (- len deleteCount) insertCount)])
                                     ;; 18 - 19
                                     (cond
                                       [(< insertCount deleteCount)
                                        ;; 20 - 30
                                        (for ([k (in-range start (- len deleteCount))])
                                          (let ([from-key (any->property-name (+ k deleteCount))]
                                                [to-key (any->property-name (+ k insertCount))])
                                            (if (has-property? this from-key)
                                                (object-put! this to-key (object-get this from-key))
                                                (object-delete! this to-key))))
                                        ;; 31 - 36
                                        (for ([k (in-range len newLength)])
                                          (object-delete! this (any->property-name (sub1 k))))]
                                       [(> insertCount deleteCount)
                                        ;; 37 - 47
                                        (for ([k (in-range (- len deleteCount) start -1)])
                                          (let ([from-key (any->property-name (+ k (sub1 deleteCount)))]
                                                [to-key (any->property-name (+ k (sub1 insertCount)))])
                                            (if (has-property? this from-key)
                                                (object-put! this to-key (object-get this from-key))
                                                (object-delete! this to-key))))])
                                     ;; 48 - 52
                                     (for ([item items]
                                           [k (in-range start (+ start insertCount))])
                                       (object-put! this (any->property-name k) item))
                                     ;; 53
                                     (object-put! this 'length newLength)
                                     ;; 54
                                     A)))))))
    ;; 15.4.4.13
    (unshift            ,(build-function 1
                           (lambda args
                             (let* ([this (current-this)]
                                    [len (object-get this 'length (lambda () 0) any->uint32)] ;; 2
                                    [count (length args)]) ;; 3
                               ;; 4 - 14
                               (for ([k (in-range len 0 -1)])
                                 (let ([from-key (any->property-name (sub1 k))] ;; 6
                                       [to-key (any->property-name (+ count (sub1 k)))]) ;; 7
                                   (if (has-property? this from-key)
                                       (object-put! this to-key (object-get this from-key))
                                       (object-delete! this to-key))))
                               ;; 15 - 20
                               (for ([item args]
                                     [k (in-range 0 count)])
                                 (object-put! this (any->property-name k) item))
                               (let ([new-len (+ len count)])
                                 ;; 21
                                 (object-put! this 'length new-len)
                                 new-len)))))
    ))

(define String-statics
  `(;; 15.5.3.2
    (fromCharCode       ,(build-function 1
                           (lambda args
                             (list->string
                              (map (compose integer->char any->uint16) args)))))
    ))

(define (show-wrapper write?)
  (build-function 1
    (lambda args
      (let ([this (current-this)]
            [out (if (null? args) (current-output-port) (car args))]
            [show (if write? write display)])
        (cond
          [(wrapper? this)
           (show (wrapper-value this) out)]
          [(object? this)
           (display "[object Object]" out)]
          [else
           (show this out)])))))

(define write-wrapper (show-wrapper #t))
(define display-wrapper (show-wrapper #f))

(define String-methods
  `((write              ,write-wrapper)
    (display            ,display-wrapper)
    ;; 15.5.4.2
    (toString           ,(build-function 0
                           (lambda args
                             (current-this))))
    ;; 15.5.4.3
    (valueOf            ,(tmp:stub 0 "valueOf"))
    ;; 15.5.4.4
    (charAt             ,(tmp:stub 1 "charAt"))
    ;; 15.5.4.5
    (charCodeAt         ,(tmp:stub 1 "charCodeAt"))
    ;; 15.5.4.6
    (concat             ,(tmp:stub 1 "concat"))
    ;; 15.5.4.7
    (indexOf            ,(tmp:stub 1 "indexOf"))
    ;; 15.5.4.8
    (lastIndexOf        ,(tmp:stub 1 "lastIndexOf"))
    ;; 15.5.4.9
    (localeCompare      ,(tmp:stub 1 "localeCompare"))
    ;; 15.5.4.10
    (match              ,(tmp:stub 1 "match"))
    ;; 15.5.4.11
    (replace            ,(tmp:stub 2 "replace"))
    ;; 15.5.4.12
    (search             ,(tmp:stub 1 "search"))
    ;; 15.5.4.13
    (slice              ,(tmp:stub 2 "slice"))
    ;; 15.5.4.14
    (split              ,(tmp:stub 2 "split"))
    ;; 15.5.4.15
    (substring          ,(tmp:stub 2 "substring"))
    ;; 15.5.4.16
    (toLowerCase        ,(tmp:stub 0 "toLowerCase"))
    ;; 15.5.4.17
    (toLocaleLowerCase  ,(tmp:stub 0 "toLocaleLowerCase"))
    ;; 15.5.4.18
    (toUpperCase        ,(tmp:stub 0 "toUpperCase"))
    ;; 15.5.4.19
    (toLocaleUpperCase  ,(tmp:stub 0 "toLocaleUpperCase"))
    ))

(define Boolean-methods
  `((write              ,write-wrapper)
    (display            ,display-wrapper)
    ;; 15.6.4.2
    (toString           ,(tmp:stub 0 "toString"))
    ;; 15.6.4.3
    (valueOf            ,(tmp:stub 0 "valueOf"))
    ))

(define Number-statics
  `(;; 15.7.3.2
    ;; TODO: fix this
    (MAX_VALUE          ,(void)               ,(bit-field DONT-DELETE? READ-ONLY? DONT-ENUM?))
    ;; 15.7.3.3
    ;; TODO: fix this
    (MIN_VALUE          ,(void)               ,(bit-field DONT-DELETE? READ-ONLY? DONT-ENUM?))
    ;; 15.7.3.4
    (NaN                +nan.0                ,(bit-field DONT-DELETE? READ-ONLY? DONT-ENUM?))
    ;; 15.7.3.5
    (NEGATIVE_INFINITY  -inf.0                ,(bit-field DONT-DELETE? READ-ONLY? DONT-ENUM?))
    ;; 15.7.3.6
    (POSITIVE_INFINITY  +inf.0                ,(bit-field DONT-DELETE? READ-ONLY? DONT-ENUM?))
    ))

(define Number-methods
  `((write              ,write-wrapper)
    (display            ,display-wrapper)
    ;; 15.7.4.2
    (toString           ,(tmp:stub 0 "toString"))
    ;; 15.7.4.3
    (toLocaleString     ,(tmp:stub 0 "toLocaleString"))
    ;; 15.7.4.4
    (valueOf            ,(tmp:stub 0 "valueOf"))
    ;; 15.7.4.5
    (toFixed            ,(tmp:stub 1 "toFixed"))
    ;; 15.7.4.6
    (toExponential      ,(tmp:stub 1 "toExponential"))
    ;; 15.7.4.7
    (toPrecision        ,(tmp:stub 1 "toPrecision"))
    ))

;; 15.8.1
(define Math-static-properties
  `(;; 15.8.1.1
    (E                  ,(exp 1)              ,(bit-field DONT-ENUM? DONT-DELETE? READ-ONLY?))
    ;; 15.8.1.2
    (LN10               ,(log 10)             ,(bit-field DONT-ENUM? DONT-DELETE? READ-ONLY?))
    ;; 15.8.1.3
    (LN2                ,(log 2)              ,(bit-field DONT-ENUM? DONT-DELETE? READ-ONLY?))
    ;; 15.8.1.4
    (LOG2E              ,(/ 1 (log 2))        ,(bit-field DONT-ENUM? DONT-DELETE? READ-ONLY?))
    ;; 15.8.1.5
    (LOG10E             ,(/ 1 (log 10))       ,(bit-field DONT-ENUM? DONT-DELETE? READ-ONLY?))
    ;; 15.8.1.6
    (PI                 ,pi                   ,(bit-field DONT-ENUM? DONT-DELETE? READ-ONLY?))
    ;; 15.8.1.7
    (SQRT1_2            ,(sqrt 1/2)           ,(bit-field DONT-ENUM? DONT-DELETE? READ-ONLY?))
    ;; 15.8.1.8
    (SQRT_2             ,(sqrt 2)             ,(bit-field DONT-ENUM? DONT-DELETE? READ-ONLY?))
    ))

;; 15.8.2
(define Math-static-methods
  `(;; 15.8.2.1
    (abs ,(tmp:stub 1 "abs"))
    ;; 15.8.2.2
    (acos ,(tmp:stub 1 "acos"))
    ;; 15.8.2.3
    (asin ,(tmp:stub 1 "asin"))
    ;; 15.8.2.4
    (atan ,(tmp:stub 1 "atan"))
    ;; 15.8.2.5
    (atan2 ,(tmp:stub 2 "atan2"))
    ;; 15.8.2.6
    (ceil ,(tmp:stub 1 "ceil"))
    ;; 15.8.2.7
    (cos ,(tmp:stub 1 "cos"))
    ;; 15.8.2.8
    (exp ,(tmp:stub 1 "exp"))
    ;; 15.8.2.9
    (floor ,(tmp:stub 1 "floor"))
    ;; 15.8.2.10
    (log ,(tmp:stub 1 "log"))
    ;; 15.8.2.11
    (max ,(tmp:stub 2 "max"))
    ;; 15.8.2.12
    (min ,(tmp:stub 2 "min"))
    ;; 15.8.2.13
    (pow ,(tmp:stub 2 "pow"))
    ;; 15.8.2.14
    (random ,(tmp:stub 0 "random"))
    ;; 15.8.2.15
    (round ,(tmp:stub 1 "round"))
    ;; 15.8.2.16
    (sin ,(tmp:stub 1 "sin"))
    ;; 15.8.2.17
    (sqrt ,(tmp:stub 1 "sqrt"))
    ;; 15.8.2.18
    (tan ,(tmp:stub 1 "tan"))
    ))

(define global-properties
  `(;; 15.1.1.1
    (NaN                +nan.0                ,(bit-field DONT-ENUM? DONT-DELETE?))
    ;; 15.1.1.2
    (Infinity           +inf.0                ,(bit-field DONT-ENUM? DONT-DELETE?))
    ;; 15.1.1.3
    (undefined          ,(void)               ,(bit-field DONT-ENUM? DONT-DELETE?))
    ))

(define eval-ref
  (let ([state js:eval])
    (make-ref (lambda () state)
              (lambda (val)
                (set! state val)
                (original-eval? (eq? val js:eval))
                val)
              (lambda () #t))))

(define global-methods
  `(;; 15.1.2.1
    (eval               ,eval-ref             ,(bit-field DONT-DELETE?))
    ;; 15.1.2.2
    (parseInt           ,js:parseInt)
    ;; 15.1.2.3
    (parseFloat         ,js:parseFloat)
    ;; 15.1.2.4
    (isNaN              ,js:isNaN)
    ;; 15.1.2.5
    (isFinite           ,js:isFinite)
    ;; 15.1.3.1
    (decodeURI          ,js:decodeURI)
    ;; 15.1.3.2
    (decodeURIComponent ,js:decodeURIComponent)
    ;; 15.1.3.3
    (encodeURI          ,js:encodeURI)
    ;; 15.1.3.4
    (encodeURIComponent ,js:encodeURIComponent)
    ))

(define global-custom-properties
  `((it                 ,(void)               ,(bit-field DONT-ENUM? DONT-DELETE?))
    ))

(define global-custom-methods
  `(;; 15
    (print              ,js:print)
    ))

(define Trace-methods
  `((toString           ,(build-function 0
                           (lambda args
                             (object-descriptor (current-this)))))
    (trace              ,(build-function 2
                           (case-lambda
                             [() (void)]
                             [(x) (void)]
                             [(x thunk . rest)
                              (let ([this (current-this)])
                                ;; XXX: check descendant of Trace
                                (unless (wrapper? this)
                                  (raise-runtime-type-error here "Trace" "object"))
                                (unless (procedure? thunk)
                                  (raise-runtime-type-error here "function" "?"))
                                (with-continuation-mark (wrapper-value this) x
                                  (thunk)))])))
    (toArray            ,(build-function 0
                           (lambda args
                             (let ([this (current-this)])
                               ;; XXX: check descendant of Trace
                               (unless (wrapper? this)
                                 (raise-runtime-type-error here "Trace" "object"))
                               (list->array
                                (continuation-mark-set->list
                                 (current-continuation-marks)
                                 (wrapper-value this)))))))
    ))

;; XXX: this toString binding should exist in the primordial prototype object

(define Name-methods
  `((toString           ,(build-function 0
                           (lambda args
                             (object-descriptor (current-this)))))
    ))

(define (install-properties! object properties)
  (for-each (lambda (property)
              (match property
                [(list name value)
                 (object-put! object name value (bit-field DONT-ENUM?))]
                [(list name value attributes)
                 (object-put! object name value attributes)]))
            properties))

(define installation-cache (make-hasheq))

(define (install-standard-library-once! global)
  (hash-ref installation-cache global (lambda ()
                                        (hash-set! installation-cache global #f)
                                        (install-standard-library! global)
                                        #t)))

(define (install-standard-library! global)
  (reset-global-object! global)
  (reset-primitive-constructors! global)

  (install-properties! global         global-properties)
  (install-properties! global         global-methods)
  (install-properties! global         global-custom-properties)
  (install-properties! global         global-custom-methods)

  (install-properties! proto:Object   Object-methods)
  (install-properties! proto:Function Function-methods)
  (install-properties! proto:Array    Array-methods)
  (install-properties! String         String-statics)
  (install-properties! proto:String   String-methods)
  (install-properties! proto:Boolean  Boolean-methods)
  (install-properties! Number         Number-statics)
  (install-properties! proto:Number   Number-methods)
  (install-properties! Math           Math-static-properties)
  (install-properties! Math           Math-static-methods)
  (install-properties! proto:Trace    Trace-methods)
  (install-properties! proto:Name     Name-methods)

  (current-this global)
  (original-eval? #t)
  global)
