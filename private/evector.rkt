#lang racket
;;; extensible-vector.scm  - 26th may 2007

;;; HISTORY

; 26 mar 2004  -  PLT version
;  2 oct 2004  -  Portable version
;  6 jan 2005  -  PLT PLaneT version
; 26 may 2007  -  Added evector-map, evector-for-each,
;                 evector-copy and evector-copy from
;                 code by Paulo Matos.


;;; EXTENSIBLE VECTOR

; This module provides extensible vectors called evectors.
; Setting LENGTH will increase the vectors length,
; new entries will be filled with FILL. Internally the
; vectors of size 16, 32, 64, ... is used in order to
; ensure amortized time O(1). Note that this implies that
; the space used by an evector is not changed by lowering
; the length.

;;; (make-evector k)
;;; (make-evector k fill)
;;; (make-evector k fill automatic-expansion?)

; Make an evector of length k with the filler fill.
; The argument automatic-expansion? affects the behaviour
; of evector-set!. If automatic-expansion? is true, then
; setting the field of an index larger than then length
; of the evector will automatically increase the length.
; If automatic-expansion? is true, then evector-set! will
; generate in an error in the same situation.

(provide 
 ; basic
 make-evector
 evector?
 evector-ref
 evector-set!
 evector-length
 set-evector-length!
 evector-sub-fill!
 evector-fill
 evector-fill!
 evector-size
 set-evector-fill!
 ; convenience
 evector
 evector->list
 evector->vector
 list->evector
 vector->evector
 evector-push!
 evector-pop!
 ; more
 evector=?
 evector-map
 evector-copy
 evector-for-each)

(require (only-in (lib "etc.ss") opt-lambda))


(define-values (; basic
                make-evector
                evector?
                evector-ref
                evector-set!
                evector-length
                set-evector-length!
                evector-sub-fill!
                evector-fill
                set-evector-fill!
                ; convenience
                evector
                evector->list
                evector->vector
                list->evector
                vector->evector
                evector-fill!
                evector-size
                evector-push!
                evector-pop!
                
                evector=?
                evector-map
                evector-copy
                evector-for-each)
  (let ()
    (define MIN-LENGTH 16)
    (define DEFAULT-FILL '())
    (define DEFAULT-EXPAND #t)
    
    ;;; THE %EVECTOR STRUCTURE
    
    (define-struct %evector (length vector fill automatic-expansion-on-set!?) #:mutable)
    
    ;      (define unique-tag (list 'extensible-vector))
    ;      (define (make-%evector length vektor fill automatic-expansion-on-set!?)
    ;        (vector unique-tag   length vektor fill automatic-expansion-on-set!?))
    ;     
    ;      (define (%evector-tag ev)    (vector-ref ev 0))
    ;      (define (%evector-length ev) (vector-ref ev 1))
    ;      (define (%evector-vector ev) (vector-ref ev 2))
    ;      (define (%evector-fill ev)   (vector-ref ev 3))
    ;      (define (%evector-automatic-expansion-on-set!? ev) (vector-ref ev 4))
    ;     
    ;      (define (set-%evector-length! ev n) (vector-set! ev 1 n))
    ;      (define (set-%evector-vector! ev v) (vector-set! ev 2 v))
    ;      (define (set-%evector-fill!   ev v) (vector-set! ev 3 v))
    ;     
    ;      (define (%evector? o)
    ;        (and (vector? o) (eq? unique-tag (%evector-tag o))))
    ;     
    ;;;
    
    (define make-evector
      (case-lambda 
        [(k)                  (make-evector k DEFAULT-FILL DEFAULT-EXPAND)]
        [(k fill)             (make-evector k fill DEFAULT-EXPAND)]
        [(k fill automatic)   (let ([len (max k MIN-LENGTH)])
                                (make-%evector k (make-vector len fill) fill 
                                               (or (eq? automatic 'automatic-expansion-on-set!)
                                                   (eq? automatic #t))))]))
    
    (define (evector-length v)
      (unless (%evector? v) (error "evector-length: expects arguments of type <evector>; given" v))
      (%evector-length v))
    
    (define (evector-ref v i)
      (unless (%evector? v)                 (error "evector-ref: expects arguments of type <evector>; given" v))
      (unless (< -1 i (%evector-length v))  (error "evector-ref: index out of range; given: " v i))
      (vector-ref (%evector-vector v) i))
    
    (define (evector-set! v i val)
      (unless (%evector? v)                (error "evector-set!: expects arguments of type <evector>; given" v))
      (unless (>= i 0)                     (error "evector-set!: index must be a non-negative integer: " v i))
      (cond
        [(< i (%evector-length v))                  (vector-set! (%evector-vector v) i val)]
        [(%evector-automatic-expansion-on-set!? v)  (begin
                                                      (set-evector-length! v (+ i 1))
                                                      (evector-set! v i val))]
        [else                                        (error "evector-set!: index out of range; given: " v i val)]))
    
    
    ; this version fills elements after length
    #;(define (set-evector-length! v l)
        (let ([max-len (vector-length (%evector-vector v))]
              [old-len (%evector-length v)])
          (cond
            [(<= 0 l max-len) (set-%evector-length! v l)]
            [(> l max-len)    (begin
                                (expand-evector! v l)
                                (set-evector-length! v l))])
          (evector-sub-fill! v old-len l)))
    
    ; this version fills after size
    (define (set-evector-length! v l)
      (let ([max-len (vector-length (%evector-vector v))])
        (cond
          [(<= 0 l max-len) (set-%evector-length! v l)]
          [(> l max-len)    (begin
                              (expand-evector! v l)
                              (let ([old-len (%evector-length v)])
                                (set-evector-length! v l)
                                (evector-sub-fill! v old-len l)))])))
    
    (define evector-sub-fill! 
      (case-lambda
        [(v start end)      (evector-sub-fill! v start end (%evector-fill v))]
        [(v start end fill) (let ([w    (%evector-vector v)]
                                  [fill (%evector-fill v)])
                              (do ([i start (add1 i)])
                                [(= i end) (void)]
                                (vector-set! w i fill)))]))
    
    (define (expand-evector! v l)
      (cond
        [(<= (* 2 l) (%evector-length v))  
         (void)]
        [else                            
         (let* ([new-size   (do ([len (* 2 (vector-length (%evector-vector v))) (* 2 len)])
                              [(<= (* 2 l) len) len])]
                [new-vector (make-vector new-size (%evector-fill v))]
                [old-vector (%evector-vector v)]
                [old-size   (vector-length old-vector)]
                [length     (%evector-length v)])
           (do ([i 0 (add1 i)])
             [(= i length) (void)]
             (vector-set! new-vector i (vector-ref old-vector i)))
           (set-%evector-vector! v new-vector))]))
    
    
    ;;; CONVENIENCE FUNCTIONS SIMILAR TO THE R5RS VECTOR OPERATIONS
    
    (define (evector . os)
      (let ([ev (make-evector (length os) #f #t)])
        (do ([os os (cdr os)]
             [i  0  (+ i 1)])
          [(null? os) ev]
          (evector-set! ev i (car os)))))
    
    (define (evector->list ev)
      (unless (%evector? ev) (error "evector->list: expects arguments of type <evector>; given" ev))
      (let ([len (evector-length ev)])
        (do ([i (- len 1) (- i 1)]
             [l  '()      (cons (evector-ref ev i) l)])
          [(< i 0) l])))
    
    
    (define (list->evector l)
      (unless (pair? l) (error "list->evector: expects arguments of type <list>; given" l))
      (let ([ev (make-evector (length l) '() #t)])
        (do ([i 0 (+ i 1)]
             [l l (cdr l)])
          [(null? l) ev]
          (evector-set! ev i (car l)))))
    
    (define (evector->vector ev)
      (unless (%evector? ev) (error "evector->vector: expects arguments of type <evector>; given" ev))
      (let* ([len   (evector-length ev)]
             [v     (make-vector len)])
        (do ([i 0 (+ i 1)])
          [(= i len) v]
          (vector-set! v i (evector-ref ev i)))))
    
    (define (vector->evector v)
      (unless (vector? v) (error "vector->evector: expects arguments of type <vector>; given" v))
      (let* ([len (vector-length v)]
             [ev (make-evector len '() #t)])
        (do ([i 0 (+ i 1)])
          [(= i len) ev]
          (evector-set! ev i (vector-ref v i)))))
    
    (define evector-fill!
      (case-lambda
        [(ev val)
         (evector-fill! ev val 0 (evector-length ev))]
        [(ev val start)
         (evector-fill! ev val start (evector-length ev))]
        [(ev val start end)
         (let ([max-len (vector-length (%evector-vector ev))])
           (cond
             [(<= 0 end max-len)  (begin
                                    (let ([v (%evector-vector ev)])
                                      (do ([i start (+ i 1)])
                                        [(= i end) (void)]
                                        (vector-set! v i val))))]
             [(> end max-len)     (begin
                                    (expand-evector! ev end)
                                    (set-%evector-length! ev end)
                                    (evector-fill! ev val start end))]))]))
    
    (define (evector-size ev)
      (unless (evector? ev) (error "evector-size: expects arguments of type <vector>; given" ev))
      (vector-length (%evector-vector ev)))
    
    (define (evector-push! ev v)
      (unless (evector? ev) (error "evector-push: expected a value of type <extensible-vector> as first argument; given" ev))
      (let ([l (%evector-length ev)])
        (evector-set! ev l v)
        l))
    
    (define (evector-pop! ev)
      (unless (evector? ev) (error "evector-pop!: expected an <extensible-vector> as argument; given" ev))
      (unless (positive? (%evector-length ev)) (error "evector-pop!: received empty extensible vector"))
      (let* ([l (%evector-length ev)])
        (set-%evector-length! ev (- l 1))
        (vector-ref (%evector-vector ev) (- l 1))))
    
    (define evector=?
      (opt-lambda (ev1 ev2 (= eqv?))
        (unless (and (evector? ev1) (evector? ev2))
          (error "evector=? : expected two <extensible-vectors>s as first arguments, got: " ev1 ev2))
        (and (= (%evector-length ev1) (%evector-length ev2))
             (let ([len (%evector-length ev1)]
                   [v1  (%evector-vector ev1)]
                   [v2  (%evector-vector ev2)])
               (let loop ([i 0])
                 (cond
                   [(>= i len) #t]
                   [(not (= (vector-ref v1 i) (vector-ref v2 i))) #f]
                   [else (loop (+ i 1))]))))))
    
    (define evector-map
      (case-lambda 
        [(f ev)
         (unless (evector? ev) (error "evector-map : expected <extensible-vector>, got: " ev))
         (let* ([len    (%evector-length ev)]
                [v      (%evector-vector ev)]
                [new-ev (make-evector len)]
                [new-v  (%evector-vector new-ev)])
           (do ([i 0 (+ i 1)])
             [(= i len) new-ev]
             (vector-set! new-v i (f (vector-ref v i)))))]
        [(f ev1 ev2)
         (unless (evector? ev1) (error "evector-map : expected <extensible-vector>, got: " ev1))
         (unless (evector? ev1) (error "evector-map : expected <extensible-vector>, got: " ev2))
         (let* ([len    (min (%evector-length ev1)
                             (%evector-length ev2))]
                [v1     (%evector-vector ev1)]
                [v2     (%evector-vector ev2)]
                [new-ev (make-evector len)]
                [new-v  (%evector-vector new-ev)])
           (do ([i 0 (+ i 1)])
             [(= i len) new-ev]
             (vector-set! new-v i (f (vector-ref v1 i) (vector-ref v2 i)))))]
        [(f . evs)
         (unless (andmap evector? evs) (error "evector-map : expected <extensible-vector>s, got: " evs))
         (let* ([len    (apply min (map %evector-length evs))]
                [vs     (map %evector-vector evs)]
                [new-ev (make-evector len)]
                [new-v  (%evector-vector new-ev)])
           (do ([i 0 (+ i 1)])
             [(= i len) new-ev]
             (vector-set! new-v i (apply f (map (lambda (v) (vector-ref v i)) vs)))))]))
    
    (define (evector-copy ev)
      (unless (evector? ev) (error "evector-copy : expected <extensible-vector>, got: " ev))
      (let* ([v      (%evector-vector ev)]
             [l      (vector-length v)]
             [new-v  (make-vector l)])
        (do ([i 0 (+ i 1)])
          [(= i l) 'done]
          (vector-set! new-v i (vector-ref v i)))
        (make-%evector (%evector-length ev)
                       new-v
                       (%evector-fill ev)
                       (%evector-automatic-expansion-on-set!? ev))))
    
    (define evector-for-each
      (case-lambda 
        [(f ev)
         (unless (evector? ev) (error "evector-for-each : expected <extensible-vector>, got: " ev))
         (let ([len    (%evector-length ev)]
               [v      (%evector-vector ev)])
           (do ([i 0 (+ i 1)])
             [(= i len) (void)]
             (f (vector-ref v i))))]
        [(f ev1 ev2)
         (unless (evector? ev1) (error "evector-for-each : expected <extensible-vector>, got: " ev1))
         (unless (evector? ev1) (error "evector-for-each : expected <extensible-vector>, got: " ev2))
         (let ([len    (min (%evector-length ev1)
                            (%evector-length ev2))]
               [v1     (%evector-vector ev1)]
               [v2     (%evector-vector ev2)])
           (do ([i 0 (+ i 1)])
             [(= i len) (void)]
             (f (vector-ref v1 i) (vector-ref v2 i))))]
        [(f . evs)
         (unless (andmap evector? evs) (error "evector-for-each : expected <extensible-vector>s, got: " evs))
         (let ([len    (apply min (map %evector-length evs))]
               [vs     (map %evector-vector evs)])
           (do ([i 0 (+ i 1)])
             [(= i len) (void)]
             (apply f (map (lambda (v) (vector-ref v i)) vs))))]))
    
    
    
    (values 
     ; basic
     make-evector 
     %evector?
     evector-ref
     evector-set!
     %evector-length
     set-evector-length!
     evector-sub-fill!
     %evector-fill
     set-%evector-fill!
     ; convenience
     evector
     evector->list
     evector->vector
     list->evector
     vector->evector
     evector-fill!
     evector-size
     evector-push!
     evector-pop!
     ; more
     evector=?
     evector-map
     evector-copy
     evector-for-each)))
