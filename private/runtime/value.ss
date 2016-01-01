#lang scheme/base

(require "../evector.rkt"
         scheme/string
         racket/math
         racket/promise
         (except-in scheme/list empty)
         "../syntax/ast-core.ss"
         "../syntax/ast-utils.ss"
         "../syntax/regexps.ss"
         "../syntax/parse.ss"
         "../compiler/context.ss"
         "exceptions.ss"
         "native.ss"
         "object.ss")

(require (rename-in scheme/base [primitive? scheme:primitive?]
                                [number->string scheme:number->string]
                                [string->number scheme:string->number]
                                [print scheme:print]))

(provide current-this)
(provide bit-field make-bit-field bit-flag-set?)
(provide READ-ONLY? DONT-ENUM? DONT-DELETE?)
(provide (struct-out object)
         (struct-out function)
         (struct-out wrapper)
         (struct-out array)
         (struct-out attributed)
         (struct-out ref)
         ;function?
         ;ref?
         set-ref! delete-ref! deref)
(provide set-array-length!)
(provide get-arg get-arg0)
(provide object-table build-object0)
(provide object-get-attributes has-property? has-own-property? has-attribute?
         object-get object-set! object-put! object-delete!
         object-keys object-keys* object-keys-stream descendant-of?)
(provide scope-chain-get scope-chain-set! scope-chain-delete!)
(provide NaN NaN? infinite? numeric?)
(provide object-class object->number object->string object->string/simple object->string/debug
         completion->value completion->string
         any->property-name
         any->boolean any->string any->object any->primitive native->primitive any->string/debug
         any->number native->number any->integer any->int32 any->uint32 any->uint16
         numeric->number)
;(provide true-value?)
(provide invoke #;call)
(provide previous-completion complete! nothing)
(provide build-object build-function build-array build-arguments-object)
(provide list->array)
;(provide make-arguments-object)
(provide global-object proto:global proto:proto proto:Array proto:Function proto:Object proto:String proto:Boolean proto:Number proto:Trace proto:Name)
(provide Array Function Object String Boolean Number Math Trace Name)
(provide current-Function-context)
(provide new-Array)

(define current-Function-context (make-parameter #'value.ss))

(define nothing
  (let ()
    (define-struct nothing ())
    (make-nothing)))

;; ===========================================================================
;; DATA DEFINITIONS AND CONSTRUCTORS
;; ===========================================================================

;; A native value is one of:
;;  - boolean?
;;  - void?
;;  - null?
;;  - number?
;;  - string?
;;  - object?

;; A completion is one of:
;;  - any
;;  - nothing

;; A property is one of:
;;  - property-value
;;  - attributed

;; An attributed is (make-attributed value attributes) where:
;;  - value is a property-value
;;  - attributes is an attributes

;; A property-value is one of:
;;  - ref
;;  - any

;; An attributes is a:
;;  - (bit-field-of READ-ONLY? DONT-ENUM? DONT-DELETE?)

;; A uint32 is an exact-integer in the range [0, 2^32)
;; An int32 is an exact-integer in the range ???
;; A uint16 is an exact-integer in the range [0, 2^16)

;; deref : property-value -> any
(define (deref val)
  (if (ref? val)
      ((ref-get val))
      val))

;; set-ref! : ref any -> any
(define (set-ref! ref val)
  ((ref-set! ref) val))

;; delete-ref! : ref -> any
(define (delete-ref! ref)
  ((ref-delete! ref)))

;(define not-a-function
;  (lambda args
;    (raise-runtime-type-error here "function" "object")))

(define (build-object0 table proto)
  (make-object proto table))
  ;(make-object not-a-function #f proto (object-class proto) table))

(define NaN +nan.0)

(define (NaN? x)
  (or (eqv? x +nan.0)
      (eqv? x -nan.0)))

(define (has-attribute? p a)
  (and (attributed? p)
       (bit-flag-set? (attributed-attributes p) a)))

(define (get-arg args i)
  (cond
    [(null? args) (void)]
    [(zero? i) (car args)]
    [else (get-arg (cdr args) (sub1 i))]))

(define (get-arg0 . args)
  (get-arg args 0))

;; ===========================================================================
;; TYPE CONVERSIONS
;; ===========================================================================

;; 9.1
(define (any->primitive v object->primitive)
  (cond
    [(primitive? v) v]
    [(object? v) (object->primitive v)]
    [else null]))

(define (native->primitive v object->primitive)
  (if (primitive? v)
      v
      (object->primitive v)))

;; 9.3
(define (any->number v)
  (cond
    [(primitive? v) (primitive->number v)]
    [(object? v) (primitive->number (object->number v))]
    [else +nan.0]))

(define (native->number v)
  (if (primitive? v)
      (primitive->number v)
      (primitive->number (object->number v))))

;; primitive->number : primitive -> number
(define (primitive->number v)
  (cond
    [(void? v) +nan.0]
    [(null? v) 0]
    [(eq? v #t) 1]
    [(eq? v #f) 0]
    [(number? v) v]
    [(string? v) (string->number v)]))

;; number-sign : number -> (number -> number)
(define (number-sign x)
  (if (negative? x) - +))

;; 9.4, 9.5, 9.6, 9.7
(define (real->integer v)
  ((number-sign v) (inexact->exact (floor (abs v)))))

;; 9.4
(define (any->integer v)
  (let ([v (any->number v)])
    (cond
      [(NaN? v) 0]
      [(or (zero? v) (infinite? v)) v]
      [else (real->integer v)])))

;; 9.5, 9.6, 9.7
(define (any->finite-integer v)
  (let ([v (any->number v)])
    (if (or (NaN? v) (infinite? v) (zero? v))
        0
        (real->integer v))))

(define 2^32 (expt 2 32))
(define 2^31 (expt 2 31))
(define 2^16 (expt 2 16))
(define 2^32-1 (sub1 (expt 2 32)))

;; 9.5
(define (any->int32 v)
  (let* ([i (any->finite-integer v)]
         [masked (modulo i 2^32)])
    (if (>= masked 2^31)
        (- masked 2^32)
        masked)))

;; 9.6
(define (any->uint32 v)
  (modulo (any->finite-integer v) 2^32))

;; 9.7
(define (any->uint16 v)
  (modulo (any->finite-integer v) 2^16))

;; 9.2
(define false-values `(#f ,(void) () 0 +nan.0 ""))

(define (any->boolean x)
  (not (member x false-values)))

;(define (value->boolean x)
;  (cond
;    [(void? x) #f]
;    [(null? x) #f]
;    [(boolean? x) x]
;    [(number? x) (not (or (zero? x) (NaN? x)))]
;    [(string? x) (string=? x "")]
;    [(object? x) #t]))

;(define (value->string/simple x)
;  (if (object? x)
;      (primitive->string (object->string/simple x))
;      (primitive->string x)))

;(define (value->string x)
;  (if (object? x)
;      (primitive->string (object->string x))
;      (primitive->string x)))


;; a property-name is one of:
;;  - string
;;  - symbol
;;  - name

;; an interned-name is one of:
;;  - symbol
;;  - name

;; any -> property-name
(define (any->property-name x)
  (cond
    [(symbol? x) x]
    [(name? x) x]
    [(object? x) (primitive->string (object->string x))]
    [(primitive? x) (primitive->string x)]
    [else (format "[native ~a]" (type-of x))]))

;; property-name -> interned-name
(define (intern name)
  (if (string? name)
      (string->symbol name)
      name))

;; interned-name -> property-name
(define (unintern name)
  (if (symbol? name)
      (symbol->string name)
      name))

(define (any->string x)
  (cond
    [(object? x) (primitive->string (object->string x))]
    [(primitive? x) (primitive->string x)]
    [else (format "[native ~a]" (type-of x))]))

(define (completion->value x)
  (if (eq? x nothing) (void) x))

(define (completion->string x)
  (if (or (eq? x nothing) (void? x))
      ""
      (any->string x)))

(define (primitive->string p)
  (cond
    [(void? p) "undefined"]
    [(null? p) "null"]
    [(eq? p #t) "true"]
    [(eq? p #f) "false"]
    [(number? p) (number->string p)]
    [(string? p) p]
    [else (error 'primitive->string "unrecognized primitive: ~v" p)]))

(define (numeric? x)
  (or (number? x)
      (and (wrapper? x) (number? (wrapper-value x)))))
;      (and (object? x) (descendant-of? x proto:Number))))

;; numeric->number : numeric -> number
(define (numeric->number x)
  (if (number? x) x (wrapper-value x))) ;(hash-ref (object-properties x) '<<value>>)))

;; number->string : number -> string
(define (number->string x)
  (cond
    [(eqv? x -inf.0) "-Infinity"]
    [(eqv? x +inf.0) "Infinity"]
    [(NaN? x) "NaN"]
    [(zero? x) "0"]
    [(integer? x) (scheme:number->string (inexact->exact x))]
    ;; TODO: follow 9.8.1
    [else (scheme:number->string x)]))

;; primitive? : any -> boolean
(define (primitive? x)
  (or (void? x)
      (null? x)
      (boolean? x)
      (number? x)
      (string? x)))

;; TODO: implement according to 9.3.1
(define (string->number x)
  (scheme:string->number x))

;; 8.6.2.6
;; try : object (listof symbol) (-> primitive) -> primitive
(define (try o method-names)
  (if (null? method-names)
      (raise-runtime-type-error here "object with string representation" "?")
      (let* ([fk (lambda ()
                   (try o (cdr method-names)))]
             [method (object-get o (car method-names) fk)])
        (cond
          [(function? method)
           (let ([result (parameterize ([current-this o])
                           ((function-call method)))])
             (if (primitive? result)
                 (primitive->string result)
                 (try o (cdr method-names))))]
          [(procedure? method)
           (let ([result (method)])
             (if (primitive? result)
                 (primitive->string result)
                 (try o (cdr method-names))))]
;          [(and (object? method) (object-call method))
;           => (lambda (f)
;                (let ([result (parameterize ([current-this o])
;                                (f))])
;                  (if (primitive? result)
;                      (primitive->string result)
;                      (try o (cdr method-names)))))]
          [else (fk)]))))
;          [else (try o (cdr method-names))]))))

(define (object->string/simple o)
  "object")

;; 8.6.2.6, 9.1, 9.8
;; object->string : object -> primitive
(define (object->string o)
  (try o '(toString valueOf)))

;; 8.6.2.6
;; object->number : object -> primitive
(define (object->number o)
  (try o '(valueOf toString)))

;; 9.9
(define (any->object v)
  (cond
    [(void? v) (raise-runtime-type-error here "defined value" "undefined")]
    [(null? v) (raise-runtime-type-error here "non-null value" "null")]
    [(boolean? v) (new-Boolean v)]
    [(number? v) (new-Number v)]
    [(string? v) (new-String v)]
    [(object? v) v]
    [else (raise-runtime-type-error here "native value" "foreign value")]))
;    [else (error 'any->object "unexpected non-value: ~v" v)]))

(define (any->string/debug v)
  (cond
    [(string? v) (string->source-string v)]
    [(object? v) (object->string/debug v)]
    [else (any->string v)]))

(define (object->string/debug o)
  (object->string/debug/immediate o))

(define (object->string/debug/immediate o)
  (string-append "{"
                 (string-join (map (lambda (key)
                                     (format "~a:~a"
                                             key
                                             (any->string/debug (object-get o key))))
                                   (object-keys o))
                              ",")
                 "}"))

;; ===========================================================================
;; ARRAY INDICES
;; ===========================================================================

;; set-array-length! : array any -> any
(define (set-array-length! a x)
  (any->array-index x
                    (lambda (length string?)
                      (set-evector-length! (array-vector a) length))
                    (lambda (name)
                      ;; TODO: range error
                      (raise-runtime-type-error here "array index" name))))

;; array-index? : any -> boolean
(define (array-index? x)
  (and (integer? x)
       ;; 15.4
       (<= 0 x 2^32-1)))

;; A success continuation takes the successfully parsed array index and
;; a string representation of the array index (if the string has been
;; computed yet) and computes a result.

;; A failure continuation takes the string representation of the array
;; index and computes a result.

;; any->array-index : any (uint32 (optional string) -> a) (property-name -> a) -> a
(define (any->array-index x sk fk)
  (cond
    [(array-index? x) (sk (inexact->exact x) #f)]
    [(number? x) (fk (number->string x))]
    [else
     (let ([name (any->property-name x)])
       (cond
         [(and (string? name) (parse-array-index name))
          => (lambda (index)
               (sk index name))]
         [else (fk name)]))]))

;; parse-array-index : string -> (optional uint32)
(define (parse-array-index s)
  (and (regexp-match-exact? rx:integer s)
       (let ([i (string->number s)])
         (and (array-index? i)
              (string=? (number->string i) s)
              (inexact->exact i)))))

;; ===========================================================================
;; OBJECT PROPERTIES
;; ===========================================================================

;; property->value : property -> any
(define (property->value p)
  (cond
    [(and (attributed? p) (ref? (attributed-value p)))
     (deref (attributed-value p))]
    [(attributed? p)
     (attributed-value p)]
    [(ref? p)
     (deref p)]
    [else p]))

;; XXX: avoid repeated calculations here

;; has-property? : object property-name -> boolean
(define (has-property? o key)
  (or (has-own-property? o key)
      (let ([proto (object-proto o)])
        (and proto (has-property? proto key)))))

;; has-own-property? : object property-name -> boolean
(define (has-own-property? o key)
  (or (and (array? o) (array-has-own-property? o key))
      (object-has-own-property? o key)))

;; array-has-own-property? : array property-name -> boolean
(define (array-has-own-property? a key)
  (any->array-index key
                    (lambda (index string?)
                      (let ([vec (array-vector a)])
                        (and (< index (evector-length vec))
                             (not (eq? (evector-ref vec index) nothing)))))
                    ;; XXX: what about the property table?!
                    (lambda (name) #f)))

;; object-has-own-property? : object property-name -> boolean
(define (object-has-own-property? o key)
  (hash-contains? (object-properties o) (intern key)))

;; object-get-attributes : object any -> (optional bit-field)
(define (object-get-attributes o key)
  (object-get/raw o
                  key
                  (lambda (prop)
                    (if (attributed? prop)
                        (attributed-attributes prop)
                        empty-bit-field))
                  (lambda () #f)))
;  (cond
;    [(object-get/raw o key)
;     => (lambda (prop)
;          (if (attributed? prop)
;              (attributed-attributes prop)
;              empty-bit-field))]
;    [else #f]))

;; object-get : object any [-> a] [-> b] -> (union any a b)
(define (object-get o key [fk (lambda () (error 'object-get (format "no such property: ~a" key)))] [sk (lambda (x) x)])
  (object-get/raw o
                  key
                  (lambda (property)
                    (sk (property->value property)))
                  fk))
;  (cond
;    [(object-get/raw o key fk) => property->value]
;    [else (fk)]))

;; object-get/raw : object any (property-value -> b) (property-name -> a) -> (union property-value a b)
(define (object-get/raw o key sk fk)
  (object-get1/raw o
                   key
                   sk
                   (lambda (name)
                     (let ([proto (object-proto o)])
                       (if (not proto)
                           (fk)
                           (object-get/raw proto name sk fk))))))
;                             (and proto (object-get/raw proto string))))))

;; object-get1/raw : object any (property-value -> b) (property-name -> a) -> (union property-value a b)
(define (object-get1/raw o key sk fk)
  (if (array? o)
      (array-get1/raw o key sk fk)
      (object-table-get/raw (object-properties o) key sk fk)))

;; array-get1/raw : array any (property-value -> b) (property-name -> a) -> (union property-value a b)
(define (array-get1/raw a key sk fk)
  (any->array-index key
                    (lambda (index string?)
                      (let ([vec (array-vector a)])
                        (if (< index (evector-length vec))
                            (let ([v (evector-ref vec index)])
                              (if (eq? v nothing)
                                  (fk (or string? (number->string index)))
                                  (sk v)))
                            (fk (or string? (number->string index))))))
                    (lambda (name)
                      (object-table-get/raw (object-properties a)
                                            name
                                            sk
                                            fk))))

;; object-table-get/raw : hash any (property-value -> b) (property-name -> a) -> (union property-value a b)
(define (object-table-get/raw table key sk fk)
  (let ([name (any->property-name key)])
    (let/ec return
      (sk (hash-ref table (intern name) (lambda () (return (fk name))))))))
;  (let* ([s (value->string key)]
;         [v (hash-ref table s (lambda () #f))])
;    (or v (fk s))))

;; XXX: putting a `name' must always make the property DONT-ENUM

;; object-put! : object any any [attributes] -> any
(define (object-put! o key value [attributes empty-bit-field])
  (if (array? o)
      (array-put! o key value attributes)
      (object-table-put! o (any->property-name key) value attributes)))

;; array-put! : array any any -> any
(define (array-put! a key value [attributes empty-bit-field])
  (any->array-index key
                    (lambda (index string?)
                      (array-vector-put! a index value attributes))
                    (lambda (name)
                      (object-table-put! a name value attributes))))

;; put!/permission : (union property nothing) (property -> any) any bit-field -> any
(define (put!/permission previous put! value attributes)
  (unless (has-attribute? previous READ-ONLY?)
    (cond
      [(and (attributed? previous) (ref? (attributed-value previous)))
       (set-ref! (attributed-value previous) value)]
      [(attributed? previous)
       (set-attributed-value! previous value)]
      [(ref? previous)
       (set-ref! previous value)]
      [previous
       (put! value)]
      [(not (empty-bit-field? attributes))
       (put! (make-attributed value attributes))]
      [else
       (put! value)])))

;; array-vector-put! : array uint32 any -> any
(define (array-vector-put! a index value [attributes empty-bit-field])
  (let ([vec (array-vector a)])
    (put!/permission (or (and (< index (evector-length vec))
                              (evector-ref vec index))
                         nothing)
                     (lambda (p)
                       (evector-set! vec index p))
                     value
                     attributes)))

;; object-table-put! : object property-name any -> any
(define (object-table-put! o key value [attributes empty-bit-field])
  (let ([name (intern key)])
    (put!/permission (hash-ref (object-properties o) name (lambda () nothing))
                     (lambda (p)
                       (hash-set! (object-properties o) name p))
                     value
                     attributes)))

;; object-delete! : object any -> boolean
(define (object-delete! o key)
  (if (array? o)
      (array-delete! o key)
      (object-table-delete! (object-properties o) key)))

;; array-delete! : array any -> boolean
(define (array-delete! a key)
  (any->array-index key
                    (lambda (index string?)
                      (array-vector-delete! (array-vector a) index))
                    (lambda (name)
                      (object-table-delete! (object-properties a) name))))

;; object-table-delete! : hash any -> boolean
(define (object-table-delete! table key)
  (let/ec return
    (let ([name (intern (any->property-name key))])
      (let ([p (hash-ref table name (lambda () (return #t)))])
        (and (not (has-attribute? p DONT-DELETE?))
             (begin (hash-remove! table name) #t))))))
;    (cond
;      [(hash-ref table key (lambda () (return #t)))
;       => (lambda (p)
;            (and (not (has-attribute? p DONT-DELETE?))
;                 (begin (hash-remove! table key) #t)))]
;      [else #t])))

;; array-vector-delete! : evector uint32 -> boolean
(define (array-vector-delete! vec i)
  (or (>= i (evector-length vec))
      (let ([p (evector-ref vec i)])
        (or (eq? p nothing)
            (and (not (has-attribute? p DONT-DELETE?))
                 (begin (evector-set! vec i nothing) #t))))))
;  (cond
;    [(and (<= i (evector-length vec))
;          (evector-ref vec i))
;     => (lambda (p)
;          (and (not (has-attribute? p DONT-DELETE?))
;               (begin (evector-set! vec i nothing) #t)))]
;    [else #t]))

;; TODO: check this against the spec for compliance
(define (descendant-of? x y)
  (and (object? x)
       (let ([proto (object-proto x)])
         (or (eq? proto y)
             (and proto (descendant-of? proto y))))))

;; ===========================================================================
;; FOR-IN LOOPS
;; ===========================================================================

(define (hash-contains? t key)
  (let/ec return
    (hash-ref t key (lambda () (return #f)))
    #t))

;; TODO: optionally catch new keys that come into existence? (hard)

(define (object-keys-stream object)
  (let ([current-object object]
        [current-keys (object-keys object)]
        [visited (make-hash)])
    (letrec ([next-key (lambda ()
                         (cond
                           [(pair? current-keys)
                            (let ([key (begin0 (car current-keys)
                                               (set! current-keys (cdr current-keys)))])
                              (if (and (not (hash-contains? visited key))
                                       (has-own-property? object key)
                                       (not (has-attribute? (hash-ref (object-properties object) key)
                                                            DONT-ENUM?)))
                                  (begin (hash-set! visited key #t)
                                         key)
                                  (next-key)))]
                           [(and current-object (null? current-keys))
                            (set! current-object (object-proto current-object))
                            (set! current-keys (and current-object (object-keys current-object)))
                            (next-key)]
                           [else #f]))])
      next-key)))

;; object-keys* : object -> (listof property-name)
(define (object-keys* o)
  (let ([next-key (object-keys-stream o)])
    (let loop ([acc '()])
      (cond
        [(next-key) => (lambda (key)
                         (loop (cons key acc)))]
        [else (reverse acc)]))))

;; object-keys : object -> (listof property-name)
(define (object-keys o)
  (append (if (array? o)
              (build-list (evector-length (array-vector o))
                          number->string)
              null)
          (hash-map (object-properties o)
                    (lambda (key value) (unintern key)))))

;; ===========================================================================
;; BOOLEANS
;; ===========================================================================

;(define (true-value? x)
;  (or (object? x)
;      (and (primitive? x)
;           (not (or (not x)
;                    (void? x)
;                    (null? x)
;                    (and (number? x) (zero? x))
;                    (and (string? x) (string=? x "")))))))

;; ===========================================================================
;; FUNCTIONS
;; ===========================================================================

;; any->callable : any -> object
(define (any->callable v)
  (if (procedure? v) v (any->object v)))

;; invoke : any property-name (listof any) (string string -> <never>) -> any
(define (invoke v name args err)
  (let* ([this (any->object v)]
         [prop (object-get this name (lambda ()
                                       (raise-runtime-type-error here "function" "undefined")))]
         [method (any->callable prop)])
    (parameterize ([current-this this])
      (apply method args))))
;    (parameterize ([current-this this])
;      (call method args err))))

;;; call : value (listof value) (string string -> <never>) -> any
;(define (call v args err)
;  (let* ([o (any->object v)]
;         [proc (object-call o)])
;    (if proc
;        (apply proc args)
;        (err "function" (value->string/simple v)))))

;; ===========================================================================
;; COMPLETIONS
;; ===========================================================================

(define previous-completion (make-parameter #f))

;; complete! : completion -> completion
(define (complete! v)
  (unless (eq? v nothing)
    (previous-completion v))
  (previous-completion))

;; ===========================================================================
;; ARGUMENTS OBJECT
;; ==========================================================================

(define (build-arguments-object func-object aliases args)
  (let ([result (make-hash)]
        [fixed (length aliases)]) ;; TODO: length property
    (for ([i (in-range fixed)]
          [alias aliases])
      (hash-set! result
                 (string->symbol (number->string i))
                 (make-attributed (make-ref (car alias) (cdr alias) (lambda () #f))
                                  (bit-field DONT-DELETE?))))
    ;; TODO: add the rest of the args
    (build-object result)))

;; ===========================================================================
;; CONVENIENCE CONSTRUCTORS
;; ===========================================================================

(define (build-object table)
  (build-object0 table proto:Object))

;; TODO: join nested function objects

(define (build-function arity proc)
  (define f #f)
  (set! f (make-function
           proto:Function
           (object-table
            ;; 13.2, 15.3.5.1
            [length arity (DONT-DELETE? READ-ONLY? DONT-ENUM?)]
            ;; 13.2, 15.3.5.2
            [prototype (build-object (object-table [constructor f (DONT-ENUM?)]))
                             (DONT-DELETE?)])
           ;; 13.2.1
           proc
           ;; 13.2.2
           (lambda args
             (let* ([proto (object-get f 'prototype (lambda () proto:Object))]
                    [new-object (build-object0 '() proto)])
               (parameterize ([current-this new-object])
                 (apply proc args))
               new-object))))
  f)

(define (list->array ls)
  (let* ([len (length ls)]
         [result (make-evector len nothing)])
    (for ([x ls]
          [i (in-range len)])
      (evector-set! result i x))
    (build-array result)))

(define (build-array vec)
  (letrec ([a (make-array proto:Array
                          (object-table
                           [constructor Array (DONT-ENUM? DONT-DELETE?)]
                           [length (lambda ()
                                     (evector-length vec))
                                   (lambda (v)
                                     (set-array-length! a v))
                                   ;; 15.4.5.2
                                   (DONT-ENUM? DONT-DELETE?)])
                          vec)])
    a))

;; 11.2.1
;; object-set! : object any any -> any
(define (object-set! object key value)
  (if (array? object)
      (any->array-index key
                        (lambda (index string?)
                          (evector-set! (array-vector object) index value)
                          value)
                        (lambda (name)
                          ;; XXX: verify this should be name and not key
                          (object-put! object name value)))
      (object-put! object key value)))

(define (string->source-string v)
  (string-append "'"
                 (apply string-append
                        (map (lambda (ch)
                               (case ch
                                 [(#\newline) "\\n"]
                                 [(#\') "\\'"]
                                 [(#\return) "\\r"]
                                 ;; TODO: etc etc
                                 [else (string ch)]))
                             (string->list v)))
                 "'"))

;; scope-chain-get : (listof object) property-name [-> a] [any -> any] -> (union a any)
(define (scope-chain-get scope-chain name [fk (lambda () (error 'scope-chain-get (format "unbound: ~a" name)))] [sk (lambda (x) x)])
  (if (null? scope-chain)
      (fk)
      (object-get (car scope-chain)
                  name
                  (lambda ()
                    (scope-chain-get (cdr scope-chain) name fk sk))
                  sk)))
;  (and (pair? scope-chain)
;       (object-get (car scope-chain) name (lambda ()
;                                            (scope-chain-get (cdr scope-chain) name)))))

;; scope-chain-set! : (listof object) property-name any -> any
(define (scope-chain-set! scope-chain name val)
  (if (or (null? (cdr scope-chain)) (has-property? (car scope-chain) name))
      (begin (object-put! (car scope-chain) name val) val)
      (scope-chain-set! (cdr scope-chain) name val)))

;; scope-chain-delete! : (listof object) property-name -> boolean
(define (scope-chain-delete! scope-chain name)
  (cond
    [(null? scope-chain) #f]
    [(has-property? (car scope-chain) name)
     (object-delete! (car scope-chain) name)]
    [else
     (scope-chain-delete! (cdr scope-chain) name)]))

;; ===========================================================================
;; CORE OBJECTS OF STANDARD LIBRARY
;; ===========================================================================

;; INVARIANT: all these uninitialized property tables are initialized by the reset-* functions

(define proto:global
  (make-object #f #f))

;; TODO: give this guy his own toString and hasOwnProperty (and what else?)
(define proto:proto
  (make-object #f #f))

;; 10.1.5
(define global-object
  (make-wrapper proto:global #f 'DrScheme #f))

(define proto:Object
  (make-object proto:proto #f))
(define proto:Array
  (make-object proto:Object #f))
(define proto:Function
  (make-object proto:proto #f))
(define proto:String
  (make-wrapper proto:proto #f 'String ""))
(define proto:Boolean
  (make-wrapper proto:proto #f 'Boolean #f))
(define proto:Number
  (make-wrapper proto:proto #f 'Number +nan.0))
(define proto:Trace
  (make-wrapper proto:proto #f 'Trace (gensym)))
(define proto:Name
  (make-wrapper proto:proto #f 'Name nothing))

;; 15.2.2.1
(define (new-Object . args)
  (if (or (null? args)
          (null? (car args))
          (void? (car args)))
      (make-object proto:Object (object-table))
      (any->object (car args))))

;; 15.3.2.1
(define (new-Function . args)
  (if (null? args)
      (build-function 0 void)
      (let ([formals (string-join (map any->string (drop-right args 1)) ",")]
            [body (any->string (last args))])
        (with-syntax ([ast (with-handlers ([exn:fail:syntax?
                                            (lambda (exn)
                                              (raise-runtime-exception here (exn-message exn)))])
                             (parse-function-constructor formals body))]
                      [function-begin (datum->syntax (current-Function-context) 'function-begin)])
          (eval #'(function-begin ast))))))

;; 15.4.2.1
(define (new-Array . args)
  (let ([len (length args)])
    (if (= len 1)
        (new-Array1 (car args))
        (let ([v (make-evector len nothing)])
          (for ([arg args]
                [i (in-range len)])
            (evector-set! v i arg))
          (build-array v)))))

;; 15.4.2.2
(define (new-Array1 len)
  (if (numeric? len)
      (let* ([val (numeric->number len)]
             [uint32 (any->uint32 val)])
        (if (= val uint32)
            (let ([a (build-array (make-evector 0 nothing))])
              (set-array-length! a uint32)
              a)
            (let ([v (make-evector 1 nothing)])
              (evector-set! v 0 len)
              (build-array v))))
      (let ([v (make-evector 1 nothing)])
        (evector-set! v 0 len)
        (build-array v))))

;; 15.5.2.1
(define (new-String . args)
  (let* ([value (if (null? args) "" (any->string (car args)))]
         [table (object-table)])
    (make-wrapper proto:String table 'String value)))

;; 15.6.2.1
(define (new-Boolean . args)
  (let* ([value (if (null? args) #f (any->boolean (car args)))]
         [table (object-table)])
    (make-wrapper proto:Boolean table 'Boolean value)))

;; 15.7.2.1
(define (new-Number . args)
  (let* ([value (if (null? args) 0 (any->number (car args)))]
         [table (object-table)])
    (make-wrapper proto:Number table 'Number value)))

(define (new-Trace . args)
  (let ([value (gensym)]
        [table (object-table)])
    (make-wrapper proto:Trace table 'Trace value)))

(define (new-Name . args)
  (let ([value (if (null? args) nothing (car args))])
    (make-wrapper proto:Name (object-table) 'Name value)))

(define Object (make-function
                proto:Function
                #f
                ;; 15.2.1.1
                (lambda args
                  (if (or (null? args)
                          (null? (car args))
                          (void? (car args)))
                      (apply new-Object args)
                      (any->object (car args))))
                new-Object))
;; 15.3.1
(define Function (make-function proto:Function #f new-Function new-Function))
;; 15.4.1
(define Array    (make-function proto:Function #f new-Array new-Array))
;; 15.5.1
(define String   (make-function proto:Function #f (compose any->string get-arg0) new-String))
;; 15.6.1
(define Boolean  (make-function proto:Function #f (compose any->boolean get-arg0) new-Boolean))
;; 15.7.1
(define Number   (make-function proto:Function #f (compose any->number get-arg0) new-Number))
;; 15.8.1
(define Math     (make-wrapper proto:Object #f 'Math #f))

(define Trace    (make-function proto:Function #f new-Trace new-Trace))
(define Name     (make-function proto:Function #f new-Name new-Name))
