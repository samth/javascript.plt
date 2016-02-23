#lang scheme/base

;; TODO: rename this to foreign.ss

(require scheme/class scheme/trait scheme/async-channel scheme/promise scheme/unit)

(provide type-of)

(define (type-of-struct s)
  (and (struct? s)
       (let-values ([(type skipped?) (struct-info s)])
         (and type (let-values ([(name init-field-cnt auto-field-cnt accessor-proc mutator-proc immutable-k-list super-type skipped?)
                                 (struct-type-info type)])
                     name)))))

(define (type-of x)
  (cond
    [(symbol? x) 'symbol]
    [(keyword? x) 'keyword]
    [(pair? x) 'pair]
    [(char? x) 'char]
    [(box? x) 'box]
    [(vector? x) 'vector]
    [(byte? x) 'byte]
    [(bytes? x) 'bytes]
    [(path? x) 'path]
    [(pregexp? x) 'pregexp]
    [(regexp? x) 'regexp]
    [(identifier? x) 'identifier]
    [(syntax? x) 'syntax]
    [(hash? x) 'hash]
    [(channel? x) 'channel]
    [(async-channel? x) 'async-channel]
    [(sequence? x) 'sequence]
    [(semaphore? x) 'semaphore]
    [(thread? x) 'thread]
    [(thread-group? x) 'thread-group]
    [(unit? x) 'unit]
    [(input-port? x) 'input-port]
    [(output-port? x) 'output-port]
    [(parameter? x) 'parameter]
    [(parameterization? x) 'parameterization]
    [(placeholder? x) 'placeholder]
    [(procedure-arity? x) 'procedure-arity]
    [(promise? x) 'promise]
    [(evt? x) 'evt]
    [(continuation? x) 'continuation]
    [(continuation-mark-set? x) 'continuation-mark-set]
    [(continuation-prompt-tag? x) 'continuation-prompt-tag]
    [(object? x) 'object]
    [(class? x) 'class]
    [(trait? x) 'trait]
    [(interface? x) 'interface]
    [(ephemeron? x) 'ephemeron]
    [(weak-box? x) 'weak-box]
    [(will-executor? x) 'will-executor]
    [(inspector? x) 'inspector]
    [(type-of-struct x)]
    ;; Overlaps with continuations and some structs:
    [(procedure? x) 'procedure]
    ;; JavaScript native values:
    [(string? x) 'string]
    [(boolean? x) 'boolean]
    [(null? x) 'null]
    [(number? x) 'number]
    [(void? x) 'void]
    [else '<unknown>]))
