#lang scheme/base

(require scheme/list
         "../syntax/ast-core.ss")

(provide (all-defined-out))

;; XXX: this "multi-return" pattern -- is it Dave Fisher's multi-return calculus?

;; XXX: no need for imported, exported in these, right?

(define-struct (FunctionDeclaration/hoisted FunctionDeclaration) (functions variables imports exports))
(define-struct (FunctionExpression/hoisted FunctionExpression) (functions variables imports exports))
(define-struct (BlockStatement/hoisted BlockStatement) (functions variables))

;; A hoisted-element is one of:
;;   - FunctionDeclaration/hoisted
;;   - Identifier
;;   - ImportDeclaration
;;   - ExportDeclaration

;; (union 'top 'function 'block) * hoisted-element
(define-struct hoisted (scope element) #:transparent)


;; =============================================================================
;; HOISTING MONAD
;; =============================================================================

;; A computation of type a in the hoisting monad is called an (H a)

;; An (H a) is (((listof hoisted) a -> b) -> b)

;; return : a -> (H a)
(define (return x)
  (lambda (k)
    (k null x)))

;; >>= : (H a) (a -> (H b)) -> (H b)
(define (>>= m f)
  (lambda (k)
    (m (lambda (h1 x)
         ((f x) (lambda (h2 y)
                  (k (append h1 h2) y)))))))

;; hoist : (union 'top 'function 'block) hoisted-element -> (H #f)
(define (hoist scope element)
  (lambda (k)
    (k (list (make-hoisted scope element)) #f)))

;; capture : (union 'top 'function 'block) (H a) -> (H (list (listof hoisted-element) a))
(define (capture scope m)
  (lambda (k)
    (m (lambda (hs val)
         (let-values ([(captured hs) (partition (lambda (h) (eq? (hoisted-scope h) scope)) hs)])
           (k hs (list captured val)))))))

;; execute : (H a) -> a
;;                    (listof Identifier)
;;                    (listof FunctionDeclaration/hoisted)
;;                    (listof Identifier)
;;                    (listof FunctionDeclaration/hoisted)
;;                    (listof ImportDeclaration)
;;                    (listof ExportDeclaration)
(define (execute m)
  (m (lambda (hs v)
       (let*-values ([(block-hoists hoists) (partition (lambda (h) (eq? (hoisted-scope h) 'block)) hs)])
         (let ([block-hoists (map hoisted-element block-hoists)]
               [elements (map hoisted-element hoists)])
           (let*-values ([(let-vars let-funs) (partition Identifier? block-hoists)]
                         [(vars elements) (partition Identifier? elements)]
                         [(funs elements) (partition FunctionDeclaration/hoisted? elements)]
                         [(imports exports) (partition ImportDeclaration? elements)])
             (values v let-vars let-funs vars funs imports exports)))))))

(define-syntax begin-hoist
  (syntax-rules (<-)
    [(begin-hoist stmt)
     stmt]
    [(begin-hoist (x <- stmt) stmts ...)
     (>>= stmt (lambda (x) (begin-hoist stmts ...)))]
    [(begin-hoist stmt stmts ...)
     (>>= stmt (lambda (_) (begin-hoist stmts ...)))]))

;; =============================================================================
;; GENERIC MONAD OPERATIONS
;; =============================================================================

;; map/m : (a -> (H b)) (listof a) -> (H (listof b))
(define (map/m f ls)
  (let g ([ls ls])
    (if (null? ls)
        (return null)
        (>>= (f (car ls))
             (lambda (x)
               (>>= (g (cdr ls))
                    (lambda (xs)
                      (return (cons x xs)))))))))

;; filter-map/m : (a -> (H (union b #f))) (listof a) -> (H (listof b))
(define (filter-map/m f ls)
  (let g ([ls ls])
    (if (null? ls)
        (return null)
        (>>= (f (car ls))
             (lambda (x)
               (>>= (g (cdr ls))
                    (lambda (xs)
                      (return (if x (cons x xs) xs)))))))))

;; append-map/m : (a -> (H (listof b))) (listof a) -> (H (listof b))
(define (append-map/m f ls)
  (let g ([ls ls])
    (if (null? ls)
        (return null)
        (>>= (f (car ls))
             (lambda (x)
               (>>= (g (cdr ls))
                    (lambda (xs)
                      (return (append x xs)))))))))
