#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v7
  rackunit)

(provide implement-safe-primops)

;; exprs-unique-lang-v7 -> exprs-unsafe-data-lang-v7
;; compiles p to Exprs-unsafe-data-lang v7 by implementing safe primitive
;; operations by inserting procedure definitions for each primitive operation
;; which perform dynamic tag checking, to ensure type safety
(define/contract (implement-safe-primops p)
  (-> exprs-unique-lang-v7? exprs-unsafe-data-lang-v7?)

  ;; func is `(define ,label (lambda (,alocs ...) ,value))
  ;; interp. a function definition

  ;; binop-unsafe-map is (Map-of exprs-unique-lang-v7.binop (list exprs-unsafe-data-lang-v7.binop Natural))
  ;; interp. maps the exprs-unique-lang-v7.binop with its unsafe binop and error code
  (define binop-unsafe-map
    (hash '*   (cons 'unsafe-fx*  1)
          '+   (cons 'unsafe-fx+  2)
          '-   (cons 'unsafe-fx-  3)
          '<   (cons 'unsafe-fx<  4)
          '<=  (cons 'unsafe-fx<= 5)
          '>   (cons 'unsafe-fx>  6)
          '>=  (cons 'unsafe-fx>= 7)))

  ;; new-funcs is (Map-of exprs-unique-lang-v7.binop (list label func))
  ;; interp. keeps track of new funcs that were created by compiling binop or unops
  (define new-funcs (make-hash))

  ;; func -> func
  (define (implement-safe-primops-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,value))
       `(define ,label (lambda (,@alocs) ,(implement-safe-primops-value value)))]))

  ;; exprs-unique-lang-v7.value -> exprs-unsafe-data-lang-v7.value
  (define (implement-safe-primops-value value)
    (match value
      [`(call ,vs ...)
       `(call ,@(map implement-safe-primops-value vs))]
      [`(let ([,alocs ,vs] ...) ,v)
       (define bindings
         (map (lambda (aloc val)
                (list aloc (implement-safe-primops-value val)))
              alocs vs))
       `(let (,@bindings) ,(implement-safe-primops-value v))]
      [`(if ,v1 ,v2 ,v3)
       `(if ,(implement-safe-primops-value v1)
            ,(implement-safe-primops-value v2)
            ,(implement-safe-primops-value v3))]
      [triv (implement-safe-primops-triv triv)]))

  ;; exprs-unique-lang-v7.triv -> exprs-unsafe-data-lang-v7.triv
  (define (implement-safe-primops-triv triv)
    (match triv
      [prim-f
       #:when (or (binop? prim-f) (unop? prim-f))
       (if (hash-has-key? new-funcs prim-f)
           (first (hash-ref new-funcs prim-f))
           (cond
             [(binop? prim-f)
              (define-values (func label) (implement-safe-primops-binop prim-f))
              (hash-set! new-funcs prim-f (list label func))
              label]
             [(unop? prim-f)
              (define-values (func label) (implement-safe-primops-unop prim-f))
              (hash-set! new-funcs prim-f (list label func))
              label]))]
      ;; Wildcard collapse case used because
      [_ triv]))

  ;; exprs-unique-lang-v7.binop -> func label
  (define (implement-safe-primops-binop binop)
    (define binop-label (fresh-label binop))
    (define arg1 (fresh))
    (define arg2 (fresh))
    (match binop
      ['eq?
       (values `(define ,binop-label (lambda (,arg1 ,arg2) (,binop ,arg1 ,arg2)))
               binop-label)]
      ;; Wildcard collapse case used because all other binops follow the same
      ;; structure of performing fixnum? checks on both arguments and applying
      ;; the corresponding unsafe operation with the associated error code.
      [_
       (define unsafe-binop (hash-ref binop-unsafe-map binop))
       (values `(define ,binop-label (lambda (,arg1 ,arg2)
                                       (if (fixnum? ,arg2)
                                           (if (fixnum? ,arg1) (,(car unsafe-binop) ,arg1 ,arg2) (error ,(cdr unsafe-binop)))
                                           (error ,(cdr unsafe-binop)))))
               binop-label)]))

  ;; exprs-unique-lang-v7.unop -> func label
  (define (implement-safe-primops-unop unop)
    (define unop-label (fresh-label unop))
    (define arg (fresh))
    (values `(define ,unop-label (lambda (,arg) (,unop ,arg))) unop-label))

  (match p
    [`(module ,funcs ... ,value)
     (define funcs^ (map implement-safe-primops-func funcs))
     (define value^ (implement-safe-primops-value value))
     `(module ,@(map second (hash-values new-funcs)) ,@funcs^ ,value^)]))

(module+ test
  (check-equal? (implement-safe-primops '(module (call + 1 2)))
                '(module
                     (define L.+.1
                       (lambda (tmp.1 tmp.2)
                         (if (fixnum? tmp.2)
                             (if (fixnum? tmp.1) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                             (error 2))))
                   (call L.+.1 1 2)))
  (check-equal? (implement-safe-primops '(module (call * 1 2)))
                '(module
                     (define L.*.2
                       (lambda (tmp.3 tmp.4)
                         (if (fixnum? tmp.4)
                             (if (fixnum? tmp.3) (unsafe-fx* tmp.3 tmp.4) (error 1))
                             (error 1))))
                   (call L.*.2 1 2)))
  (check-equal? (implement-safe-primops '(module (call - 1 2)))
                '(module
                     (define L.-.3
                       (lambda (tmp.5 tmp.6)
                         (if (fixnum? tmp.6)
                             (if (fixnum? tmp.5) (unsafe-fx- tmp.5 tmp.6) (error 3))
                             (error 3))))
                   (call L.-.3 1 2)))
  (check-equal? (implement-safe-primops '(module (call eq? 1 2)))
                '(module
                     (define L.eq?.4 (lambda (tmp.7 tmp.8) (eq? tmp.7 tmp.8)))
                   (call L.eq?.4 1 2)))
  (check-equal? (implement-safe-primops '(module (call > 1 2)))
                '(module
                     (define L.>.5
                       (lambda (tmp.9 tmp.10)
                         (if (fixnum? tmp.10)
                             (if (fixnum? tmp.9) (unsafe-fx> tmp.9 tmp.10) (error 6))
                             (error 6))))
                   (call L.>.5 1 2)))
  (check-equal? (implement-safe-primops '(module (call >= 1 2)))
                '(module
                     (define L.>=.6
                       (lambda (tmp.11 tmp.12)
                         (if (fixnum? tmp.12)
                             (if (fixnum? tmp.11) (unsafe-fx>= tmp.11 tmp.12) (error 7))
                             (error 7))))
                   (call L.>=.6 1 2)))
  (check-equal? (implement-safe-primops '(module (call < 1 2)))
                '(module
                     (define L.<.7
                       (lambda (tmp.13 tmp.14)
                         (if (fixnum? tmp.14)
                             (if (fixnum? tmp.13) (unsafe-fx< tmp.13 tmp.14) (error 4))
                             (error 4))))
                   (call L.<.7 1 2)))
  (check-equal? (implement-safe-primops '(module (call <= 1 2)))
                '(module
                     (define L.<=.8
                       (lambda (tmp.15 tmp.16)
                         (if (fixnum? tmp.16)
                             (if (fixnum? tmp.15) (unsafe-fx<= tmp.15 tmp.16) (error 5))
                             (error 5))))
                   (call L.<=.8 1 2)))
  (check-equal? (implement-safe-primops '(module (call fixnum? 1)))
                '(module
                     (define L.fixnum?.9 (lambda (tmp.17) (fixnum? tmp.17)))
                   (call L.fixnum?.9 1)))
  (check-equal? (implement-safe-primops '(module
                                             (define L.odd?.4
                                               (lambda (x.45)
                                                 (if (call eq? x.45 0)
                                                     0
                                                     (let ((y.46 (call + x.45 -1))) (call L.even?.5 y.46)))))
                                           (define L.even?.5
                                             (lambda (x.47)
                                               (if (call eq? x.47 0)
                                                   1
                                                   (let ((y.48 (call + x.47 -1))) (call L.odd?.4 y.48)))))
                                           (call L.even?.5 5)))
                '(module
                     (define L.+.11
                       (lambda (tmp.20 tmp.21)
                         (if (fixnum? tmp.21)
                             (if (fixnum? tmp.20) (unsafe-fx+ tmp.20 tmp.21) (error 2))
                             (error 2))))
                   (define L.eq?.10 (lambda (tmp.18 tmp.19) (eq? tmp.18 tmp.19)))
                   (define L.odd?.4
                     (lambda (x.45)
                       (if (call L.eq?.10 x.45 0)
                           0
                           (let ((y.46 (call L.+.11 x.45 -1))) (call L.even?.5 y.46)))))
                   (define L.even?.5
                     (lambda (x.47)
                       (if (call L.eq?.10 x.47 0)
                           1
                           (let ((y.48 (call L.+.11 x.47 -1))) (call L.odd?.4 y.48)))))
                   (call L.even?.5 5))))