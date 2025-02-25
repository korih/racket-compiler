#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4
  rackunit)

(provide uniquify)

;; values-lang-v4 -> values-unique-lang-v4
;; converts all lexical identifiers to abstract locations
(define/contract (uniquify p)
  (-> values-lang-v4? values-unique-lang-v4?)

  ;; values-lang-v4.tail (Env-of aloc) -> values-lang-v4.tail
  (define (uniquify-tail tail env)
    (match tail
      [`(let ([,x ,v] ...) ,body)
       (define unique-names (map fresh x))
       (define new-env (extend-env* env x unique-names))
       (define unique-binds
         (map (lambda (uname value)
                (list uname (uniquify-value value env)))
              unique-names v))
       `(let (,@unique-binds) ,(uniquify-tail body new-env))]
      [`(if ,pred ,t-tail ,f-tail)
       `(if ,(uniquify-pred pred env)
            ,(uniquify-tail t-tail env)
            ,(uniquify-tail f-tail env))]
      [v (uniquify-value v env)]))

  ;; values-lang-v4.tail (Env-of aloc) -> values-lang-v4.tail
  ;; converts all lexical identifiers in the predicate to abstract locations
  (define (uniquify-pred pred env)
    (match pred
      ['(true) '(true)]
      ['(false) '(false)]
      [`(not ,pred) `(not ,(uniquify-pred pred env))]
      [`(let ([,xs ,vs] ...) ,pred)
       (define-values (bindings new-env)
         (for/fold ([bindings-acc empty] [env-acc env])
                   ([x xs] [v vs])
           (define loc (fresh x))
           (values (cons (list loc (uniquify-value v env-acc)) bindings-acc) (extend-env env-acc x loc))))
       (displayln bindings)
       `(let (,@bindings) ,(uniquify-pred pred new-env))]
      [`(if ,pred ,t-pred ,f-pred) `(if ,(uniquify-pred pred env)
                                        ,(uniquify-pred t-pred env)
                                        ,(uniquify-pred f-pred env))]
      [`(,relop ,op1 ,op2) `(,relop ,(uniquify-triv op1 env) ,(uniquify-triv op2 env))]))

  ;; values-lang-v4.value (Env-of aloc) -> values-lang-v4.value
  (define (uniquify-value value env)
    (match value
      [`(let ([,x ,v] ...) ,body)
       (define unique-names (map fresh x))
       (define new-env (extend-env* env x unique-names))
       (define unique-binds
         (map (lambda (uname value)
                (list uname (uniquify-value value env)))
              unique-names v))
       `(let (,@unique-binds) ,(uniquify-value body new-env))]
      [`(if ,pred ,t-value ,f-value)
       `(if ,(uniquify-pred pred env)
            ,(uniquify-value t-value env)
            ,(uniquify-value f-value env))]
      [`(,binop ,triv1 ,triv2)
       `(,binop ,(uniquify-triv triv1 env) ,(uniquify-triv triv2 env))]
      [triv (uniquify-triv triv env)]))

  ;; values-lang-v4.triv (Env-of aloc) -> values-lang-v4.triv
  (define (uniquify-triv triv env)
    (match triv
      [int64 #:when (int64? int64) int64]
      [x #:when (name? x) (lookup-env env x)]))

  (match p
    [`(module ,tail)
     `(module ,(uniquify-tail tail empty-env))]))

(module+ test
  (check-equal? (uniquify '(module (+ 2 2)))
                '(module (+ 2 2)))
  (check-equal? (uniquify '(module (let ([x 5]) x)))
                '(module (let ((x.1 5)) x.1)))
  (check-equal? (uniquify '(module (let ([x 5]) (+ x 1))))
                '(module (let ((x.2 5)) (+ x.2 1))))
  (check-equal? (uniquify '(module (let ([x 5] [y 10]) (+ x y))))
                '(module (let ((x.3 5) (y.4 10)) (+ x.3 y.4))))
  (check-equal? (uniquify '(module (let ([x 5] [y (+ 3 2)]) (* x y))))
                '(module (let ((x.5 5) (y.6 (+ 3 2))) (* x.5 y.6))))

  ;; Nested let bindings
  (check-equal? (uniquify '(module (let ([x 5]) (let ([y x]) (+ y 1)))))
                '(module (let ((x.7 5)) (let ((y.8 x.7)) (+ y.8 1)))))

  ;; Multiple nested let expressions
  (check-equal? (uniquify '(module (let ([x 5]) (let ([y x]) (let ([z y]) (+ z 2))))))
                '(module (let ((x.9 5)) (let ((y.10 x.9)) (let ((z.11 y.10)) (+ z.11 2))))))

  ;; Variable shadowing (inner `x` should be renamed)
  (check-equal? (uniquify '(module (let ([x 5]) (let ([x 6]) x))))
                '(module (let ((x.12 5)) (let ((x.13 6)) x.13))))

  ;; Multiple shadowing variables
  (check-equal? (uniquify '(module (let ([x 5]) (let ([x (+ x 1)]) (let ([x (+ x 2)]) x)))))
                '(module (let ((x.14 5)) (let ((x.15 (+ x.14 1))) (let ((x.16 (+ x.15 2))) x.16)))))

  ;; Unused variable
  (check-equal? (uniquify '(module (let ([x 5]) 42)))
                '(module (let ((x.17 5)) 42)))

  ;; Large numbers and edge values
  (check-equal? (uniquify `(module (let ([x ,(- (expt 2 63))] [y ,(- (expt 2 63) 1)]) (+ x y))))
                `(module (let ((x.18 ,(- (expt 2 63))) (y.19 ,(- (expt 2 63) 1))) (+ x.18 y.19))))

  ;; Shadowing with external references
  (check-equal? (uniquify '(module (let ([x 5]) (let ([y x]) (let ([x y]) (+ x y))))))
                '(module (let ((x.20 5)) (let ((y.21 x.20)) (let ((x.22 y.21)) (+ x.22 y.21))))))

  ;; Nested variable dependencies
  (check-equal? (uniquify '(module (let ([a 1]) (let ([b (+ a 1)]) (let ([c (+ b 1)]) (+ c 2))))))
                '(module (let ((a.23 1)) (let ((b.24 (+ a.23 1))) (let ((c.25 (+ b.24 1))) (+ c.25 2))))))

  ;; Reusing names inside different let blocks
  (check-equal? (uniquify '(module (let ([x 1]) (let ([y (+ x 2)]) (let ([x (+ y 3)]) (+ x y))))))
                '(module (let ((x.26 1)) (let ((y.27 (+ x.26 2))) (let ((x.28 (+ y.27 3))) (+ x.28 y.27))))))

  ;; Unbound variable (should fail)
  (check-exn exn:fail? (λ () (uniquify '(module (let ([x y]) x)))))

  ;; predicate in tail position
  (check-equal? (uniquify '(module (if (true) (let ([x 5]) x) -1)))
                '(module (if (true) (let ([x.30 5]) x.30) -1)))

  ;; predicate in value position
  (check-equal? (uniquify '(module (let ([y (if (if (true) (false) (true))
                                                (* 3 3)
                                                0)])
                                     (+ y -1))))
                '(module (let ([y.31 (if (if (true) (false) (true))
                                         (* 3 3)
                                         0)])
                           (+ y.31 -1)))))
