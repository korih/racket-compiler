#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v6
  rackunit)

(provide uniquify)

;; values-lang-v6 -> values-unique-lang-v6
;; compiles p to Values-unique-lang v6 by resolving top-level lexical
;; identifiers into unique labels, and all other lexical identifiers into
;; unique abstract locations
(define/contract (uniquify p)
  (-> values-lang-v6? values-unique-lang-v6?)

  ;; func is `(define ,label (lambda (,alocs ...) ,tail))
  ;; interp. a function definition

  ;; (List-of func) -> (Env-of values-unique-lang-v6.triv)
  ;; interp. creates an environment with all the unique function labels
  (define (initialize-env funcs)
    (for/fold ([env empty-env])
              ([fun funcs])
      (match fun
        [`(define ,funcName (lambda (,args ...) ,tail))
         (define unique-label (fresh-label funcName))
         (define env^ (extend-env env funcName unique-label))
         env^])))

  ;; (List-of func) (Env-of values-unique-lang-v6.triv) -> (values (List-of func) (Env-of values-unique-lang-v6.triv))
  ;; interp. processes each function definition by assigning lexical identifiers with unique labels and abstract locations 
  (define (process-functions funcs env)
    (for/fold ([updated-funcs '()]
               [updated-env env])
              ([func funcs])
      (define-values (updated-func new-env) (uniquify-func func updated-env))
      (values (cons updated-func updated-funcs) new-env)))

  ;; func (Env-of values-unique-lang-v6.triv) -> (values func (Env-of values-unique-lang-v6.triv))
  (define (uniquify-func func env)
    (match func
      [`(define ,funcName (lambda (,args ...) ,tail))
       (define unique-label (lookup-env env funcName))
       (define unique-args (map fresh args))
       (define new-env (extend-env* env args unique-args))
       (values `(define ,unique-label (lambda (,@unique-args) ,(uniquify-tail tail new-env)))
               env)]))

  ;; values-lang-v6.tail (Env-of values-unique-lang-v6.triv) -> values-unique-lang-v6.tail
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
      [`(call ,x ,trivs ...)
       `(call ,(lookup-env env x) ,@(map (lambda (triv) (uniquify-triv triv env)) trivs))]
      [v (uniquify-value v env)]))

  ;; values-lang-v6.tail (Env-of values-unique-lang-v6.triv) -> values-unique-lang-v6.tail
  (define (uniquify-pred pred env)
    (match pred
      ['(true) '(true)]
      ['(false) '(false)]
      [`(not ,pred) `(not ,(uniquify-pred pred env))]
      [`(let ([,xs ,vs] ...) ,pred)
       (define unique-names (map fresh xs))
       (define new-env (extend-env* env xs unique-names))
       (define unique-binds
         (map (lambda (uname value)
                (list uname (uniquify-value value env)))
              unique-names vs))
       `(let (,@unique-binds) ,(uniquify-pred pred new-env))]
      [`(if ,pred ,t-pred ,f-pred)
       `(if ,(uniquify-pred pred env)
            ,(uniquify-pred t-pred env)
            ,(uniquify-pred f-pred env))]
      [`(,relop ,triv1 ,triv2)
       `(,relop ,(uniquify-triv triv1 env) ,(uniquify-triv triv2 env))]))

  ;; values-lang-v6.value (Env-of values-unique-lang-v6.triv) -> values-unique-lang-v6.value
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
      [`(call ,x ,trivs ...)
       `(call ,(lookup-env env x) ,@(map (lambda (triv) (uniquify-triv triv env)) trivs))]
      [`(,binop ,triv1 ,triv2)
       `(,binop ,(uniquify-triv triv1 env) ,(uniquify-triv triv2 env))]
      [triv (uniquify-triv triv env)]))

  ;; values-lang-v6.triv (Env-of values-unique-lang-v6.triv) -> values-unique-lang-v6.triv
  (define (uniquify-triv triv env)
    (match triv
      [int64 #:when (int64? int64) int64]
      [x #:when (name? x) (lookup-env env x)]))

  (match p
    [`(module ,funcs ... ,tail)
     (define defined-funs (initialize-env funcs))
     (define-values (updated-funcs updated-env) (process-functions funcs defined-funs))
     `(module ,@(reverse updated-funcs) ,(uniquify-tail tail updated-env))]))

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
                           (+ y.31 -1))))
  (check-equal? (uniquify '(module
                               (define f (lambda (x y) (+ x y)))
                             (define x (lambda (z) (let ([x 1])
                                                     (+ x z))))
                             (if (true)
                                 (call f 1 2)
                                 (call x 1))))
                '(module
                     (define L.f.1 (lambda (x.32 y.33) (+ x.32 y.33)))
                   (define L.x.2 (lambda (z.34) (let ((x.35 1)) (+ x.35 z.34))))
                   (if (true) (call L.f.1 1 2) (call L.x.2 1))))
  (check-equal? (uniquify '(module
                               (define odd? (lambda (x) (if (= x 0) 0 (let ((y (+ x -1))) (call even? y)))))
                             (define even? (lambda (x) (if (= x 0) 1 (let ((y (+ x -1))) (call odd? y)))))
                             (call even? 5)))
                '(module
                     (define L.odd?.3
                       (lambda (x.36)
                         (if (= x.36 0) 0 (let ((y.37 (+ x.36 -1))) (call L.even?.4 y.37)))))
                   (define L.even?.4
                     (lambda (x.38)
                       (if (= x.38 0) 1 (let ((y.39 (+ x.38 -1))) (call L.odd?.3 y.39)))))
                   (call L.even?.4 5)))

  (check-equal? (uniquify '(module
                               (define f (lambda (x y) (+ x y)))
                             (define x (lambda (z) (let ([x 1])
                                                     (+ x z))))
                             (let ([x (call f 1 2)])
                               (let ([y (call x x)])
                                 y))))
                '(module
                     (define L.f.5 (lambda (x.40 y.41) (+ x.40 y.41)))
                   (define L.x.6 (lambda (z.42) (let ((x.43 1)) (+ x.43 z.42))))
                   (let ((x.44 (call L.f.5 1 2))) (let ((y.45 (call x.44 x.44))) y.45))))
  (check-equal? (uniquify '(module
                               (define f (lambda (x y) (+ x y)))
                             (define x (lambda (z) (let ([x 1])
                                                     (+ x z))))
                             (let ([a (call f 1 2)])
                               (let ([y (call x a)])
                                 y))))
                '(module
                     (define L.f.7 (lambda (x.46 y.47) (+ x.46 y.47)))
                   (define L.x.8 (lambda (z.48) (let ((x.49 1)) (+ x.49 z.48))))
                   (let ((a.50 (call L.f.7 1 2))) (let ((y.51 (call L.x.8 a.50))) y.51)))))