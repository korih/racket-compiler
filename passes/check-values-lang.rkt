#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5
  rackunit)

(provide check-values-lang)

(define (check-types-lang p)
  (void))

(define (check-syntax-lang p)

  (define (check-syntax-lang-func func)
    (match func
      [`(define ,funcName (lambda (,args ...) ,tail))
       #:when (and (symbol? funcName) (andmap symbol? args))
       (check-syntax-lang-tail tail)
       func]
      [else (error 'check-syntax-lang "Invalid function definition")]))

  (define (check-syntax-lang-tail tail)
    (match tail
      [`(let ([,xs ,vs] ...) ,t)
       #:when (andmap symbol? xs)
       (for-each check-syntax-lang-value vs)
       (check-syntax-lang-tail t)
       tail]
      [`(if ,pred ,t1 ,t2)
       (check-syntax-lang-pred pred)
       (check-syntax-lang-tail t1)
       (check-syntax-lang-tail t2)
       tail]
      [`(call ,funcName ,triv ...)
       #:when (symbol? funcName)
       (for-each check-syntax-lang-triv triv)
       tail]
      [value
       (check-syntax-lang-value value)
       tail]
      [else (error 'check-syntax-lang (format "Invalid tail structure: ~a" tail))]))

  (define (check-syntax-lang-value value)
    (match value
      [`(let ([,xs ,vs] ...) ,v)
       #:when (andmap symbol? xs)
       (for-each check-syntax-lang-value vs)
       (check-syntax-lang-value v)
       value]
      [`(if ,pred ,v1 ,v2)
       (check-syntax-lang-pred pred)
       (check-syntax-lang-value v1)
       (check-syntax-lang-value v2)
       value]
      [`(,binop ,triv1 ,triv2)
       #:when (binop? binop)
       (check-syntax-lang-triv triv1)
       (check-syntax-lang-triv triv2)
       value]
      [triv
       (check-syntax-lang-triv triv)
       value]
      [else (error 'check-syntax-lang (format "Invalid value structure: ~a" value))]))

  (define (check-syntax-lang-pred pred)
    (match pred
      ['(true) pred]
      ['(false) pred]
      [`(not ,p) (check-syntax-lang-pred p)]
      [`(let ([,xs ,vs] ...) ,p)
       #:when (andmap symbol? xs)
       (for-each check-syntax-lang-value vs)
       (check-syntax-lang-pred p)
       pred]
      [`(if ,p1 ,p2 ,p3)
       (check-syntax-lang-pred p1)
       (check-syntax-lang-pred p2)
       (check-syntax-lang-pred p3)
       pred]
      [`(,relop ,triv1 ,triv2)
       #:when (relop? relop)
       (check-syntax-lang-triv triv1)
       (check-syntax-lang-triv triv2)
       pred]
      [else (error 'check-syntax-lang (format "Invalid pred structure: ~a" pred))]))

  (define (check-syntax-lang-triv triv)
    (match triv
      [(? integer?) (void)]
      [(? symbol?) (void)]
      [else (error 'check-syntax-lang (format "Invalid triv structure: ~a" triv))]))

  (match p
    [`(module ,funcs ... ,tail)
     (for-each check-syntax-lang-func funcs)
     (check-syntax-lang-tail tail)
     p]
    [else (error 'check-syntax-lang "Invalid module structure")]))

;; any -> values-lang-v5
;; validates that p is a syntactically well-formed, well bound and well
;; typed Values-lang v5: all procedure calls pass the correct number of
;; arguments, and all binop and relop are never used with labels
(define/contract (check-values-lang p)
  (-> any/c values-lang-v5?)
  
  (check-types-lang (check-syntax-lang p)))

(module+ test
  (check-equal? (check-values-lang '(module 5))
                '(module 5))
  (check-equal? (check-values-lang '(module (+ 1 2)))
                '(module (+ 1 2)))
  (check-equal? (check-values-lang '(module (let ([x 1]) x)))
                '(module (let ([x 1]) x)))
  (check-equal? (check-values-lang '(module (let ([x 1] [y 2]) (+ x y))))
                '(module (let ([x 1] [y 2]) (+ x y))))
  (check-equal? (check-values-lang '(module
                                        (define f (lambda (x)
                                                    (let ([y 5])
                                                      (* x y))))
                                      (if (if (let ([x 1] [y 2]) (> x y)) (not (true)) (!= 5 1))
                                          (let ([x (let ([y 1]) (+ y 2))])
                                            (if (< x 10)
                                                (+ x 5)
                                                (* 10 x)))
                                          (call f 10))))
                '(module
                     (define f (lambda (z) (let ((y 5)) (* z y))))
                   (if (if (let ((x 1) (y 2)) (> x y)) (not (true)) (!= 5 1))
                       (let ((x (let ((y 1)) (+ y 2)))) (if (< x 10) (+ x 5) (* 10 x)))
                       (call f 10))))

  (check-equal? (check-values-lang '(module (define x (lambda (y) y)) (call x 1)))
                '(module (define x (lambda (y) y)) (call x 1)))
  (check-equal? (check-values-lang '(module (define f (lambda (x y) (+ x y))) (call f 1 2)))
                '(module (define f (lambda (x y) (+ x y))) (call f 1 2)))
  (check-equal? (check-values-lang '(module (define x (lambda () (< 1 2))) (call x)))
                '(module (define x (lambda () (< 1 2))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (<= 3 3))) (call x)))
                '(module (define x (lambda () (<= 3 3))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (= 5 5))) (call x)))
                '(module (define x (lambda () (= 5 5))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (> 10 9))) (call x)))
                '(module (define x (lambda () (> 10 9))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (>= 2 2))) (call x)))
                '(module (define x (lambda () (>= 2 2))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (!= 4 5))) (call x)))
                '(module (define x (lambda () (!= 4 5))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (true))) (call x)))
                '(module (define x (lambda () (true))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (false))) (call x)))
                '(module (define x (lambda () (false))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (not (true)))) (call x)))
                '(module (define x (lambda () (not (true)))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (let ([y 10]) (< y 20)))) (call x)))
                '(module (define x (lambda () (let ([y 10]) (< y 20)))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (if (true) (false) (true)))) (call x)))
                '(module (define x (lambda () (if (true) (false) (true)))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () 42)) (call x)))
                '(module (define x (lambda () 42)) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (let ([y 10]) y))) (call x)))
                '(module (define x (lambda () (let ([y 10]) y))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (if (< 2 3) 5 10))) (call x)))
                '(module (define x (lambda () (if (< 2 3) 5 10))) (call x)))
  (check-equal? (check-values-lang '(module (define f (lambda (y z) (+ y z))) (define x (lambda () (call f 1 2))) (call x)))
                '(module (define f (lambda (y z) (+ y z))) (define x (lambda () (call f 1 2))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (+ 3 4))) (call x)))
                '(module (define x (lambda () (+ 3 4))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (* 2 5))) (call x)))
                '(module (define x (lambda () (* 2 5))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (let ([y 10]) (+ y 5)))) (call x)))
                '(module (define x (lambda () (let ([y 10]) (+ y 5)))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (if (true) 1 0))) (call x)))
                '(module (define x (lambda () (if (true) 1 0))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () 100)) (call x)))
                '(module (define x (lambda () 100)) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda (y) y)) (call x 1)))
                '(module (define x (lambda (y) y)) (call x 1)))
  (check-equal? (check-values-lang '(module
                                        (define f (lambda () 1))
                                      (define g (lambda () (call f)))
                                      (call g)))
                '(module (define f (lambda () 1)) (define g (lambda () (call f))) (call g)))

  (check-exn exn:fail? (lambda ()
                         (check-values-lang '(module (define x (lambda (y) y)) x))))
  (check-exn exn:fail? (lambda ()
                         (check-values-lang '(module (define x (lambda (y) x)) (call x)))))
  (check-exn exn:fail? (lambda ()
                         (check-values-lang '(module
                                                 (define f (lambda (x) x))
                                                 (define g (lambda (x) x))
                                               (call f g)))))
  (check-exn exn:fail? (lambda ()
                         (check-values-lang '(module
                                                 (define f (lambda (x) x))
                                               (call f)))))
  (check-exn exn:fail? (lambda ()
                         (check-values-lang '(module
                                                 (define f (lambda (x y z) x))
                                               (call f 1 2 3 4 5)))))
  (check-exn exn:fail? (lambda ()
                         (check-values-lang '(module
                                                 (define f (lambda (x) x))
                                                 (define g (lambda (x) x))
                                               (+ f g)))))
  (check-exn exn:fail? (lambda ()
                         (check-values-lang '(module
                                                 (define f (lambda (x) x))
                                                 (define g (lambda (x) x))
                                               (if (> f g)
                                                   1
                                                   2))))))
                                            