#lang racket

(require
  cpsc411/langs/v6
  rackunit)

(provide sequentialize-let)

;; values-unique-lang-v6 -> imp-mf-lang-v6
;; compiles p to imp-mf-lang-v6 by picking a particular order to implement
;; let expressions using set!
(define/contract (sequentialize-let p)
  (-> values-unique-lang-v6? imp-mf-lang-v6?)

  ;; func is `(define ,label (lambda (,alocs ...) ,tail))
  ;; interp. a function definition

  ;; func -> func
  (define (sequentialize-let-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,tail))
       `(define ,label (lambda (,@alocs) ,(sequentialize-let-tail tail)))]))

  ;; values-unique-lang-v6.tail -> imp-mf-lang-v6.tail
  (define (sequentialize-let-tail tail)
    (match tail
      [`(let ([,xs ,vs] ...) ,tail)
       (define sequentialize-let-values (for/list ([x xs] [v vs])
                                          `(set! ,x ,(sequentialize-let-value v))))
       `(begin ,@sequentialize-let-values ,(sequentialize-let-tail tail))]
      [`(if ,p ,t1 ,t2)
       (define p^ (sequentialize-let-pred p))
       (define t1^ (sequentialize-let-tail t1))
       (define t2^ (sequentialize-let-tail t2))
       `(if ,p^ ,t1^ ,t2^)]
      [`(call ,triv ,opand ...) tail]
      [value (sequentialize-let-value value)]))

  ;; values-unique-lang-v6.value -> imp-mf-lang-v6.value
  (define (sequentialize-let-value v)
    (match v
      [`(let ([,xs ,vs] ...) ,v)
       (define sequentialize-let-values (for/list ([x xs] [v vs])
                                          `(set! ,x ,(sequentialize-let-value v))))
       `(begin ,@sequentialize-let-values ,(sequentialize-let-value v))]
      [`(if ,p ,v1 ,v2)
       (define p^ (sequentialize-let-pred p))
       (define v1^ (sequentialize-let-value v1))
       (define v2^ (sequentialize-let-value v2))
       `(if ,p^ ,v1^ ,v2^)]
      ;; Using wildcard collapse case because in the other two cases, the
      ;; expression is already in imp-mf-lang-v5.value form
      [_ v]))

  ;; values-unique-lang-v6.pred -> imp-mf-lang-v6.pred
  (define (sequentialize-let-pred p)
    (match p
      [`(let ([,xs ,vs] ...) ,pred)
       (define sequentialize-let-values (for/list ([x xs] [v vs])
                                          `(set! ,x ,(sequentialize-let-value v))))
       `(begin ,@sequentialize-let-values ,(sequentialize-let-pred pred))]
      [`(,relop ,t1 ,t2) `(,relop ,t1 ,t2)]
      [`(true) '(true)]
      [`(false) '(false)]
      [`(not ,pred)
       (define pred^ (sequentialize-let-pred pred))
       `(not ,pred^)]
      [`(if ,p1 ,p2 ,p3)
       (define p1^ (sequentialize-let-pred p1))
       (define p2^ (sequentialize-let-pred p2))
       (define p3^ (sequentialize-let-pred p3))
       `(if ,p1^ ,p2^ ,p3^)]))

  (match p
    [`(module ,funcs ... ,tail)
     `(module ,@(map sequentialize-let-func funcs) ,(sequentialize-let-tail tail))]))

(module+ test
  (check-equal? (sequentialize-let '(module
                                        (define L.f.1 (lambda (x.1 y.1)
                                                        (let ([a.1 2] [b.1 (call L.g.1 x.1 y.1)])
                                                          (let ([c.1 (call L.g.1 a.1 b.1)])
                                                            (- a.1 b.1)))))
                                      (define L.g.1 (lambda (x.2 y.2)
                                                      (let ([z.1 (+ x.2 y.2)])
                                                        (let ([a.2 (- y.2 x.2)])
                                                          (* z.1 a.2)))))
                                      (let ([x.2 (call L.f.1 1 2)])
                                        (if (let ([x.3 (call L.g.1 1 2)])
                                              (not (!= x.2 x.3)))
                                            (call L.f.1 10 20)
                                            (call L.f.1 x.2 1)))))
                '(module
                     (define L.f.1
                       (lambda (x.1 y.1)
                         (begin
                           (set! a.1 2)
                           (set! b.1 (call L.g.1 x.1 y.1))
                           (begin (set! c.1 (call L.g.1 a.1 b.1)) (- a.1 b.1)))))
                   (define L.g.1
                     (lambda (x.2 y.2)
                       (begin
                         (set! z.1 (+ x.2 y.2))
                         (begin (set! a.2 (- y.2 x.2)) (* z.1 a.2)))))
                   (begin
                     (set! x.2 (call L.f.1 1 2))
                     (if (begin (set! x.3 (call L.g.1 1 2)) (not (!= x.2 x.3)))
                         (call L.f.1 10 20)
                         (call L.f.1 x.2 1)))))
  (check-equal? (sequentialize-let '(module
                                        (define L.f.1 (lambda (x.1)
                                                        (let ([y.1 1] [z.1 2])
                                                          (let ([a.1 (* y.1 x.1)]
                                                                [b.1 (* z.1 x.1)])
                                                            (+ a.1 b.1)))))
                                      (let ([x.2 10])
                                        (if (let ([x.3 100])
                                              (not (!= x.2 x.3)))
                                            (call L.f.1 x.2)
                                            (call L.f.2 1000)))))
                '(module
                     (define L.f.1
                       (lambda (x.1)
                         (begin
                           (set! y.1 1)
                           (set! z.1 2)
                           (begin
                             (set! a.1 (* y.1 x.1))
                             (set! b.1 (* z.1 x.1))
                             (+ a.1 b.1)))))
                   (begin
                     (set! x.2 10)
                     (if (begin
                           (set! x.3 100)
                           (not (!= x.2 x.3)))
                         (call L.f.1 x.2)
                         (call L.f.2 1000)))))
  (check-equal? (sequentialize-let '(module (if (let ([x.1 1] [y.2 2])
                                                  (if (not (< x.1 y.2)) (> x.1 y.2) (false)))
                                                (let ([x.2 0]
                                                      [x.3 1])
                                                  (+ x.2 x.3))
                                                (let ([x.4 2]
                                                      [x.5 3])
                                                  (+ x.4 x.5)))))
                '(module
                     (if (begin
                           (set! x.1 1)
                           (set! y.2 2)
                           (if (not (< x.1 y.2)) (> x.1 y.2) (false)))
                         (begin (set! x.2 0) (set! x.3 1) (+ x.2 x.3))
                         (begin (set! x.4 2) (set! x.5 3) (+ x.4 x.5)))))
  (check-equal? (sequentialize-let '(module (if (let ([x.1 1] [y.2 2]) (> x.1 y.2))
                                                (let ([x.2 0]
                                                      [x.3 1])
                                                  (+ x.2 x.3))
                                                (let ([x.4 2]
                                                      [x.5 3])
                                                  (+ x.4 x.5)))))
                '(module
                     (if (begin (set! x.1 1) (set! y.2 2) (> x.1 y.2))
                         (begin (set! x.2 0) (set! x.3 1) (+ x.2 x.3))
                         (begin (set! x.4 2) (set! x.5 3) (+ x.4 x.5)))))
  (check-equal? (sequentialize-let '(module (let ([x.1 3]) x.1))) '(module (begin (set! x.1 3) x.1)))
  (check-equal? (sequentialize-let '(module (let ([x.1 0]
                                                  [x.2 1])
                                              (+ x.1 x.2))))
                '(module (begin (set! x.1 0) (set! x.2 1) (+ x.1 x.2))))
  (check-equal? (sequentialize-let '(module (let ([x.1 (let ([x.7 5]) (* 5 x.7))]) (+ x.1 -1))))
                '(module (begin (set! x.1 (begin (set! x.7 5) (* 5 x.7))) (+ x.1 -1)))))
