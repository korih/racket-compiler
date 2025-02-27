#lang racket

(require
  cpsc411/langs/v5
  rackunit)

(provide normalize-bind)

;; imp-mf-lang-v5 -> proc-imp-cmf-lang-v5
;; compiles p to to to Proc-imp-cmf-lang v5 by pushing set! under begin so that
;; the right-hand-side of each set! is simple value-producing operation
(define/contract (normalize-bind p)
  (-> imp-mf-lang-v5? proc-imp-cmf-lang-v5?)

  (define (normalize-bind-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,tail))
       `(define ,label (lambda (,@alocs) ,(normalize-bind-tail tail)))]))

  ;; imp-mf-lang-v5.tail -> proc-imp-cmf-lang-v5.tail
  (define (normalize-bind-tail tail)
    (match tail
      [`(begin ,e ... ,t)
       `(begin ,@(map normalize-bind-effect e) ,(normalize-bind-tail t))]
      [`(if ,p ,t1 ,t2)
       `(if ,(normalize-bind-pred p) ,(normalize-bind-tail t1) ,(normalize-bind-tail t2))]
      [`(call ,triv ,opand ...) tail]
      [v (normalize-bind-value v (lambda (v) v))]))

  ;; imp-mf-lang-v5.effect -> proc-imp-cmf-lang-v5.effect
  (define (normalize-bind-effect effect)
    (match effect
      [`(set! ,aloc ,v)
       (normalize-bind-value v (lambda (simple-v)
                                 `(set! ,aloc ,simple-v)))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(normalize-bind-pred pred) ,(normalize-bind-effect e1) ,(normalize-bind-effect e2))]
      [`(begin ,e ...)
       `(begin ,@(map normalize-bind-effect e))]))

  ;; imp-mf-lang-v5.value (imp-mf-lang-v5.value -> proc-imp-cmf-lang-v5.effect) -> proc-imp-cmf-lang-v5.value
  (define (normalize-bind-value value cont)
    (match value
      [`(begin ,e ... ,v)
       `(begin ,@(map normalize-bind-effect e) ,(normalize-bind-value v cont))]
      [`(if ,p ,v1 ,v2)
       `(if ,(normalize-bind-pred p)
            ,(normalize-bind-value v1 cont)
            ,(normalize-bind-value v2 cont))]
      [`(,binop ,triv ,triv) (cont value)]
      [triv (cont triv)]))

  ;; imp-mf-lang-v5.pred -> proc-imp-cmf-lang-v5.pred
  (define (normalize-bind-pred pred)
    (match pred
      ['(true) pred]
      ['(false) pred]
      [`(not ,p) `(not ,(normalize-bind-pred p))]
      [`(begin ,e ... ,p)
       `(begin ,@(map normalize-bind-effect e) ,(normalize-bind-pred p))]
      [`(if ,p1 ,p2 ,p3)
       `(if ,(normalize-bind-pred p1) ,(normalize-bind-pred p2) ,(normalize-bind-pred p3))]
      [`(,relop ,t1 ,t2) pred]))

  (match p
    [`(module ,funcs ... ,tail)
     `(module ,@(map normalize-bind-func funcs) ,(normalize-bind-tail tail))]))

(module+ test
  (check-equal? (normalize-bind '(module
                                     (define L.f.1 (lambda (x.1) (begin
                                                                   (set! x.2 (begin
                                                                               (set! x.1 (+ x.1 10))
                                                                               (* x.1 100)))
                                                                   (set! x.3 (if (begin
                                                                                   (set! x.4 (begin
                                                                                               (set! x.5 x.1)
                                                                                               (+ x.5 100)))
                                                                                   (> x.4 x.2))
                                                                                 (+ x.2 100)
                                                                                 (* x.2 100)))
                                                                   (+ x.2 x.3))))
                                   (call L.f.1 10)))
                '(module
                     (define L.f.1
                       (lambda (x.1)
                         (begin
                           (begin (set! x.1 (+ x.1 10)) (set! x.2 (* x.1 100)))
                           (if (begin (begin (set! x.5 x.1) (set! x.4 (+ x.5 100))) (> x.4 x.2))
                               (set! x.3 (+ x.2 100))
                               (set! x.3 (* x.2 100)))
                           (+ x.2 x.3))))
                   (call L.f.1 10)))
  (check-equal? (normalize-bind '(module
                                     (begin
                                       (set! x.1 (if (begin
                                                       (set! x.2 (if (begin
                                                                       (set! x.3 (begin
                                                                                   (set! x.4 10)
                                                                                   (* x.4 100)))
                                                                       (set! x.5 (begin
                                                                                   (set! x.6 100)
                                                                                   (+ x.6 100)))
                                                                       (> x.3 x.5))
                                                                     (begin
                                                                       (set! x.7 (if (not (true))
                                                                                     10
                                                                                     1000))
                                                                       (+ x.7 10))
                                                                     (begin
                                                                       (set! x.8 (begin
                                                                                   (set! x.9 100)
                                                                                   (+ x.9 100)))
                                                                       (* x.8 100))))
                                                       (> x.2 100))
                                                     100
                                                     10))
                                       (+ x.1 100))))
                '(module
                     (begin
                       (if (begin
                             (if (begin
                                   (begin (set! x.4 10) (set! x.3 (* x.4 100)))
                                   (begin (set! x.6 100) (set! x.5 (+ x.6 100)))
                                   (> x.3 x.5))
                                 (begin
                                   (if (not (true)) (set! x.7 10) (set! x.7 1000))
                                   (set! x.2 (+ x.7 10)))
                                 (begin
                                   (begin (set! x.9 100) (set! x.8 (+ x.9 100)))
                                   (set! x.2 (* x.8 100))))
                             (> x.2 100))
                           (set! x.1 100)
                           (set! x.1 10))
                       (+ x.1 100))))                                
  (check-equal? (normalize-bind '(module (begin (set! x.6 (+ 2 3)) (set! x.7 (+ x.6 x.6)) (begin (set! y.2 5) x.6))))
                '(module
                     (begin (set! x.6 (+ 2 3)) (set! x.7 (+ x.6 x.6)) (begin (set! y.2 5) x.6))))
  (check-equal? (normalize-bind '(module (begin (set! x.1 (if (true) 1 2)) x.1)))
                '(module (begin (if (true) (set! x.1 1) (set! x.1 2)) x.1)))
  (check-equal? (normalize-bind '(module (if (begin
                                               (set! x.1 (if (not (= 1 2))
                                                             x.2
                                                             5))
                                               (set! x.2 (begin
                                                           (if (> x.1 2)
                                                               (set! x.1 1)
                                                               (set! x.1 2))
                                                           5))
                                               (false))
                                             x.1
                                             x.2)))
                '(module
                     (if (begin
                           (if (not (= 1 2)) (set! x.1 x.2) (set! x.1 5))
                           (begin (if (> x.1 2) (set! x.1 1) (set! x.1 2)) (set! x.2 5))
                           (false))
                         x.1
                         x.2)))
  (check-equal? (normalize-bind '(module x.1)) '(module x.1))
  (check-equal? (normalize-bind '(module (begin (set! x.1 1) x.1))) '(module (begin (set! x.1 1) x.1)))
  (check-equal? (normalize-bind '(module (begin (set! x.2 2) (begin (set! x.3 4) x.3))))
                '(module (begin (set! x.2 2) (begin (set! x.3 4) x.3))))
  (check-equal? (normalize-bind '(module (begin (set! x.4 1) (set! x.5 1) (+ x.4 x.5))))
                '(module (begin (set! x.4 1) (set! x.5 1) (+ x.4 x.5))))
  (check-equal? (normalize-bind '(module (begin (set! x.5 (begin (set! x.6 5) -2)) (+ x.5 1))))
                '(module (begin (begin (set! x.6 5) (set! x.5 -2)) (+ x.5 1))))
  (check-equal? (normalize-bind '(module (begin (set! x.0 0) (set! x.1 (begin (set! x.2 2) (+ x.2 1))) x.0)))
                '(module (begin (set! x.0 0) (begin (set! x.2 2) (set! x.1 (+ x.2 1))) x.0)))
  (check-equal? (normalize-bind '(module (begin 0))) '(module (begin 0)))
  (check-equal? (normalize-bind '(module (begin (set! x.1 1)
                                                (begin (set! x.2 -2)
                                                       (set! x.3 3)
                                                       (set! x.4 (+ x.2 x.3))
                                                       (begin (set! x.5 0)
                                                              (begin (set! x.6 7)
                                                                     x.6))))))
                '(module (begin (set! x.1 1)
                                (begin (set! x.2 -2)
                                       (set! x.3 3)
                                       (set! x.4 (+ x.2 x.3))
                                       (begin (set! x.5 0)
                                              (begin (set! x.6 7)
                                                     x.6))))))
  (check-equal? (normalize-bind '(module (begin (set! x.0 -3)
                                                (set! x.1 (begin (set! x.2 2)
                                                                 (set! x.3 (begin (set! x.4 4) x.4)) (+ x.2 x.3))) x.0)))
                '(module (begin (set! x.0 -3)
                                (begin (set! x.2 2)
                                       (begin (set! x.4 4)
                                              (set! x.3 x.4))
                                       (set! x.1 (+ x.2 x.3))) x.0))))