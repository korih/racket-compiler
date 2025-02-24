#lang racket

(require
  cpsc411/langs/v4
  rackunit)

(provide normalize-bind)

;; Exercise 19
;; imp-mf-lang-v4 -> imp-cmf-lang-v4
;; compiles p to to Imp-cmf-lang v4 by pushing set! under begin and if so that
;; the right-hand-side of each set! is a simple value-producing operation
(define/contract (normalize-bind p)
  (-> imp-mf-lang-v4? imp-cmf-lang-v4?)

  ;; imp-mf-lang-v4.tail -> imp-cmf-lang-v4.tail
  (define (normalize-bind-tail tail)
    (match tail
      [`(begin ,e ... ,t)
       `(begin ,@(map normalize-bind-effect e) ,(normalize-bind-tail t))]
      [`(if ,p ,t1 ,t2)
       `(if ,(normalize-bind-pred p) ,(normalize-bind-tail t1) ,(normalize-bind-tail t2))]
      [v (normalize-bind-value v (lambda (v) v))]))

  ;; imp-mf-lang-v4.effect -> imp-cmf-lang-v4.effect
  (define (normalize-bind-effect effect)
    (match effect
      [`(set! ,aloc ,v)
       (normalize-bind-value v (lambda (simple-v)
                                 `(set! ,aloc ,simple-v)))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(normalize-bind-pred pred) ,(normalize-bind-effect e1) ,(normalize-bind-effect e2))]
      [`(begin ,e ...)
       `(begin ,@(map normalize-bind-effect e))]))

  ;; imp-mf-lang-v4.value (imp-mf-lang-v4.value -> imp-cmf-lang-v4.effect) -> imp-cmf-lang-v4.value
  (define (normalize-bind-value value cont)
    (match value
      [`(begin ,e ... ,v)
       `(begin ,@(map normalize-bind-effect e) ,(normalize-bind-value v cont))]
      [`(if ,p ,v1 ,v2)
       `(if ,(normalize-bind-pred p)
            ,(cont v1)
            ,(cont v2))]
      [`(,binop ,triv ,triv) (cont value)]
      [triv (cont triv)]))

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
    [`(module ,tail)
     `(module ,(normalize-bind-tail tail))]))

(module+ test
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