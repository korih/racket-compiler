#lang racket

(require
  cpsc411/langs/v3
  rackunit)

(provide normalize-bind)

;; imp-mf-lang-v3 -> imp-cmf-lang-v3
;; compiles p to imp-cmf-lang-v3 by ensuring the right-hand-side of each set! is
;; a simple value-producing operation
(define/contract (normalize-bind p)
  (-> imp-mf-lang-v3? imp-cmf-lang-v3?)

  ;; imp-mf-lang-v3.tail -> imp-cmf-lang-v3.tail
  (define (normalize-bind-tail tail)
    (match tail
      [`(begin ,e ... ,t)
       `(begin ,@(map normalize-bind-effect e) ,(normalize-bind-tail t))]
      [v (normalize-bind-value v (lambda (v) v))]))

  ;; imp-mf-lang-v3.value (imp-mf-lang-v3.value -> imp-mf-lang-v3.value) imp-mf-lang-v3.value
  (define (normalize-bind-value value cont)
    (match value
      [`(begin ,e ... ,v)
       `(begin ,@(map normalize-bind-effect e) ,(normalize-bind-value v cont))]
      [triv (cont triv)]))

  ;; imp-mf-lang-v3.effect -> imp-mf-lang-v3.effect
  (define (normalize-bind-effect effect)
    (match effect
      [`(set! ,aloc ,v)
       (normalize-bind-value v (lambda (simple-v)
                                 `(set! ,aloc ,simple-v)))]
      [`(begin ,e ...)
       `(begin ,@(map normalize-bind-effect e))]
      ;; Using a wildcard collapse case as it captures all other well-formed
      ;; expressions without transformation
      [_ effect]))

  (match p
    [`(module ,tail)
     `(module ,(normalize-bind-tail tail))]))

(module+ test
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