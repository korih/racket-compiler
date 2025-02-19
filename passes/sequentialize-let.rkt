#lang racket

(require
  cpsc411/langs/v3
  rackunit)

(provide sequentialize-let)

;; values-unique-lang-v3 -> imp-mf-lang-v3
;; compiles p to imp-mf-lang-v3 by picking a particular order to implement
;; let expressions using set!
(define/contract (sequentialize-let p)
  (-> values-unique-lang-v3? imp-mf-lang-v3?)

  ;; values-unique-lang-v3.tail -> imp-mf-lang-v3.tail
  (define (sequentialize-let-tail tail)
    (match tail
      [`(let ([,xs ,vs] ...) ,tail)
       (define sequentialize-let-values (for/list ([x xs] [v vs])
                                          `(set! ,x ,(sequentialize-let-value v))))
       `(begin ,@sequentialize-let-values ,(sequentialize-let-tail tail))]
      [value (sequentialize-let-value value)]))

  ;; values-unique-lang-v3.value -> imp-mf-lang-v3.value
  (define (sequentialize-let-value v)
    (match v
      [`(let ([,xs ,vs] ...) ,v)
       (define sequentialize-let-values (for/list ([x xs] [v vs])
                                          `(set! ,x ,(sequentialize-let-value v))))
       `(begin ,@sequentialize-let-values ,(sequentialize-let-value v))]
      ;; Using wildcard collapse case because in the other two cases, the
      ;; expression is already in imp-mf-lang-v3.value form
      [_ v]))

  (match p
    [`(module ,tail)
     `(module ,(sequentialize-let-tail tail))]))

(module+ test
  (check-equal? (sequentialize-let '(module (let ([x.1 3]) x.1))) '(module (begin (set! x.1 3) x.1)))
  (check-equal? (sequentialize-let '(module (let ([x.1 0]
                                                  [x.2 1])
                                              (+ x.1 x.2))))
                '(module (begin (set! x.1 0) (set! x.2 1) (+ x.1 x.2))))
  (check-equal? (sequentialize-let '(module (let ([x.1 (let ([x.7 5]) (* 5 x.7))]) (+ x.1 -1))))
                '(module (begin (set! x.1 (begin (set! x.7 5) (* 5 x.7))) (+ x.1 -1)))))