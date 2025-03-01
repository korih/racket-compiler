#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5
  rackunit)

(provide replace-locations)

;; asm-lang-v5/assignments -> nested-asm-lang-v5
;; interp. replaces the abstract locations with the concrete locations
(define (replace-locations p)
  (-> asm-pred-lang-v5/assignments? nested-asm-lang-v5?)

  ;; asm-lang-v5/assignments.tail (dictof asm-lang-v5/assignments.aloc asm-lang-v5/assignments.rloc)
  ;; -> nested-asm-lang-v5.tail
  (define (replace-locations-tail t assignments)
    (match t
      [`(halt ,triv)
       `(halt ,(replace-locations-triv triv assignments))]
      [`(begin ,fx ... ,tail)
       (define compiled-fx (for/list ([e fx])
                             (replace-locations-effect e assignments)))
       (define compiled-tail (replace-locations-tail tail assignments))
       `(begin ,@compiled-fx ,compiled-tail)]
      [`(if ,pred ,t1 ,t2)
       (define pred^ (replace-locations-pred pred assignments))
       (define t1^ (replace-locations-tail t1 assignments))
       (define t2^ (replace-locations-tail t2 assignments))
       `(if ,pred^ ,t1^ ,t2^)]))

  ;; asm-lang-v5/assignments.effect (dictof asm-lang-v5/assignments.aloc asm-lang-v5/assignments.rloc)
  ;; -> nested-asm-lang-v5.effect
  (define (replace-locations-effect e assignments)
    (match e
      [`(set! ,x (,binop ,x ,v))
       (define reg (dict-ref assignments x))
       `(set! ,reg (,binop ,reg ,(replace-locations-triv v assignments)))]
      [`(set! ,x ,v)
       `(set! ,(dict-ref assignments x) ,(replace-locations-triv v assignments))]
      [`(begin ,fx ... ,e)
       (define compiled-fx (for/list ([e fx]) (replace-locations-effect e assignments)))
       (define compiled-e (replace-locations-effect e assignments))
       `(begin ,@compiled-fx ,compiled-e)]
      [`(if ,pred ,e1 ,e2)
       (define pred^ (replace-locations-pred pred assignments))
       (define e1^ (replace-locations-effect e1 assignments))
       (define e2^ (replace-locations-effect e2 assignments))
       `(if ,pred^ ,e1^ ,e2^)]))

  ;; asm-lang-v5/assignments.pred (dictof asm-lang-v5/assignments.aloc asm-lang-v5/assignments.rloc)
  ;; -> nested-asm-lang-v5.pred
  (define (replace-locations-pred p assignments)
    (match p
      [`(true) `(true)]
      [`(false) `(false)]
      [`(begin ,effects ... ,pred)
       (define effects^
         (for/list ([effect effects])
           (replace-locations-effect effect assignments)))
       (define pred^ (replace-locations-pred pred assignments))
       `(begin ,@effects^ ,pred^)]
      [`(not ,pred) `(not ,(replace-locations-pred pred assignments))]
      [`(,relop ,aloc ,triv)
       (define reg (dict-ref assignments aloc))
       `(,relop ,reg ,triv)]
      [`(if ,p1 ,p2 ,p3)
       (define p1^ (replace-locations-pred p1 assignments))
       (define p2^ (replace-locations-pred p2 assignments))
       (define p3^ (replace-locations-pred p3 assignments))
       `(if ,p1^ ,p2^ ,p3^)]))

  ;; asm-lang-v5/assignments.triv (dictof asm-lang-v5/assignments.aloc asm-lang-v5/assignments.rloc)
  ;; -> nested-asm-lang-v5.effect
  (define (replace-locations-triv t assignments)
    (match t
      [`,x #:when (aloc? x) (dict-ref assignments x)]
      [`,x x]))

  (match p
    [`(module ,info ,tail)
     (define assignments (for/fold ([acc (hash)])
                                   ([pair (info-ref info 'assignment)])
                           (dict-set acc (first pair) (second pair))))
     `(module ,(replace-locations-tail tail assignments))]))

(module+ test
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (begin (set! x.1 1) (if (> x.1 0) (set! x.1 2) (set! x.1 3)) (halt x.1))))
                '(module
                     (begin (set! rax 1) (if (> rax 0) (set! rax 2) (set! rax 3)) (halt rax))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (if (true) (true) (false)) (halt 1) (halt 0))))
                '(module (if (if (true) (true) (false)) (halt 1) (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (begin (set! x.1 1) (begin (set! x.1 2) (set! x.1 3)) (> x.1 0)) (halt 1) (halt 0))))
                '(module
                     (if (begin (set! rax 1) (begin (set! rax 2) (set! rax 3)) (> rax 0))
                         (halt 1)
                         (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (begin (set! x.1 1) (set! x.1 1) (> x.1 0)) (halt 1) (halt 0))))
                '(module (if (begin (set! rax 1) (set! rax 1) (> rax 0)) (halt 1) (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (begin (set! x.1 1) (> x.1 0)) (halt 1) (halt 0))))
                '(module (if (begin (set! rax 1) (> rax 0)) (halt 1) (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (not (> x.1 1)) (halt 1) (halt 0))))
                '(module (if (not (> rax 1)) (halt 1) (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (true) (halt 1) (halt 0))))
                '(module (if (true) (halt 1) (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (> x.1 1) (halt 1) (halt 0))))
                '(module (if (> rax 1) (halt 1) (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (begin (if (true)
                                                 (begin
                                                   (set! x.1 (+ x.1 1))
                                                   (if (> x.1 0) (set! x.1 1) (set! x.1 0))
                                                   (halt x.1))
                                                 (begin
                                                   (set! x.1 (+ x.1 2))
                                                   (if (if (true) (false) (false))
                                                       (set! x.1 2)
                                                       (set! x.1 3))
                                                   (halt x.1))))))
                '(module
                     (begin
                       (if (true)
                           (begin
                             (set! rax (+ rax 1))
                             (if (> rax 0) (set! rax 1) (set! rax 0))
                             (halt rax))
                           (begin
                             (set! rax (+ rax 2))
                             (if (if (true) (false) (false)) (set! rax 2) (set! rax 3))
                             (halt rax))))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (begin (set! x.1 (+ x.1 1)) (halt x.1))))
                '(module (begin (set! rax (+ rax 1)) (halt rax))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax)))) (begin (set! x.1 0) (halt x.1))))
                '(module (begin (set! rax 0) (halt rax))))
  (check-equal? (replace-locations '(module ((locals (x.1 y.1 w.1)) (assignment ((x.1 rax) (y.1 rbx) (w.1 r9))))
                                      (begin (set! x.1 0)
                                             (set! y.1 x.1)
                                             (set! w.1 1)
                                             (set! w.1 (+ w.1 y.1))
                                             (halt w.1))))
                '(module (begin (set! rax 0) (set! rbx rax) (set! r9 1) (set! r9 (+ r9 rbx)) (halt r9)))))
