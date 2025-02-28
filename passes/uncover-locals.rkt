#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5
  rackunit)

(provide uncover-locals)

;; asm-pred-lang-v5 -> asm-pred-lang-v5/locals
;; compiles p to to Asm-pred-lang v5/locals by analysing which abstract
;; locations are used in the module and decorating the module with the set of
;; variables in an info field.
(define/contract (uncover-locals p)
  (-> asm-pred-lang-v5? asm-pred-lang-v5/locals?)

  ;; unique-alocs is (Set-of aloc)
  ;; keeps track of unique abstract locations
  (define unique-alocs (mutable-set))

  (define (uncover-locals-func f)
    (set-clear! unique-alocs)
    (match f
      [`(define ,label () ,tail)
       (uncover-locals-tail tail)
       `(define ,label ,(info-set '() 'locals (set->list unique-alocs)) ,tail)]))

  ;; asm-pred-lang-v5.tail ->
  ;; EFFECTS: adds alocs from tail t to unique-alocs
  (define (uncover-locals-tail t)
    (match t
      [`(halt ,op)
       (uncover-locals-opand op)]
      [`(begin ,ef ... ,ta)
       (for-each uncover-locals-effect ef)
       (uncover-locals-tail ta)]
      [`(jump ,trg ,locs ...)
       (uncover-locals-trg trg)
       (for-each uncover-locals-loc locs)]
      [`(if ,pred ,tail1 ,tail2)
       (uncover-locals-pred pred)
       (for-each uncover-locals-tail (list tail1 tail2))]))

  ;; asm-pred-lang-v5.effect ->
  ;; EFFECTS: adds alocs from effect e to unique-alocs
  (define (uncover-locals-effect e)
    (match e
      [`(set! ,loc (,binop ,loc ,op))
       (uncover-locals-loc loc)
       (uncover-locals-opand op)]
      [`(set! ,loc ,triv)
       (uncover-locals-loc loc)
       (uncover-locals-triv triv)]
      [`(begin ,ef ...)
       (for-each uncover-locals-effect ef)]
      [`(if ,pred ,e1 ,e2)
       (uncover-locals-pred pred)
       (for-each uncover-locals-effect (list e1 e2))]))

  ;; asm-pred-lang-v5.pred ->
  ;; EFFECTS: adds alocs from pred p to unique-alocs
  (define (uncover-locals-pred p)
    (match p
      [`(not ,pred)
       (uncover-locals-pred pred)]
      [`(begin ,e ... ,pred)
       (for-each uncover-locals-effect e)
       (uncover-locals-pred pred)]
      [`(if ,pred1 ,pred2 ,pred3)
       (for-each uncover-locals-pred (list pred1 pred2 pred3))]
      [`(,relop ,loc ,op)
       (uncover-locals-loc loc)
       (uncover-locals-opand op)]
      ['(true) (void)]
      ['(false) (void)]))

  ;; asm-pred-lang-v5.triv ->
  ;; EFFECTS: adds alocs from triv t to unique-alocs
  (define (uncover-locals-triv t)
    (match t
      [label #:when (label? label) (void)]
      [opand (uncover-locals-opand opand)]))

  ;; asm-pred-lang-v5.opand ->
  ;; EFFECTS: adds alocs from opand op to unique-alocs
  (define (uncover-locals-opand op)
    (match op
      [int64 #:when (int64? int64) (void)]
      [loc (uncover-locals-loc loc)]))

  ;; asm-pred-lang-v5.loc ->
  ;; EFFECTS: adds alocs from loc to unique-alocs
  (define (uncover-locals-loc loc)
    (match loc
      [aloc #:when (aloc? aloc) (set-add! unique-alocs aloc)]
      [rloc #:when (rloc? rloc) (void)]))

  ;; asm-pred-lang-v5.trg ->
  ;; EFFECTS: adds alocs from trg to unique-alocs
  (define (uncover-locals-trg trg)
    (match trg
      [label #:when (label? label) (void)]
      [loc (uncover-locals-loc loc)]))

  (match p
    [`(module () ,funcs ... ,t)
     (define uncovered-funcs (map uncover-locals-func funcs))
     (set-clear! unique-alocs)
     (uncover-locals-tail t)
     `(module ,(info-set '() 'locals (set->list unique-alocs)) ,@uncovered-funcs ,t)]))

(module+ test
  (check-equal? (uncover-locals '(module () (define L.f.1 () (begin (set! x.1 rdi)
                                                                    (halt x.1)))
                                   (begin
                                     (set! rdi 1)
                                     (jump L.f.1 rbp rdi))))
                '(module
                     ((locals ()))
                   (define L.f.1 ((locals (x.1))) (begin (set! x.1 rdi) (halt x.1)))
                   (begin (set! rdi 1) (jump L.f.1 rbp rdi))))
  (check-equal? (uncover-locals '(module () (define L.f.1 () (begin (set! x.1 rdi)
                                                                    (halt x.1)))
                                   (begin
                                     (set! a.1 L.f.1)
                                     (set! rdi 1)
                                     (jump a.1 rbp rdi))))
                '(module
                     ((locals (a.1)))
                   (define L.f.1 ((locals (x.1))) (begin (set! x.1 rdi) (halt x.1)))
                   (begin (set! a.1 L.f.1) (set! rdi 1) (jump a.1 rbp rdi))))
  (check-equal? (uncover-locals '(module () (define L.f.1 () (begin (set! x.1 rdi)
                                                                    (halt x.1)))
                                   (begin
                                     (set! r13 L.f.1)
                                     (set! rdi 1)
                                     (jump r13 rbp rdi))))
                '(module
                     ((locals ()))
                   (define L.f.1 ((locals (x.1))) (begin (set! x.1 rdi) (halt x.1)))
                   (begin (set! r13 L.f.1) (set! rdi 1) (jump r13 rbp rdi))))
  (check-equal? (uncover-locals '(module () (define L.f.1 () (begin
                                                               (set! x.1 rdi)
                                                               (halt x.1)))
                                   (define L.g.1 () (begin
                                                      (set! x.1 rdi)
                                                      (set! y.1 rsi)
                                                      (set! z.1 rdx)
                                                      (set! rdi x.1)
                                                      (jump L.f.1 rbp rdi)))
                                   (if (true)
                                       (begin
                                         (set! rdx 3)
                                         (set! rsi 2)
                                         (set! rdi 1)
                                         (jump L.g.1 rbp rdi rsi rdx))
                                       (begin
                                         (set! rdi 1)
                                         (jump L.f.1 rbp rdi)))))
                '(module
                     ((locals ()))
                   (define L.f.1 ((locals (x.1))) (begin (set! x.1 rdi) (halt x.1)))
                   (define L.g.1
                     ((locals (y.1 x.1 z.1)))
                     (begin
                       (set! x.1 rdi)
                       (set! y.1 rsi)
                       (set! z.1 rdx)
                       (set! rdi x.1)
                       (jump L.f.1 rbp rdi)))
                   (if (true)
                       (begin
                         (set! rdx 3)
                         (set! rsi 2)
                         (set! rdi 1)
                         (jump L.g.1 rbp rdi rsi rdx))
                       (begin (set! rdi 1) (jump L.f.1 rbp rdi)))))
  (check-equal? (uncover-locals '(module () (begin (set! x.1 0) (halt x.1))))
                '(module ((locals (x.1))) (begin (set! x.1 0) (halt x.1))))
  (match-let ([`(module ((locals (,ls ...))) ,_) (uncover-locals '(module () (begin (set! x.1 0)
                                                                                    (set! y.1 x.1)
                                                                                    (set! y.1 (+ y.1 x.1))
                                                                                    (halt y.1))))])
    (check-equal? (list->set ls) (set 'x.1 'y.1)))
  (check-equal? (uncover-locals '(module () (halt 5)))
                '(module ((locals ())) (halt 5)))
  (check-equal? (uncover-locals '(module () (halt x.1)))
                '(module ((locals (x.1))) (halt x.1)))
  (check-equal? (uncover-locals '(module () (begin (halt 1))))
                '(module ((locals ())) (begin (halt 1))))
  (check-equal? (uncover-locals '(module () (if (true) (halt 0) (halt 1))))
                '(module ((locals ())) (if (true) (halt 0) (halt 1))))
  (check-equal? (uncover-locals '(module () (if (= x.2 0) (halt 1) (halt 2))))
                '(module ((locals (x.2))) (if (= x.2 0) (halt 1) (halt 2))))
  (check-equal? (uncover-locals '(module ()
                                   (if (= a.1 0)
                                       (begin
                                         (set! b.1 (+ b.1 1))
                                         (halt b.1))
                                       (begin
                                         (set! c.1 2)
                                         (halt c.1)))))
                '(module
                     ((locals (b.1 c.1 a.1)))
                   (if (= a.1 0)
                       (begin (set! b.1 (+ b.1 1)) (halt b.1))
                       (begin (set! c.1 2) (halt c.1)))))
  (check-equal? (uncover-locals '(module ()
                                   (begin
                                     (set! x.1 10)
                                     (begin
                                       (set! y.1 (+ y.1 x.1))
                                       (halt y.1)))))
                '(module
                     ((locals (y.1 x.1)))
                   (begin (set! x.1 10) (begin (set! y.1 (+ y.1 x.1)) (halt y.1)))))
  (check-equal? (uncover-locals '(module ()
                                   (begin
                                     (set! m.1 3)
                                     (set! n.1 4)
                                     (halt m.1))))
                '(module ((locals (m.1 n.1)))
                   (begin
                     (set! m.1 3)
                     (set! n.1 4)
                     (halt m.1))))
  (check-equal? (uncover-locals '(module ()
                                   (begin
                                     (set! p.1 (+ p.1 q.1))
                                     (halt 0))))
                '(module ((locals (q.1 p.1))) (begin (set! p.1 (+ p.1 q.1)) (halt 0))))
  (check-equal? (uncover-locals '(module ()
                                   (if (= x.1 0)
                                       (if (= y.1 1)
                                           (halt z.1)
                                           (halt w.1))
                                       (halt v.1))))
                '(module
                     ((locals (v.1 w.1 y.1 x.1 z.1)))
                   (if (= x.1 0) (if (= y.1 1) (halt z.1) (halt w.1)) (halt v.1))))
  (check-equal? (uncover-locals '(module ()
                                   (if (begin (set! x.1 2) (= y.1 3))
                                       (halt z.1)
                                       (halt w.1))))
                '(module
                     ((locals (w.1 y.1 x.1 z.1)))
                   (if (begin (set! x.1 2) (= y.1 3)) (halt z.1) (halt w.1))))
  (check-equal? (uncover-locals '(module ()
                                   (begin
                                     (set! a.1 (+ a.1 c.1))
                                     (begin
                                       (set! d.1 (+ d.1 f.1))
                                       (halt g.1)))))
                '(module
                     ((locals (c.1 d.1 a.1 g.1 f.1)))
                   (begin (set! a.1 (+ a.1 c.1)) (begin (set! d.1 (+ d.1 f.1)) (halt g.1)))))
  (check-equal? (uncover-locals '(module ()
                                   (if (if (= x.1 0)
                                           (= y.1 1)
                                           (= z.1 2))
                                       (halt w.1)
                                       (halt v.1))))
                '(module
                     ((locals (v.1 w.1 y.1 x.1 z.1)))
                   (if (if (= x.1 0) (= y.1 1) (= z.1 2)) (halt w.1) (halt v.1))))
  (check-equal? (uncover-locals '(module ()
                                   (begin
                                     (set! a.1 10)
                                     (if (= b.1 0)
                                         (begin
                                           (set! c.1 (+ c.1 e.1))
                                           (halt f.1))
                                         (begin
                                           (set! g.1 20)
                                           (halt h.1))))))
                '(module
                     ((locals (h.1 b.1 e.1 c.1 a.1 g.1 f.1)))
                   (begin
                     (set! a.1 10)
                     (if (= b.1 0)
                         (begin (set! c.1 (+ c.1 e.1)) (halt f.1))
                         (begin (set! g.1 20) (halt h.1))))))
  (check-equal? (uncover-locals '(module ()
                                   (begin
                                     (if (if (true) (false) (not (= x.1 0)))
                                         (begin
                                           (if (< y.1 10)
                                               (set! g.1 8)
                                               (set! g.1 (* g.1 h.1)))
                                           (halt i.1))
                                         (begin
                                           (if (>= j.1 5)
                                               (halt k.1)
                                               (halt m.1)))))))
                '(module
                     ((locals (h.1 k.1 m.1 j.1 y.1 x.1 g.1 i.1)))
                   (begin
                     (if (if (true) (false) (not (= x.1 0)))
                         (begin (if (< y.1 10) (set! g.1 8) (set! g.1 (* g.1 h.1))) (halt i.1))
                         (begin (if (>= j.1 5) (halt k.1) (halt m.1))))))))