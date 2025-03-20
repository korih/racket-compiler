#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v7
  rackunit)

(provide uncover-locals)

;; asm-pred-lang-v7 -> asm-pred-lang-v7/locals
;; compiles p to to Asm-pred-lang v7/locals by analysing which abstract
;; locations are used in the module and decorating the module with the set of
;; variables in an info field.
(define/contract (uncover-locals p)
  (-> asm-pred-lang-v7? asm-pred-lang-v7/locals?)

  ;; func-info is `(define ,label ,info ,tail)
  ;; interp. a function definition that has metadata

  ;; unique-alocs is (Set-of aloc)
  ;; keeps track of unique abstract locations
  (define unique-alocs (mutable-set))

  ;; func-info -> func-info
  (define (uncover-locals-func f)
    (set-clear! unique-alocs)
    (match f
      [`(define ,label ,info ,tail)
       (uncover-locals-tail tail)
       `(define ,label ,(info-set info 'locals (set->list unique-alocs)) ,tail)]))

  ;; asm-pred-lang-v7.tail ->
  ;; EFFECTS: adds alocs from tail t to unique-alocs
  (define (uncover-locals-tail t)
    (match t
      [`(begin ,ef ... ,ta)
       (for-each uncover-locals-effect ef)
       (uncover-locals-tail ta)]
      [`(jump ,trg ,locs ...)
       (uncover-locals-trg trg)
       (for-each uncover-locals-loc locs)]
      [`(if ,pred ,tail1 ,tail2)
       (uncover-locals-pred pred)
       (for-each uncover-locals-tail (list tail1 tail2))]))

  ;; asm-pred-lang-v7.effect ->
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
       (for-each uncover-locals-effect (list e1 e2))]
      [`(return-point ,label ,tail)
       (uncover-locals-tail tail)]))

  ;; asm-pred-lang-v7.pred ->
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

  ;; asm-pred-lang-v7.triv ->
  ;; EFFECTS: adds alocs from triv t to unique-alocs
  (define (uncover-locals-triv t)
    (match t
      [label #:when (label? label) (void)]
      [opand (uncover-locals-opand opand)]))

  ;; asm-pred-lang-v7.opand ->
  ;; EFFECTS: adds alocs from opand op to unique-alocs
  (define (uncover-locals-opand op)
    (match op
      [int64 #:when (int64? int64) (void)]
      [loc (uncover-locals-loc loc)]))

  ;; asm-pred-lang-v7.loc ->
  ;; EFFECTS: adds alocs from loc to unique-alocs
  (define (uncover-locals-loc loc)
    (match loc
      [aloc #:when (aloc? aloc) (set-add! unique-alocs aloc)]
      [rloc #:when (rloc? rloc) (void)]))

  ;; asm-pred-lang-v7.trg ->
  ;; EFFECTS: adds alocs from trg to unique-alocs
  (define (uncover-locals-trg trg)
    (match trg
      [label #:when (label? label) (void)]
      [loc (uncover-locals-loc loc)]))

  (match p
    [`(module ,info ,funcs ... ,t)
     (define uncovered-funcs (map uncover-locals-func funcs))
     (set-clear! unique-alocs)
     (uncover-locals-tail t)
     `(module ,(info-set info 'locals (set->list unique-alocs)) ,@uncovered-funcs ,t)]))

(module+ test
  (check-equal? (uncover-locals '(module
                                     ((new-frames ()))
                                   (define L.f.1
                                     ((new-frames ()))
                                     (begin (set! tmp-ra.1 r15) (set! tmp.1 1) (set! tmp.1 (* tmp.1 2)) (set! rax tmp.1) (jump tmp-ra.1 rbp rax)))
                                   (begin
                                     (set! tmp-ra.2 r15)
                                     (set! r15 tmp-ra.2)
                                     (jump L.f.1 rbp r15))))
                '(module
                     ((new-frames ()) (locals (tmp-ra.2)))
                   (define L.f.1
                     ((new-frames ()) (locals (tmp-ra.1 tmp.1)))
                     (begin
                       (set! tmp-ra.1 r15)
                       (set! tmp.1 1)
                       (set! tmp.1 (* tmp.1 2))
                       (set! rax tmp.1)
                       (jump tmp-ra.1 rbp rax)))
                   (begin (set! tmp-ra.2 r15) (set! r15 tmp-ra.2) (jump L.f.1 rbp r15))))
  (check-equal? (uncover-locals '(module
                                     ((new-frames ((nfv.36) (nfv.35))))
                                   (define L.f.1
                                     ((new-frames ()))
                                     (begin
                                       (set! tmp-ra.24 r15)
                                       (set! x.1 fv0)
                                       (set! y.1 fv1)
                                       (set! rax x.1)
                                       (set! rax (+ rax y.1))
                                       (jump tmp-ra.24 rbp rax)))
                                   (define L.g.1
                                     ((new-frames
                                       ((nfv.32 nfv.33) (nfv.30 nfv.31) (nfv.28 nfv.29) (nfv.26 nfv.27))))
                                     (begin
                                       (set! tmp-ra.25 r15)
                                       (set! x.1 fv0)
                                       (return-point L.rp.6
                                                     (begin
                                                       (set! nfv.26 x.1)
                                                       (set! nfv.27 1)
                                                       (set! r15 L.rp.6)
                                                       (jump L.f.1 rbp r15 nfv.26 nfv.27)))
                                       (set! y.1 rax)
                                       (return-point L.rp.7
                                                     (begin
                                                       (set! nfv.28 x.1)
                                                       (set! nfv.29 2)
                                                       (set! r15 L.rp.7)
                                                       (jump L.f.1 rbp r15 nfv.28 nfv.29)))
                                       (set! z.1 rax)
                                       (if (true)
                                           (begin
                                             (return-point L.rp.8
                                                           (begin
                                                             (set! nfv.30 y.1)
                                                             (set! nfv.31 z.1)
                                                             (set! r15 L.rp.8)
                                                             (jump L.f.1 rbp r15 nfv.30 nfv.31)))
                                             (set! a.1 rax)
                                             (set! rax a.1)
                                             (set! rax (* rax x.1))
                                             (jump tmp-ra.25 rbp rax))
                                           (begin
                                             (return-point L.rp.9
                                                           (begin
                                                             (set! nfv.32 y.1)
                                                             (set! nfv.33 x.1)
                                                             (set! r15 L.rp.9)
                                                             (jump L.f.1 rbp r15 nfv.32 nfv.33)))
                                             (set! b.1 rax)
                                             (set! rax b.1)
                                             (set! rax (- rax z.1))
                                             (jump tmp-ra.25 rbp rax)))))
                                   (begin
                                     (set! tmp-ra.34 r15)
                                     (return-point L.rp.10
                                                   (begin (set! nfv.35 1) (set! r15 L.rp.10) (jump L.g.1 rbp r15 nfv.35)))
                                     (set! x.1 rax)
                                     (return-point L.rp.11
                                                   (begin (set! nfv.36 2) (set! r15 L.rp.11) (jump L.g.1 rbp r15 nfv.36)))
                                     (set! x.2 rax)
                                     (set! rax x.1)
                                     (set! rax (* rax x.2))
                                     (jump tmp-ra.34 rbp rax))))
                '(module
                     ((new-frames ((nfv.36) (nfv.35)))
                      (locals (nfv.35 x.1 tmp-ra.34 x.2 nfv.36)))
                   (define L.f.1
                     ((new-frames ()) (locals (y.1 tmp-ra.24 x.1)))
                     (begin
                       (set! tmp-ra.24 r15)
                       (set! x.1 fv0)
                       (set! y.1 fv1)
                       (set! rax x.1)
                       (set! rax (+ rax y.1))
                       (jump tmp-ra.24 rbp rax)))
                   (define L.g.1
                     ((new-frames
                       ((nfv.32 nfv.33) (nfv.30 nfv.31) (nfv.28 nfv.29) (nfv.26 nfv.27)))
                      (locals
                       (nfv.30
                        b.1
                        nfv.29
                        y.1
                        nfv.26
                        z.1
                        x.1
                        a.1
                        nfv.33
                        nfv.27
                        tmp-ra.25
                        nfv.32
                        nfv.31
                        nfv.28)))
                     (begin
                       (set! tmp-ra.25 r15)
                       (set! x.1 fv0)
                       (return-point L.rp.6
                                     (begin
                                       (set! nfv.26 x.1)
                                       (set! nfv.27 1)
                                       (set! r15 L.rp.6)
                                       (jump L.f.1 rbp r15 nfv.26 nfv.27)))
                       (set! y.1 rax)
                       (return-point L.rp.7
                                     (begin
                                       (set! nfv.28 x.1)
                                       (set! nfv.29 2)
                                       (set! r15 L.rp.7)
                                       (jump L.f.1 rbp r15 nfv.28 nfv.29)))
                       (set! z.1 rax)
                       (if (true)
                           (begin
                             (return-point L.rp.8
                                           (begin
                                             (set! nfv.30 y.1)
                                             (set! nfv.31 z.1)
                                             (set! r15 L.rp.8)
                                             (jump L.f.1 rbp r15 nfv.30 nfv.31)))
                             (set! a.1 rax)
                             (set! rax a.1)
                             (set! rax (* rax x.1))
                             (jump tmp-ra.25 rbp rax))
                           (begin
                             (return-point L.rp.9
                                           (begin
                                             (set! nfv.32 y.1)
                                             (set! nfv.33 x.1)
                                             (set! r15 L.rp.9)
                                             (jump L.f.1 rbp r15 nfv.32 nfv.33)))
                             (set! b.1 rax)
                             (set! rax b.1)
                             (set! rax (- rax z.1))
                             (jump tmp-ra.25 rbp rax)))))
                   (begin
                     (set! tmp-ra.34 r15)
                     (return-point L.rp.10
                                   (begin (set! nfv.35 1) (set! r15 L.rp.10) (jump L.g.1 rbp r15 nfv.35)))
                     (set! x.1 rax)
                     (return-point L.rp.11
                                   (begin (set! nfv.36 2) (set! r15 L.rp.11) (jump L.g.1 rbp r15 nfv.36)))
                     (set! x.2 rax)
                     (set! rax x.1)
                     (set! rax (* rax x.2))
                     (jump tmp-ra.34 rbp rax))))
  (check-equal? (uncover-locals '(module
                                     ((new-frames ()))
                                   (define L.f.1
                                     ((new-frames ()))
                                     (begin
                                       (set! tmp-ra.1 r15)
                                       (set! x.1 rdi)
                                       (set! y.1 1)
                                       (set! z.1 2)
                                       (set! a.1 y.1)
                                       (set! a.1 (bitwise-and a.1 x.1))
                                       (set! b.1 z.1)
                                       (set! b.1 (bitwise-ior b.1 x.1))
                                       (set! a.1 (bitwise-xor a.1 b.1))
                                       (set! rax a.1)
                                       (set! rax (arithmetic-shift-right rax 3))
                                       (jump tmp-ra.1 rbp rax)))
                                   (begin
                                     (set! tmp-ra.2 r15)
                                     (set! x.2 10)
                                     (if (begin (set! x.3 100) (not (!= x.2 x.3)))
                                         (begin (set! rdi x.2) (set! r15 tmp-ra.2) (jump L.f.1 rbp r15 rdi))
                                         (begin (set! rdi 1000) (set! r15 tmp-ra.2) (jump L.f.2 rbp r15 rdi))))))
                '(module
                     ((new-frames ()) (locals (x.3 tmp-ra.2 x.2)))
                   (define L.f.1
                     ((new-frames ()) (locals (tmp-ra.1 b.1 y.1 x.1 z.1 a.1)))
                     (begin
                       (set! tmp-ra.1 r15)
                       (set! x.1 rdi)
                       (set! y.1 1)
                       (set! z.1 2)
                       (set! a.1 y.1)
                       (set! a.1 (bitwise-and a.1 x.1))
                       (set! b.1 z.1)
                       (set! b.1 (bitwise-ior b.1 x.1))
                       (set! a.1 (bitwise-xor a.1 b.1))
                       (set! rax a.1)
                       (set! rax (arithmetic-shift-right rax 3))
                       (jump tmp-ra.1 rbp rax)))
                   (begin
                     (set! tmp-ra.2 r15)
                     (set! x.2 10)
                     (if (begin (set! x.3 100) (not (!= x.2 x.3)))
                         (begin (set! rdi x.2) (set! r15 tmp-ra.2) (jump L.f.1 rbp r15 rdi))
                         (begin (set! rdi 1000) (set! r15 tmp-ra.2) (jump L.f.2 rbp r15 rdi))))))
  (check-equal? (uncover-locals '(module
                                     ((new-frames ()))
                                   (define L.*.17
                                     ((new-frames ()))
                                     (begin
                                       (set! tmp-ra.93 r15)
                                       (set! tmp.41 rdi)
                                       (set! tmp.42 rsi)
                                       (if (begin
                                             (if (begin
                                                   (set! tmp.79 tmp.42)
                                                   (set! tmp.79 (bitwise-and tmp.79 7))
                                                   (= tmp.79 0))
                                                 (set! tmp.78 14)
                                                 (set! tmp.78 6))
                                             (!= tmp.78 6))
                                           (if (begin
                                                 (if (begin
                                                       (set! tmp.81 tmp.41)
                                                       (set! tmp.81 (bitwise-and tmp.81 7))
                                                       (= tmp.81 0))
                                                     (set! tmp.80 14)
                                                     (set! tmp.80 6))
                                                 (!= tmp.80 6))
                                               (begin
                                                 (set! tmp.82 tmp.42)
                                                 (set! tmp.82 (arithmetic-shift-right tmp.82 3))
                                                 (set! rax tmp.41)
                                                 (set! rax (* rax tmp.82))
                                                 (jump tmp-ra.93 rbp rax))
                                               (begin (set! rax 318) (jump tmp-ra.93 rbp rax)))
                                           (begin (set! rax 318) (jump tmp-ra.93 rbp rax)))))
                                   (define L.+.16
                                     ((new-frames ()))
                                     (begin
                                       (set! tmp-ra.94 r15)
                                       (set! tmp.39 rdi)
                                       (set! tmp.40 rsi)
                                       (if (begin
                                             (if (begin
                                                   (set! tmp.84 tmp.40)
                                                   (set! tmp.84 (bitwise-and tmp.84 7))
                                                   (= tmp.84 0))
                                                 (set! tmp.83 14)
                                                 (set! tmp.83 6))
                                             (!= tmp.83 6))
                                           (if (begin
                                                 (if (begin
                                                       (set! tmp.86 tmp.39)
                                                       (set! tmp.86 (bitwise-and tmp.86 7))
                                                       (= tmp.86 0))
                                                     (set! tmp.85 14)
                                                     (set! tmp.85 6))
                                                 (!= tmp.85 6))
                                               (begin
                                                 (set! rax tmp.39)
                                                 (set! rax (+ rax tmp.40))
                                                 (jump tmp-ra.94 rbp rax))
                                               (begin (set! rax 574) (jump tmp-ra.94 rbp rax)))
                                           (begin (set! rax 574) (jump tmp-ra.94 rbp rax)))))
                                   (begin
                                     (set! tmp-ra.95 r15)
                                     (begin
                                       (set! rbp (- rbp 16))
                                       (return-point L.rp.19
                                                     (begin
                                                       (set! rdi 40)
                                                       (set! rsi 48)
                                                       (set! r15 L.rp.19)
                                                       (jump L.+.16 rbp r15 rdi rsi)))
                                       (set! rbp (+ rbp 16)))
                                     (set! tmp.87 rax)
                                     (begin
                                       (set! rbp (- rbp 16))
                                       (return-point L.rp.20
                                                     (begin
                                                       (set! rdi 32)
                                                       (set! rsi 40)
                                                       (set! r15 L.rp.20)
                                                       (jump L.*.17 rbp r15 rdi rsi)))
                                       (set! rbp (+ rbp 16)))
                                     (set! tmp.88 rax)
                                     (set! rdi tmp.87)
                                     (set! rsi tmp.88)
                                     (set! r15 tmp-ra.95)
                                     (jump L.+.16 rbp r15 rdi rsi))))
                '(module
                     ((new-frames ()) (locals (tmp.88 tmp-ra.95 tmp.87)))
                   (define L.*.17
                     ((new-frames ())
                      (locals (tmp.80 tmp.78 tmp.41 tmp.82 tmp.42 tmp.81 tmp.79 tmp-ra.93)))
                     (begin
                       (set! tmp-ra.93 r15)
                       (set! tmp.41 rdi)
                       (set! tmp.42 rsi)
                       (if (begin
                             (if (begin
                                   (set! tmp.79 tmp.42)
                                   (set! tmp.79 (bitwise-and tmp.79 7))
                                   (= tmp.79 0))
                                 (set! tmp.78 14)
                                 (set! tmp.78 6))
                             (!= tmp.78 6))
                           (if (begin
                                 (if (begin
                                       (set! tmp.81 tmp.41)
                                       (set! tmp.81 (bitwise-and tmp.81 7))
                                       (= tmp.81 0))
                                     (set! tmp.80 14)
                                     (set! tmp.80 6))
                                 (!= tmp.80 6))
                               (begin
                                 (set! tmp.82 tmp.42)
                                 (set! tmp.82 (arithmetic-shift-right tmp.82 3))
                                 (set! rax tmp.41)
                                 (set! rax (* rax tmp.82))
                                 (jump tmp-ra.93 rbp rax))
                               (begin (set! rax 318) (jump tmp-ra.93 rbp rax)))
                           (begin (set! rax 318) (jump tmp-ra.93 rbp rax)))))
                   (define L.+.16
                     ((new-frames ())
                      (locals (tmp.39 tmp.85 tmp-ra.94 tmp.84 tmp.86 tmp.40 tmp.83)))
                     (begin
                       (set! tmp-ra.94 r15)
                       (set! tmp.39 rdi)
                       (set! tmp.40 rsi)
                       (if (begin
                             (if (begin
                                   (set! tmp.84 tmp.40)
                                   (set! tmp.84 (bitwise-and tmp.84 7))
                                   (= tmp.84 0))
                                 (set! tmp.83 14)
                                 (set! tmp.83 6))
                             (!= tmp.83 6))
                           (if (begin
                                 (if (begin
                                       (set! tmp.86 tmp.39)
                                       (set! tmp.86 (bitwise-and tmp.86 7))
                                       (= tmp.86 0))
                                     (set! tmp.85 14)
                                     (set! tmp.85 6))
                                 (!= tmp.85 6))
                               (begin
                                 (set! rax tmp.39)
                                 (set! rax (+ rax tmp.40))
                                 (jump tmp-ra.94 rbp rax))
                               (begin (set! rax 574) (jump tmp-ra.94 rbp rax)))
                           (begin (set! rax 574) (jump tmp-ra.94 rbp rax)))))
                   (begin
                     (set! tmp-ra.95 r15)
                     (begin
                       (set! rbp (- rbp 16))
                       (return-point L.rp.19
                                     (begin
                                       (set! rdi 40)
                                       (set! rsi 48)
                                       (set! r15 L.rp.19)
                                       (jump L.+.16 rbp r15 rdi rsi)))
                       (set! rbp (+ rbp 16)))
                     (set! tmp.87 rax)
                     (begin
                       (set! rbp (- rbp 16))
                       (return-point L.rp.20
                                     (begin
                                       (set! rdi 32)
                                       (set! rsi 40)
                                       (set! r15 L.rp.20)
                                       (jump L.*.17 rbp r15 rdi rsi)))
                       (set! rbp (+ rbp 16)))
                     (set! tmp.88 rax)
                     (set! rdi tmp.87)
                     (set! rsi tmp.88)
                     (set! r15 tmp-ra.95)
                     (jump L.+.16 rbp r15 rdi rsi)))))