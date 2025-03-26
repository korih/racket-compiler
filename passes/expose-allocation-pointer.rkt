#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8
  rackunit)

(provide expose-allocation-pointer)

;; asm-alloc-lang-v8 -> asm-pred-lang-v8
;; compiles p to Asm-pred-lang v8 by implementing the allocation primitive in
;; terms of pointer arithmetic on the current-heap-base-pointer-register
(define/contract (expose-allocation-pointer p)
  (-> asm-alloc-lang-v8? asm-pred-lang-v8?)

  ;; func is `(define ,label ,info ,tail)
  ;; interp. a function definition

  ;; func -> func
  (define (expose-allocation-pointer-func func)
    (match func
      [`(define ,label ,info ,tail)
       `(define ,label ,info ,(expose-allocation-pointer-tail tail))]))

  ;; asm-alloc-lang-v8.tail -> asm-pred-lang-v8.tail
  (define (expose-allocation-pointer-tail tail)
    (match tail
      [`(begin ,es ... ,t)
       `(begin ,@(map expose-allocation-pointer-effect es) ,(expose-allocation-pointer-tail t))]
      [`(if ,pred ,t1 ,t2)
       `(if ,(expose-allocation-pointer-pred pred)
            ,(expose-allocation-pointer-tail t1)
            ,(expose-allocation-pointer-tail t2))]
      [`(jump ,trg ,locs ...) tail]))

  ;; asm-alloc-lang-v8.effect -> asm-pred-lang-v8.effect
  (define (expose-allocation-pointer-effect effect)
    (match effect
      [`(set! ,loc (alloc ,index))
       (define hbp (current-heap-base-pointer-register))
       `(begin
          (set! ,loc ,hbp)
          (set! ,hbp (+ ,hbp ,index)))]
      [`(begin ,es ...)
       `(begin ,@(map expose-allocation-pointer-effect es))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(expose-allocation-pointer-pred pred)
            ,(expose-allocation-pointer-effect e1)
            ,(expose-allocation-pointer-effect e2))]
      [`(return-point ,label ,tail)
       `(return-point ,label ,(expose-allocation-pointer-tail tail))]
      ;; Using wildcard collapse case because in all other cases, the
      ;; expression is already in asm-pred-lang-v8.effect form
      [_ effect]))

  ;; asm-alloc-lang-v8.pred -> asm-pred-lang-v8.pred
  (define (expose-allocation-pointer-pred pred)
    (match pred
      [`(not ,p)
       `(not ,(expose-allocation-pointer-pred p))]
      [`(begin ,es ... ,p)
       `(begin ,@(map expose-allocation-pointer-effect es) ,(expose-allocation-pointer-pred p))]
      [`(if ,p1 ,p2 ,p3)
       `(if ,(expose-allocation-pointer-pred p1)
            ,(expose-allocation-pointer-pred p2)
            ,(expose-allocation-pointer-pred p3))]
      ;; Using wildcard collapse case because in all other cases, the
      ;; expression is already in asm-pred-lang-v8.pred form
      [_ pred]))

  (match p
    [`(module ,info ,funcs ... ,tail)
     `(module ,info ,@(map expose-allocation-pointer-func funcs) ,(expose-allocation-pointer-tail tail))]))

(module+ test
  (check-equal? (expose-allocation-pointer '(module ((new-frames ()))
                                              (begin
                                                (set! tmp-ra.1 r15)
                                                (set! x.1 (alloc 1))
                                                (jump tmp-ra.1 rbp))))
                '(module
                     ((new-frames ()))
                   (begin
                     (set! tmp-ra.1 r15)
                     (begin (set! x.1 r12) (set! r12 (+ r12 1)))
                     (jump tmp-ra.1 rbp))))
  (check-equal? (expose-allocation-pointer '(module ((new-frames ()))
                                              (begin
                                                (set! tmp-ra.1 r15)
                                                (set! y.1 1)
                                                (set! y.1 (bitwise-ior y.1 8))
                                                (set! x.1 (alloc y.1))
                                                (jump tmp-ra.1 rbp))))
                '(module
                     ((new-frames ()))
                   (begin
                     (set! tmp-ra.1 r15)
                     (set! y.1 1)
                     (set! y.1 (bitwise-ior y.1 8))
                     (begin (set! x.1 r12) (set! r12 (+ r12 y.1)))
                     (jump tmp-ra.1 rbp))))
  (check-equal? (expose-allocation-pointer '(module
                                                ((new-frames ()))
                                              (define L.f.1
                                                ((new-frames (())))
                                                (begin
                                                  (set! tmp-ra.50 r15)
                                                  (set! x.1 rdi)
                                                  (set! x.2 rsi)
                                                  (set! tmp.39 10)
                                                  (set! tmp.39 (+ tmp.39 6))
                                                  (set! tmp.38 (alloc tmp.39))
                                                  (return-point L.rp.21 (begin (set! r15 L.rp.21) (jump L.g.1 rbp r15)))
                                                  (set! tmp.40 rax)
                                                  (if (true) (mset! tmp.38 tmp.40 x.1) (mset! tmp.38 tmp.40 x.2))
                                                  (set! tmp.42 10)
                                                  (set! tmp.42 (+ tmp.42 6))
                                                  (set! tmp.41 (alloc tmp.42))
                                                  (set! tmp.43 8)
                                                  (set! tmp.43 (bitwise-and tmp.43 8))
                                                  (set! rax (mref tmp.41 tmp.43))
                                                  (jump tmp-ra.50 rbp rax)))
                                              (define L.g.1
                                                ((new-frames ()))
                                                (begin (set! tmp-ra.51 r15) (set! rax 8) (jump tmp-ra.51 rbp rax)))
                                              (begin
                                                (set! tmp-ra.52 r15)
                                                (set! rdi 1)
                                                (set! rsi 2)
                                                (set! r15 tmp-ra.52)
                                                (jump L.f.1 rbp r15 rdi rsi))))
                '(module
                     ((new-frames ()))
                   (define L.f.1
                     ((new-frames (())))
                     (begin
                       (set! tmp-ra.50 r15)
                       (set! x.1 rdi)
                       (set! x.2 rsi)
                       (set! tmp.39 10)
                       (set! tmp.39 (+ tmp.39 6))
                       (begin (set! tmp.38 r12) (set! r12 (+ r12 tmp.39)))
                       (return-point L.rp.21 (begin (set! r15 L.rp.21) (jump L.g.1 rbp r15)))
                       (set! tmp.40 rax)
                       (if (true) (mset! tmp.38 tmp.40 x.1) (mset! tmp.38 tmp.40 x.2))
                       (set! tmp.42 10)
                       (set! tmp.42 (+ tmp.42 6))
                       (begin (set! tmp.41 r12) (set! r12 (+ r12 tmp.42)))
                       (set! tmp.43 8)
                       (set! tmp.43 (bitwise-and tmp.43 8))
                       (set! rax (mref tmp.41 tmp.43))
                       (jump tmp-ra.50 rbp rax)))
                   (define L.g.1
                     ((new-frames ()))
                     (begin (set! tmp-ra.51 r15) (set! rax 8) (jump tmp-ra.51 rbp rax)))
                   (begin
                     (set! tmp-ra.52 r15)
                     (set! rdi 1)
                     (set! rsi 2)
                     (set! r15 tmp-ra.52)
                     (jump L.f.1 rbp r15 rdi rsi)))))