#lang racket

(require
  rackunit
  "../passes/expose-allocation-pointer.rkt")

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
