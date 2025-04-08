#lang racket

(require
  cpsc411/compiler-lib
  "../passes/impose-calling-conventions.rkt"
  rackunit)

(module+ test
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda (x.1) x.1))
                                               (begin
                                                 (set! x.1 (call L.f.1 1))
                                                 x.1)))
                '(module
                     ((new-frames (())))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.1 r15)
                       (begin (set! x.1 rdi) (begin (set! rax x.1) (jump tmp-ra.1 rbp rax)))))
                   (begin
                     (set! tmp-ra.2 r15)
                     (begin
                       (begin
                         (return-point L.rp.1
                                       (begin (set! rdi 1) (set! r15 L.rp.1) (jump L.f.1 rbp r15 rdi)))
                         (set! x.1 rax))
                       (begin (set! rax x.1) (jump tmp-ra.2 rbp rax))))))
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda (a.1 b.1 c.1 d.1 e.1 f.1 g.1) x.1))
                                               (begin
                                                 (set! x.1 (call L.f.1 1 2 3 4 5 6 7))
                                                 x.1)))
                '(module
                     ((new-frames ((nfv.5))))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.3 r15)
                       (begin
                         (set! a.1 rdi)
                         (set! b.1 rsi)
                         (set! c.1 rdx)
                         (set! d.1 rcx)
                         (set! e.1 r8)
                         (set! f.1 r9)
                         (set! g.1 fv0)
                         (begin (set! rax x.1) (jump tmp-ra.3 rbp rax)))))
                   (begin
                     (set! tmp-ra.4 r15)
                     (begin
                       (begin
                         (return-point L.rp.2
                                       (begin
                                         (set! rdi 1)
                                         (set! rsi 2)
                                         (set! rdx 3)
                                         (set! rcx 4)
                                         (set! r8 5)
                                         (set! r9 6)
                                         (set! nfv.5 7)
                                         (set! r15 L.rp.2)
                                         (jump L.f.1 rbp r15 rdi rsi rdx rcx r8 r9 nfv.5)))
                         (set! x.1 rax))
                       (begin (set! rax x.1) (jump tmp-ra.4 rbp rax))))))
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda (x.1) x.1))
                                               (begin
                                                 (set! x.1 1)
                                                 (set! x.2 10)
                                                 (set! x.3 (+ x.1 x.2))
                                                 (set! x.4 (call L.f.4 x.3))
                                                 (* x.4 x.3))))
                '(module
                     ((new-frames (())))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.6 r15)
                       (begin (set! x.1 rdi) (begin (set! rax x.1) (jump tmp-ra.6 rbp rax)))))
                   (begin
                     (set! tmp-ra.7 r15)
                     (begin
                       (set! x.1 1)
                       (set! x.2 10)
                       (set! x.3 (+ x.1 x.2))
                       (begin
                         (return-point L.rp.3
                                       (begin (set! rdi x.3) (set! r15 L.rp.3) (jump L.f.4 rbp r15 rdi)))
                         (set! x.4 rax))
                       (begin (set! rax (* x.4 x.3)) (jump tmp-ra.7 rbp rax))))))
  (check-equal? (impose-calling-conventions '(module
                                                 (define L.f.2
                                                   (lambda (x.1 x.2)
                                                     (+ x.1 x.2)))
                                               (define L.f.3
                                                 (lambda (y.1)
                                                   (call L.f.2 y.1 10)))
                                               (begin
                                                 (set! x.1 5)
                                                 (set! x.2 10)
                                                 (set! x.3 (call L.f.2 x.1 x.2))
                                                 (set! x.4 (call L.f.3 x.3))
                                                 (set! x.5 (- x.3 x.4))
                                                 (call L.f.3 x.5))))
                '(module
                     ((new-frames (() ())))
                   (define L.f.2
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.8 r15)
                       (begin
                         (set! x.1 rdi)
                         (set! x.2 rsi)
                         (begin (set! rax (+ x.1 x.2)) (jump tmp-ra.8 rbp rax)))))
                   (define L.f.3
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.9 r15)
                       (begin
                         (set! y.1 rdi)
                         (begin
                           (set! rdi y.1)
                           (set! rsi 10)
                           (set! r15 tmp-ra.9)
                           (jump L.f.2 rbp r15 rdi rsi)))))
                   (begin
                     (set! tmp-ra.10 r15)
                     (begin
                       (set! x.1 5)
                       (set! x.2 10)
                       (begin
                         (return-point L.rp.4
                                       (begin
                                         (set! rdi x.1)
                                         (set! rsi x.2)
                                         (set! r15 L.rp.4)
                                         (jump L.f.2 rbp r15 rdi rsi)))
                         (set! x.3 rax))
                       (begin
                         (return-point L.rp.5
                                       (begin (set! rdi x.3) (set! r15 L.rp.5) (jump L.f.3 rbp r15 rdi)))
                         (set! x.4 rax))
                       (set! x.5 (- x.3 x.4))
                       (begin (set! rdi x.5) (set! r15 tmp-ra.10) (jump L.f.3 rbp r15 rdi))))))
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda () (* 1 2))) (call L.f.1)))
                '(module
                     ((new-frames ()))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.11 r15)
                       (begin (begin (set! rax (* 1 2)) (jump tmp-ra.11 rbp rax)))))
                   (begin (set! tmp-ra.12 r15) (begin (set! r15 tmp-ra.12) (jump L.f.1 rbp r15)))))
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda (a.1 b.1 c.1 d.1 e.1 f.1 g.1 h.1 i.1 j.1 k.1) 10))
                                               (begin
                                                 (set! a.1 1)
                                                 (set! b.1 2)
                                                 (set! c.1 3)
                                                 (set! d.1 4)
                                                 (set! e.1 5)
                                                 (set! f.1 6)
                                                 (set! g.1 7)
                                                 (set! h.1 8)
                                                 (set! i.1 9)
                                                 (set! j.1 10)
                                                 (set! k.1 11)
                                                 (call L.f.1 a.1 b.1 c.1 d.1 e.1 f.1 g.1 h.1 i.1 j.1 k.1))))
                '(module
                     ((new-frames ()))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.13 r15)
                       (begin
                         (set! a.1 rdi)
                         (set! b.1 rsi)
                         (set! c.1 rdx)
                         (set! d.1 rcx)
                         (set! e.1 r8)
                         (set! f.1 r9)
                         (set! g.1 fv4)
                         (set! h.1 fv3)
                         (set! i.1 fv2)
                         (set! j.1 fv1)
                         (set! k.1 fv0)
                         (begin (set! rax 10) (jump tmp-ra.13 rbp rax)))))
                   (begin
                     (set! tmp-ra.14 r15)
                     (begin
                       (set! a.1 1)
                       (set! b.1 2)
                       (set! c.1 3)
                       (set! d.1 4)
                       (set! e.1 5)
                       (set! f.1 6)
                       (set! g.1 7)
                       (set! h.1 8)
                       (set! i.1 9)
                       (set! j.1 10)
                       (set! k.1 11)
                       (begin
                         (set! rdi a.1)
                         (set! rsi b.1)
                         (set! rdx c.1)
                         (set! rcx d.1)
                         (set! r8 e.1)
                         (set! r9 f.1)
                         (set! fv0 g.1)
                         (set! fv1 h.1)
                         (set! fv2 i.1)
                         (set! fv3 j.1)
                         (set! fv4 k.1)
                         (set! r15 tmp-ra.14)
                         (jump L.f.1 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4))))))
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda (a.1 b.1 c.1 d.1 e.1 f.1) (begin
                                                                                                       (set! a.1 (+ a.1 b.1))
                                                                                                       (set! a.1 (+ a.1 c.1))
                                                                                                       (set! a.1 (+ a.1 d.1))
                                                                                                       (set! a.1 (+ a.1 e.1))
                                                                                                       (set! a.1 (+ a.1 f.1))
                                                                                                       a.1)))
                                               (call L.f.1 1 2 3 4 5 6)))
                '(module
                     ((new-frames ()))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.15 r15)
                       (begin
                         (set! a.1 rdi)
                         (set! b.1 rsi)
                         (set! c.1 rdx)
                         (set! d.1 rcx)
                         (set! e.1 r8)
                         (set! f.1 r9)
                         (begin
                           (set! a.1 (+ a.1 b.1))
                           (set! a.1 (+ a.1 c.1))
                           (set! a.1 (+ a.1 d.1))
                           (set! a.1 (+ a.1 e.1))
                           (set! a.1 (+ a.1 f.1))
                           (begin (set! rax a.1) (jump tmp-ra.15 rbp rax))))))
                   (begin
                     (set! tmp-ra.16 r15)
                     (begin
                       (set! rdi 1)
                       (set! rsi 2)
                       (set! rdx 3)
                       (set! rcx 4)
                       (set! r8 5)
                       (set! r9 6)
                       (set! r15 tmp-ra.16)
                       (jump L.f.1 rbp r15 rdi rsi rdx rcx r8 r9)))))
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda (a.1 b.1 c.1 d.1 e.1 f.1 g.1 h.1 i.1 j.1 k.1) (begin
                                                                                                                           (set! a.1 (+ a.1 b.1))
                                                                                                                           (set! a.1 (+ a.1 c.1))
                                                                                                                           (set! a.1 (+ a.1 d.1))
                                                                                                                           (set! a.1 (+ a.1 e.1))
                                                                                                                           (set! a.1 (+ a.1 f.1))
                                                                                                                           (set! a.1 (+ a.1 g.1))
                                                                                                                           (set! a.1 (+ a.1 h.1))
                                                                                                                           (set! a.1 (+ a.1 i.1))
                                                                                                                           (set! a.1 (+ a.1 j.1))
                                                                                                                           (set! a.1 (+ a.1 k.1))
                                                                                                                           a.1)))
                                               (call L.f.1 1 2 3 4 5 6 7 8 9 10 11)))
                '(module
                     ((new-frames ()))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.17 r15)
                       (begin
                         (set! a.1 rdi)
                         (set! b.1 rsi)
                         (set! c.1 rdx)
                         (set! d.1 rcx)
                         (set! e.1 r8)
                         (set! f.1 r9)
                         (set! g.1 fv4)
                         (set! h.1 fv3)
                         (set! i.1 fv2)
                         (set! j.1 fv1)
                         (set! k.1 fv0)
                         (begin
                           (set! a.1 (+ a.1 b.1))
                           (set! a.1 (+ a.1 c.1))
                           (set! a.1 (+ a.1 d.1))
                           (set! a.1 (+ a.1 e.1))
                           (set! a.1 (+ a.1 f.1))
                           (set! a.1 (+ a.1 g.1))
                           (set! a.1 (+ a.1 h.1))
                           (set! a.1 (+ a.1 i.1))
                           (set! a.1 (+ a.1 j.1))
                           (set! a.1 (+ a.1 k.1))
                           (begin (set! rax a.1) (jump tmp-ra.17 rbp rax))))))
                   (begin
                     (set! tmp-ra.18 r15)
                     (begin
                       (set! rdi 1)
                       (set! rsi 2)
                       (set! rdx 3)
                       (set! rcx 4)
                       (set! r8 5)
                       (set! r9 6)
                       (set! fv0 7)
                       (set! fv1 8)
                       (set! fv2 9)
                       (set! fv3 10)
                       (set! fv4 11)
                       (set! r15 tmp-ra.18)
                       (jump L.f.1 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4)))))
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda (x.1 y.1 z.1) (begin
                                                                                           (set! a.1 (+ x.1 y.1))
                                                                                           (* z.1 a.1))))
                                               (call L.f.1 1 2 3)))
                '(module
                     ((new-frames ()))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.19 r15)
                       (begin
                         (set! x.1 rdi)
                         (set! y.1 rsi)
                         (set! z.1 rdx)
                         (begin
                           (set! a.1 (+ x.1 y.1))
                           (begin (set! rax (* z.1 a.1)) (jump tmp-ra.19 rbp rax))))))
                   (begin
                     (set! tmp-ra.20 r15)
                     (begin
                       (set! rdi 1)
                       (set! rsi 2)
                       (set! rdx 3)
                       (set! r15 tmp-ra.20)
                       (jump L.f.1 rbp r15 rdi rsi rdx)))))
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda (x.1) (begin
                                                                                   (set! y.1 100)
                                                                                   (* x.1 y.1))))
                                               (define L.g.1 (lambda (x.1 y.1) (if (not (> x.1 y.1))
                                                                                   (call L.f.1 x.1)
                                                                                   (call L.f.1 y.1))))
                                               (begin
                                                 (set! x.1 1)
                                                 (set! x.2 2)
                                                 (if (<= x.1 x.2)
                                                     (call L.f.1 x.1)
                                                     (call L.g.1 x.2 x.1)))))
                '(module
                     ((new-frames ()))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.21 r15)
                       (begin
                         (set! x.1 rdi)
                         (begin
                           (set! y.1 100)
                           (begin (set! rax (* x.1 y.1)) (jump tmp-ra.21 rbp rax))))))
                   (define L.g.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.22 r15)
                       (begin
                         (set! x.1 rdi)
                         (set! y.1 rsi)
                         (if (not (> x.1 y.1))
                             (begin (set! rdi x.1) (set! r15 tmp-ra.22) (jump L.f.1 rbp r15 rdi))
                             (begin
                               (set! rdi y.1)
                               (set! r15 tmp-ra.22)
                               (jump L.f.1 rbp r15 rdi))))))
                   (begin
                     (set! tmp-ra.23 r15)
                     (begin
                       (set! x.1 1)
                       (set! x.2 2)
                       (if (<= x.1 x.2)
                           (begin (set! rdi x.1) (set! r15 tmp-ra.23) (jump L.f.1 rbp r15 rdi))
                           (begin
                             (set! rdi x.2)
                             (set! rsi x.1)
                             (set! r15 tmp-ra.23)
                             (jump L.g.1 rbp r15 rdi rsi)))))))
  (check-equal? (parameterize ([current-parameter-registers '()])
                  (impose-calling-conventions '(module
                                                   (define L.f.1 (lambda (x.1 y.1) (+ x.1 y.1)))
                                                 (define L.g.1 (lambda (x.1)
                                                                 (begin
                                                                   (set! y.1 (call L.f.1 x.1 1))
                                                                   (set! z.1 (call L.f.1 x.1 2))
                                                                   (if (true)
                                                                       (begin
                                                                         (set! a.1 (call L.f.1 y.1 z.1))
                                                                         (* a.1 x.1))
                                                                       (begin
                                                                         (set! b.1 (call L.f.1 y.1 x.1))
                                                                         (- b.1 z.1))))))
                                                 (begin
                                                   (set! x.1 (call L.g.1 1))
                                                   (set! x.2 (call L.g.1 2))
                                                   (* x.1 x.2)))))
                '(module
                     ((new-frames ((nfv.36) (nfv.35))))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.24 r15)
                       (begin
                         (set! x.1 fv1)
                         (set! y.1 fv0)
                         (begin (set! rax (+ x.1 y.1)) (jump tmp-ra.24 rbp rax)))))
                   (define L.g.1
                     ((new-frames
                       ((nfv.32 nfv.33) (nfv.30 nfv.31) (nfv.28 nfv.29) (nfv.26 nfv.27))))
                     (begin
                       (set! tmp-ra.25 r15)
                       (begin
                         (set! x.1 fv0)
                         (begin
                           (begin
                             (return-point L.rp.6
                                           (begin
                                             (set! nfv.26 x.1)
                                             (set! nfv.27 1)
                                             (set! r15 L.rp.6)
                                             (jump L.f.1 rbp r15 nfv.26 nfv.27)))
                             (set! y.1 rax))
                           (begin
                             (return-point L.rp.7
                                           (begin
                                             (set! nfv.28 x.1)
                                             (set! nfv.29 2)
                                             (set! r15 L.rp.7)
                                             (jump L.f.1 rbp r15 nfv.28 nfv.29)))
                             (set! z.1 rax))
                           (if (true)
                               (begin
                                 (begin
                                   (return-point L.rp.8
                                                 (begin
                                                   (set! nfv.30 y.1)
                                                   (set! nfv.31 z.1)
                                                   (set! r15 L.rp.8)
                                                   (jump L.f.1 rbp r15 nfv.30 nfv.31)))
                                   (set! a.1 rax))
                                 (begin (set! rax (* a.1 x.1)) (jump tmp-ra.25 rbp rax)))
                               (begin
                                 (begin
                                   (return-point L.rp.9
                                                 (begin
                                                   (set! nfv.32 y.1)
                                                   (set! nfv.33 x.1)
                                                   (set! r15 L.rp.9)
                                                   (jump L.f.1 rbp r15 nfv.32 nfv.33)))
                                   (set! b.1 rax))
                                 (begin (set! rax (- b.1 z.1)) (jump tmp-ra.25 rbp rax))))))))
                   (begin
                     (set! tmp-ra.34 r15)
                     (begin
                       (begin
                         (return-point L.rp.10
                                       (begin
                                         (set! nfv.35 1)
                                         (set! r15 L.rp.10)
                                         (jump L.g.1 rbp r15 nfv.35)))
                         (set! x.1 rax))
                       (begin
                         (return-point L.rp.11
                                       (begin
                                         (set! nfv.36 2)
                                         (set! r15 L.rp.11)
                                         (jump L.g.1 rbp r15 nfv.36)))
                         (set! x.2 rax))
                       (begin (set! rax (* x.1 x.2)) (jump tmp-ra.34 rbp rax))))))
  (check-equal? (impose-calling-conventions '(module
                                                 (define L.f.1
                                                   (lambda (x.1)
                                                     (begin
                                                       (set! y.1 1)
                                                       (set! z.1 2)
                                                       (begin
                                                         (set! a.1 (bitwise-and y.1 x.1))
                                                         (set! b.1 (bitwise-ior z.1 x.1))
                                                         (begin
                                                           (set! a.1 (bitwise-xor a.1 b.1))
                                                           (arithmetic-shift-right a.1 3))))))
                                               (begin
                                                 (set! x.2 10)
                                                 (if (begin (set! x.3 100) (not (!= x.2 x.3)))
                                                     (call L.f.1 x.2)
                                                     (call L.f.2 1000)))))
                '(module
                     ((new-frames ()))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.37 r15)
                       (begin
                         (set! x.1 rdi)
                         (begin
                           (set! y.1 1)
                           (set! z.1 2)
                           (begin
                             (set! a.1 (bitwise-and y.1 x.1))
                             (set! b.1 (bitwise-ior z.1 x.1))
                             (begin
                               (set! a.1 (bitwise-xor a.1 b.1))
                               (begin
                                 (set! rax (arithmetic-shift-right a.1 3))
                                 (jump tmp-ra.37 rbp rax))))))))
                   (begin
                     (set! tmp-ra.38 r15)
                     (begin
                       (set! x.2 10)
                       (if (begin (set! x.3 100) (not (!= x.2 x.3)))
                           (begin (set! rdi x.2) (set! r15 tmp-ra.38) (jump L.f.1 rbp r15 rdi))
                           (begin
                             (set! rdi 1000)
                             (set! r15 tmp-ra.38)
                             (jump L.f.2 rbp r15 rdi)))))))
  (check-equal? (impose-calling-conventions '(module
                                                 (define L.*.2
                                                   (lambda (tmp.1 tmp.2)
                                                     (if (begin
                                                           (if (begin (set! tmp.24 (bitwise-and tmp.2 7)) (= tmp.24 0))
                                                               (set! tmp.23 14)
                                                               (set! tmp.23 6))
                                                           (!= tmp.23 6))
                                                         (if (begin
                                                               (if (begin (set! tmp.26 (bitwise-and tmp.1 7)) (= tmp.26 0))
                                                                   (set! tmp.25 14)
                                                                   (set! tmp.25 6))
                                                               (!= tmp.25 6))
                                                             (begin
                                                               (set! tmp.27 (arithmetic-shift-right tmp.2 3))
                                                               (* tmp.1 tmp.27))
                                                             318)
                                                         318)))
                                               (define L.+.1
                                                 (lambda (tmp.3 tmp.4)
                                                   (if (begin
                                                         (if (begin (set! tmp.29 (bitwise-and tmp.4 7)) (= tmp.29 0))
                                                             (set! tmp.28 14)
                                                             (set! tmp.28 6))
                                                         (!= tmp.28 6))
                                                       (if (begin
                                                             (if (begin (set! tmp.31 (bitwise-and tmp.3 7)) (= tmp.31 0))
                                                                 (set! tmp.30 14)
                                                                 (set! tmp.30 6))
                                                             (!= tmp.30 6))
                                                           (+ tmp.3 tmp.4)
                                                           574)
                                                       574)))
                                               (define L.add.10
                                                 (lambda (a.61 b.62 c.63 d.64 e.65 f.66 g.67 h.68)
                                                   (begin
                                                     (begin
                                                       (begin
                                                         (begin
                                                           (begin
                                                             (begin
                                                               (set! tmp.37 (call L.+.1 g.67 h.68))
                                                               (set! tmp.36 (call L.+.1 f.66 tmp.37)))
                                                             (set! tmp.35 (call L.+.1 e.65 tmp.36)))
                                                           (set! tmp.34 (call L.+.1 d.64 tmp.35)))
                                                         (set! tmp.33 (call L.+.1 c.63 tmp.34)))
                                                       (set! tmp.32 (call L.+.1 b.62 tmp.33)))
                                                     (call L.+.1 a.61 tmp.32))))
                                               (define L.add-and-multiply.11
                                                 (lambda (a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76 i.77)
                                                   (begin
                                                     (set! sum.78 (call L.add.10 a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76))
                                                     (call L.*.2 sum.78 i.77))))
                                               (call L.add-and-multiply.11 8 16 24 32 40 48 56 64 16)))
                '(module
                     ((new-frames ()))
                   (define L.*.2
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.39 r15)
                       (begin
                         (set! tmp.1 rdi)
                         (set! tmp.2 rsi)
                         (if (begin
                               (if (begin (set! tmp.24 (bitwise-and tmp.2 7)) (= tmp.24 0))
                                   (set! tmp.23 14)
                                   (set! tmp.23 6))
                               (!= tmp.23 6))
                             (if (begin
                                   (if (begin (set! tmp.26 (bitwise-and tmp.1 7)) (= tmp.26 0))
                                       (set! tmp.25 14)
                                       (set! tmp.25 6))
                                   (!= tmp.25 6))
                                 (begin
                                   (set! tmp.27 (arithmetic-shift-right tmp.2 3))
                                   (begin (set! rax (* tmp.1 tmp.27)) (jump tmp-ra.39 rbp rax)))
                                 (begin (set! rax 318) (jump tmp-ra.39 rbp rax)))
                             (begin (set! rax 318) (jump tmp-ra.39 rbp rax))))))
                   (define L.+.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.40 r15)
                       (begin
                         (set! tmp.3 rdi)
                         (set! tmp.4 rsi)
                         (if (begin
                               (if (begin (set! tmp.29 (bitwise-and tmp.4 7)) (= tmp.29 0))
                                   (set! tmp.28 14)
                                   (set! tmp.28 6))
                               (!= tmp.28 6))
                             (if (begin
                                   (if (begin (set! tmp.31 (bitwise-and tmp.3 7)) (= tmp.31 0))
                                       (set! tmp.30 14)
                                       (set! tmp.30 6))
                                   (!= tmp.30 6))
                                 (begin (set! rax (+ tmp.3 tmp.4)) (jump tmp-ra.40 rbp rax))
                                 (begin (set! rax 574) (jump tmp-ra.40 rbp rax)))
                             (begin (set! rax 574) (jump tmp-ra.40 rbp rax))))))
                   (define L.add.10
                     ((new-frames (() () () () () ())))
                     (begin
                       (set! tmp-ra.41 r15)
                       (begin
                         (set! a.61 rdi)
                         (set! b.62 rsi)
                         (set! c.63 rdx)
                         (set! d.64 rcx)
                         (set! e.65 r8)
                         (set! f.66 r9)
                         (set! g.67 fv1)
                         (set! h.68 fv0)
                         (begin
                           (begin
                             (begin
                               (begin
                                 (begin
                                   (begin
                                     (begin
                                       (return-point L.rp.12
                                                     (begin
                                                       (set! rdi g.67)
                                                       (set! rsi h.68)
                                                       (set! r15 L.rp.12)
                                                       (jump L.+.1 rbp r15 rdi rsi)))
                                       (set! tmp.37 rax))
                                     (begin
                                       (return-point L.rp.13
                                                     (begin
                                                       (set! rdi f.66)
                                                       (set! rsi tmp.37)
                                                       (set! r15 L.rp.13)
                                                       (jump L.+.1 rbp r15 rdi rsi)))
                                       (set! tmp.36 rax)))
                                   (begin
                                     (return-point L.rp.14
                                                   (begin
                                                     (set! rdi e.65)
                                                     (set! rsi tmp.36)
                                                     (set! r15 L.rp.14)
                                                     (jump L.+.1 rbp r15 rdi rsi)))
                                     (set! tmp.35 rax)))
                                 (begin
                                   (return-point L.rp.15
                                                 (begin
                                                   (set! rdi d.64)
                                                   (set! rsi tmp.35)
                                                   (set! r15 L.rp.15)
                                                   (jump L.+.1 rbp r15 rdi rsi)))
                                   (set! tmp.34 rax)))
                               (begin
                                 (return-point L.rp.16
                                               (begin
                                                 (set! rdi c.63)
                                                 (set! rsi tmp.34)
                                                 (set! r15 L.rp.16)
                                                 (jump L.+.1 rbp r15 rdi rsi)))
                                 (set! tmp.33 rax)))
                             (begin
                               (return-point L.rp.17
                                             (begin
                                               (set! rdi b.62)
                                               (set! rsi tmp.33)
                                               (set! r15 L.rp.17)
                                               (jump L.+.1 rbp r15 rdi rsi)))
                               (set! tmp.32 rax)))
                           (begin
                             (set! rdi a.61)
                             (set! rsi tmp.32)
                             (set! r15 tmp-ra.41)
                             (jump L.+.1 rbp r15 rdi rsi))))))
                   (define L.add-and-multiply.11
                     ((new-frames ((nfv.43 nfv.44))))
                     (begin
                       (set! tmp-ra.42 r15)
                       (begin
                         (set! a.69 rdi)
                         (set! b.70 rsi)
                         (set! c.71 rdx)
                         (set! d.72 rcx)
                         (set! e.73 r8)
                         (set! f.74 r9)
                         (set! g.75 fv2)
                         (set! h.76 fv1)
                         (set! i.77 fv0)
                         (begin
                           (begin
                             (return-point L.rp.18
                                           (begin
                                             (set! rdi a.69)
                                             (set! rsi b.70)
                                             (set! rdx c.71)
                                             (set! rcx d.72)
                                             (set! r8 e.73)
                                             (set! r9 f.74)
                                             (set! nfv.43 g.75)
                                             (set! nfv.44 h.76)
                                             (set! r15 L.rp.18)
                                             (jump L.add.10 rbp r15 rdi rsi rdx rcx r8 r9 nfv.43 nfv.44)))
                             (set! sum.78 rax))
                           (begin
                             (set! rdi sum.78)
                             (set! rsi i.77)
                             (set! r15 tmp-ra.42)
                             (jump L.*.2 rbp r15 rdi rsi))))))
                   (begin
                     (set! tmp-ra.45 r15)
                     (begin
                       (set! rdi 8)
                       (set! rsi 16)
                       (set! rdx 24)
                       (set! rcx 32)
                       (set! r8 40)
                       (set! r9 48)
                       (set! fv0 56)
                       (set! fv1 64)
                       (set! fv2 16)
                       (set! r15 tmp-ra.45)
                       (jump
                        L.add-and-multiply.11
                        rbp
                        r15
                        rdi
                        rsi
                        rdx
                        rcx
                        r8
                        r9
                        fv0
                        fv1
                        fv2)))))
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda ()
                                                                     (begin
                                                                       (set! x.1 1)
                                                                       x.1)))
                                               (begin
                                                 (set! x.1 (call L.f.1))
                                                 x.1)))
                '(module
                     ((new-frames (())))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.46 r15)
                       (begin
                         (begin (set! x.1 1) (begin (set! rax x.1) (jump tmp-ra.46 rbp rax))))))
                   (begin
                     (set! tmp-ra.47 r15)
                     (begin
                       (begin
                         (return-point L.rp.19 (begin (set! r15 L.rp.19) (jump L.f.1 rbp r15)))
                         (set! x.1 rax))
                       (begin (set! rax x.1) (jump tmp-ra.47 rbp rax))))))
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda () (if (true) 0 1)))
                                               (begin
                                                 (set! x.1 (call L.f.1))
                                                 x.1)))
                '(module
                     ((new-frames (())))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.48 r15)
                       (begin
                         (if (true)
                             (begin (set! rax 0) (jump tmp-ra.48 rbp rax))
                             (begin (set! rax 1) (jump tmp-ra.48 rbp rax))))))
                   (begin
                     (set! tmp-ra.49 r15)
                     (begin
                       (begin
                         (return-point L.rp.20 (begin (set! r15 L.rp.20) (jump L.f.1 rbp r15)))
                         (set! x.1 rax))
                       (begin (set! rax x.1) (jump tmp-ra.49 rbp rax))))))
  (check-equal? (impose-calling-conventions '(module
                                                 (define L.f.1
                                                   (lambda (x.1 x.2)
                                                     (begin
                                                       (begin
                                                         (begin (set! tmp.39 (+ 10 6)) (set! tmp.38 (alloc tmp.39)))
                                                         (begin
                                                           (set! tmp.40 (call L.g.1))
                                                           (if (true) (mset! tmp.38 tmp.40 x.1) (mset! tmp.38 tmp.40 x.2))))
                                                       (begin
                                                         (begin (set! tmp.42 (+ 10 6)) (set! tmp.41 (alloc tmp.42)))
                                                         (begin (set! tmp.43 (bitwise-and 8 8)) (mref tmp.41 tmp.43))))))
                                               (define L.g.1 (lambda () 8))
                                               (call L.f.1 1 2)))
                '(module
                     ((new-frames ()))
                   (define L.f.1
                     ((new-frames (())))
                     (begin
                       (set! tmp-ra.50 r15)
                       (begin
                         (set! x.1 rdi)
                         (set! x.2 rsi)
                         (begin
                           (begin
                             (begin (set! tmp.39 (+ 10 6)) (set! tmp.38 (alloc tmp.39)))
                             (begin
                               (begin
                                 (return-point L.rp.21
                                               (begin (set! r15 L.rp.21) (jump L.g.1 rbp r15)))
                                 (set! tmp.40 rax))
                               (if (true)
                                   (mset! tmp.38 tmp.40 x.1)
                                   (mset! tmp.38 tmp.40 x.2))))
                           (begin
                             (begin (set! tmp.42 (+ 10 6)) (set! tmp.41 (alloc tmp.42)))
                             (begin
                               (set! tmp.43 (bitwise-and 8 8))
                               (begin
                                 (set! rax (mref tmp.41 tmp.43))
                                 (jump tmp-ra.50 rbp rax))))))))
                   (define L.g.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.51 r15)
                       (begin (begin (set! rax 8) (jump tmp-ra.51 rbp rax)))))
                   (begin
                     (set! tmp-ra.52 r15)
                     (begin
                       (set! rdi 1)
                       (set! rsi 2)
                       (set! r15 tmp-ra.52)
                       (jump L.f.1 rbp r15 rdi rsi))))))
