#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8
  rackunit
  "../passes/uncover-locals.rkt")

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
                     (jump L.+.16 rbp r15 rdi rsi))))
  (check-equal? (uncover-locals '(module
                                     ((new-frames ()))
                                   (define L.+.31
                                     ((new-frames ()))
                                     (begin
                                       (set! tmp-ra.232 r15)
                                       (set! tmp.96 rdi)
                                       (set! tmp.97 rsi)
                                       (if (begin
                                             (if (begin
                                                   (set! tmp.184 tmp.97)
                                                   (set! tmp.184 (bitwise-and tmp.184 7))
                                                   (= tmp.184 0))
                                                 (set! tmp.183 14)
                                                 (set! tmp.183 6))
                                             (!= tmp.183 6))
                                           (if (begin
                                                 (if (begin
                                                       (set! tmp.186 tmp.96)
                                                       (set! tmp.186 (bitwise-and tmp.186 7))
                                                       (= tmp.186 0))
                                                     (set! tmp.185 14)
                                                     (set! tmp.185 6))
                                                 (!= tmp.185 6))
                                               (begin
                                                 (set! rax tmp.96)
                                                 (set! rax (+ rax tmp.97))
                                                 (jump tmp-ra.232 rbp rax))
                                               (begin (set! rax 574) (jump tmp-ra.232 rbp rax)))
                                           (begin (set! rax 574) (jump tmp-ra.232 rbp rax)))))
                                   (define L.F.6
                                     ((new-frames ()))
                                     (begin
                                       (set! tmp-ra.233 r15)
                                       (set! a.19 rdi)
                                       (set! b.20 rsi)
                                       (set! c.21 rdx)
                                       (set! d.22 rcx)
                                       (set! e.23 r8)
                                       (set! f.24 r9)
                                       (set! g.25 fv0)
                                       (return-point L.rp.47
                                                     (begin
                                                       (set! rdi a.19)
                                                       (set! rsi b.20)
                                                       (set! rdx c.21)
                                                       (set! rcx d.22)
                                                       (set! r8 e.23)
                                                       (set! r9 f.24)
                                                       (set! nfv.234 g.25)
                                                       (set! nfv.235 64)
                                                       (set! r15 L.rp.47)
                                                       (jump L.G.7 rbp r15 rdi rsi rdx rcx r8 r9 nfv.234 nfv.235)))
                                       (set! tmp.187 rax)
                                       (set! rdi 80)
                                       (set! rsi tmp.187)
                                       (set! r15 tmp-ra.233)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                                   (define L.G.7
                                     ((new-frames ()))
                                     (begin
                                       (set! tmp-ra.236 r15)
                                       (set! a.26 rdi)
                                       (set! b.27 rsi)
                                       (set! c.28 rdx)
                                       (set! d.29 rcx)
                                       (set! e.30 r8)
                                       (set! f.31 r9)
                                       (set! g.32 fv0)
                                       (set! h.33 fv1)
                                       (set! rdi a.26)
                                       (set! rsi b.27)
                                       (set! rdx c.28)
                                       (set! rcx d.29)
                                       (set! r8 e.30)
                                       (set! r9 f.31)
                                       (set! fv0 g.32)
                                       (set! fv1 h.33)
                                       (set! fv2 72)
                                       (set! r15 tmp-ra.236)
                                       (jump L.H.8 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2)))
                                   (define L.H.8
                                     ((new-frames ()))
                                     (begin
                                       (set! tmp-ra.237 r15)
                                       (set! a.34 rdi)
                                       (set! b.35 rsi)
                                       (set! c.36 rdx)
                                       (set! d.37 rcx)
                                       (set! e.38 r8)
                                       (set! f.39 r9)
                                       (set! g.40 fv0)
                                       (set! h.41 fv1)
                                       (set! j.42 fv2)
                                       (return-point L.rp.48
                                                     (begin
                                                       (set! rdi a.34)
                                                       (set! rsi b.35)
                                                       (set! r15 L.rp.48)
                                                       (jump L.+.31 rbp r15 rdi rsi)))
                                       (set! r1.43 rax)
                                       (return-point L.rp.49
                                                     (begin
                                                       (set! rdi r1.43)
                                                       (set! rsi c.36)
                                                       (set! r15 L.rp.49)
                                                       (jump L.+.31 rbp r15 rdi rsi)))
                                       (set! r2.44 rax)
                                       (return-point L.rp.50
                                                     (begin
                                                       (set! rdi r2.44)
                                                       (set! rsi d.37)
                                                       (set! r15 L.rp.50)
                                                       (jump L.+.31 rbp r15 rdi rsi)))
                                       (set! r3.45 rax)
                                       (return-point L.rp.51
                                                     (begin
                                                       (set! rdi r3.45)
                                                       (set! rsi e.38)
                                                       (set! r15 L.rp.51)
                                                       (jump L.+.31 rbp r15 rdi rsi)))
                                       (set! r4.46 rax)
                                       (return-point L.rp.52
                                                     (begin
                                                       (set! rdi r4.46)
                                                       (set! rsi f.39)
                                                       (set! r15 L.rp.52)
                                                       (jump L.+.31 rbp r15 rdi rsi)))
                                       (set! r5.47 rax)
                                       (return-point L.rp.53
                                                     (begin
                                                       (set! rdi r5.47)
                                                       (set! rsi g.40)
                                                       (set! r15 L.rp.53)
                                                       (jump L.+.31 rbp r15 rdi rsi)))
                                       (set! r6.48 rax)
                                       (return-point L.rp.54
                                                     (begin
                                                       (set! rdi r6.48)
                                                       (set! rsi h.41)
                                                       (set! r15 L.rp.54)
                                                       (jump L.+.31 rbp r15 rdi rsi)))
                                       (set! r7.49 rax)
                                       (set! rdi r7.49)
                                       (set! rsi j.42)
                                       (set! r15 tmp-ra.237)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                                   (begin
                                     (set! tmp-ra.238 r15)
                                     (set! rdi 8)
                                     (set! rsi 16)
                                     (set! rdx 24)
                                     (set! rcx 32)
                                     (set! r8 40)
                                     (set! r9 48)
                                     (set! fv0 56)
                                     (set! r15 tmp-ra.238)
                                     (jump L.F.6 rbp r15 rdi rsi rdx rcx r8 r9 fv0))))
                '(module
                     ((new-frames ()) (locals (tmp-ra.238)))
                   (define L.+.31
                     ((new-frames ())
                      (locals (tmp.183 tmp.96 tmp.185 tmp.184 tmp.97 tmp-ra.232 tmp.186)))
                     (begin
                       (set! tmp-ra.232 r15)
                       (set! tmp.96 rdi)
                       (set! tmp.97 rsi)
                       (if (begin
                             (if (begin
                                   (set! tmp.184 tmp.97)
                                   (set! tmp.184 (bitwise-and tmp.184 7))
                                   (= tmp.184 0))
                                 (set! tmp.183 14)
                                 (set! tmp.183 6))
                             (!= tmp.183 6))
                           (if (begin
                                 (if (begin
                                       (set! tmp.186 tmp.96)
                                       (set! tmp.186 (bitwise-and tmp.186 7))
                                       (= tmp.186 0))
                                     (set! tmp.185 14)
                                     (set! tmp.185 6))
                                 (!= tmp.185 6))
                               (begin
                                 (set! rax tmp.96)
                                 (set! rax (+ rax tmp.97))
                                 (jump tmp-ra.232 rbp rax))
                               (begin (set! rax 574) (jump tmp-ra.232 rbp rax)))
                           (begin (set! rax 574) (jump tmp-ra.232 rbp rax)))))
                   (define L.F.6
                     ((new-frames ())
                      (locals
                       (b.20
                        f.24
                        g.25
                        a.19
                        nfv.234
                        d.22
                        nfv.235
                        e.23
                        tmp-ra.233
                        tmp.187
                        c.21)))
                     (begin
                       (set! tmp-ra.233 r15)
                       (set! a.19 rdi)
                       (set! b.20 rsi)
                       (set! c.21 rdx)
                       (set! d.22 rcx)
                       (set! e.23 r8)
                       (set! f.24 r9)
                       (set! g.25 fv0)
                       (return-point L.rp.47
                                     (begin
                                       (set! rdi a.19)
                                       (set! rsi b.20)
                                       (set! rdx c.21)
                                       (set! rcx d.22)
                                       (set! r8 e.23)
                                       (set! r9 f.24)
                                       (set! nfv.234 g.25)
                                       (set! nfv.235 64)
                                       (set! r15 L.rp.47)
                                       (jump L.G.7 rbp r15 rdi rsi rdx rcx r8 r9 nfv.234 nfv.235)))
                       (set! tmp.187 rax)
                       (set! rdi 80)
                       (set! rsi tmp.187)
                       (set! r15 tmp-ra.233)
                       (jump L.+.31 rbp r15 rdi rsi)))
                   (define L.G.7
                     ((new-frames ())
                      (locals (g.32 c.28 tmp-ra.236 h.33 f.31 e.30 b.27 d.29 a.26)))
                     (begin
                       (set! tmp-ra.236 r15)
                       (set! a.26 rdi)
                       (set! b.27 rsi)
                       (set! c.28 rdx)
                       (set! d.29 rcx)
                       (set! e.30 r8)
                       (set! f.31 r9)
                       (set! g.32 fv0)
                       (set! h.33 fv1)
                       (set! rdi a.26)
                       (set! rsi b.27)
                       (set! rdx c.28)
                       (set! rcx d.29)
                       (set! r8 e.30)
                       (set! r9 f.31)
                       (set! fv0 g.32)
                       (set! fv1 h.33)
                       (set! fv2 72)
                       (set! r15 tmp-ra.236)
                       (jump L.H.8 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2)))
                   (define L.H.8
                     ((new-frames ())
                      (locals
                       (tmp-ra.237
                        d.37
                        r5.47
                        f.39
                        r6.48
                        r3.45
                        r4.46
                        a.34
                        j.42
                        g.40
                        r2.44
                        e.38
                        b.35
                        h.41
                        c.36
                        r7.49
                        r1.43)))
                     (begin
                       (set! tmp-ra.237 r15)
                       (set! a.34 rdi)
                       (set! b.35 rsi)
                       (set! c.36 rdx)
                       (set! d.37 rcx)
                       (set! e.38 r8)
                       (set! f.39 r9)
                       (set! g.40 fv0)
                       (set! h.41 fv1)
                       (set! j.42 fv2)
                       (return-point L.rp.48
                                     (begin
                                       (set! rdi a.34)
                                       (set! rsi b.35)
                                       (set! r15 L.rp.48)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                       (set! r1.43 rax)
                       (return-point L.rp.49
                                     (begin
                                       (set! rdi r1.43)
                                       (set! rsi c.36)
                                       (set! r15 L.rp.49)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                       (set! r2.44 rax)
                       (return-point L.rp.50
                                     (begin
                                       (set! rdi r2.44)
                                       (set! rsi d.37)
                                       (set! r15 L.rp.50)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                       (set! r3.45 rax)
                       (return-point L.rp.51
                                     (begin
                                       (set! rdi r3.45)
                                       (set! rsi e.38)
                                       (set! r15 L.rp.51)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                       (set! r4.46 rax)
                       (return-point L.rp.52
                                     (begin
                                       (set! rdi r4.46)
                                       (set! rsi f.39)
                                       (set! r15 L.rp.52)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                       (set! r5.47 rax)
                       (return-point L.rp.53
                                     (begin
                                       (set! rdi r5.47)
                                       (set! rsi g.40)
                                       (set! r15 L.rp.53)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                       (set! r6.48 rax)
                       (return-point L.rp.54
                                     (begin
                                       (set! rdi r6.48)
                                       (set! rsi h.41)
                                       (set! r15 L.rp.54)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                       (set! r7.49 rax)
                       (set! rdi r7.49)
                       (set! rsi j.42)
                       (set! r15 tmp-ra.237)
                       (jump L.+.31 rbp r15 rdi rsi)))
                   (begin
                     (set! tmp-ra.238 r15)
                     (set! rdi 8)
                     (set! rsi 16)
                     (set! rdx 24)
                     (set! rcx 32)
                     (set! r8 40)
                     (set! r9 48)
                     (set! fv0 56)
                     (set! r15 tmp-ra.238)
                     (jump L.F.6 rbp r15 rdi rsi rdx rcx r8 r9 fv0))))
  (check-equal? (uncover-locals '(module
                                     ((new-frames ()))
                                   (define L.*.2
                                     ((new-frames ()))
                                     (begin
                                       (set! tmp-ra.39 r15)
                                       (set! tmp.1 rdi)
                                       (set! tmp.2 rsi)
                                       (if (begin
                                             (if (begin
                                                   (begin
                                                     (set! tmp.24 tmp.2)
                                                     (set! tmp.24 (bitwise-and tmp.24 7)))
                                                   (= tmp.24 0))
                                                 (set! tmp.23 14)
                                                 (set! tmp.23 6))
                                             (!= tmp.23 6))
                                           (if (begin
                                                 (if (begin
                                                       (begin
                                                         (set! tmp.26 tmp.1)
                                                         (set! tmp.26 (bitwise-and tmp.26 7)))
                                                       (= tmp.26 0))
                                                     (set! tmp.25 14)
                                                     (set! tmp.25 6))
                                                 (!= tmp.25 6))
                                               (begin
                                                 (set! tmp.27 tmp.2)
                                                 (set! tmp.27 (arithmetic-shift-right tmp.27 3))
                                                 (set! rax tmp.1)
                                                 (set! rax (* rax tmp.27))
                                                 (jump tmp-ra.39 rbp rax))
                                               (begin (set! rax 318) (jump tmp-ra.39 rbp rax)))
                                           (begin (set! rax 318) (jump tmp-ra.39 rbp rax)))))
                                   (define L.+.1
                                     ((new-frames ()))
                                     (begin
                                       (set! tmp-ra.40 r15)
                                       (set! tmp.3 rdi)
                                       (set! tmp.4 rsi)
                                       (if (begin
                                             (if (begin
                                                   (begin
                                                     (set! tmp.29 tmp.4)
                                                     (set! tmp.29 (bitwise-and tmp.29 7)))
                                                   (= tmp.29 0))
                                                 (set! tmp.28 14)
                                                 (set! tmp.28 6))
                                             (!= tmp.28 6))
                                           (if (begin
                                                 (if (begin
                                                       (begin
                                                         (set! tmp.31 tmp.3)
                                                         (set! tmp.31 (bitwise-and tmp.31 7)))
                                                       (= tmp.31 0))
                                                     (set! tmp.30 14)
                                                     (set! tmp.30 6))
                                                 (!= tmp.30 6))
                                               (begin
                                                 (set! rax tmp.3)
                                                 (set! rax (+ rax tmp.4))
                                                 (jump tmp-ra.40 rbp rax))
                                               (begin (set! rax 574) (jump tmp-ra.40 rbp rax)))
                                           (begin (set! rax 574) (jump tmp-ra.40 rbp rax)))))
                                   (define L.add.10
                                     ((new-frames (() () () () () ())))
                                     (begin
                                       (set! tmp-ra.41 r15)
                                       (set! a.61 rdi)
                                       (set! b.62 rsi)
                                       (set! c.63 rdx)
                                       (set! d.64 rcx)
                                       (set! e.65 r8)
                                       (set! f.66 r9)
                                       (set! g.67 fv0)
                                       (set! h.68 fv1)
                                       (return-point L.rp.12
                                                     (begin
                                                       (set! rdi g.67)
                                                       (set! rsi h.68)
                                                       (set! r15 L.rp.12)
                                                       (jump L.+.1 rbp r15 rdi rsi)))
                                       (set! tmp.37 rax)
                                       (return-point L.rp.13
                                                     (begin
                                                       (set! rdi f.66)
                                                       (set! rsi tmp.37)
                                                       (set! r15 L.rp.13)
                                                       (jump L.+.1 rbp r15 rdi rsi)))
                                       (set! tmp.36 rax)
                                       (return-point L.rp.14
                                                     (begin
                                                       (set! rdi e.65)
                                                       (set! rsi tmp.36)
                                                       (set! r15 L.rp.14)
                                                       (jump L.+.1 rbp r15 rdi rsi)))
                                       (set! tmp.35 rax)
                                       (return-point L.rp.15
                                                     (begin
                                                       (set! rdi d.64)
                                                       (set! rsi tmp.35)
                                                       (set! r15 L.rp.15)
                                                       (jump L.+.1 rbp r15 rdi rsi)))
                                       (set! tmp.34 rax)
                                       (return-point L.rp.16
                                                     (begin
                                                       (set! rdi c.63)
                                                       (set! rsi tmp.34)
                                                       (set! r15 L.rp.16)
                                                       (jump L.+.1 rbp r15 rdi rsi)))
                                       (set! tmp.33 rax)
                                       (return-point L.rp.17
                                                     (begin
                                                       (set! rdi b.62)
                                                       (set! rsi tmp.33)
                                                       (set! r15 L.rp.17)
                                                       (jump L.+.1 rbp r15 rdi rsi)))
                                       (set! tmp.32 rax)
                                       (set! rdi a.61)
                                       (set! rsi tmp.32)
                                       (set! r15 tmp-ra.41)
                                       (jump L.+.1 rbp r15 rdi rsi)))
                                   (define L.add-and-multiply.11
                                     ((new-frames ((nfv.43 nfv.44))))
                                     (begin
                                       (set! tmp-ra.42 r15)
                                       (set! a.69 rdi)
                                       (set! b.70 rsi)
                                       (set! c.71 rdx)
                                       (set! d.72 rcx)
                                       (set! e.73 r8)
                                       (set! f.74 r9)
                                       (set! g.75 fv0)
                                       (set! h.76 fv1)
                                       (set! i.77 fv2)
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
                                       (set! sum.78 rax)
                                       (set! rdi sum.78)
                                       (set! rsi i.77)
                                       (set! r15 tmp-ra.42)
                                       (jump L.*.2 rbp r15 rdi rsi)))
                                   (begin
                                     (set! tmp-ra.45 r15)
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
                                     (jump L.add-and-multiply.11 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2))))
                '(module
                     ((new-frames ()) (locals (tmp-ra.45)))
                   (define L.*.2
                     ((new-frames ())
                      (locals (tmp.24 tmp.27 tmp.2 tmp.25 tmp-ra.39 tmp.23 tmp.1 tmp.26)))
                     (begin
                       (set! tmp-ra.39 r15)
                       (set! tmp.1 rdi)
                       (set! tmp.2 rsi)
                       (if (begin
                             (if (begin
                                   (begin
                                     (set! tmp.24 tmp.2)
                                     (set! tmp.24 (bitwise-and tmp.24 7)))
                                   (= tmp.24 0))
                                 (set! tmp.23 14)
                                 (set! tmp.23 6))
                             (!= tmp.23 6))
                           (if (begin
                                 (if (begin
                                       (begin
                                         (set! tmp.26 tmp.1)
                                         (set! tmp.26 (bitwise-and tmp.26 7)))
                                       (= tmp.26 0))
                                     (set! tmp.25 14)
                                     (set! tmp.25 6))
                                 (!= tmp.25 6))
                               (begin
                                 (set! tmp.27 tmp.2)
                                 (set! tmp.27 (arithmetic-shift-right tmp.27 3))
                                 (set! rax tmp.1)
                                 (set! rax (* rax tmp.27))
                                 (jump tmp-ra.39 rbp rax))
                               (begin (set! rax 318) (jump tmp-ra.39 rbp rax)))
                           (begin (set! rax 318) (jump tmp-ra.39 rbp rax)))))
                   (define L.+.1
                     ((new-frames ())
                      (locals (tmp.4 tmp.31 tmp-ra.40 tmp.3 tmp.28 tmp.30 tmp.29)))
                     (begin
                       (set! tmp-ra.40 r15)
                       (set! tmp.3 rdi)
                       (set! tmp.4 rsi)
                       (if (begin
                             (if (begin
                                   (begin
                                     (set! tmp.29 tmp.4)
                                     (set! tmp.29 (bitwise-and tmp.29 7)))
                                   (= tmp.29 0))
                                 (set! tmp.28 14)
                                 (set! tmp.28 6))
                             (!= tmp.28 6))
                           (if (begin
                                 (if (begin
                                       (begin
                                         (set! tmp.31 tmp.3)
                                         (set! tmp.31 (bitwise-and tmp.31 7)))
                                       (= tmp.31 0))
                                     (set! tmp.30 14)
                                     (set! tmp.30 6))
                                 (!= tmp.30 6))
                               (begin
                                 (set! rax tmp.3)
                                 (set! rax (+ rax tmp.4))
                                 (jump tmp-ra.40 rbp rax))
                               (begin (set! rax 574) (jump tmp-ra.40 rbp rax)))
                           (begin (set! rax 574) (jump tmp-ra.40 rbp rax)))))
                   (define L.add.10
                     ((new-frames (() () () () () ()))
                      (locals
                       (e.65
                        c.63
                        tmp.34
                        tmp.37
                        g.67
                        tmp.35
                        tmp.36
                        a.61
                        tmp.32
                        h.68
                        f.66
                        d.64
                        b.62
                        tmp-ra.41
                        tmp.33)))
                     (begin
                       (set! tmp-ra.41 r15)
                       (set! a.61 rdi)
                       (set! b.62 rsi)
                       (set! c.63 rdx)
                       (set! d.64 rcx)
                       (set! e.65 r8)
                       (set! f.66 r9)
                       (set! g.67 fv0)
                       (set! h.68 fv1)
                       (return-point L.rp.12
                                     (begin
                                       (set! rdi g.67)
                                       (set! rsi h.68)
                                       (set! r15 L.rp.12)
                                       (jump L.+.1 rbp r15 rdi rsi)))
                       (set! tmp.37 rax)
                       (return-point L.rp.13
                                     (begin
                                       (set! rdi f.66)
                                       (set! rsi tmp.37)
                                       (set! r15 L.rp.13)
                                       (jump L.+.1 rbp r15 rdi rsi)))
                       (set! tmp.36 rax)
                       (return-point L.rp.14
                                     (begin
                                       (set! rdi e.65)
                                       (set! rsi tmp.36)
                                       (set! r15 L.rp.14)
                                       (jump L.+.1 rbp r15 rdi rsi)))
                       (set! tmp.35 rax)
                       (return-point L.rp.15
                                     (begin
                                       (set! rdi d.64)
                                       (set! rsi tmp.35)
                                       (set! r15 L.rp.15)
                                       (jump L.+.1 rbp r15 rdi rsi)))
                       (set! tmp.34 rax)
                       (return-point L.rp.16
                                     (begin
                                       (set! rdi c.63)
                                       (set! rsi tmp.34)
                                       (set! r15 L.rp.16)
                                       (jump L.+.1 rbp r15 rdi rsi)))
                       (set! tmp.33 rax)
                       (return-point L.rp.17
                                     (begin
                                       (set! rdi b.62)
                                       (set! rsi tmp.33)
                                       (set! r15 L.rp.17)
                                       (jump L.+.1 rbp r15 rdi rsi)))
                       (set! tmp.32 rax)
                       (set! rdi a.61)
                       (set! rsi tmp.32)
                       (set! r15 tmp-ra.41)
                       (jump L.+.1 rbp r15 rdi rsi)))
                   (define L.add-and-multiply.11
                     ((new-frames ((nfv.43 nfv.44)))
                      (locals
                       (a.69
                        g.75
                        sum.78
                        h.76
                        i.77
                        tmp-ra.42
                        c.71
                        nfv.44
                        d.72
                        b.70
                        f.74
                        e.73
                        nfv.43)))
                     (begin
                       (set! tmp-ra.42 r15)
                       (set! a.69 rdi)
                       (set! b.70 rsi)
                       (set! c.71 rdx)
                       (set! d.72 rcx)
                       (set! e.73 r8)
                       (set! f.74 r9)
                       (set! g.75 fv0)
                       (set! h.76 fv1)
                       (set! i.77 fv2)
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
                       (set! sum.78 rax)
                       (set! rdi sum.78)
                       (set! rsi i.77)
                       (set! r15 tmp-ra.42)
                       (jump L.*.2 rbp r15 rdi rsi)))
                   (begin
                     (set! tmp-ra.45 r15)
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
                     (jump L.add-and-multiply.11 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2))))
  (check-equal? (uncover-locals '(module
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
                                     (jump L.f.1 rbp r15 rdi rsi))))
                '(module
                     ((new-frames ()) (locals (tmp-ra.52)))
                   (define L.f.1
                     ((new-frames (()))
                      (locals (tmp.41 x.1 x.2 tmp.40 tmp.43 tmp.39 tmp.42 tmp-ra.50 tmp.38)))
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
                     ((new-frames ()) (locals (tmp-ra.51)))
                     (begin (set! tmp-ra.51 r15) (set! rax 8) (jump tmp-ra.51 rbp rax)))
                   (begin
                     (set! tmp-ra.52 r15)
                     (set! rdi 1)
                     (set! rsi 2)
                     (set! r15 tmp-ra.52)
                     (jump L.f.1 rbp r15 rdi rsi)))))
