#lang racket

(require rackunit
         cpsc411/graph-lib
         "../passes/conflict-analysis.rkt")

(module+ test
  (check-equal? (conflict-analysis '(module
                                        ((new-frames ())
                                         (locals (ra.12))
                                         (call-undead ())
                                         (undead-out ((ra.12 rbp) (ra.12 fv0 rbp) (fv0 r15 rbp) (fv0 r15 rbp))))
                                      (define L.fact.4
                                        ((new-frames ((nfv.16)))
                                         (locals (ra.13 x.9 tmp.14 tmp.15 new-n.10 nfv.16 factn-1.11 tmp.17))
                                         (undead-out
                                          ((r15 x.9 rbp)
                                           (x.9 ra.13 rbp)
                                           ((x.9 ra.13 rbp)
                                            ((ra.13 rax rbp) (rax rbp))
                                            ((tmp.14 x.9 ra.13 rbp)
                                             (tmp.14 tmp.15 x.9 ra.13 rbp)
                                             (tmp.15 x.9 ra.13 rbp)
                                             (new-n.10 x.9 ra.13 rbp)
                                             ((rax x.9 ra.13 rbp) ((nfv.16 rbp) (nfv.16 r15 rbp) (nfv.16 r15 rbp)))
                                             (x.9 factn-1.11 ra.13 rbp)
                                             (factn-1.11 tmp.17 ra.13 rbp)
                                             (tmp.17 ra.13 rbp)
                                             (ra.13 rax rbp)
                                             (rax rbp)))))
                                         (call-undead (x.9 ra.13)))
                                        (begin
                                          (set! x.9 fv0)
                                          (set! ra.13 r15)
                                          (if (= x.9 0)
                                              (begin (set! rax 1) (jump ra.13 rbp rax))
                                              (begin
                                                (set! tmp.14 -1)
                                                (set! tmp.15 x.9)
                                                (set! tmp.15 (+ tmp.15 tmp.14))
                                                (set! new-n.10 tmp.15)
                                                (return-point L.rp.6
                                                              (begin
                                                                (set! nfv.16 new-n.10)
                                                                (set! r15 L.rp.6)
                                                                (jump L.fact.4 rbp r15 nfv.16)))
                                                (set! factn-1.11 rax)
                                                (set! tmp.17 x.9)
                                                (set! tmp.17 (* tmp.17 factn-1.11))
                                                (set! rax tmp.17)
                                                (jump ra.13 rbp rax)))))
                                      (begin
                                        (set! ra.12 r15)
                                        (set! fv0 5)
                                        (set! r15 ra.12)
                                        (jump L.fact.4 rbp r15 fv0))))
                '(module
                     ((new-frames ())
                      (locals (ra.12))
                      (call-undead ())
                      (undead-out ((ra.12 rbp) (ra.12 fv0 rbp) (fv0 r15 rbp) (fv0 r15 rbp)))
                      (conflicts
                       ((ra.12 (fv0 rbp))
                        (rbp (r15 fv0 ra.12))
                        (fv0 (r15 ra.12 rbp))
                        (r15 (fv0 rbp)))))
                   (define L.fact.4
                     ((new-frames ((nfv.16)))
                      (locals (ra.13 x.9 tmp.14 tmp.15 new-n.10 nfv.16 factn-1.11 tmp.17))
                      (undead-out
                       ((r15 x.9 rbp)
                        (x.9 ra.13 rbp)
                        ((x.9 ra.13 rbp)
                         ((ra.13 rax rbp) (rax rbp))
                         ((tmp.14 x.9 ra.13 rbp)
                          (tmp.14 tmp.15 x.9 ra.13 rbp)
                          (tmp.15 x.9 ra.13 rbp)
                          (new-n.10 x.9 ra.13 rbp)
                          ((rax x.9 ra.13 rbp)
                           ((nfv.16 rbp) (nfv.16 r15 rbp) (nfv.16 r15 rbp)))
                          (x.9 factn-1.11 ra.13 rbp)
                          (factn-1.11 tmp.17 ra.13 rbp)
                          (tmp.17 ra.13 rbp)
                          (ra.13 rax rbp)
                          (rax rbp)))))
                      (call-undead (x.9 ra.13))
                      (conflicts
                       ((tmp.17 (factn-1.11 ra.13 rbp))
                        (factn-1.11 (tmp.17 x.9 ra.13 rbp))
                        (nfv.16 (r15 rbp))
                        (new-n.10 (x.9 ra.13 rbp))
                        (tmp.15 (x.9 tmp.14 ra.13 rbp))
                        (tmp.14 (tmp.15 x.9 ra.13 rbp))
                        (x.9 (factn-1.11 new-n.10 tmp.15 tmp.14 ra.13 r15 rbp))
                        (ra.13 (tmp.17 factn-1.11 new-n.10 tmp.15 tmp.14 rax x.9 rbp))
                        (rbp
                         (tmp.17 factn-1.11 r15 nfv.16 new-n.10 tmp.15 tmp.14 rax ra.13 x.9))
                        (r15 (nfv.16 rbp x.9))
                        (rax (ra.13 rbp)))))
                     (begin
                       (set! x.9 fv0)
                       (set! ra.13 r15)
                       (if (= x.9 0)
                           (begin (set! rax 1) (jump ra.13 rbp rax))
                           (begin
                             (set! tmp.14 -1)
                             (set! tmp.15 x.9)
                             (set! tmp.15 (+ tmp.15 tmp.14))
                             (set! new-n.10 tmp.15)
                             (return-point L.rp.6
                                           (begin
                                             (set! nfv.16 new-n.10)
                                             (set! r15 L.rp.6)
                                             (jump L.fact.4 rbp r15 nfv.16)))
                             (set! factn-1.11 rax)
                             (set! tmp.17 x.9)
                             (set! tmp.17 (* tmp.17 factn-1.11))
                             (set! rax tmp.17)
                             (jump ra.13 rbp rax)))))
                   (begin
                     (set! ra.12 r15)
                     (set! fv0 5)
                     (set! r15 ra.12)
                     (jump L.fact.4 rbp r15 fv0))))
  (check-equal? (conflict-analysis '(module
                                        ((new-frames ())
                                         (locals ())
                                         (call-undead ())
                                         (undead-out (rbp)))
                                      (define L.f.1
                                        ((new-frames ())
                                         (locals (tmp.1))
                                         (call-undead ())
                                         (undead-out ((tmp.1) (tmp.1) ())))
                                        (begin (set! tmp.1 1) (set! tmp.1 (* tmp.1 2)) (jump ra.13 rbp rax)))
                                      (jump L.f.1 rbp)))
                '(module
                     ((new-frames ())
                      (locals ())
                      (call-undead ())
                      (undead-out (rbp))
                      (conflicts ()))
                   (define L.f.1
                     ((new-frames ())
                      (locals (tmp.1))
                      (call-undead ())
                      (undead-out ((tmp.1) (tmp.1) ()))
                      (conflicts ((tmp.1 ()))))
                     (begin (set! tmp.1 1) (set! tmp.1 (* tmp.1 2)) (jump ra.13 rbp rax)))
                   (jump L.f.1 rbp)))
  (check-equal? (conflict-analysis '(module
                                        ((locals ())
                                         (new-frames ())
                                         (call-undead ())
                                         (undead-out
                                          ((r9 rbp)
                                           (r9 r8 rbp)
                                           (r9 r8 rcx rbp)
                                           (r9 r8 rcx rdx rbp)
                                           (r9 r8 rcx rdx rsi rbp)
                                           (r9 r8 rcx rdx rsi rdi rbp)
                                           (r9 r8 rcx rdx rsi rdi rbp))))
                                      (define L.f.1
                                        ((locals (f.1 e.1 d.1 c.1 b.1 a.1))
                                         (new-frames ())
                                         (call-undead ())
                                         (undead-out
                                          ((rsi rdx rcx r8 r9 a.1)
                                           (rdx rcx r8 r9 b.1 a.1)
                                           (rcx r8 r9 b.1 a.1 c.1)
                                           (r8 r9 b.1 a.1 c.1 d.1)
                                           (r9 b.1 a.1 c.1 d.1 e.1)
                                           (b.1 a.1 c.1 d.1 e.1 f.1)
                                           (c.1 a.1 d.1 e.1 f.1)
                                           (d.1 a.1 e.1 f.1)
                                           (e.1 a.1 f.1)
                                           (f.1 a.1)
                                           (a.1)
                                           ())))
                                        (begin
                                          (set! a.1 rdi)
                                          (set! b.1 rsi)
                                          (set! c.1 rdx)
                                          (set! d.1 rcx)
                                          (set! e.1 r8)
                                          (set! f.1 r9)
                                          (set! a.1 (+ a.1 b.1))
                                          (set! a.1 (+ a.1 c.1))
                                          (set! a.1 (+ a.1 d.1))
                                          (set! a.1 (+ a.1 e.1))
                                          (set! a.1 (+ a.1 f.1))
                                          (jump ra.13 a.1)))
                                      (begin
                                        (set! r9 6)
                                        (set! r8 5)
                                        (set! rcx 4)
                                        (set! rdx 3)
                                        (set! rsi 2)
                                        (set! rdi 1)
                                        (jump L.f.1 rbp rdi rsi rdx rcx r8 r9))))
                '(module
                     ((locals ())
                      (new-frames ())
                      (call-undead ())
                      (undead-out
                       ((r9 rbp)
                        (r9 r8 rbp)
                        (r9 r8 rcx rbp)
                        (r9 r8 rcx rdx rbp)
                        (r9 r8 rcx rdx rsi rbp)
                        (r9 r8 rcx rdx rsi rdi rbp)
                        (r9 r8 rcx rdx rsi rdi rbp)))
                      (conflicts
                       ((r9 (rdi rsi rdx rcx r8 rbp))
                        (rbp (rdi rsi rdx rcx r8 r9))
                        (r8 (rdi rsi rdx rcx r9 rbp))
                        (rcx (rdi rsi rdx r9 r8 rbp))
                        (rdx (rdi rsi r9 r8 rcx rbp))
                        (rsi (rdi r9 r8 rcx rdx rbp))
                        (rdi (r9 r8 rcx rdx rsi rbp)))))
                   (define L.f.1
                     ((locals (f.1 e.1 d.1 c.1 b.1 a.1))
                      (new-frames ())
                      (call-undead ())
                      (undead-out
                       ((rsi rdx rcx r8 r9 a.1)
                        (rdx rcx r8 r9 b.1 a.1)
                        (rcx r8 r9 b.1 a.1 c.1)
                        (r8 r9 b.1 a.1 c.1 d.1)
                        (r9 b.1 a.1 c.1 d.1 e.1)
                        (b.1 a.1 c.1 d.1 e.1 f.1)
                        (c.1 a.1 d.1 e.1 f.1)
                        (d.1 a.1 e.1 f.1)
                        (e.1 a.1 f.1)
                        (f.1 a.1)
                        (a.1)
                        ()))
                      (conflicts
                       ((a.1 (f.1 e.1 d.1 c.1 b.1 rsi rdx rcx r8 r9))
                        (b.1 (f.1 e.1 d.1 c.1 rdx rcx r8 r9 a.1))
                        (c.1 (f.1 e.1 d.1 rcx r8 r9 b.1 a.1))
                        (d.1 (f.1 e.1 r8 r9 b.1 a.1 c.1))
                        (e.1 (f.1 r9 b.1 a.1 c.1 d.1))
                        (f.1 (b.1 a.1 c.1 d.1 e.1))
                        (r9 (e.1 d.1 c.1 b.1 a.1))
                        (r8 (d.1 c.1 b.1 a.1))
                        (rcx (c.1 b.1 a.1))
                        (rdx (b.1 a.1))
                        (rsi (a.1)))))
                     (begin
                       (set! a.1 rdi)
                       (set! b.1 rsi)
                       (set! c.1 rdx)
                       (set! d.1 rcx)
                       (set! e.1 r8)
                       (set! f.1 r9)
                       (set! a.1 (+ a.1 b.1))
                       (set! a.1 (+ a.1 c.1))
                       (set! a.1 (+ a.1 d.1))
                       (set! a.1 (+ a.1 e.1))
                       (set! a.1 (+ a.1 f.1))
                       (jump ra.13 a.1)))
                   (begin
                     (set! r9 6)
                     (set! r8 5)
                     (set! rcx 4)
                     (set! rdx 3)
                     (set! rsi 2)
                     (set! rdi 1)
                     (jump L.f.1 rbp rdi rsi rdx rcx r8 r9))))
  (check-equal? (conflict-analysis '(module
                                        ((locals (k.1 j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                                         (new-frames ())
                                         (call-undead ())
                                         (undead-out
                                          ((a.1 rbp)
                                           (b.1 a.1 rbp)
                                           (c.1 b.1 a.1 rbp)
                                           (d.1 c.1 b.1 a.1 rbp)
                                           (e.1 d.1 c.1 b.1 a.1 rbp)
                                           (f.1 e.1 d.1 c.1 b.1 a.1 rbp)
                                           (g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp)
                                           (h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp)
                                           (i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp)
                                           (j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp)
                                           (k.1 j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp)
                                           (j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 fv4 rbp)
                                           (i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 fv4 fv3 rbp)
                                           (h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 rbp)
                                           (g.1 f.1 e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 rbp)
                                           (f.1 e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 rbp)
                                           (e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 rbp)
                                           (d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rbp)
                                           (c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rbp)
                                           (b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rdx rbp)
                                           (a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rdx rsi rbp)
                                           (fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)
                                           (fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp))))
                                      (define L.f.1
                                        ((locals (k.1 j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                                         (new-frames ())
                                         (call-undead ())
                                         (undead-out
                                          ((rsi rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4)
                                           (rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4)
                                           (rcx r8 r9 fv0 fv1 fv2 fv3 fv4)
                                           (r8 r9 fv0 fv1 fv2 fv3 fv4)
                                           (r9 fv0 fv1 fv2 fv3 fv4)
                                           (fv0 fv1 fv2 fv3 fv4)
                                           (fv1 fv2 fv3 fv4)
                                           (fv2 fv3 fv4)
                                           (fv3 fv4)
                                           (fv4)
                                           ()
                                           ())))
                                        (begin
                                          (set! a.1 rdi)
                                          (set! b.1 rsi)
                                          (set! c.1 rdx)
                                          (set! d.1 rcx)
                                          (set! e.1 r8)
                                          (set! f.1 r9)
                                          (set! g.1 fv0)
                                          (set! h.1 fv1)
                                          (set! i.1 fv2)
                                          (set! j.1 fv3)
                                          (set! k.1 fv4)
                                          (jump done)))
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
                                        (set! fv4 k.1)
                                        (set! fv3 j.1)
                                        (set! fv2 i.1)
                                        (set! fv1 h.1)
                                        (set! fv0 g.1)
                                        (set! r9 f.1)
                                        (set! r8 e.1)
                                        (set! rcx d.1)
                                        (set! rdx c.1)
                                        (set! rsi b.1)
                                        (set! rdi a.1)
                                        (jump L.f.1 rbp rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4))))
                '(module
                     ((locals (k.1 j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                      (new-frames ())
                      (call-undead ())
                      (undead-out
                       ((a.1 rbp)
                        (b.1 a.1 rbp)
                        (c.1 b.1 a.1 rbp)
                        (d.1 c.1 b.1 a.1 rbp)
                        (e.1 d.1 c.1 b.1 a.1 rbp)
                        (f.1 e.1 d.1 c.1 b.1 a.1 rbp)
                        (g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp)
                        (h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp)
                        (i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp)
                        (j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp)
                        (k.1 j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp)
                        (j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 fv4 rbp)
                        (i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 fv4 fv3 rbp)
                        (h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 rbp)
                        (g.1 f.1 e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 rbp)
                        (f.1 e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 rbp)
                        (e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 rbp)
                        (d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rbp)
                        (c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rbp)
                        (b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rdx rbp)
                        (a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rdx rsi rbp)
                        (fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)
                        (fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)))
                      (conflicts
                       ((a.1
                         (rsi
                          rdx
                          rcx
                          r8
                          r9
                          fv0
                          fv1
                          fv2
                          fv3
                          fv4
                          k.1
                          j.1
                          i.1
                          h.1
                          g.1
                          f.1
                          e.1
                          d.1
                          c.1
                          b.1
                          rbp))
                        (b.1
                         (rdx
                          rcx
                          r8
                          r9
                          fv0
                          fv1
                          fv2
                          fv3
                          fv4
                          k.1
                          j.1
                          i.1
                          h.1
                          g.1
                          f.1
                          e.1
                          d.1
                          c.1
                          a.1
                          rbp))
                        (c.1
                         (rcx
                          r8
                          r9
                          fv0
                          fv1
                          fv2
                          fv3
                          fv4
                          k.1
                          j.1
                          i.1
                          h.1
                          g.1
                          f.1
                          e.1
                          d.1
                          b.1
                          a.1
                          rbp))
                        (d.1
                         (r8 r9 fv0 fv1 fv2 fv3 fv4 k.1 j.1 i.1 h.1 g.1 f.1 e.1 c.1 b.1 a.1 rbp))
                        (e.1
                         (r9 fv0 fv1 fv2 fv3 fv4 k.1 j.1 i.1 h.1 g.1 f.1 d.1 c.1 b.1 a.1 rbp))
                        (f.1 (fv0 fv1 fv2 fv3 fv4 k.1 j.1 i.1 h.1 g.1 e.1 d.1 c.1 b.1 a.1 rbp))
                        (g.1 (fv1 fv2 fv3 fv4 k.1 j.1 i.1 h.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp))
                        (h.1 (fv2 fv3 fv4 k.1 j.1 i.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp))
                        (i.1 (fv3 fv4 k.1 j.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp))
                        (j.1 (fv4 k.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp))
                        (k.1 (j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp))
                        (rbp
                         (rdi
                          rsi
                          rdx
                          rcx
                          r8
                          r9
                          fv0
                          fv1
                          fv2
                          fv3
                          fv4
                          k.1
                          j.1
                          i.1
                          h.1
                          g.1
                          f.1
                          e.1
                          d.1
                          c.1
                          b.1
                          a.1))
                        (fv4
                         (rdi
                          rsi
                          rdx
                          rcx
                          r8
                          r9
                          fv0
                          fv1
                          fv2
                          fv3
                          j.1
                          i.1
                          h.1
                          g.1
                          f.1
                          e.1
                          d.1
                          c.1
                          b.1
                          a.1
                          rbp))
                        (fv3
                         (rdi
                          rsi
                          rdx
                          rcx
                          r8
                          r9
                          fv0
                          fv1
                          fv2
                          i.1
                          h.1
                          g.1
                          f.1
                          e.1
                          d.1
                          c.1
                          b.1
                          a.1
                          fv4
                          rbp))
                        (fv2
                         (rdi
                          rsi
                          rdx
                          rcx
                          r8
                          r9
                          fv0
                          fv1
                          h.1
                          g.1
                          f.1
                          e.1
                          d.1
                          c.1
                          b.1
                          a.1
                          fv4
                          fv3
                          rbp))
                        (fv1
                         (rdi rsi rdx rcx r8 r9 fv0 g.1 f.1 e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 rbp))
                        (fv0 (rdi rsi rdx rcx r8 r9 f.1 e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 rbp))
                        (r9 (rdi rsi rdx rcx r8 e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 rbp))
                        (r8 (rdi rsi rdx rcx d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 rbp))
                        (rcx (rdi rsi rdx c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rbp))
                        (rdx (rdi rsi b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rbp))
                        (rsi (rdi a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rdx rbp))
                        (rdi (fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rdx rsi rbp)))))
                   (define L.f.1
                     ((locals (k.1 j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                      (new-frames ())
                      (call-undead ())
                      (undead-out
                       ((rsi rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4)
                        (rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4)
                        (rcx r8 r9 fv0 fv1 fv2 fv3 fv4)
                        (r8 r9 fv0 fv1 fv2 fv3 fv4)
                        (r9 fv0 fv1 fv2 fv3 fv4)
                        (fv0 fv1 fv2 fv3 fv4)
                        (fv1 fv2 fv3 fv4)
                        (fv2 fv3 fv4)
                        (fv3 fv4)
                        (fv4)
                        ()
                        ()))
                      (conflicts
                       ((a.1 (rsi rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4))
                        (b.1 (rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4))
                        (c.1 (rcx r8 r9 fv0 fv1 fv2 fv3 fv4))
                        (d.1 (r8 r9 fv0 fv1 fv2 fv3 fv4))
                        (e.1 (r9 fv0 fv1 fv2 fv3 fv4))
                        (f.1 (fv0 fv1 fv2 fv3 fv4))
                        (g.1 (fv1 fv2 fv3 fv4))
                        (h.1 (fv2 fv3 fv4))
                        (i.1 (fv3 fv4))
                        (j.1 (fv4))
                        (k.1 ())
                        (fv4 (j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                        (fv3 (i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                        (fv2 (h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                        (fv1 (g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                        (fv0 (f.1 e.1 d.1 c.1 b.1 a.1))
                        (r9 (e.1 d.1 c.1 b.1 a.1))
                        (r8 (d.1 c.1 b.1 a.1))
                        (rcx (c.1 b.1 a.1))
                        (rdx (b.1 a.1))
                        (rsi (a.1)))))
                     (begin
                       (set! a.1 rdi)
                       (set! b.1 rsi)
                       (set! c.1 rdx)
                       (set! d.1 rcx)
                       (set! e.1 r8)
                       (set! f.1 r9)
                       (set! g.1 fv0)
                       (set! h.1 fv1)
                       (set! i.1 fv2)
                       (set! j.1 fv3)
                       (set! k.1 fv4)
                       (jump done)))
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
                     (set! fv4 k.1)
                     (set! fv3 j.1)
                     (set! fv2 i.1)
                     (set! fv1 h.1)
                     (set! fv0 g.1)
                     (set! r9 f.1)
                     (set! r8 e.1)
                     (set! rcx d.1)
                     (set! rdx c.1)
                     (set! rsi b.1)
                     (set! rdi a.1)
                     (jump L.f.1 rbp rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4))))
  (check-equal? (conflict-analysis '(module ((locals ())
                                             (new-frames ())
                                             (call-undead ())
                                             (undead-out ((rdi rbp) (rdi rbp))))
                                      (define L.f.1
                                        ((locals (x.1))
                                         (new-frames ())
                                         (call-undead ())
                                         (undead-out ((x.1) ())))
                                        (begin (set! x.1 rdi) (jump done x.1)))
                                      (begin (set! rdi 1) (jump L.f.1 rbp rdi))))
                '(module
                     ((locals ())
                      (new-frames ())
                      (call-undead ())
                      (undead-out ((rdi rbp) (rdi rbp)))
                      (conflicts ((rdi (rbp)) (rbp (rdi)))))
                   (define L.f.1
                     ((locals (x.1))
                      (new-frames ())
                      (call-undead ())
                      (undead-out ((x.1) ()))
                      (conflicts ((x.1 ()))))
                     (begin (set! x.1 rdi) (jump done x.1)))
                   (begin (set! rdi 1) (jump L.f.1 rbp rdi))))
  (check-equal? (conflict-analysis '(module
                                        ((locals (a.1))
                                         (new-frames ())
                                         (call-undead ())
                                         (undead-out ((rbp a.1) (rdi rbp a.1) (rdi rbp a.1))))
                                      (define L.f.1
                                        ((locals (x.1))
                                         (new-frames ())
                                         (call-undead ())
                                         (undead-out ((x.1) ())))
                                        (begin (set! x.1 rdi) (jump done x.1)))
                                      (begin (set! a.1 L.f.1) (set! rdi 1) (jump a.1 rbp rdi))))
                '(module
                     ((locals (a.1))
                      (new-frames ())
                      (call-undead ())
                      (undead-out ((rbp a.1) (rdi rbp a.1) (rdi rbp a.1)))
                      (conflicts ((a.1 (rdi rbp)) (rbp (rdi a.1)) (rdi (rbp a.1)))))
                   (define L.f.1
                     ((locals (x.1))
                      (new-frames ())
                      (call-undead ())
                      (undead-out ((x.1) ()))
                      (conflicts ((x.1 ()))))
                     (begin (set! x.1 rdi) (jump done x.1)))
                   (begin (set! a.1 L.f.1) (set! rdi 1) (jump a.1 rbp rdi))))
  (check-equal? (conflict-analysis '(module
                                        ((locals ())
                                         (new-frames ())
                                         (call-undead ())
                                         (undead-out ((rbp r13) (rdi rbp r13) (rdi rbp r13))))
                                      (define L.f.1
                                        ((locals (x.1))
                                         (new-frames ())
                                         (call-undead ())
                                         (undead-out ((x.1) ())))
                                        (begin (set! x.1 rdi) (jump done x.1)))
                                      (begin (set! r13 L.f.1) (set! rdi 1) (jump r13 rbp rdi))))
                '(module
                     ((locals ())
                      (new-frames ())
                      (call-undead ())
                      (undead-out ((rbp r13) (rdi rbp r13) (rdi rbp r13)))
                      (conflicts ((r13 (rdi rbp)) (rbp (rdi r13)) (rdi (rbp r13)))))
                   (define L.f.1
                     ((locals (x.1))
                      (new-frames ())
                      (call-undead ())
                      (undead-out ((x.1) ()))
                      (conflicts ((x.1 ()))))
                     (begin (set! x.1 rdi) (jump done x.1)))
                   (begin (set! r13 L.f.1) (set! rdi 1) (jump r13 rbp rdi))))
  (check-equal? (conflict-analysis '(module
                                        ((locals ())
                                         (new-frames ())
                                         (call-undead ())
                                         (undead-out
                                          ((rbp)
                                           ((rdx rbp) (rdx rsi rbp) (rdx rsi rdi rbp) (rdx rsi rdi rbp))
                                           ((rdi rbp) (rdi rbp)))))
                                      (define L.f.1
                                        ((locals (x.1))
                                         (new-frames ())
                                         (call-undead ())
                                         (undead-out ((x.1) ())))
                                        (begin (set! x.1 rdi) (jump done x.1)))
                                      (define L.g.1
                                        ((locals (y.1 x.1 z.1))
                                         (new-frames ())
                                         (call-undead ())
                                         (undead-out
                                          ((rsi rdx x.1 rbp) (rdx x.1 rbp) (x.1 rbp) (rdi rbp) (rdi rbp))))
                                        (begin
                                          (set! x.1 rdi)
                                          (set! y.1 rsi)
                                          (set! z.1 rdx)
                                          (set! rdi x.1)
                                          (jump L.f.1 rbp rdi)))
                                      (if (true)
                                          (begin (set! rdx 3) (set! rsi 2) (set! rdi 1) (jump L.g.1 rbp rdi rsi rdx))
                                          (begin (set! rdi 1) (jump L.f.1 rbp rdi)))))
                '(module
                     ((locals ())
                      (new-frames ())
                      (call-undead ())
                      (undead-out
                       ((rbp)
                        ((rdx rbp) (rdx rsi rbp) (rdx rsi rdi rbp) (rdx rsi rdi rbp))
                        ((rdi rbp) (rdi rbp))))
                      (conflicts
                       ((rdx (rdi rsi rbp))
                        (rbp (rdi rsi rdx))
                        (rsi (rdi rdx rbp))
                        (rdi (rdx rsi rbp)))))
                   (define L.f.1
                     ((locals (x.1))
                      (new-frames ())
                      (call-undead ())
                      (undead-out ((x.1) ()))
                      (conflicts ((x.1 ()))))
                     (begin (set! x.1 rdi) (jump done x.1)))
                   (define L.g.1
                     ((locals (y.1 x.1 z.1))
                      (new-frames ())
                      (call-undead ())
                      (undead-out
                       ((rsi rdx x.1 rbp) (rdx x.1 rbp) (x.1 rbp) (rdi rbp) (rdi rbp)))
                      (conflicts
                       ((z.1 (x.1 rbp))
                        (x.1 (z.1 y.1 rsi rdx rbp))
                        (y.1 (rdx x.1 rbp))
                        (rbp (rdi z.1 y.1 x.1))
                        (rdx (y.1 x.1))
                        (rsi (x.1))
                        (rdi (rbp)))))
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
  (check-equal? (conflict-analysis '(module ((locals (x.1))
                                             (new-frames ())
                                             (call-undead ())
                                             (undead-out ((x.1) ())))
                                      (begin (set! x.1 42) (jump done x.1))))
                '(module ((locals (x.1))
                          (new-frames ())
                          (call-undead ())
                          (undead-out ((x.1) ()))
                          (conflicts ((x.1 ()))))
                   (begin (set! x.1 42) (jump done x.1))))

  (match (conflict-analysis '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                                      (new-frames ())
                                      (call-undead ())
                                      (undead-out ((v.1)
                                                   (v.1 w.2)
                                                   (w.2 x.3)
                                                   (p.1 w.2 x.3)
                                                   (w.2 x.3)
                                                   (y.4 w.2 x.3)
                                                   (p.1 y.4 w.2 x.3)
                                                   (y.4 w.2 x.3)
                                                   (z.5 y.4 w.2)
                                                   (z.5 y.4)
                                                   (t.6 z.5)
                                                   (t.6 z.5 p.1)
                                                   (t.6 z.5)
                                                   (z.5)
                                                   ())))
                               (begin (set! v.1 1)
                                      (set! w.2 46)
                                      (set! x.3 v.1)
                                      (set! p.1 7)
                                      (set! x.3 (+ x.3 p.1))
                                      (set! y.4 x.3)
                                      (set! p.1 4)
                                      (set! y.4 (+ y.4 p.1))
                                      (set! z.5 x.3)
                                      (set! z.5 (+ z.5 w.2))
                                      (set! t.6 y.4)
                                      (set! p.1 -1)
                                      (set! t.6 (* t.6 p.1))
                                      (set! z.5 (+ z.5 t.6))
                                      (jump done z.5))))
    [`(module ((locals ,ls) (new-frames ()) (call-undead ()) (undead-out ,udt) (conflicts ,conflicts)) ,tail)
     (check-true (set=? (get-neighbors conflicts 'v.1) (list 'w.2)))
     (check-true (set=? (get-neighbors conflicts 'w.2) (list 'z.5 'y.4 'p.1 'x.3 'v.1)))
     (check-true (set=? (get-neighbors conflicts 'x.3) (list 'y.4 'p.1 'w.2)))
     (check-true (set=? (get-neighbors conflicts 'y.4) (list 'z.5 'x.3 'p.1 'w.2)))
     (check-true (set=? (get-neighbors conflicts 'z.5) (list 'p.1 't.6 'w.2 'y.4)))
     (check-true (set=? (get-neighbors conflicts 't.6) (list 'p.1 'z.5)))
     (check-true (set=? (get-neighbors conflicts 'p.1) (list 'z.5 't.6 'y.4 'x.3 'w.2)))])
  (check-equal? (conflict-analysis '(module
                                        ((new-frames ())
                                         (locals (x.3 tmp-ra.2 x.2))
                                         (call-undead ())
                                         (undead-out
                                          ((tmp-ra.2 rbp)
                                           (x.2 tmp-ra.2 rbp)
                                           (((x.3 x.2 tmp-ra.2 rbp) (x.2 tmp-ra.2 rbp))
                                            ((tmp-ra.2 rdi rbp) (rdi r15 rbp) (rbp r15 rdi))
                                            ((tmp-ra.2 rdi rbp) (rdi r15 rbp) (rbp r15 rdi))))))
                                      (define L.f.1
                                        ((new-frames ())
                                         (locals (tmp-ra.1 b.1 y.1 x.1 z.1 a.1))
                                         (undead-out
                                          ((rdi rbp tmp-ra.1)
                                           (x.1 rbp tmp-ra.1)
                                           (y.1 x.1 rbp tmp-ra.1)
                                           (y.1 z.1 x.1 rbp tmp-ra.1)
                                           (a.1 z.1 x.1 rbp tmp-ra.1)
                                           (z.1 x.1 a.1 rbp tmp-ra.1)
                                           (x.1 b.1 a.1 rbp tmp-ra.1)
                                           (b.1 a.1 rbp tmp-ra.1)
                                           (a.1 rbp tmp-ra.1)
                                           (rax rbp tmp-ra.1)
                                           (rax rbp tmp-ra.1)
                                           (rbp rax)))
                                         (call-undead ()))
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
                     ((new-frames ())
                      (locals (x.3 tmp-ra.2 x.2))
                      (call-undead ())
                      (undead-out
                       ((tmp-ra.2 rbp)
                        (x.2 tmp-ra.2 rbp)
                        (((x.3 x.2 tmp-ra.2 rbp) (x.2 tmp-ra.2 rbp))
                         ((tmp-ra.2 rdi rbp) (rdi r15 rbp) (rbp r15 rdi))
                         ((tmp-ra.2 rdi rbp) (rdi r15 rbp) (rbp r15 rdi)))))
                      (conflicts
                       ((x.2 (x.3 tmp-ra.2 rbp))
                        (tmp-ra.2 (rdi x.3 x.2 rbp))
                        (x.3 (x.2 tmp-ra.2 rbp))
                        (rbp (r15 rdi x.3 x.2 tmp-ra.2))
                        (rdi (r15 tmp-ra.2 rbp))
                        (r15 (rdi rbp)))))
                   (define L.f.1
                     ((new-frames ())
                      (locals (tmp-ra.1 b.1 y.1 x.1 z.1 a.1))
                      (undead-out
                       ((rdi rbp tmp-ra.1)
                        (x.1 rbp tmp-ra.1)
                        (y.1 x.1 rbp tmp-ra.1)
                        (y.1 z.1 x.1 rbp tmp-ra.1)
                        (a.1 z.1 x.1 rbp tmp-ra.1)
                        (z.1 x.1 a.1 rbp tmp-ra.1)
                        (x.1 b.1 a.1 rbp tmp-ra.1)
                        (b.1 a.1 rbp tmp-ra.1)
                        (a.1 rbp tmp-ra.1)
                        (rax rbp tmp-ra.1)
                        (rax rbp tmp-ra.1)
                        (rbp rax)))
                      (call-undead ())
                      (conflicts
                       ((a.1 (b.1 z.1 x.1 rbp tmp-ra.1))
                        (z.1 (a.1 y.1 x.1 rbp tmp-ra.1))
                        (x.1 (b.1 a.1 z.1 y.1 rbp tmp-ra.1))
                        (y.1 (z.1 x.1 rbp tmp-ra.1))
                        (b.1 (x.1 a.1 rbp tmp-ra.1))
                        (tmp-ra.1 (rax b.1 a.1 z.1 y.1 x.1 rdi rbp))
                        (rbp (rax b.1 a.1 z.1 y.1 x.1 tmp-ra.1))
                        (rdi (tmp-ra.1))
                        (rax (rbp tmp-ra.1)))))
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
  (check-equal? (conflict-analysis '(module
                                        ((new-frames ())
                                         (locals (tmp-ra.52))
                                         (call-undead ())
                                         (undead-out
                                          ((tmp-ra.52 rbp)
                                           (tmp-ra.52 rdi rbp)
                                           (tmp-ra.52 rsi rdi rbp)
                                           (rsi rdi r15 rbp)
                                           (rbp r15 rdi rsi))))
                                      (define L.f.1
                                        ((new-frames (()))
                                         (locals (tmp.41 x.1 x.2 tmp.40 tmp.43 tmp.39 tmp.42 tmp-ra.50 tmp.38))
                                         (undead-out
                                          ((rdi rsi r12 tmp-ra.50 rbp)
                                           (rsi r12 tmp-ra.50 x.1 rbp)
                                           (r12 tmp-ra.50 x.1 x.2 rbp)
                                           (tmp.39 r12 tmp-ra.50 x.1 x.2 rbp)
                                           (tmp.39 r12 tmp-ra.50 x.1 x.2 rbp)
                                           ((tmp.39 r12 tmp-ra.50 tmp.38 x.1 x.2 rbp)
                                            (tmp-ra.50 r12 tmp.38 x.1 x.2 rbp))
                                           ((rax x.2 x.1 tmp.38 r12 rbp tmp-ra.50) ((r15 rbp) (rbp r15)))
                                           (x.2 x.1 tmp.40 tmp.38 r12 rbp tmp-ra.50)
                                           ((x.2 x.1 tmp.40 tmp.38 r12 rbp tmp-ra.50)
                                            (r12 rbp tmp-ra.50)
                                            (r12 rbp tmp-ra.50))
                                           (tmp.42 r12 rbp tmp-ra.50)
                                           (tmp.42 r12 rbp tmp-ra.50)
                                           ((tmp.42 r12 tmp.41 rbp tmp-ra.50) (tmp.41 rbp tmp-ra.50))
                                           (tmp.43 tmp.41 rbp tmp-ra.50)
                                           (tmp.43 tmp.41 rbp tmp-ra.50)
                                           (rax rbp tmp-ra.50)
                                           (rbp rax)))
                                         (call-undead (tmp-ra.50 tmp.38 x.1 x.2)))
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
                                        ((new-frames ())
                                         (locals (tmp-ra.51))
                                         (undead-out ((rbp tmp-ra.51) (rax rbp tmp-ra.51) (rbp rax)))
                                         (call-undead ()))
                                        (begin (set! tmp-ra.51 r15) (set! rax 8) (jump tmp-ra.51 rbp rax)))
                                      (begin
                                        (set! tmp-ra.52 r15)
                                        (set! rdi 1)
                                        (set! rsi 2)
                                        (set! r15 tmp-ra.52)
                                        (jump L.f.1 rbp r15 rdi rsi))))
                '(module
                     ((new-frames ())
                      (locals (tmp-ra.52))
                      (call-undead ())
                      (undead-out
                       ((tmp-ra.52 rbp)
                        (tmp-ra.52 rdi rbp)
                        (tmp-ra.52 rsi rdi rbp)
                        (rsi rdi r15 rbp)
                        (rbp r15 rdi rsi)))
                      (conflicts
                       ((tmp-ra.52 (rsi rdi rbp))
                        (rbp (r15 rsi rdi tmp-ra.52))
                        (rdi (r15 rsi tmp-ra.52 rbp))
                        (rsi (r15 tmp-ra.52 rdi rbp))
                        (r15 (rsi rdi rbp)))))
                   (define L.f.1
                     ((new-frames (()))
                      (locals (tmp.41 x.1 x.2 tmp.40 tmp.43 tmp.39 tmp.42 tmp-ra.50 tmp.38))
                      (undead-out
                       ((rdi rsi r12 tmp-ra.50 rbp)
                        (rsi r12 tmp-ra.50 x.1 rbp)
                        (r12 tmp-ra.50 x.1 x.2 rbp)
                        (tmp.39 r12 tmp-ra.50 x.1 x.2 rbp)
                        (tmp.39 r12 tmp-ra.50 x.1 x.2 rbp)
                        ((tmp.39 r12 tmp-ra.50 tmp.38 x.1 x.2 rbp)
                         (tmp-ra.50 r12 tmp.38 x.1 x.2 rbp))
                        ((rax x.2 x.1 tmp.38 r12 rbp tmp-ra.50) ((r15 rbp) (rbp r15)))
                        (x.2 x.1 tmp.40 tmp.38 r12 rbp tmp-ra.50)
                        ((x.2 x.1 tmp.40 tmp.38 r12 rbp tmp-ra.50)
                         (r12 rbp tmp-ra.50)
                         (r12 rbp tmp-ra.50))
                        (tmp.42 r12 rbp tmp-ra.50)
                        (tmp.42 r12 rbp tmp-ra.50)
                        ((tmp.42 r12 tmp.41 rbp tmp-ra.50) (tmp.41 rbp tmp-ra.50))
                        (tmp.43 tmp.41 rbp tmp-ra.50)
                        (tmp.43 tmp.41 rbp tmp-ra.50)
                        (rax rbp tmp-ra.50)
                        (rbp rax)))
                      (call-undead (tmp-ra.50 tmp.38 x.1 x.2))
                      (conflicts
                       ((tmp.38 (tmp.40 r12 tmp.39 tmp-ra.50 x.1 x.2 rbp))
                        (tmp-ra.50
                         (rax
                          tmp.43
                          tmp.41
                          tmp.42
                          tmp.40
                          tmp.38
                          tmp.39
                          x.2
                          x.1
                          rdi
                          rsi
                          r12
                          rbp))
                        (tmp.42 (tmp.41 r12 rbp tmp-ra.50))
                        (tmp.39 (tmp.38 r12 tmp-ra.50 x.1 x.2 rbp))
                        (tmp.43 (tmp.41 rbp tmp-ra.50))
                        (tmp.40 (x.2 x.1 tmp.38 r12 rbp tmp-ra.50))
                        (x.2 (tmp.40 tmp.38 tmp.39 r12 tmp-ra.50 x.1 rbp))
                        (x.1 (tmp.40 tmp.38 tmp.39 x.2 rsi r12 tmp-ra.50 rbp))
                        (tmp.41 (tmp.43 r12 tmp.42 rbp tmp-ra.50))
                        (rbp
                         (rax
                          tmp.43
                          tmp.41
                          tmp.42
                          tmp.40
                          r15
                          r12
                          tmp.38
                          tmp.39
                          x.2
                          x.1
                          tmp-ra.50))
                        (r12 (tmp.41 tmp.42 tmp.40 tmp.38 rbp tmp.39 x.2 x.1 tmp-ra.50))
                        (rsi (x.1 tmp-ra.50))
                        (rdi (tmp-ra.50))
                        (r15 (rbp))
                        (rax (rbp tmp-ra.50)))))
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
                     ((new-frames ())
                      (locals (tmp-ra.51))
                      (undead-out ((rbp tmp-ra.51) (rax rbp tmp-ra.51) (rbp rax)))
                      (call-undead ())
                      (conflicts
                       ((tmp-ra.51 (rax rbp)) (rbp (rax tmp-ra.51)) (rax (rbp tmp-ra.51)))))
                     (begin (set! tmp-ra.51 r15) (set! rax 8) (jump tmp-ra.51 rbp rax)))
                   (begin
                     (set! tmp-ra.52 r15)
                     (set! rdi 1)
                     (set! rsi 2)
                     (set! r15 tmp-ra.52)
                     (jump L.f.1 rbp r15 rdi rsi))))
  #;
  (match (conflict-analysis (undead-analysis (uncover-locals '(module ((new-frames (())))
                                                                (begin (set! x.6 2)
                                                                       (set! x.6 (+ x.6 3))
                                                                       (set! x.7 x.6)
                                                                       (set! x.7 (+ x.7 x.6))
                                                                       (begin (set! y.2 5)
                                                                              (jump L.f.1 x.6)))))))
    [`(module ((locals ,ls) (conflicts ,conflicts)) ,tail)
     (check-true (set=? (get-neighbors conflicts 'y.2) (list 'x.6)))
     (check-true (set=? (get-neighbors conflicts 'x.6) (list 'y.2 'x.7))
                 (format "unexpected conflict graph: ~a" (get-neighbors conflicts 'x.6)))
     (check-true (set=? (get-neighbors conflicts 'x.7) (list 'x.6)))])
  #;
  (match (conflict-analysis (undead-analysis (uncover-locals '(module ()
                                                                (begin (set! x.1 1)
                                                                       (set! x.2 x.1)
                                                                       (set! x.1 (+ x.1 x.1))
                                                                       (halt x.2))))))
    [`(module ((locals ,ls) (conflicts ,conflicts)) ,tail)
     (check-true (set=? (get-neighbors conflicts 'x.1) (list 'x.2)))
     (check-true (set=? (get-neighbors conflicts 'x.2) (list 'x.1)))])
  #;
  (match (conflict-analysis (undead-analysis (uncover-locals '(module ()
                                                                (begin (set! x.1 1)
                                                                       (set! x.2 x.1)
                                                                       (set! x.2 (+ x.2 x.1))
                                                                       (if (> x.2 2)
                                                                           (set! x.3 3)
                                                                           (set! x.3 4))
                                                                       (set! x.2 (+ x.2 x.3))
                                                                       (halt x.2))))))
    [`(module ((locals ,ls) (conflicts ,conflicts)) ,tail)
     (check-true (set=? (get-neighbors conflicts 'x.1) empty))
     (check-true (set=? (get-neighbors conflicts 'x.2) (list 'x.3)))
     (check-true (set=? (get-neighbors conflicts 'x.3) (list 'x.2)))])
  #;
  (match (conflict-analysis (undead-analysis (uncover-locals '(module ()
                                                                (if (if (begin (set! x.1 1)
                                                                               (set! x.2 x.1)
                                                                               (set! x.3 x.1)
                                                                               (set! x.3 (+ x.3 x.2))
                                                                               (true))
                                                                        (true)
                                                                        (false))
                                                                    (halt x.3)
                                                                    (halt x.2))))))
    [`(module ((locals ,ls) (conflicts ,conflicts)) ,tail)
     (check-true (set=? (get-neighbors conflicts 'x.1) empty))
     (check-true (set=? (get-neighbors conflicts 'x.2) (list 'x.3)))
     (check-true (set=? (get-neighbors conflicts 'x.3) (list 'x.2)))]))
