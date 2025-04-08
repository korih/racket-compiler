#lang racket

(require rackunit
         cpsc411/langs/v8
         "../passes/allocate-frames.rkt")

(module+ test
  (check-equal? (allocate-frames '(module
                                      ((new-frames ())
                                       (locals (tmp-ra.10))
                                       (call-undead ())
                                       (undead-out
                                        ((tmp-ra.10 rbp)
                                         (tmp-ra.10 fv1 rbp)
                                         (tmp-ra.10 fv1 fv0 rbp)
                                         (fv1 fv0 r15 rbp)
                                         (fv1 fv0 r15 rbp)))
                                       (conflicts
                                        ((tmp-ra.10 (fv0 fv1 rbp))
                                         (rbp (r15 fv0 fv1 tmp-ra.10))
                                         (fv1 (r15 fv0 rbp tmp-ra.10))
                                         (fv0 (r15 rbp fv1 tmp-ra.10))
                                         (r15 (rbp fv0 fv1))))
                                       (assignment ()))
                                    (define L.swap.1
                                      ((new-frames ((nfv.8 nfv.9)))
                                       (locals (y.2 x.1 z.3 nfv.9 nfv.8))
                                       (undead-out
                                        ((fv0 fv1 tmp-ra.7 rbp)
                                         (fv1 x.1 tmp-ra.7 rbp)
                                         (y.2 x.1 tmp-ra.7 rbp)
                                         ((y.2 x.1 tmp-ra.7 rbp)
                                          ((tmp-ra.7 rax rbp) (rax rbp))
                                          (((rax tmp-ra.7 rbp)
                                            ((y.2 nfv.9 rbp)
                                             (nfv.9 nfv.8 rbp)
                                             (nfv.9 nfv.8 r15 rbp)
                                             (nfv.9 nfv.8 r15 rbp)))
                                           (z.3 tmp-ra.7 rbp)
                                           (tmp-ra.7 rax rbp)
                                           (rax rbp)))))
                                       (call-undead (tmp-ra.7))
                                       (conflicts
                                        ((y.2 (rbp tmp-ra.7 x.1 nfv.9))
                                         (x.1 (y.2 rbp tmp-ra.7 fv1))
                                         (tmp-ra.7 (y.2 x.1 rbp fv1 fv0 rax z.3))
                                         (z.3 (rbp tmp-ra.7))
                                         (nfv.9 (r15 nfv.8 rbp y.2))
                                         (nfv.8 (r15 rbp nfv.9))
                                         (rbp (y.2 x.1 tmp-ra.7 rax z.3 r15 nfv.8 nfv.9))
                                         (r15 (rbp nfv.8 nfv.9))
                                         (rax (rbp tmp-ra.7))
                                         (fv0 (tmp-ra.7))
                                         (fv1 (x.1 tmp-ra.7))))
                                       (assignment ((tmp-ra.7 fv2))))
                                      (begin
                                        (set! tmp-ra.7 r15)
                                        (set! x.1 fv0)
                                        (set! y.2 fv1)
                                        (if (< y.2 x.1)
                                            (begin (set! rax x.1) (jump tmp-ra.7 rbp rax))
                                            (begin
                                              (return-point L.rp.3
                                                            (begin
                                                              (set! nfv.9 x.1)
                                                              (set! nfv.8 y.2)
                                                              (set! r15 L.rp.3)
                                                              (jump L.swap.1 rbp r15 nfv.8 nfv.9)))
                                              (set! z.3 rax)
                                              (set! rax z.3)
                                              (jump tmp-ra.7 rbp rax)))))
                                    (begin
                                      (set! tmp-ra.10 r15)
                                      (set! fv1 2)
                                      (set! fv0 1)
                                      (set! r15 tmp-ra.10)
                                      (jump L.swap.1 rbp r15 fv0 fv1))))
                '(module
                     ((locals (tmp-ra.10))
                      (conflicts
                       ((tmp-ra.10 (fv0 fv1 rbp))
                        (rbp (r15 fv0 fv1 tmp-ra.10))
                        (fv1 (r15 fv0 rbp tmp-ra.10))
                        (fv0 (r15 rbp fv1 tmp-ra.10))
                        (r15 (rbp fv0 fv1))))
                      (assignment ()))
                   (define L.swap.1
                     ((locals (z.3 x.1 y.2))
                      (conflicts
                       ((y.2 (rbp tmp-ra.7 x.1 nfv.9))
                        (x.1 (y.2 rbp tmp-ra.7 fv1))
                        (tmp-ra.7 (y.2 x.1 rbp fv1 fv0 rax z.3))
                        (z.3 (rbp tmp-ra.7))
                        (nfv.9 (r15 nfv.8 rbp y.2))
                        (nfv.8 (r15 rbp nfv.9))
                        (rbp (y.2 x.1 tmp-ra.7 rax z.3 r15 nfv.8 nfv.9))
                        (r15 (rbp nfv.8 nfv.9))
                        (rax (rbp tmp-ra.7))
                        (fv0 (tmp-ra.7))
                        (fv1 (x.1 tmp-ra.7))))
                      (assignment ((nfv.9 fv4) (nfv.8 fv3) (tmp-ra.7 fv2))))
                     (begin
                       (set! tmp-ra.7 r15)
                       (set! x.1 fv0)
                       (set! y.2 fv1)
                       (if (< y.2 x.1)
                           (begin (set! rax x.1) (jump tmp-ra.7 rbp rax))
                           (begin
                             (begin
                               (set! rbp (- rbp 24))
                               (return-point L.rp.3
                                             (begin
                                               (set! nfv.9 x.1)
                                               (set! nfv.8 y.2)
                                               (set! r15 L.rp.3)
                                               (jump L.swap.1 rbp r15 nfv.8 nfv.9)))
                               (set! rbp (+ rbp 24)))
                             (set! z.3 rax)
                             (set! rax z.3)
                             (jump tmp-ra.7 rbp rax)))))
                   (begin
                     (set! tmp-ra.10 r15)
                     (set! fv1 2)
                     (set! fv0 1)
                     (set! r15 tmp-ra.10)
                     (jump L.swap.1 rbp r15 fv0 fv1))))
  (check-equal? (allocate-frames '(module
                                      ((locals (y.2 x.1))
                                       (call-undead ())
                                       (new-frames (()))
                                       (undead-out ((x.1 r15) (x.1 y.2 r15) ((x.1 y.2 r15) ((r15) ()) ((r15) ()))))
                                       (conflicts ((y.2 (r15)) (x.1 (r15)) (r15 (rax y.2 x.1)) (rax (r15))))
                                       (assignment ()))
                                    (begin
                                      (set! x.1 3)
                                      (set! y.2 x.1)
                                      (if (> y.2 x.1)
                                          (begin (set! rax x.1) (jump r15))
                                          (begin (set! rax y.2) (jump r15))))))
                '(module
                     ((locals (x.1 y.2))
                      (conflicts ((y.2 (r15)) (x.1 (r15)) (r15 (rax y.2 x.1)) (rax (r15))))
                      (assignment ()))
                   (begin
                     (set! x.1 3)
                     (set! y.2 x.1)
                     (if (> y.2 x.1)
                         (begin (set! rax x.1) (jump r15))
                         (begin (set! rax y.2) (jump r15))))))
  (check-equal? (allocate-frames '(module
                                      ((locals (y.2 x.1))
                                       (call-undead ())
                                       (new-frames (()))
                                       (undead-out ((x.1 r15) (x.1 y.2 r15) ((x.1 y.2 r15) ((r15) ()) ((r15) ()))))
                                       (conflicts ((y.2 (r15)) (x.1 (r15)) (r15 (rax y.2 x.1)) (rax (r15))))
                                       (assignment ()))
                                    (begin
                                      (set! x.1 3)
                                      (set! y.2 x.1)
                                      (if (begin
                                            (set! x.1 4)
                                            (set! y.2 3)
                                            (> x.1 y.2))
                                          (set! x.1 5)
                                          (set! x.1 10))
                                      (if (> y.2 x.1)
                                          (begin (set! rax x.1) (jump r15))
                                          (begin (set! rax y.2) (jump r15))))))

                '(module
                     ((locals (x.1 y.2))
                      (conflicts ((y.2 (r15)) (x.1 (r15)) (r15 (rax y.2 x.1)) (rax (r15))))
                      (assignment ()))
                   (begin
                     (set! x.1 3)
                     (set! y.2 x.1)
                     (if (begin (set! x.1 4) (set! y.2 3) (> x.1 y.2))
                         (set! x.1 5)
                         (set! x.1 10))
                     (if (> y.2 x.1)
                         (begin (set! rax x.1) (jump r15))
                         (begin (set! rax y.2) (jump r15))))))
  (check-equal? (allocate-frames '(module
                                      ((locals (y.2 x.1))
                                       (call-undead ())
                                       (new-frames (()))
                                       (undead-out ((x.1 r15) (x.1 y.2 r15) ((x.1 y.2 r15) ((r15) ()) ((r15) ()))))
                                       (conflicts ((y.2 (r15)) (x.1 (r15)) (r15 (rax y.2 x.1)) (rax (r15))))
                                       (assignment ()))
                                    (begin
                                      (set! x.1 3)
                                      (set! y.2 x.1)
                                      (if (begin
                                            (set! x.1 4)
                                            (return-point L.rp.1 (jump L.haha.2))
                                            (> x.1 y.2))
                                          (set! x.1 5)
                                          (set! x.1 10))
                                      (if (> y.2 x.1)
                                          (begin (set! rax x.1) (jump r15))
                                          (begin (set! rax y.2) (jump r15))))))

                '(module
                     ((locals (x.1 y.2))
                      (conflicts ((y.2 (r15)) (x.1 (r15)) (r15 (rax y.2 x.1)) (rax (r15))))
                      (assignment ()))
                   (begin
                     (set! x.1 3)
                     (set! y.2 x.1)
                     (if (begin
                           (set! x.1 4)
                           (begin
                             (set! rbp (- rbp 0))
                             (return-point L.rp.1 (jump L.haha.2))
                             (set! rbp (+ rbp 0)))
                           (> x.1 y.2))
                         (set! x.1 5)
                         (set! x.1 10))
                     (if (> y.2 x.1)
                         (begin (set! rax x.1) (jump r15))
                         (begin (set! rax y.2) (jump r15))))))
  (check-equal? (allocate-frames '(module ((new-frames ())
                                           (locals (tmp-ra.137))
                                           (call-undead ())
                                           (undead-out
                                            ((tmp-ra.137 rbp)
                                             (tmp-ra.137 rdi rbp)
                                             (rdi r15 rbp)
                                             (rbp r15 rdi)))
                                           (conflicts
                                            ((tmp-ra.137 (rdi rbp))
                                             (rbp (r15 rdi tmp-ra.137))
                                             (rdi (r15 tmp-ra.137 rbp))
                                             (r15 (rdi rbp))))
                                           (assignment ()))
                                    (define L.fact.21 ((new-frames (()))
                                                       (locals (y.57 z.56))
                                                       (undead-out ((rdi rbp tmp-ra.136)
                                                                    (x.55 rbp tmp-ra.136)
                                                                    ((x.55 rbp tmp-ra.136)
                                                                     ((rax rbp tmp-ra.136) (rbp rax))
                                                                     ((z.56 tmp-ra.136 x.55 rbp)
                                                                      (tmp-ra.136 x.55 z.56 rbp)
                                                                      ((rax x.55 rbp tmp-ra.136)
                                                                       ((rdi rbp)
                                                                        (rdi r15 rbp)
                                                                        (rbp r15 rdi)))
                                                                      (x.55 y.57 rbp tmp-ra.136)
                                                                      (y.57 rax rbp tmp-ra.136)
                                                                      (rax rbp tmp-ra.136) (rbp rax)))))
                                                       (call-undead (x.55 tmp-ra.136))
                                                       (conflicts ((y.57 (rax x.55 rbp tmp-ra.136))
                                                                   (x.55 (y.57 z.56 rbp tmp-ra.136))
                                                                   (z.56 (x.55 tmp-ra.136 rbp))
                                                                   (tmp-ra.136 (y.57 z.56 rax x.55 rdi rbp))
                                                                   (rbp (y.57 r15 rdi z.56 rax x.55 tmp-ra.136))
                                                                   (rdi (r15 rbp tmp-ra.136))
                                                                   (rax (y.57 rbp tmp-ra.136))
                                                                   (r15 (rdi rbp))))
                                                       (assignment ((tmp-ra.136 fv0) (x.55 fv0))))
                                      (begin
                                        (set! tmp-ra.136 r15)
                                        (set! x.55 rdi)
                                        (if (= x.55 0)
                                            (begin
                                              (set! rax 1)
                                              (jump tmp-ra.136 rbp rax))
                                            (begin
                                              (set! z.56 x.55)
                                              (set! z.56 (+ z.56 -1))
                                              (return-point L.rp.32
                                                            (begin (set! rdi z.56)
                                                                   (set! r15 L.rp.32)
                                                                   (jump L.fact.21 rbp r15 rdi)))
                                              (set! y.57 rax)
                                              (set! rax x.55)
                                              (set! rax (* rax y.57))
                                              (jump tmp-ra.136 rbp rax)))))
                                    (begin
                                      (set! tmp-ra.137 r15)
                                      (set! rdi 10)
                                      (set! r15 tmp-ra.137)
                                      (jump L.fact.21 rbp r15 rdi))))
                '(module
                     ((locals (tmp-ra.137))
                      (conflicts
                       ((tmp-ra.137 (rdi rbp))
                        (rbp (r15 rdi tmp-ra.137))
                        (rdi (r15 tmp-ra.137 rbp))
                        (r15 (rdi rbp))))
                      (assignment ()))
                   (define L.fact.21
                     ((locals (z.56 y.57))
                      (conflicts
                       ((y.57 (rax x.55 rbp tmp-ra.136))
                        (x.55 (y.57 z.56 rbp tmp-ra.136))
                        (z.56 (x.55 tmp-ra.136 rbp))
                        (tmp-ra.136 (y.57 z.56 rax x.55 rdi rbp))
                        (rbp (y.57 r15 rdi z.56 rax x.55 tmp-ra.136))
                        (rdi (r15 rbp tmp-ra.136))
                        (rax (y.57 rbp tmp-ra.136))
                        (r15 (rdi rbp))))
                      (assignment ((tmp-ra.136 fv0) (x.55 fv0))))
                     (begin
                       (set! tmp-ra.136 r15)
                       (set! x.55 rdi)
                       (if (= x.55 0)
                           (begin (set! rax 1) (jump tmp-ra.136 rbp rax))
                           (begin
                             (set! z.56 x.55)
                             (set! z.56 (+ z.56 -1))
                             (begin
                               (set! rbp (- rbp 16))
                               (return-point L.rp.32
                                             (begin
                                               (set! rdi z.56)
                                               (set! r15 L.rp.32)
                                               (jump L.fact.21 rbp r15 rdi)))
                               (set! rbp (+ rbp 16)))
                             (set! y.57 rax)
                             (set! rax x.55)
                             (set! rax (* rax y.57))
                             (jump tmp-ra.136 rbp rax)))))
                   (begin
                     (set! tmp-ra.137 r15)
                     (set! rdi 10)
                     (set! r15 tmp-ra.137)
                     (jump L.fact.21 rbp r15 rdi))))
  (check-equal? (allocate-frames '(module
                                      ((new-frames ())
                                       (locals (tmp-ra.102))
                                       (call-undead ())
                                       (conflicts
                                        ((tmp-ra.102 (rdi rbp))
                                         (rbp (r15 rdi tmp-ra.102))
                                         (rdi (r15 tmp-ra.102 rbp))
                                         (r15 (rdi rbp))))
                                       (assignment ()))
                                    (define L.fact.19
                                      ((new-frames ())
                                       (locals (z.37 y.38))
                                       (call-undead (x.36 tmp-ra.101))
                                       (conflicts
                                        ((tmp-ra.101 (y.38 z.37 rax x.36 rdi rbp))
                                         (y.38 (rax x.36 rbp tmp-ra.101))
                                         (z.37 (x.36 tmp-ra.101 rbp))
                                         (x.36 (y.38 z.37 rbp tmp-ra.101))
                                         (rbp (y.38 r15 rdi z.37 rax x.36 tmp-ra.101))
                                         (rdi (r15 rbp tmp-ra.101))
                                         (rax (y.38 rbp tmp-ra.101))
                                         (r15 (rdi rbp))))
                                       (assignment ((tmp-ra.101 fv0) (x.36 fv0))))
                                      (begin
                                        (set! tmp-ra.101 r15)
                                        (set! x.36 rdi)
                                        (if (= x.36 0)
                                            (begin (set! rax 1) (jump tmp-ra.101 rbp rax))
                                            (begin
                                              (set! z.37 x.36)
                                              (set! z.37 (+ z.37 -1))
                                              (return-point L.rp.25
                                                            (begin
                                                              (set! rdi z.37)
                                                              (set! r15 L.rp.25)
                                                              (jump L.fact.19 rbp r15 rdi)))
                                              (set! y.38 rax)
                                              (set! rax x.36)
                                              (set! rax (* rax y.38))
                                              (jump tmp-ra.101 rbp rax)))))
                                    (begin
                                      (set! tmp-ra.102 r15)
                                      (set! rdi 5)
                                      (set! r15 tmp-ra.102)
                                      (jump L.fact.19 rbp r15 rdi))))
                '(module
                     ((locals (tmp-ra.102))
                      (conflicts
                       ((tmp-ra.102 (rdi rbp))
                        (rbp (r15 rdi tmp-ra.102))
                        (rdi (r15 tmp-ra.102 rbp))
                        (r15 (rdi rbp))))
                      (assignment ()))
                   (define L.fact.19
                     ((locals (y.38 z.37))
                      (conflicts
                       ((tmp-ra.101 (y.38 z.37 rax x.36 rdi rbp))
                        (y.38 (rax x.36 rbp tmp-ra.101))
                        (z.37 (x.36 tmp-ra.101 rbp))
                        (x.36 (y.38 z.37 rbp tmp-ra.101))
                        (rbp (y.38 r15 rdi z.37 rax x.36 tmp-ra.101))
                        (rdi (r15 rbp tmp-ra.101))
                        (rax (y.38 rbp tmp-ra.101))
                        (r15 (rdi rbp))))
                      (assignment ((tmp-ra.101 fv0) (x.36 fv0))))
                     (begin
                       (set! tmp-ra.101 r15)
                       (set! x.36 rdi)
                       (if (= x.36 0)
                           (begin (set! rax 1) (jump tmp-ra.101 rbp rax))
                           (begin
                             (set! z.37 x.36)
                             (set! z.37 (+ z.37 -1))
                             (begin
                               (set! rbp (- rbp 16))
                               (return-point L.rp.25
                                             (begin
                                               (set! rdi z.37)
                                               (set! r15 L.rp.25)
                                               (jump L.fact.19 rbp r15 rdi)))
                               (set! rbp (+ rbp 16)))
                             (set! y.38 rax)
                             (set! rax x.36)
                             (set! rax (* rax y.38))
                             (jump tmp-ra.101 rbp rax)))))
                   (begin
                     (set! tmp-ra.102 r15)
                     (set! rdi 5)
                     (set! r15 tmp-ra.102)
                     (jump L.fact.19 rbp r15 rdi))))
  (check-equal? (allocate-frames '(module
                                      ((new-frames ())
                                       (locals (x.2 tmp-ra.2 x.3))
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
                                         (r15 (rdi rbp))))
                                       (assignment ()))
                                    (define L.f.1
                                      ((new-frames ())
                                       (locals (a.1 z.1 x.1 y.1 b.1 tmp-ra.1))
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
                                         (rax (rbp tmp-ra.1))))
                                       (assignment ()))
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
                     ((locals (x.3 tmp-ra.2 x.2))
                      (conflicts
                       ((x.2 (x.3 tmp-ra.2 rbp))
                        (tmp-ra.2 (rdi x.3 x.2 rbp))
                        (x.3 (x.2 tmp-ra.2 rbp))
                        (rbp (r15 rdi x.3 x.2 tmp-ra.2))
                        (rdi (r15 tmp-ra.2 rbp))
                        (r15 (rdi rbp))))
                      (assignment ()))
                   (define L.f.1
                     ((locals (tmp-ra.1 b.1 y.1 x.1 z.1 a.1))
                      (conflicts
                       ((a.1 (b.1 z.1 x.1 rbp tmp-ra.1))
                        (z.1 (a.1 y.1 x.1 rbp tmp-ra.1))
                        (x.1 (b.1 a.1 z.1 y.1 rbp tmp-ra.1))
                        (y.1 (z.1 x.1 rbp tmp-ra.1))
                        (b.1 (x.1 a.1 rbp tmp-ra.1))
                        (tmp-ra.1 (rax b.1 a.1 z.1 y.1 x.1 rdi rbp))
                        (rbp (rax b.1 a.1 z.1 y.1 x.1 tmp-ra.1))
                        (rdi (tmp-ra.1))
                        (rax (rbp tmp-ra.1))))
                      (assignment ()))
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
  (check-equal? (allocate-frames '(module ((new-frames (() ())) (locals (tmp.88)) (call-undead (tmp-ra.95 tmp.87)) (undead-out ((tmp-ra.95 rbp) ((rax tmp-ra.95 rbp) ((rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi))) (tmp-ra.95 tmp.87 rbp) ((rax tmp.87 tmp-ra.95 rbp) ((rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi))) (tmp.87 tmp.88 tmp-ra.95 rbp) (tmp.88 tmp-ra.95 rdi rbp) (tmp-ra.95 rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi))) (conflicts ((tmp.87 (tmp.88 tmp-ra.95 rbp)) (tmp-ra.95 (rsi rdi tmp.88 tmp.87 rbp)) (tmp.88 (rdi tmp.87 tmp-ra.95 rbp)) (rbp (tmp.88 tmp.87 r15 rsi rdi tmp-ra.95)) (rdi (tmp.88 tmp-ra.95 r15 rsi rbp)) (rsi (tmp-ra.95 r15 rdi rbp)) (r15 (rsi rdi rbp)))) (assignment ((tmp.87 fv0) (tmp-ra.95 fv0)))) (define L.*.17 ((new-frames ()) (locals (tmp-ra.93 tmp.79 tmp.81 tmp.42 tmp.82 tmp.41 tmp.78 tmp.80)) (undead-out ((rdi rsi rbp tmp-ra.93) (rsi tmp.41 rbp tmp-ra.93) (tmp.42 tmp.41 rbp tmp-ra.93) (((((tmp.79 tmp.42 tmp.41 rbp tmp-ra.93) (tmp.79 tmp.42 tmp.41 rbp tmp-ra.93) (tmp.42 tmp.41 rbp tmp-ra.93)) (tmp.78 tmp.42 tmp.41 rbp tmp-ra.93) (tmp.78 tmp.42 tmp.41 rbp tmp-ra.93)) (tmp.42 tmp.41 rbp tmp-ra.93)) (((((tmp.81 tmp.42 tmp.41 rbp tmp-ra.93) (tmp.81 tmp.42 tmp.41 rbp tmp-ra.93) (tmp.42 tmp.41 rbp tmp-ra.93)) (tmp.80 tmp.42 tmp.41 rbp tmp-ra.93) (tmp.80 tmp.42 tmp.41 rbp tmp-ra.93)) (tmp.42 tmp.41 rbp tmp-ra.93)) ((tmp.82 tmp.41 rbp tmp-ra.93) (tmp.41 tmp.82 rbp tmp-ra.93) (tmp.82 rax rbp tmp-ra.93) (rax rbp tmp-ra.93) (rbp rax)) ((rax rbp tmp-ra.93) (rbp rax))) ((rax rbp tmp-ra.93) (rbp rax))))) (call-undead ()) (conflicts ((tmp-ra.93 (rax tmp.82 tmp.80 tmp.81 tmp.78 tmp.79 tmp.42 tmp.41 rdi rsi rbp)) (tmp.79 (tmp.42 tmp.41 rbp tmp-ra.93)) (tmp.81 (tmp.41 tmp.42 rbp tmp-ra.93)) (tmp.42 (tmp.80 tmp.81 tmp.78 tmp.79 tmp.41 rbp tmp-ra.93)) (tmp.82 (rax tmp.41 rbp tmp-ra.93)) (tmp.41 (tmp.82 tmp.80 tmp.81 tmp.78 tmp.79 tmp.42 rsi rbp tmp-ra.93)) (tmp.78 (tmp.42 tmp.41 rbp tmp-ra.93)) (tmp.80 (tmp.42 tmp.41 rbp tmp-ra.93)) (rbp (rax tmp.82 tmp.80 tmp.81 tmp.78 tmp.79 tmp.42 tmp.41 tmp-ra.93)) (rsi (tmp.41 tmp-ra.93)) (rdi (tmp-ra.93)) (rax (tmp.82 rbp tmp-ra.93)))) (assignment ())) (begin (set! tmp-ra.93 r15) (set! tmp.41 rdi) (set! tmp.42 rsi) (if (begin (if (begin (set! tmp.79 tmp.42) (set! tmp.79 (bitwise-and tmp.79 7)) (= tmp.79 0)) (set! tmp.78 14) (set! tmp.78 6)) (!= tmp.78 6)) (if (begin (if (begin (set! tmp.81 tmp.41) (set! tmp.81 (bitwise-and tmp.81 7)) (= tmp.81 0)) (set! tmp.80 14) (set! tmp.80 6)) (!= tmp.80 6)) (begin (set! tmp.82 tmp.42) (set! tmp.82 (arithmetic-shift-right tmp.82 3)) (set! rax tmp.41) (set! rax (* rax tmp.82)) (jump tmp-ra.93 rbp rax)) (begin (set! rax 318) (jump tmp-ra.93 rbp rax))) (begin (set! rax 318) (jump tmp-ra.93 rbp rax))))) (define L.+.16 ((new-frames ()) (locals (tmp.83 tmp.40 tmp.86 tmp.84 tmp-ra.94 tmp.85 tmp.39)) (undead-out ((rdi rsi rbp tmp-ra.94) (rsi tmp.39 rbp tmp-ra.94) (tmp.39 tmp.40 rbp tmp-ra.94) (((((tmp.84 tmp.39 tmp.40 rbp tmp-ra.94) (tmp.84 tmp.39 tmp.40 rbp tmp-ra.94) (tmp.39 tmp.40 rbp tmp-ra.94)) (tmp.83 tmp.39 tmp.40 rbp tmp-ra.94) (tmp.83 tmp.39 tmp.40 rbp tmp-ra.94)) (tmp.39 tmp.40 rbp tmp-ra.94)) (((((tmp.86 tmp.39 tmp.40 rbp tmp-ra.94) (tmp.86 tmp.39 tmp.40 rbp tmp-ra.94) (tmp.39 tmp.40 rbp tmp-ra.94)) (tmp.85 tmp.39 tmp.40 rbp tmp-ra.94) (tmp.85 tmp.39 tmp.40 rbp tmp-ra.94)) (tmp.39 tmp.40 rbp tmp-ra.94)) ((tmp.40 rax rbp tmp-ra.94) (rax rbp tmp-ra.94) (rbp rax)) ((rax rbp tmp-ra.94) (rbp rax))) ((rax rbp tmp-ra.94) (rbp rax))))) (call-undead ()) (conflicts ((tmp.83 (tmp.39 tmp.40 rbp tmp-ra.94)) (tmp.40 (rax tmp.85 tmp.86 tmp.83 tmp.84 tmp.39 rbp tmp-ra.94)) (tmp.86 (tmp.39 tmp.40 rbp tmp-ra.94)) (tmp.84 (tmp.40 tmp.39 rbp tmp-ra.94)) (tmp-ra.94 (rax tmp.85 tmp.86 tmp.83 tmp.84 tmp.40 tmp.39 rdi rsi rbp)) (tmp.85 (tmp.39 tmp.40 rbp tmp-ra.94)) (tmp.39 (tmp.85 tmp.86 tmp.83 tmp.84 tmp.40 rsi rbp tmp-ra.94)) (rbp (rax tmp.85 tmp.86 tmp.83 tmp.84 tmp.40 tmp.39 tmp-ra.94)) (rsi (tmp.39 tmp-ra.94)) (rdi (tmp-ra.94)) (rax (tmp.40 rbp tmp-ra.94)))) (assignment ())) (begin (set! tmp-ra.94 r15) (set! tmp.39 rdi) (set! tmp.40 rsi) (if (begin (if (begin (set! tmp.84 tmp.40) (set! tmp.84 (bitwise-and tmp.84 7)) (= tmp.84 0)) (set! tmp.83 14) (set! tmp.83 6)) (!= tmp.83 6)) (if (begin (if (begin (set! tmp.86 tmp.39) (set! tmp.86 (bitwise-and tmp.86 7)) (= tmp.86 0)) (set! tmp.85 14) (set! tmp.85 6)) (!= tmp.85 6)) (begin (set! rax tmp.39) (set! rax (+ rax tmp.40)) (jump tmp-ra.94 rbp rax)) (begin (set! rax 574) (jump tmp-ra.94 rbp rax))) (begin (set! rax 574) (jump tmp-ra.94 rbp rax))))) (begin (set! tmp-ra.95 r15) (return-point L.rp.19 (begin (set! rdi 40) (set! rsi 48) (set! r15 L.rp.19) (jump L.+.16 rbp r15 rdi rsi))) (set! tmp.87 rax) (return-point L.rp.20 (begin (set! rdi 32) (set! rsi 40) (set! r15 L.rp.20) (jump L.*.17 rbp r15 rdi rsi))) (set! tmp.88 rax) (set! rdi tmp.87) (set! rsi tmp.88) (set! r15 tmp-ra.95) (jump L.+.16 rbp r15 rdi rsi))))
                '(module
                     ((locals (tmp.88))
                      (conflicts
                       ((tmp.87 (tmp.88 tmp-ra.95 rbp))
                        (tmp-ra.95 (rsi rdi tmp.88 tmp.87 rbp))
                        (tmp.88 (rdi tmp.87 tmp-ra.95 rbp))
                        (rbp (tmp.88 tmp.87 r15 rsi rdi tmp-ra.95))
                        (rdi (tmp.88 tmp-ra.95 r15 rsi rbp))
                        (rsi (tmp-ra.95 r15 rdi rbp))
                        (r15 (rsi rdi rbp))))
                      (assignment ((tmp.87 fv0) (tmp-ra.95 fv0))))
                   (define L.*.17
                     ((locals (tmp.80 tmp.78 tmp.41 tmp.82 tmp.42 tmp.81 tmp.79 tmp-ra.93))
                      (conflicts
                       ((tmp-ra.93
                         (rax tmp.82 tmp.80 tmp.81 tmp.78 tmp.79 tmp.42 tmp.41 rdi rsi rbp))
                        (tmp.79 (tmp.42 tmp.41 rbp tmp-ra.93))
                        (tmp.81 (tmp.41 tmp.42 rbp tmp-ra.93))
                        (tmp.42 (tmp.80 tmp.81 tmp.78 tmp.79 tmp.41 rbp tmp-ra.93))
                        (tmp.82 (rax tmp.41 rbp tmp-ra.93))
                        (tmp.41 (tmp.82 tmp.80 tmp.81 tmp.78 tmp.79 tmp.42 rsi rbp tmp-ra.93))
                        (tmp.78 (tmp.42 tmp.41 rbp tmp-ra.93))
                        (tmp.80 (tmp.42 tmp.41 rbp tmp-ra.93))
                        (rbp (rax tmp.82 tmp.80 tmp.81 tmp.78 tmp.79 tmp.42 tmp.41 tmp-ra.93))
                        (rsi (tmp.41 tmp-ra.93))
                        (rdi (tmp-ra.93))
                        (rax (tmp.82 rbp tmp-ra.93))))
                      (assignment ()))
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
                     ((locals (tmp.39 tmp.85 tmp-ra.94 tmp.84 tmp.86 tmp.40 tmp.83))
                      (conflicts
                       ((tmp.83 (tmp.39 tmp.40 rbp tmp-ra.94))
                        (tmp.40 (rax tmp.85 tmp.86 tmp.83 tmp.84 tmp.39 rbp tmp-ra.94))
                        (tmp.86 (tmp.39 tmp.40 rbp tmp-ra.94))
                        (tmp.84 (tmp.40 tmp.39 rbp tmp-ra.94))
                        (tmp-ra.94 (rax tmp.85 tmp.86 tmp.83 tmp.84 tmp.40 tmp.39 rdi rsi rbp))
                        (tmp.85 (tmp.39 tmp.40 rbp tmp-ra.94))
                        (tmp.39 (tmp.85 tmp.86 tmp.83 tmp.84 tmp.40 rsi rbp tmp-ra.94))
                        (rbp (rax tmp.85 tmp.86 tmp.83 tmp.84 tmp.40 tmp.39 tmp-ra.94))
                        (rsi (tmp.39 tmp-ra.94))
                        (rdi (tmp-ra.94))
                        (rax (tmp.40 rbp tmp-ra.94))))
                      (assignment ()))
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
  (check-equal? (allocate-frames '(module
                                      ((new-frames ())
                                       (locals (tmp-ra.238))
                                       (call-undead ())
                                       (undead-out
                                        ((tmp-ra.238 rbp)
                                         (tmp-ra.238 rdi rbp)
                                         (tmp-ra.238 rsi rdi rbp)
                                         (tmp-ra.238 rdx rsi rdi rbp)
                                         (tmp-ra.238 rcx rdx rsi rdi rbp)
                                         (tmp-ra.238 r8 rcx rdx rsi rdi rbp)
                                         (tmp-ra.238 r9 r8 rcx rdx rsi rdi rbp)
                                         (tmp-ra.238 fv0 r9 r8 rcx rdx rsi rdi rbp)
                                         (fv0 r9 r8 rcx rdx rsi rdi r15 rbp)
                                         (fv0 r9 r8 rcx rdx rsi rdi r15 rbp)))
                                       (conflicts
                                        ((tmp-ra.238 (fv0 r9 r8 rcx rdx rsi rdi rbp))
                                         (rbp (r15 fv0 r9 r8 rcx rdx rsi rdi tmp-ra.238))
                                         (rdi (r15 fv0 r9 r8 rcx rdx rsi rbp tmp-ra.238))
                                         (rsi (r15 fv0 r9 r8 rcx rdx rbp rdi tmp-ra.238))
                                         (rdx (r15 fv0 r9 r8 rcx rbp rdi rsi tmp-ra.238))
                                         (rcx (r15 fv0 r9 r8 rbp rdi rsi rdx tmp-ra.238))
                                         (r8 (r15 fv0 r9 rbp rdi rsi rdx rcx tmp-ra.238))
                                         (r9 (r15 fv0 rbp rdi rsi rdx rcx r8 tmp-ra.238))
                                         (fv0 (r15 rbp rdi rsi rdx rcx r8 r9 tmp-ra.238))
                                         (r15 (rbp rdi rsi rdx rcx r8 r9 fv0))))
                                       (assignment ()))
                                    (define L.+.31
                                      ((new-frames ())
                                       (locals (tmp.183 tmp.97 tmp.184 tmp-ra.232 tmp.186 tmp.96 tmp.185))
                                       (undead-out
                                        ((rdi rsi tmp-ra.232 rbp)
                                         (rsi tmp.96 tmp-ra.232 rbp)
                                         (tmp.96 tmp.97 tmp-ra.232 rbp)
                                         (((((tmp.184 tmp.96 tmp.97 tmp-ra.232 rbp)
                                             (tmp.184 tmp.96 tmp.97 tmp-ra.232 rbp)
                                             (tmp.96 tmp.97 tmp-ra.232 rbp))
                                            (tmp.183 tmp.96 tmp.97 tmp-ra.232 rbp)
                                            (tmp.183 tmp.96 tmp.97 tmp-ra.232 rbp))
                                           (tmp.96 tmp.97 tmp-ra.232 rbp))
                                          (((((tmp.186 tmp.96 tmp.97 tmp-ra.232 rbp)
                                              (tmp.186 tmp.96 tmp.97 tmp-ra.232 rbp)
                                              (tmp.96 tmp.97 tmp-ra.232 rbp))
                                             (tmp.185 tmp.96 tmp.97 tmp-ra.232 rbp)
                                             (tmp.185 tmp.96 tmp.97 tmp-ra.232 rbp))
                                            (tmp.96 tmp.97 tmp-ra.232 rbp))
                                           ((tmp.97 rax tmp-ra.232 rbp) (tmp-ra.232 rax rbp) (rax rbp))
                                           ((tmp-ra.232 rax rbp) (rax rbp)))
                                          ((tmp-ra.232 rax rbp) (rax rbp)))))
                                       (call-undead ())
                                       (conflicts
                                        ((tmp.183 (rbp tmp-ra.232 tmp.97 tmp.96))
                                         (tmp.97 (rbp tmp-ra.232 tmp.96 tmp.184 tmp.183 tmp.186 tmp.185 rax))
                                         (tmp.184 (tmp.97 rbp tmp-ra.232 tmp.96))
                                         (tmp-ra.232
                                          (tmp.97 tmp.96 rbp rsi rdi tmp.184 tmp.183 tmp.186 tmp.185 rax))
                                         (tmp.186 (tmp.96 rbp tmp-ra.232 tmp.97))
                                         (tmp.96 (tmp.97 rbp tmp-ra.232 rsi tmp.184 tmp.183 tmp.186 tmp.185))
                                         (tmp.185 (rbp tmp-ra.232 tmp.97 tmp.96))
                                         (rax (tmp.97 rbp tmp-ra.232))
                                         (rbp (tmp.97 tmp.96 tmp-ra.232 tmp.184 tmp.183 tmp.186 tmp.185 rax))
                                         (rdi (tmp-ra.232))
                                         (rsi (tmp.96 tmp-ra.232))))
                                       (assignment ()))
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
                                       (locals (a.19 b.20 c.21 d.22 e.23 f.24 g.25 nfv.235 nfv.234 tmp.187))
                                       (undead-out
                                        ((rdi rsi rdx rcx r8 r9 fv0 tmp-ra.233 rbp)
                                         (rsi rdx rcx r8 r9 fv0 a.19 tmp-ra.233 rbp)
                                         (rdx rcx r8 r9 fv0 b.20 a.19 tmp-ra.233 rbp)
                                         (rcx r8 r9 fv0 c.21 b.20 a.19 tmp-ra.233 rbp)
                                         (r8 r9 fv0 d.22 c.21 b.20 a.19 tmp-ra.233 rbp)
                                         (r9 fv0 e.23 d.22 c.21 b.20 a.19 tmp-ra.233 rbp)
                                         (fv0 f.24 e.23 d.22 c.21 b.20 a.19 tmp-ra.233 rbp)
                                         (g.25 f.24 e.23 d.22 c.21 b.20 a.19 tmp-ra.233 rbp)
                                         ((rax tmp-ra.233 rbp)
                                          ((b.20 c.21 d.22 e.23 f.24 g.25 rdi rbp)
                                           (c.21 d.22 e.23 f.24 g.25 rsi rdi rbp)
                                           (d.22 e.23 f.24 g.25 rdx rsi rdi rbp)
                                           (e.23 f.24 g.25 rcx rdx rsi rdi rbp)
                                           (f.24 g.25 r8 rcx rdx rsi rdi rbp)
                                           (g.25 r9 r8 rcx rdx rsi rdi rbp)
                                           (nfv.234 r9 r8 rcx rdx rsi rdi rbp)
                                           (nfv.235 nfv.234 r9 r8 rcx rdx rsi rdi rbp)
                                           (nfv.235 nfv.234 r9 r8 rcx rdx rsi rdi r15 rbp)
                                           (nfv.235 nfv.234 r9 r8 rcx rdx rsi rdi r15 rbp)))
                                         (tmp.187 tmp-ra.233 rbp)
                                         (tmp.187 tmp-ra.233 rdi rbp)
                                         (tmp-ra.233 rsi rdi rbp)
                                         (rsi rdi r15 rbp)
                                         (rsi rdi r15 rbp)))
                                       (call-undead (tmp-ra.233))
                                       (conflicts
                                        ((tmp-ra.233
                                          (tmp.187
                                           g.25
                                           f.24
                                           e.23
                                           d.22
                                           c.21
                                           b.20
                                           a.19
                                           rbp
                                           fv0
                                           r9
                                           r8
                                           rcx
                                           rdx
                                           rsi
                                           rdi))
                                         (a.19
                                          (g.25 f.24 e.23 d.22 c.21 b.20 rbp tmp-ra.233 fv0 r9 r8 rcx rdx rsi))
                                         (b.20
                                          (rdi g.25 f.24 e.23 d.22 c.21 rbp tmp-ra.233 a.19 fv0 r9 r8 rcx rdx))
                                         (c.21
                                          (rsi rdi g.25 f.24 e.23 d.22 rbp tmp-ra.233 a.19 b.20 fv0 r9 r8 rcx))
                                         (d.22
                                          (rdx rsi rdi g.25 f.24 e.23 rbp tmp-ra.233 a.19 b.20 c.21 fv0 r9 r8))
                                         (e.23
                                          (rcx rdx rsi rdi g.25 f.24 rbp tmp-ra.233 a.19 b.20 c.21 d.22 fv0 r9))
                                         (f.24
                                          (r8 rcx rdx rsi rdi g.25 rbp tmp-ra.233 a.19 b.20 c.21 d.22 e.23 fv0))
                                         (g.25
                                          (r9 r8 rcx rdx rsi rdi rbp tmp-ra.233 a.19 b.20 c.21 d.22 e.23 f.24))
                                         (nfv.235 (r15 rbp rdi rsi rdx rcx r8 r9 nfv.234))
                                         (nfv.234 (r15 nfv.235 rbp rdi rsi rdx rcx r8 r9))
                                         (tmp.187 (rdi rbp tmp-ra.233))
                                         (rdi
                                          (tmp.187
                                           r15
                                           nfv.235
                                           nfv.234
                                           r9
                                           r8
                                           rcx
                                           rdx
                                           rsi
                                           rbp
                                           g.25
                                           f.24
                                           e.23
                                           d.22
                                           c.21
                                           b.20
                                           tmp-ra.233))
                                         (rsi
                                          (r15
                                           nfv.235
                                           nfv.234
                                           r9
                                           r8
                                           rcx
                                           rdx
                                           rbp
                                           rdi
                                           g.25
                                           f.24
                                           e.23
                                           d.22
                                           c.21
                                           a.19
                                           tmp-ra.233))
                                         (rdx
                                          (r15
                                           nfv.235
                                           nfv.234
                                           r9
                                           r8
                                           rcx
                                           rbp
                                           rdi
                                           rsi
                                           g.25
                                           f.24
                                           e.23
                                           d.22
                                           b.20
                                           a.19
                                           tmp-ra.233))
                                         (rcx
                                          (r15
                                           nfv.235
                                           nfv.234
                                           r9
                                           r8
                                           rbp
                                           rdi
                                           rsi
                                           rdx
                                           g.25
                                           f.24
                                           e.23
                                           c.21
                                           b.20
                                           a.19
                                           tmp-ra.233))
                                         (r8
                                          (r15
                                           nfv.235
                                           nfv.234
                                           r9
                                           rbp
                                           rdi
                                           rsi
                                           rdx
                                           rcx
                                           g.25
                                           f.24
                                           d.22
                                           c.21
                                           b.20
                                           a.19
                                           tmp-ra.233))
                                         (r9
                                          (r15
                                           nfv.235
                                           nfv.234
                                           rbp
                                           rdi
                                           rsi
                                           rdx
                                           rcx
                                           r8
                                           g.25
                                           e.23
                                           d.22
                                           c.21
                                           b.20
                                           a.19
                                           tmp-ra.233))
                                         (fv0 (f.24 e.23 d.22 c.21 b.20 a.19 tmp-ra.233))
                                         (rbp
                                          (tmp.187
                                           r15
                                           nfv.235
                                           nfv.234
                                           r9
                                           r8
                                           rcx
                                           rdx
                                           rsi
                                           rdi
                                           g.25
                                           f.24
                                           e.23
                                           d.22
                                           c.21
                                           b.20
                                           a.19
                                           tmp-ra.233))
                                         (r15 (rbp rdi rsi rdx rcx r8 r9 nfv.234 nfv.235))))
                                       (assignment ((tmp-ra.233 fv1))))
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
                                       (locals (tmp-ra.236 a.26 b.27 c.28 d.29 e.30 f.31 g.32 h.33))
                                       (undead-out
                                        ((rdi rsi rdx rcx r8 r9 fv0 fv1 tmp-ra.236 rbp)
                                         (rsi rdx rcx r8 r9 fv0 fv1 a.26 tmp-ra.236 rbp)
                                         (rdx rcx r8 r9 fv0 fv1 a.26 b.27 tmp-ra.236 rbp)
                                         (rcx r8 r9 fv0 fv1 a.26 b.27 c.28 tmp-ra.236 rbp)
                                         (r8 r9 fv0 fv1 a.26 b.27 c.28 d.29 tmp-ra.236 rbp)
                                         (r9 fv0 fv1 a.26 b.27 c.28 d.29 e.30 tmp-ra.236 rbp)
                                         (fv0 fv1 a.26 b.27 c.28 d.29 e.30 f.31 tmp-ra.236 rbp)
                                         (fv1 a.26 b.27 c.28 d.29 e.30 f.31 g.32 tmp-ra.236 rbp)
                                         (a.26 b.27 c.28 d.29 e.30 f.31 g.32 h.33 tmp-ra.236 rbp)
                                         (b.27 c.28 d.29 e.30 f.31 g.32 h.33 tmp-ra.236 rdi rbp)
                                         (c.28 d.29 e.30 f.31 g.32 h.33 tmp-ra.236 rsi rdi rbp)
                                         (d.29 e.30 f.31 g.32 h.33 tmp-ra.236 rdx rsi rdi rbp)
                                         (e.30 f.31 g.32 h.33 tmp-ra.236 rcx rdx rsi rdi rbp)
                                         (f.31 g.32 h.33 tmp-ra.236 r8 rcx rdx rsi rdi rbp)
                                         (g.32 h.33 tmp-ra.236 r9 r8 rcx rdx rsi rdi rbp)
                                         (h.33 tmp-ra.236 fv0 r9 r8 rcx rdx rsi rdi rbp)
                                         (tmp-ra.236 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)
                                         (tmp-ra.236 fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)
                                         (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi r15 rbp)
                                         (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi r15 rbp)))
                                       (call-undead ())
                                       (conflicts
                                        ((tmp-ra.236
                                          (fv2
                                           h.33
                                           g.32
                                           f.31
                                           e.30
                                           d.29
                                           c.28
                                           b.27
                                           a.26
                                           rbp
                                           fv1
                                           fv0
                                           r9
                                           r8
                                           rcx
                                           rdx
                                           rsi
                                           rdi))
                                         (a.26
                                          (h.33
                                           g.32
                                           f.31
                                           e.30
                                           d.29
                                           c.28
                                           b.27
                                           rbp
                                           tmp-ra.236
                                           fv1
                                           fv0
                                           r9
                                           r8
                                           rcx
                                           rdx
                                           rsi))
                                         (b.27
                                          (rdi
                                           h.33
                                           g.32
                                           f.31
                                           e.30
                                           d.29
                                           c.28
                                           rbp
                                           tmp-ra.236
                                           a.26
                                           fv1
                                           fv0
                                           r9
                                           r8
                                           rcx
                                           rdx))
                                         (c.28
                                          (rsi
                                           rdi
                                           h.33
                                           g.32
                                           f.31
                                           e.30
                                           d.29
                                           rbp
                                           tmp-ra.236
                                           b.27
                                           a.26
                                           fv1
                                           fv0
                                           r9
                                           r8
                                           rcx))
                                         (d.29
                                          (rdx
                                           rsi
                                           rdi
                                           h.33
                                           g.32
                                           f.31
                                           e.30
                                           rbp
                                           tmp-ra.236
                                           c.28
                                           b.27
                                           a.26
                                           fv1
                                           fv0
                                           r9
                                           r8))
                                         (e.30
                                          (rcx
                                           rdx
                                           rsi
                                           rdi
                                           h.33
                                           g.32
                                           f.31
                                           rbp
                                           tmp-ra.236
                                           d.29
                                           c.28
                                           b.27
                                           a.26
                                           fv1
                                           fv0
                                           r9))
                                         (f.31
                                          (r8
                                           rcx
                                           rdx
                                           rsi
                                           rdi
                                           h.33
                                           g.32
                                           rbp
                                           tmp-ra.236
                                           e.30
                                           d.29
                                           c.28
                                           b.27
                                           a.26
                                           fv1
                                           fv0))
                                         (g.32
                                          (r9
                                           r8
                                           rcx
                                           rdx
                                           rsi
                                           rdi
                                           h.33
                                           rbp
                                           tmp-ra.236
                                           f.31
                                           e.30
                                           d.29
                                           c.28
                                           b.27
                                           a.26
                                           fv1))
                                         (h.33
                                          (fv0
                                           r9
                                           r8
                                           rcx
                                           rdx
                                           rsi
                                           rdi
                                           rbp
                                           tmp-ra.236
                                           g.32
                                           f.31
                                           e.30
                                           d.29
                                           c.28
                                           b.27
                                           a.26))
                                         (rdi
                                          (r15
                                           fv2
                                           fv1
                                           fv0
                                           r9
                                           r8
                                           rcx
                                           rdx
                                           rsi
                                           rbp
                                           h.33
                                           g.32
                                           f.31
                                           e.30
                                           d.29
                                           c.28
                                           b.27
                                           tmp-ra.236))
                                         (rsi
                                          (r15
                                           fv2
                                           fv1
                                           fv0
                                           r9
                                           r8
                                           rcx
                                           rdx
                                           rbp
                                           rdi
                                           h.33
                                           g.32
                                           f.31
                                           e.30
                                           d.29
                                           c.28
                                           a.26
                                           tmp-ra.236))
                                         (rdx
                                          (r15
                                           fv2
                                           fv1
                                           fv0
                                           r9
                                           r8
                                           rcx
                                           rbp
                                           rdi
                                           rsi
                                           h.33
                                           g.32
                                           f.31
                                           e.30
                                           d.29
                                           b.27
                                           a.26
                                           tmp-ra.236))
                                         (rcx
                                          (r15
                                           fv2
                                           fv1
                                           fv0
                                           r9
                                           r8
                                           rbp
                                           rdi
                                           rsi
                                           rdx
                                           h.33
                                           g.32
                                           f.31
                                           e.30
                                           c.28
                                           b.27
                                           a.26
                                           tmp-ra.236))
                                         (r8
                                          (r15
                                           fv2
                                           fv1
                                           fv0
                                           r9
                                           rbp
                                           rdi
                                           rsi
                                           rdx
                                           rcx
                                           h.33
                                           g.32
                                           f.31
                                           d.29
                                           c.28
                                           b.27
                                           a.26
                                           tmp-ra.236))
                                         (r9
                                          (r15
                                           fv2
                                           fv1
                                           fv0
                                           rbp
                                           rdi
                                           rsi
                                           rdx
                                           rcx
                                           r8
                                           h.33
                                           g.32
                                           e.30
                                           d.29
                                           c.28
                                           b.27
                                           a.26
                                           tmp-ra.236))
                                         (fv0
                                          (r15
                                           fv2
                                           fv1
                                           rbp
                                           rdi
                                           rsi
                                           rdx
                                           rcx
                                           r8
                                           r9
                                           h.33
                                           f.31
                                           e.30
                                           d.29
                                           c.28
                                           b.27
                                           a.26
                                           tmp-ra.236))
                                         (fv1
                                          (r15
                                           fv2
                                           rbp
                                           rdi
                                           rsi
                                           rdx
                                           rcx
                                           r8
                                           r9
                                           fv0
                                           g.32
                                           f.31
                                           e.30
                                           d.29
                                           c.28
                                           b.27
                                           a.26
                                           tmp-ra.236))
                                         (rbp
                                          (r15
                                           fv2
                                           fv1
                                           fv0
                                           r9
                                           r8
                                           rcx
                                           rdx
                                           rsi
                                           rdi
                                           h.33
                                           g.32
                                           f.31
                                           e.30
                                           d.29
                                           c.28
                                           b.27
                                           a.26
                                           tmp-ra.236))
                                         (fv2 (r15 rbp rdi rsi rdx rcx r8 r9 fv0 fv1 tmp-ra.236))
                                         (r15 (rbp rdi rsi rdx rcx r8 r9 fv0 fv1 fv2))))
                                       (assignment ()))
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
                                       (locals (a.34 b.35 r1.43 r2.44 r3.45 r4.46 r5.47 r6.48 r7.49))
                                       (undead-out
                                        ((rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.237 rbp)
                                         (rsi rdx rcx r8 r9 fv0 fv1 fv2 a.34 tmp-ra.237 rbp)
                                         (rdx rcx r8 r9 fv0 fv1 fv2 b.35 a.34 tmp-ra.237 rbp)
                                         (rcx r8 r9 fv0 fv1 fv2 b.35 a.34 c.36 tmp-ra.237 rbp)
                                         (r8 r9 fv0 fv1 fv2 b.35 a.34 c.36 d.37 tmp-ra.237 rbp)
                                         (r9 fv0 fv1 fv2 b.35 a.34 c.36 d.37 e.38 tmp-ra.237 rbp)
                                         (fv0 fv1 fv2 b.35 a.34 c.36 d.37 e.38 f.39 tmp-ra.237 rbp)
                                         (fv1 fv2 b.35 a.34 c.36 d.37 e.38 f.39 g.40 tmp-ra.237 rbp)
                                         (fv2 b.35 a.34 c.36 d.37 e.38 f.39 g.40 h.41 tmp-ra.237 rbp)
                                         (b.35 a.34 c.36 d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                         ((rax c.36 d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                          ((b.35 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                         (c.36 r1.43 d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                         ((rax d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                          ((c.36 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                         (d.37 r2.44 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                         ((rax e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                          ((d.37 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                         (e.38 r3.45 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                         ((rax f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                          ((e.38 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                         (f.39 r4.46 g.40 h.41 j.42 tmp-ra.237 rbp)
                                         ((rax g.40 h.41 j.42 tmp-ra.237 rbp)
                                          ((f.39 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                         (g.40 r5.47 h.41 j.42 tmp-ra.237 rbp)
                                         ((rax h.41 j.42 tmp-ra.237 rbp)
                                          ((g.40 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                         (h.41 r6.48 j.42 tmp-ra.237 rbp)
                                         ((rax j.42 tmp-ra.237 rbp)
                                          ((h.41 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                         (r7.49 j.42 tmp-ra.237 rbp)
                                         (j.42 tmp-ra.237 rdi rbp)
                                         (tmp-ra.237 rsi rdi rbp)
                                         (rsi rdi r15 rbp)
                                         (rsi rdi r15 rbp)))
                                       (call-undead (c.36 d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237))
                                       (conflicts
                                        ((tmp-ra.237
                                          (r7.49
                                           r6.48
                                           r5.47
                                           r4.46
                                           r3.45
                                           r2.44
                                           r1.43
                                           j.42
                                           h.41
                                           g.40
                                           f.39
                                           e.38
                                           d.37
                                           c.36
                                           b.35
                                           a.34
                                           rbp
                                           fv2
                                           fv1
                                           fv0
                                           r9
                                           r8
                                           rcx
                                           rdx
                                           rsi
                                           rdi))
                                         (a.34
                                          (j.42
                                           h.41
                                           g.40
                                           f.39
                                           e.38
                                           d.37
                                           c.36
                                           b.35
                                           rbp
                                           tmp-ra.237
                                           fv2
                                           fv1
                                           fv0
                                           r9
                                           r8
                                           rcx
                                           rdx
                                           rsi))
                                         (b.35
                                          (rdi
                                           j.42
                                           h.41
                                           g.40
                                           f.39
                                           e.38
                                           d.37
                                           c.36
                                           rbp
                                           tmp-ra.237
                                           a.34
                                           fv2
                                           fv1
                                           fv0
                                           r9
                                           r8
                                           rcx
                                           rdx))
                                         (c.36
                                          (rdi
                                           r1.43
                                           j.42
                                           h.41
                                           g.40
                                           f.39
                                           e.38
                                           d.37
                                           rbp
                                           tmp-ra.237
                                           a.34
                                           b.35
                                           fv2
                                           fv1
                                           fv0
                                           r9
                                           r8
                                           rcx))
                                         (d.37
                                          (rdi
                                           r2.44
                                           r1.43
                                           j.42
                                           h.41
                                           g.40
                                           f.39
                                           e.38
                                           rbp
                                           tmp-ra.237
                                           c.36
                                           a.34
                                           b.35
                                           fv2
                                           fv1
                                           fv0
                                           r9
                                           r8))
                                         (e.38
                                          (rdi
                                           r3.45
                                           r2.44
                                           r1.43
                                           j.42
                                           h.41
                                           g.40
                                           f.39
                                           rbp
                                           tmp-ra.237
                                           d.37
                                           c.36
                                           a.34
                                           b.35
                                           fv2
                                           fv1
                                           fv0
                                           r9))
                                         (f.39
                                          (rdi
                                           r4.46
                                           r3.45
                                           r2.44
                                           r1.43
                                           j.42
                                           h.41
                                           g.40
                                           rbp
                                           tmp-ra.237
                                           e.38
                                           d.37
                                           c.36
                                           a.34
                                           b.35
                                           fv2
                                           fv1
                                           fv0))
                                         (g.40
                                          (rdi
                                           r5.47
                                           r4.46
                                           r3.45
                                           r2.44
                                           r1.43
                                           j.42
                                           h.41
                                           rbp
                                           tmp-ra.237
                                           f.39
                                           e.38
                                           d.37
                                           c.36
                                           a.34
                                           b.35
                                           fv2
                                           fv1))
                                         (h.41
                                          (rdi
                                           r6.48
                                           r5.47
                                           r4.46
                                           r3.45
                                           r2.44
                                           r1.43
                                           j.42
                                           rbp
                                           tmp-ra.237
                                           g.40
                                           f.39
                                           e.38
                                           d.37
                                           c.36
                                           a.34
                                           b.35
                                           fv2))
                                         (j.42
                                          (rdi
                                           r7.49
                                           r6.48
                                           r5.47
                                           r4.46
                                           r3.45
                                           r2.44
                                           r1.43
                                           rbp
                                           tmp-ra.237
                                           h.41
                                           g.40
                                           f.39
                                           e.38
                                           d.37
                                           c.36
                                           a.34
                                           b.35))
                                         (r1.43 (rbp tmp-ra.237 j.42 h.41 g.40 f.39 e.38 d.37 c.36))
                                         (r2.44 (rbp tmp-ra.237 j.42 h.41 g.40 f.39 e.38 d.37))
                                         (r3.45 (rbp tmp-ra.237 j.42 h.41 g.40 f.39 e.38))
                                         (r4.46 (rbp tmp-ra.237 j.42 h.41 g.40 f.39))
                                         (r5.47 (rbp tmp-ra.237 j.42 h.41 g.40))
                                         (r6.48 (rbp tmp-ra.237 j.42 h.41))
                                         (r7.49 (rbp tmp-ra.237 j.42))
                                         (rdi (j.42 h.41 g.40 f.39 e.38 d.37 c.36 r15 rsi rbp b.35 tmp-ra.237))
                                         (rsi (r15 rbp rdi a.34 tmp-ra.237))
                                         (rdx (b.35 a.34 tmp-ra.237))
                                         (rcx (c.36 b.35 a.34 tmp-ra.237))
                                         (r8 (d.37 c.36 b.35 a.34 tmp-ra.237))
                                         (r9 (e.38 d.37 c.36 b.35 a.34 tmp-ra.237))
                                         (fv0 (f.39 e.38 d.37 c.36 b.35 a.34 tmp-ra.237))
                                         (fv1 (g.40 f.39 e.38 d.37 c.36 b.35 a.34 tmp-ra.237))
                                         (fv2 (h.41 g.40 f.39 e.38 d.37 c.36 b.35 a.34 tmp-ra.237))
                                         (rbp
                                          (r7.49
                                           r6.48
                                           r5.47
                                           r4.46
                                           r3.45
                                           r2.44
                                           r1.43
                                           r15
                                           rsi
                                           rdi
                                           j.42
                                           h.41
                                           g.40
                                           f.39
                                           e.38
                                           d.37
                                           c.36
                                           b.35
                                           a.34
                                           tmp-ra.237))
                                         (r15 (rbp rdi rsi))))
                                       (assignment
                                        ((tmp-ra.237 fv3)
                                         (j.42 fv0)
                                         (h.41 fv1)
                                         (g.40 fv4)
                                         (f.39 fv5)
                                         (e.38 fv6)
                                         (d.37 fv7)
                                         (c.36 fv8))))
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
                     ((locals (tmp-ra.238))
                      (conflicts
                       ((tmp-ra.238 (fv0 r9 r8 rcx rdx rsi rdi rbp))
                        (rbp (r15 fv0 r9 r8 rcx rdx rsi rdi tmp-ra.238))
                        (rdi (r15 fv0 r9 r8 rcx rdx rsi rbp tmp-ra.238))
                        (rsi (r15 fv0 r9 r8 rcx rdx rbp rdi tmp-ra.238))
                        (rdx (r15 fv0 r9 r8 rcx rbp rdi rsi tmp-ra.238))
                        (rcx (r15 fv0 r9 r8 rbp rdi rsi rdx tmp-ra.238))
                        (r8 (r15 fv0 r9 rbp rdi rsi rdx rcx tmp-ra.238))
                        (r9 (r15 fv0 rbp rdi rsi rdx rcx r8 tmp-ra.238))
                        (fv0 (r15 rbp rdi rsi rdx rcx r8 r9 tmp-ra.238))
                        (r15 (rbp rdi rsi rdx rcx r8 r9 fv0))))
                      (assignment ()))
                   (define L.+.31
                     ((locals (tmp.185 tmp.96 tmp.186 tmp-ra.232 tmp.184 tmp.97 tmp.183))
                      (conflicts
                       ((tmp.183 (rbp tmp-ra.232 tmp.97 tmp.96))
                        (tmp.97 (rbp tmp-ra.232 tmp.96 tmp.184 tmp.183 tmp.186 tmp.185 rax))
                        (tmp.184 (tmp.97 rbp tmp-ra.232 tmp.96))
                        (tmp-ra.232
                         (tmp.97 tmp.96 rbp rsi rdi tmp.184 tmp.183 tmp.186 tmp.185 rax))
                        (tmp.186 (tmp.96 rbp tmp-ra.232 tmp.97))
                        (tmp.96 (tmp.97 rbp tmp-ra.232 rsi tmp.184 tmp.183 tmp.186 tmp.185))
                        (tmp.185 (rbp tmp-ra.232 tmp.97 tmp.96))
                        (rax (tmp.97 rbp tmp-ra.232))
                        (rbp (tmp.97 tmp.96 tmp-ra.232 tmp.184 tmp.183 tmp.186 tmp.185 rax))
                        (rdi (tmp-ra.232))
                        (rsi (tmp.96 tmp-ra.232))))
                      (assignment ()))
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
                     ((locals (tmp.187 nfv.234 nfv.235 g.25 f.24 e.23 d.22 c.21 b.20 a.19))
                      (conflicts
                       ((tmp-ra.233
                         (tmp.187
                          g.25
                          f.24
                          e.23
                          d.22
                          c.21
                          b.20
                          a.19
                          rbp
                          fv0
                          r9
                          r8
                          rcx
                          rdx
                          rsi
                          rdi))
                        (a.19
                         (g.25 f.24 e.23 d.22 c.21 b.20 rbp tmp-ra.233 fv0 r9 r8 rcx rdx rsi))
                        (b.20
                         (rdi g.25 f.24 e.23 d.22 c.21 rbp tmp-ra.233 a.19 fv0 r9 r8 rcx rdx))
                        (c.21
                         (rsi rdi g.25 f.24 e.23 d.22 rbp tmp-ra.233 a.19 b.20 fv0 r9 r8 rcx))
                        (d.22
                         (rdx rsi rdi g.25 f.24 e.23 rbp tmp-ra.233 a.19 b.20 c.21 fv0 r9 r8))
                        (e.23
                         (rcx rdx rsi rdi g.25 f.24 rbp tmp-ra.233 a.19 b.20 c.21 d.22 fv0 r9))
                        (f.24
                         (r8 rcx rdx rsi rdi g.25 rbp tmp-ra.233 a.19 b.20 c.21 d.22 e.23 fv0))
                        (g.25
                         (r9 r8 rcx rdx rsi rdi rbp tmp-ra.233 a.19 b.20 c.21 d.22 e.23 f.24))
                        (nfv.235 (r15 rbp rdi rsi rdx rcx r8 r9 nfv.234))
                        (nfv.234 (r15 nfv.235 rbp rdi rsi rdx rcx r8 r9))
                        (tmp.187 (rdi rbp tmp-ra.233))
                        (rdi
                         (tmp.187
                          r15
                          nfv.235
                          nfv.234
                          r9
                          r8
                          rcx
                          rdx
                          rsi
                          rbp
                          g.25
                          f.24
                          e.23
                          d.22
                          c.21
                          b.20
                          tmp-ra.233))
                        (rsi
                         (r15
                          nfv.235
                          nfv.234
                          r9
                          r8
                          rcx
                          rdx
                          rbp
                          rdi
                          g.25
                          f.24
                          e.23
                          d.22
                          c.21
                          a.19
                          tmp-ra.233))
                        (rdx
                         (r15
                          nfv.235
                          nfv.234
                          r9
                          r8
                          rcx
                          rbp
                          rdi
                          rsi
                          g.25
                          f.24
                          e.23
                          d.22
                          b.20
                          a.19
                          tmp-ra.233))
                        (rcx
                         (r15
                          nfv.235
                          nfv.234
                          r9
                          r8
                          rbp
                          rdi
                          rsi
                          rdx
                          g.25
                          f.24
                          e.23
                          c.21
                          b.20
                          a.19
                          tmp-ra.233))
                        (r8
                         (r15
                          nfv.235
                          nfv.234
                          r9
                          rbp
                          rdi
                          rsi
                          rdx
                          rcx
                          g.25
                          f.24
                          d.22
                          c.21
                          b.20
                          a.19
                          tmp-ra.233))
                        (r9
                         (r15
                          nfv.235
                          nfv.234
                          rbp
                          rdi
                          rsi
                          rdx
                          rcx
                          r8
                          g.25
                          e.23
                          d.22
                          c.21
                          b.20
                          a.19
                          tmp-ra.233))
                        (fv0 (f.24 e.23 d.22 c.21 b.20 a.19 tmp-ra.233))
                        (rbp
                         (tmp.187
                          r15
                          nfv.235
                          nfv.234
                          r9
                          r8
                          rcx
                          rdx
                          rsi
                          rdi
                          g.25
                          f.24
                          e.23
                          d.22
                          c.21
                          b.20
                          a.19
                          tmp-ra.233))
                        (r15 (rbp rdi rsi rdx rcx r8 r9 nfv.234 nfv.235))))
                      (assignment ((tmp-ra.233 fv1))))
                     (begin
                       (set! tmp-ra.233 r15)
                       (set! a.19 rdi)
                       (set! b.20 rsi)
                       (set! c.21 rdx)
                       (set! d.22 rcx)
                       (set! e.23 r8)
                       (set! f.24 r9)
                       (set! g.25 fv0)
                       (begin
                         (set! rbp (- rbp 16))
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
                         (set! rbp (+ rbp 16)))
                       (set! tmp.187 rax)
                       (set! rdi 80)
                       (set! rsi tmp.187)
                       (set! r15 tmp-ra.233)
                       (jump L.+.31 rbp r15 rdi rsi)))
                   (define L.G.7
                     ((locals (h.33 g.32 f.31 e.30 d.29 c.28 b.27 a.26 tmp-ra.236))
                      (conflicts
                       ((tmp-ra.236
                         (fv2
                          h.33
                          g.32
                          f.31
                          e.30
                          d.29
                          c.28
                          b.27
                          a.26
                          rbp
                          fv1
                          fv0
                          r9
                          r8
                          rcx
                          rdx
                          rsi
                          rdi))
                        (a.26
                         (h.33
                          g.32
                          f.31
                          e.30
                          d.29
                          c.28
                          b.27
                          rbp
                          tmp-ra.236
                          fv1
                          fv0
                          r9
                          r8
                          rcx
                          rdx
                          rsi))
                        (b.27
                         (rdi
                          h.33
                          g.32
                          f.31
                          e.30
                          d.29
                          c.28
                          rbp
                          tmp-ra.236
                          a.26
                          fv1
                          fv0
                          r9
                          r8
                          rcx
                          rdx))
                        (c.28
                         (rsi
                          rdi
                          h.33
                          g.32
                          f.31
                          e.30
                          d.29
                          rbp
                          tmp-ra.236
                          b.27
                          a.26
                          fv1
                          fv0
                          r9
                          r8
                          rcx))
                        (d.29
                         (rdx
                          rsi
                          rdi
                          h.33
                          g.32
                          f.31
                          e.30
                          rbp
                          tmp-ra.236
                          c.28
                          b.27
                          a.26
                          fv1
                          fv0
                          r9
                          r8))
                        (e.30
                         (rcx
                          rdx
                          rsi
                          rdi
                          h.33
                          g.32
                          f.31
                          rbp
                          tmp-ra.236
                          d.29
                          c.28
                          b.27
                          a.26
                          fv1
                          fv0
                          r9))
                        (f.31
                         (r8
                          rcx
                          rdx
                          rsi
                          rdi
                          h.33
                          g.32
                          rbp
                          tmp-ra.236
                          e.30
                          d.29
                          c.28
                          b.27
                          a.26
                          fv1
                          fv0))
                        (g.32
                         (r9
                          r8
                          rcx
                          rdx
                          rsi
                          rdi
                          h.33
                          rbp
                          tmp-ra.236
                          f.31
                          e.30
                          d.29
                          c.28
                          b.27
                          a.26
                          fv1))
                        (h.33
                         (fv0
                          r9
                          r8
                          rcx
                          rdx
                          rsi
                          rdi
                          rbp
                          tmp-ra.236
                          g.32
                          f.31
                          e.30
                          d.29
                          c.28
                          b.27
                          a.26))
                        (rdi
                         (r15
                          fv2
                          fv1
                          fv0
                          r9
                          r8
                          rcx
                          rdx
                          rsi
                          rbp
                          h.33
                          g.32
                          f.31
                          e.30
                          d.29
                          c.28
                          b.27
                          tmp-ra.236))
                        (rsi
                         (r15
                          fv2
                          fv1
                          fv0
                          r9
                          r8
                          rcx
                          rdx
                          rbp
                          rdi
                          h.33
                          g.32
                          f.31
                          e.30
                          d.29
                          c.28
                          a.26
                          tmp-ra.236))
                        (rdx
                         (r15
                          fv2
                          fv1
                          fv0
                          r9
                          r8
                          rcx
                          rbp
                          rdi
                          rsi
                          h.33
                          g.32
                          f.31
                          e.30
                          d.29
                          b.27
                          a.26
                          tmp-ra.236))
                        (rcx
                         (r15
                          fv2
                          fv1
                          fv0
                          r9
                          r8
                          rbp
                          rdi
                          rsi
                          rdx
                          h.33
                          g.32
                          f.31
                          e.30
                          c.28
                          b.27
                          a.26
                          tmp-ra.236))
                        (r8
                         (r15
                          fv2
                          fv1
                          fv0
                          r9
                          rbp
                          rdi
                          rsi
                          rdx
                          rcx
                          h.33
                          g.32
                          f.31
                          d.29
                          c.28
                          b.27
                          a.26
                          tmp-ra.236))
                        (r9
                         (r15
                          fv2
                          fv1
                          fv0
                          rbp
                          rdi
                          rsi
                          rdx
                          rcx
                          r8
                          h.33
                          g.32
                          e.30
                          d.29
                          c.28
                          b.27
                          a.26
                          tmp-ra.236))
                        (fv0
                         (r15
                          fv2
                          fv1
                          rbp
                          rdi
                          rsi
                          rdx
                          rcx
                          r8
                          r9
                          h.33
                          f.31
                          e.30
                          d.29
                          c.28
                          b.27
                          a.26
                          tmp-ra.236))
                        (fv1
                         (r15
                          fv2
                          rbp
                          rdi
                          rsi
                          rdx
                          rcx
                          r8
                          r9
                          fv0
                          g.32
                          f.31
                          e.30
                          d.29
                          c.28
                          b.27
                          a.26
                          tmp-ra.236))
                        (rbp
                         (r15
                          fv2
                          fv1
                          fv0
                          r9
                          r8
                          rcx
                          rdx
                          rsi
                          rdi
                          h.33
                          g.32
                          f.31
                          e.30
                          d.29
                          c.28
                          b.27
                          a.26
                          tmp-ra.236))
                        (fv2 (r15 rbp rdi rsi rdx rcx r8 r9 fv0 fv1 tmp-ra.236))
                        (r15 (rbp rdi rsi rdx rcx r8 r9 fv0 fv1 fv2))))
                      (assignment ()))
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
                     ((locals (r7.49 r6.48 r5.47 r4.46 r3.45 r2.44 r1.43 b.35 a.34))
                      (conflicts
                       ((tmp-ra.237
                         (r7.49
                          r6.48
                          r5.47
                          r4.46
                          r3.45
                          r2.44
                          r1.43
                          j.42
                          h.41
                          g.40
                          f.39
                          e.38
                          d.37
                          c.36
                          b.35
                          a.34
                          rbp
                          fv2
                          fv1
                          fv0
                          r9
                          r8
                          rcx
                          rdx
                          rsi
                          rdi))
                        (a.34
                         (j.42
                          h.41
                          g.40
                          f.39
                          e.38
                          d.37
                          c.36
                          b.35
                          rbp
                          tmp-ra.237
                          fv2
                          fv1
                          fv0
                          r9
                          r8
                          rcx
                          rdx
                          rsi))
                        (b.35
                         (rdi
                          j.42
                          h.41
                          g.40
                          f.39
                          e.38
                          d.37
                          c.36
                          rbp
                          tmp-ra.237
                          a.34
                          fv2
                          fv1
                          fv0
                          r9
                          r8
                          rcx
                          rdx))
                        (c.36
                         (rdi
                          r1.43
                          j.42
                          h.41
                          g.40
                          f.39
                          e.38
                          d.37
                          rbp
                          tmp-ra.237
                          a.34
                          b.35
                          fv2
                          fv1
                          fv0
                          r9
                          r8
                          rcx))
                        (d.37
                         (rdi
                          r2.44
                          r1.43
                          j.42
                          h.41
                          g.40
                          f.39
                          e.38
                          rbp
                          tmp-ra.237
                          c.36
                          a.34
                          b.35
                          fv2
                          fv1
                          fv0
                          r9
                          r8))
                        (e.38
                         (rdi
                          r3.45
                          r2.44
                          r1.43
                          j.42
                          h.41
                          g.40
                          f.39
                          rbp
                          tmp-ra.237
                          d.37
                          c.36
                          a.34
                          b.35
                          fv2
                          fv1
                          fv0
                          r9))
                        (f.39
                         (rdi
                          r4.46
                          r3.45
                          r2.44
                          r1.43
                          j.42
                          h.41
                          g.40
                          rbp
                          tmp-ra.237
                          e.38
                          d.37
                          c.36
                          a.34
                          b.35
                          fv2
                          fv1
                          fv0))
                        (g.40
                         (rdi
                          r5.47
                          r4.46
                          r3.45
                          r2.44
                          r1.43
                          j.42
                          h.41
                          rbp
                          tmp-ra.237
                          f.39
                          e.38
                          d.37
                          c.36
                          a.34
                          b.35
                          fv2
                          fv1))
                        (h.41
                         (rdi
                          r6.48
                          r5.47
                          r4.46
                          r3.45
                          r2.44
                          r1.43
                          j.42
                          rbp
                          tmp-ra.237
                          g.40
                          f.39
                          e.38
                          d.37
                          c.36
                          a.34
                          b.35
                          fv2))
                        (j.42
                         (rdi
                          r7.49
                          r6.48
                          r5.47
                          r4.46
                          r3.45
                          r2.44
                          r1.43
                          rbp
                          tmp-ra.237
                          h.41
                          g.40
                          f.39
                          e.38
                          d.37
                          c.36
                          a.34
                          b.35))
                        (r1.43 (rbp tmp-ra.237 j.42 h.41 g.40 f.39 e.38 d.37 c.36))
                        (r2.44 (rbp tmp-ra.237 j.42 h.41 g.40 f.39 e.38 d.37))
                        (r3.45 (rbp tmp-ra.237 j.42 h.41 g.40 f.39 e.38))
                        (r4.46 (rbp tmp-ra.237 j.42 h.41 g.40 f.39))
                        (r5.47 (rbp tmp-ra.237 j.42 h.41 g.40))
                        (r6.48 (rbp tmp-ra.237 j.42 h.41))
                        (r7.49 (rbp tmp-ra.237 j.42))
                        (rdi (j.42 h.41 g.40 f.39 e.38 d.37 c.36 r15 rsi rbp b.35 tmp-ra.237))
                        (rsi (r15 rbp rdi a.34 tmp-ra.237))
                        (rdx (b.35 a.34 tmp-ra.237))
                        (rcx (c.36 b.35 a.34 tmp-ra.237))
                        (r8 (d.37 c.36 b.35 a.34 tmp-ra.237))
                        (r9 (e.38 d.37 c.36 b.35 a.34 tmp-ra.237))
                        (fv0 (f.39 e.38 d.37 c.36 b.35 a.34 tmp-ra.237))
                        (fv1 (g.40 f.39 e.38 d.37 c.36 b.35 a.34 tmp-ra.237))
                        (fv2 (h.41 g.40 f.39 e.38 d.37 c.36 b.35 a.34 tmp-ra.237))
                        (rbp
                         (r7.49
                          r6.48
                          r5.47
                          r4.46
                          r3.45
                          r2.44
                          r1.43
                          r15
                          rsi
                          rdi
                          j.42
                          h.41
                          g.40
                          f.39
                          e.38
                          d.37
                          c.36
                          b.35
                          a.34
                          tmp-ra.237))
                        (r15 (rbp rdi rsi))))
                      (assignment
                       ((tmp-ra.237 fv3)
                        (j.42 fv0)
                        (h.41 fv1)
                        (g.40 fv4)
                        (f.39 fv5)
                        (e.38 fv6)
                        (d.37 fv7)
                        (c.36 fv8))))
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
                       (begin
                         (set! rbp (- rbp 72))
                         (return-point L.rp.48
                                       (begin
                                         (set! rdi a.34)
                                         (set! rsi b.35)
                                         (set! r15 L.rp.48)
                                         (jump L.+.31 rbp r15 rdi rsi)))
                         (set! rbp (+ rbp 72)))
                       (set! r1.43 rax)
                       (begin
                         (set! rbp (- rbp 72))
                         (return-point L.rp.49
                                       (begin
                                         (set! rdi r1.43)
                                         (set! rsi c.36)
                                         (set! r15 L.rp.49)
                                         (jump L.+.31 rbp r15 rdi rsi)))
                         (set! rbp (+ rbp 72)))
                       (set! r2.44 rax)
                       (begin
                         (set! rbp (- rbp 72))
                         (return-point L.rp.50
                                       (begin
                                         (set! rdi r2.44)
                                         (set! rsi d.37)
                                         (set! r15 L.rp.50)
                                         (jump L.+.31 rbp r15 rdi rsi)))
                         (set! rbp (+ rbp 72)))
                       (set! r3.45 rax)
                       (begin
                         (set! rbp (- rbp 72))
                         (return-point L.rp.51
                                       (begin
                                         (set! rdi r3.45)
                                         (set! rsi e.38)
                                         (set! r15 L.rp.51)
                                         (jump L.+.31 rbp r15 rdi rsi)))
                         (set! rbp (+ rbp 72)))
                       (set! r4.46 rax)
                       (begin
                         (set! rbp (- rbp 72))
                         (return-point L.rp.52
                                       (begin
                                         (set! rdi r4.46)
                                         (set! rsi f.39)
                                         (set! r15 L.rp.52)
                                         (jump L.+.31 rbp r15 rdi rsi)))
                         (set! rbp (+ rbp 72)))
                       (set! r5.47 rax)
                       (begin
                         (set! rbp (- rbp 72))
                         (return-point L.rp.53
                                       (begin
                                         (set! rdi r5.47)
                                         (set! rsi g.40)
                                         (set! r15 L.rp.53)
                                         (jump L.+.31 rbp r15 rdi rsi)))
                         (set! rbp (+ rbp 72)))
                       (set! r6.48 rax)
                       (begin
                         (set! rbp (- rbp 72))
                         (return-point L.rp.54
                                       (begin
                                         (set! rdi r6.48)
                                         (set! rsi h.41)
                                         (set! r15 L.rp.54)
                                         (jump L.+.31 rbp r15 rdi rsi)))
                         (set! rbp (+ rbp 72)))
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

  (check-equal? (allocate-frames '(module
                                      ((new-frames ())
                                       (locals (tmp-ra.45))
                                       (call-undead ())
                                       (undead-out
                                        ((tmp-ra.45 rbp)
                                         (tmp-ra.45 rdi rbp)
                                         (tmp-ra.45 rsi rdi rbp)
                                         (tmp-ra.45 rdx rsi rdi rbp)
                                         (tmp-ra.45 rcx rdx rsi rdi rbp)
                                         (tmp-ra.45 r8 rcx rdx rsi rdi rbp)
                                         (tmp-ra.45 r9 r8 rcx rdx rsi rdi rbp)
                                         (tmp-ra.45 fv0 r9 r8 rcx rdx rsi rdi rbp)
                                         (tmp-ra.45 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)
                                         (tmp-ra.45 fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)
                                         (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi r15 rbp)
                                         (rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2)))
                                       (conflicts
                                        ((tmp-ra.45 (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp))
                                         (rbp (r15 fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi tmp-ra.45))
                                         (rdi (r15 fv2 fv1 fv0 r9 r8 rcx rdx rsi tmp-ra.45 rbp))
                                         (rsi (r15 fv2 fv1 fv0 r9 r8 rcx rdx tmp-ra.45 rdi rbp))
                                         (rdx (r15 fv2 fv1 fv0 r9 r8 rcx tmp-ra.45 rsi rdi rbp))
                                         (rcx (r15 fv2 fv1 fv0 r9 r8 tmp-ra.45 rdx rsi rdi rbp))
                                         (r8 (r15 fv2 fv1 fv0 r9 tmp-ra.45 rcx rdx rsi rdi rbp))
                                         (r9 (r15 fv2 fv1 fv0 tmp-ra.45 r8 rcx rdx rsi rdi rbp))
                                         (fv0 (r15 fv2 fv1 tmp-ra.45 r9 r8 rcx rdx rsi rdi rbp))
                                         (fv1 (r15 fv2 tmp-ra.45 fv0 r9 r8 rcx rdx rsi rdi rbp))
                                         (fv2 (r15 tmp-ra.45 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp))
                                         (r15 (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp))))
                                       (assignment ()))
                                    (define L.*.2
                                      ((new-frames ())
                                       (locals (tmp.26 tmp.1 tmp.23 tmp-ra.39 tmp.25 tmp.2 tmp.27 tmp.24))
                                       (undead-out
                                        ((rdi rsi rbp tmp-ra.39)
                                         (rsi tmp.1 rbp tmp-ra.39)
                                         (tmp.2 tmp.1 rbp tmp-ra.39)
                                         ((((((tmp.24 tmp.2 tmp.1 rbp tmp-ra.39)
                                              (tmp.24 tmp.2 tmp.1 rbp tmp-ra.39))
                                             (tmp.2 tmp.1 rbp tmp-ra.39))
                                            (tmp.23 tmp.2 tmp.1 rbp tmp-ra.39)
                                            (tmp.23 tmp.2 tmp.1 rbp tmp-ra.39))
                                           (tmp.2 tmp.1 rbp tmp-ra.39))
                                          ((((((tmp.26 tmp.2 tmp.1 rbp tmp-ra.39)
                                               (tmp.26 tmp.2 tmp.1 rbp tmp-ra.39))
                                              (tmp.2 tmp.1 rbp tmp-ra.39))
                                             (tmp.25 tmp.2 tmp.1 rbp tmp-ra.39)
                                             (tmp.25 tmp.2 tmp.1 rbp tmp-ra.39))
                                            (tmp.2 tmp.1 rbp tmp-ra.39))
                                           ((tmp.27 tmp.1 rbp tmp-ra.39)
                                            (tmp.1 tmp.27 rbp tmp-ra.39)
                                            (tmp.27 rax rbp tmp-ra.39)
                                            (rax rbp tmp-ra.39)
                                            (rbp rax))
                                           ((rax rbp tmp-ra.39) (rbp rax)))
                                          ((rax rbp tmp-ra.39) (rbp rax)))))
                                       (call-undead ())
                                       (conflicts
                                        ((tmp.26 (tmp.1 tmp.2 rbp tmp-ra.39))
                                         (tmp.1 (tmp.27 tmp.25 tmp.26 tmp.23 tmp.24 tmp.2 rsi rbp tmp-ra.39))
                                         (tmp.23 (tmp.2 tmp.1 rbp tmp-ra.39))
                                         (tmp-ra.39
                                          (rax tmp.27 tmp.25 tmp.26 tmp.23 tmp.24 tmp.2 tmp.1 rdi rsi rbp))
                                         (tmp.25 (tmp.2 tmp.1 rbp tmp-ra.39))
                                         (tmp.2 (tmp.25 tmp.26 tmp.23 tmp.24 tmp.1 rbp tmp-ra.39))
                                         (tmp.27 (rax tmp.1 rbp tmp-ra.39))
                                         (tmp.24 (tmp.2 tmp.1 rbp tmp-ra.39))
                                         (rbp (rax tmp.27 tmp.25 tmp.26 tmp.23 tmp.24 tmp.2 tmp.1 tmp-ra.39))
                                         (rsi (tmp.1 tmp-ra.39))
                                         (rdi (tmp-ra.39))
                                         (rax (tmp.27 rbp tmp-ra.39))))
                                       (assignment ()))
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
                                       (locals (tmp.29 tmp.30 tmp.28 tmp.3 tmp-ra.40 tmp.31 tmp.4))
                                       (undead-out
                                        ((rdi rsi rbp tmp-ra.40)
                                         (rsi tmp.3 rbp tmp-ra.40)
                                         (tmp.3 tmp.4 rbp tmp-ra.40)
                                         ((((((tmp.29 tmp.3 tmp.4 rbp tmp-ra.40)
                                              (tmp.29 tmp.3 tmp.4 rbp tmp-ra.40))
                                             (tmp.3 tmp.4 rbp tmp-ra.40))
                                            (tmp.28 tmp.3 tmp.4 rbp tmp-ra.40)
                                            (tmp.28 tmp.3 tmp.4 rbp tmp-ra.40))
                                           (tmp.3 tmp.4 rbp tmp-ra.40))
                                          ((((((tmp.31 tmp.3 tmp.4 rbp tmp-ra.40)
                                               (tmp.31 tmp.3 tmp.4 rbp tmp-ra.40))
                                              (tmp.3 tmp.4 rbp tmp-ra.40))
                                             (tmp.30 tmp.3 tmp.4 rbp tmp-ra.40)
                                             (tmp.30 tmp.3 tmp.4 rbp tmp-ra.40))
                                            (tmp.3 tmp.4 rbp tmp-ra.40))
                                           ((tmp.4 rax rbp tmp-ra.40) (rax rbp tmp-ra.40) (rbp rax))
                                           ((rax rbp tmp-ra.40) (rbp rax)))
                                          ((rax rbp tmp-ra.40) (rbp rax)))))
                                       (call-undead ())
                                       (conflicts
                                        ((tmp.29 (tmp.4 tmp.3 rbp tmp-ra.40))
                                         (tmp.30 (tmp.3 tmp.4 rbp tmp-ra.40))
                                         (tmp.28 (tmp.3 tmp.4 rbp tmp-ra.40))
                                         (tmp.3 (tmp.30 tmp.31 tmp.28 tmp.29 tmp.4 rsi rbp tmp-ra.40))
                                         (tmp-ra.40 (rax tmp.30 tmp.31 tmp.28 tmp.29 tmp.4 tmp.3 rdi rsi rbp))
                                         (tmp.31 (tmp.3 tmp.4 rbp tmp-ra.40))
                                         (tmp.4 (rax tmp.30 tmp.31 tmp.28 tmp.29 tmp.3 rbp tmp-ra.40))
                                         (rbp (rax tmp.30 tmp.31 tmp.28 tmp.29 tmp.4 tmp.3 tmp-ra.40))
                                         (rsi (tmp.3 tmp-ra.40))
                                         (rdi (tmp-ra.40))
                                         (rax (tmp.4 rbp tmp-ra.40))))
                                       (assignment ()))
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
                                       (locals (tmp.33 h.68 tmp.32 tmp.36 tmp.35 g.67 tmp.37 tmp.34))
                                       (undead-out
                                        ((rdi rsi rdx rcx r8 r9 fv0 fv1 tmp-ra.41 rbp)
                                         (rsi rdx rcx r8 r9 fv0 fv1 a.61 tmp-ra.41 rbp)
                                         (rdx rcx r8 r9 fv0 fv1 b.62 a.61 tmp-ra.41 rbp)
                                         (rcx r8 r9 fv0 fv1 b.62 a.61 tmp-ra.41 c.63 rbp)
                                         (r8 r9 fv0 fv1 d.64 b.62 a.61 tmp-ra.41 c.63 rbp)
                                         (r9 fv0 fv1 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp)
                                         (fv0 fv1 f.66 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp)
                                         (fv1 f.66 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 g.67 rbp)
                                         (f.66 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 g.67 h.68 rbp)
                                         ((rax e.65 c.63 tmp-ra.41 a.61 b.62 d.64 f.66 rbp)
                                          ((h.68 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
                                         (e.65 c.63 tmp-ra.41 a.61 b.62 d.64 f.66 tmp.37 rbp)
                                         ((rax d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp)
                                          ((tmp.37 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
                                         (d.64 b.62 a.61 tmp-ra.41 c.63 e.65 tmp.36 rbp)
                                         ((rax c.63 tmp-ra.41 a.61 b.62 d.64 rbp)
                                          ((tmp.36 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
                                         (c.63 tmp-ra.41 a.61 b.62 d.64 tmp.35 rbp)
                                         ((rax b.62 a.61 tmp-ra.41 c.63 rbp)
                                          ((tmp.35 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
                                         (b.62 a.61 tmp-ra.41 c.63 tmp.34 rbp)
                                         ((rax tmp-ra.41 a.61 b.62 rbp)
                                          ((tmp.34 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
                                         (tmp-ra.41 a.61 b.62 tmp.33 rbp)
                                         ((rax a.61 tmp-ra.41 rbp)
                                          ((tmp.33 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
                                         (a.61 tmp.32 tmp-ra.41 rbp)
                                         (tmp.32 tmp-ra.41 rdi rbp)
                                         (tmp-ra.41 rsi rdi rbp)
                                         (rsi rdi r15 rbp)
                                         (rbp r15 rdi rsi)))
                                       (call-undead (f.66 e.65 d.64 c.63 b.62 tmp-ra.41 a.61))
                                       (conflicts
                                        ((tmp.33 (rdi tmp-ra.41 a.61 b.62 rbp))
                                         (tmp-ra.41
                                          (tmp.32
                                           tmp.33
                                           tmp.34
                                           tmp.35
                                           tmp.36
                                           tmp.37
                                           h.68
                                           g.67
                                           f.66
                                           e.65
                                           d.64
                                           c.63
                                           b.62
                                           a.61
                                           rdi
                                           rsi
                                           rdx
                                           rcx
                                           r8
                                           r9
                                           fv0
                                           fv1
                                           rbp))
                                         (b.62
                                          (tmp.33
                                           tmp.34
                                           tmp.35
                                           tmp.36
                                           tmp.37
                                           h.68
                                           g.67
                                           f.66
                                           e.65
                                           d.64
                                           c.63
                                           rdx
                                           rcx
                                           r8
                                           r9
                                           fv0
                                           fv1
                                           a.61
                                           tmp-ra.41
                                           rbp))
                                         (d.64
                                          (tmp.35
                                           tmp.36
                                           tmp.37
                                           h.68
                                           g.67
                                           f.66
                                           e.65
                                           r8
                                           r9
                                           fv0
                                           fv1
                                           b.62
                                           a.61
                                           tmp-ra.41
                                           c.63
                                           rbp))
                                         (f.66
                                          (tmp.37 h.68 g.67 fv0 fv1 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp))
                                         (h.68 (rdi f.66 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 g.67 rbp))
                                         (tmp.32 (rdi a.61 tmp-ra.41 rbp))
                                         (a.61
                                          (tmp.32
                                           tmp.33
                                           tmp.34
                                           tmp.35
                                           tmp.36
                                           tmp.37
                                           h.68
                                           g.67
                                           f.66
                                           e.65
                                           d.64
                                           c.63
                                           b.62
                                           rsi
                                           rdx
                                           rcx
                                           r8
                                           r9
                                           fv0
                                           fv1
                                           tmp-ra.41
                                           rbp))
                                         (tmp.36 (rdi d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp))
                                         (tmp.35 (rdi c.63 tmp-ra.41 a.61 b.62 d.64 rbp))
                                         (g.67 (h.68 fv1 f.66 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp))
                                         (tmp.37 (rdi e.65 c.63 tmp-ra.41 a.61 b.62 d.64 f.66 rbp))
                                         (tmp.34 (rdi b.62 a.61 tmp-ra.41 c.63 rbp))
                                         (c.63
                                          (tmp.34
                                           tmp.35
                                           tmp.36
                                           tmp.37
                                           h.68
                                           g.67
                                           f.66
                                           e.65
                                           d.64
                                           rcx
                                           r8
                                           r9
                                           fv0
                                           fv1
                                           b.62
                                           a.61
                                           tmp-ra.41
                                           rbp))
                                         (e.65
                                          (tmp.36
                                           tmp.37
                                           h.68
                                           g.67
                                           f.66
                                           r9
                                           fv0
                                           fv1
                                           d.64
                                           b.62
                                           a.61
                                           tmp-ra.41
                                           c.63
                                           rbp))
                                         (rbp
                                          (tmp.32
                                           tmp.33
                                           tmp.34
                                           tmp.35
                                           tmp.36
                                           tmp.37
                                           r15
                                           rsi
                                           rdi
                                           h.68
                                           g.67
                                           f.66
                                           e.65
                                           d.64
                                           c.63
                                           b.62
                                           a.61
                                           tmp-ra.41))
                                         (fv1 (g.67 f.66 e.65 d.64 c.63 b.62 a.61 tmp-ra.41))
                                         (fv0 (f.66 e.65 d.64 c.63 b.62 a.61 tmp-ra.41))
                                         (r9 (e.65 d.64 c.63 b.62 a.61 tmp-ra.41))
                                         (r8 (d.64 c.63 b.62 a.61 tmp-ra.41))
                                         (rcx (c.63 b.62 a.61 tmp-ra.41))
                                         (rdx (b.62 a.61 tmp-ra.41))
                                         (rsi (r15 rdi rbp a.61 tmp-ra.41))
                                         (rdi
                                          (tmp.32
                                           tmp.33
                                           tmp.34
                                           tmp.35
                                           tmp.36
                                           tmp.37
                                           r15
                                           rsi
                                           h.68
                                           rbp
                                           tmp-ra.41))
                                         (r15 (rsi rdi rbp))))
                                       (assignment
                                        ((f.66 fv8)
                                         (e.65 fv7)
                                         (d.64 fv6)
                                         (c.63 fv5)
                                         (b.62 fv4)
                                         (tmp-ra.41 fv3)
                                         (a.61 fv2))))
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
                                       (locals (nfv.43 e.73 f.74 b.70 d.72 nfv.44 c.71 h.76 sum.78 g.75 a.69))
                                       (undead-out
                                        ((rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.42 rbp)
                                         (rsi rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.42 a.69 rbp)
                                         (rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.42 a.69 b.70 rbp)
                                         (rcx r8 r9 fv0 fv1 fv2 tmp-ra.42 a.69 b.70 c.71 rbp)
                                         (r8 r9 fv0 fv1 fv2 tmp-ra.42 a.69 b.70 c.71 d.72 rbp)
                                         (r9 fv0 fv1 fv2 tmp-ra.42 a.69 b.70 c.71 d.72 e.73 rbp)
                                         (fv0 fv1 fv2 tmp-ra.42 a.69 b.70 c.71 d.72 e.73 f.74 rbp)
                                         (fv1 fv2 tmp-ra.42 a.69 b.70 c.71 d.72 e.73 f.74 g.75 rbp)
                                         (fv2 tmp-ra.42 a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76 rbp)
                                         (tmp-ra.42 i.77 a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76 rbp)
                                         ((rax i.77 tmp-ra.42 rbp)
                                          ((b.70 c.71 d.72 e.73 f.74 g.75 h.76 rdi rbp)
                                           (c.71 d.72 e.73 f.74 g.75 h.76 rsi rdi rbp)
                                           (d.72 e.73 f.74 g.75 h.76 rdx rsi rdi rbp)
                                           (e.73 f.74 g.75 h.76 rcx rdx rsi rdi rbp)
                                           (f.74 g.75 h.76 r8 rcx rdx rsi rdi rbp)
                                           (g.75 h.76 r9 r8 rcx rdx rsi rdi rbp)
                                           (h.76 nfv.43 r9 r8 rcx rdx rsi rdi rbp)
                                           (nfv.44 nfv.43 r9 r8 rcx rdx rsi rdi rbp)
                                           (nfv.44 nfv.43 r9 r8 rcx rdx rsi rdi r15 rbp)
                                           (rbp r15 rdi rsi rdx rcx r8 r9 nfv.43 nfv.44)))
                                         (sum.78 i.77 tmp-ra.42 rbp)
                                         (i.77 tmp-ra.42 rdi rbp)
                                         (tmp-ra.42 rsi rdi rbp)
                                         (rsi rdi r15 rbp)
                                         (rbp r15 rdi rsi)))
                                       (call-undead (tmp-ra.42 i.77))
                                       (conflicts
                                        ((nfv.43 (r15 nfv.44 h.76 r9 r8 rcx rdx rsi rdi rbp))
                                         (e.73
                                          (rcx
                                           rdx
                                           rsi
                                           rdi
                                           i.77
                                           h.76
                                           g.75
                                           f.74
                                           r9
                                           fv0
                                           fv1
                                           fv2
                                           tmp-ra.42
                                           a.69
                                           b.70
                                           c.71
                                           d.72
                                           rbp))
                                         (f.74
                                          (r8
                                           rcx
                                           rdx
                                           rsi
                                           rdi
                                           i.77
                                           h.76
                                           g.75
                                           fv0
                                           fv1
                                           fv2
                                           tmp-ra.42
                                           a.69
                                           b.70
                                           c.71
                                           d.72
                                           e.73
                                           rbp))
                                         (b.70
                                          (rdi
                                           i.77
                                           h.76
                                           g.75
                                           f.74
                                           e.73
                                           d.72
                                           c.71
                                           rdx
                                           rcx
                                           r8
                                           r9
                                           fv0
                                           fv1
                                           fv2
                                           tmp-ra.42
                                           a.69
                                           rbp))
                                         (d.72
                                          (rdx
                                           rsi
                                           rdi
                                           i.77
                                           h.76
                                           g.75
                                           f.74
                                           e.73
                                           r8
                                           r9
                                           fv0
                                           fv1
                                           fv2
                                           tmp-ra.42
                                           a.69
                                           b.70
                                           c.71
                                           rbp))
                                         (nfv.44 (r15 nfv.43 r9 r8 rcx rdx rsi rdi rbp))
                                         (c.71
                                          (rsi
                                           rdi
                                           i.77
                                           h.76
                                           g.75
                                           f.74
                                           e.73
                                           d.72
                                           rcx
                                           r8
                                           r9
                                           fv0
                                           fv1
                                           fv2
                                           tmp-ra.42
                                           a.69
                                           b.70
                                           rbp))
                                         (tmp-ra.42
                                          (sum.78
                                           i.77
                                           h.76
                                           g.75
                                           f.74
                                           e.73
                                           d.72
                                           c.71
                                           b.70
                                           a.69
                                           rdi
                                           rsi
                                           rdx
                                           rcx
                                           r8
                                           r9
                                           fv0
                                           fv1
                                           fv2
                                           rbp))
                                         (i.77
                                          (rdi sum.78 tmp-ra.42 a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76 rbp))
                                         (h.76
                                          (nfv.43
                                           r9
                                           r8
                                           rcx
                                           rdx
                                           rsi
                                           rdi
                                           i.77
                                           fv2
                                           tmp-ra.42
                                           a.69
                                           b.70
                                           c.71
                                           d.72
                                           e.73
                                           f.74
                                           g.75
                                           rbp))
                                         (sum.78 (i.77 tmp-ra.42 rbp))
                                         (g.75
                                          (r9
                                           r8
                                           rcx
                                           rdx
                                           rsi
                                           rdi
                                           i.77
                                           h.76
                                           fv1
                                           fv2
                                           tmp-ra.42
                                           a.69
                                           b.70
                                           c.71
                                           d.72
                                           e.73
                                           f.74
                                           rbp))
                                         (a.69
                                          (i.77
                                           h.76
                                           g.75
                                           f.74
                                           e.73
                                           d.72
                                           c.71
                                           b.70
                                           rsi
                                           rdx
                                           rcx
                                           r8
                                           r9
                                           fv0
                                           fv1
                                           fv2
                                           tmp-ra.42
                                           rbp))
                                         (rbp
                                          (sum.78
                                           r15
                                           nfv.44
                                           nfv.43
                                           r9
                                           r8
                                           rcx
                                           rdx
                                           rsi
                                           rdi
                                           i.77
                                           h.76
                                           g.75
                                           f.74
                                           e.73
                                           d.72
                                           c.71
                                           b.70
                                           a.69
                                           tmp-ra.42))
                                         (fv2 (h.76 g.75 f.74 e.73 d.72 c.71 b.70 a.69 tmp-ra.42))
                                         (fv1 (g.75 f.74 e.73 d.72 c.71 b.70 a.69 tmp-ra.42))
                                         (fv0 (f.74 e.73 d.72 c.71 b.70 a.69 tmp-ra.42))
                                         (r9
                                          (r15
                                           nfv.44
                                           nfv.43
                                           g.75
                                           h.76
                                           r8
                                           rcx
                                           rdx
                                           rsi
                                           rdi
                                           rbp
                                           e.73
                                           d.72
                                           c.71
                                           b.70
                                           a.69
                                           tmp-ra.42))
                                         (r8
                                          (r15
                                           nfv.44
                                           nfv.43
                                           r9
                                           f.74
                                           g.75
                                           h.76
                                           rcx
                                           rdx
                                           rsi
                                           rdi
                                           rbp
                                           d.72
                                           c.71
                                           b.70
                                           a.69
                                           tmp-ra.42))
                                         (rcx
                                          (r15
                                           nfv.44
                                           nfv.43
                                           r9
                                           r8
                                           e.73
                                           f.74
                                           g.75
                                           h.76
                                           rdx
                                           rsi
                                           rdi
                                           rbp
                                           c.71
                                           b.70
                                           a.69
                                           tmp-ra.42))
                                         (rdx
                                          (r15
                                           nfv.44
                                           nfv.43
                                           r9
                                           r8
                                           rcx
                                           d.72
                                           e.73
                                           f.74
                                           g.75
                                           h.76
                                           rsi
                                           rdi
                                           rbp
                                           b.70
                                           a.69
                                           tmp-ra.42))
                                         (rsi
                                          (r15
                                           nfv.44
                                           nfv.43
                                           r9
                                           r8
                                           rcx
                                           rdx
                                           c.71
                                           d.72
                                           e.73
                                           f.74
                                           g.75
                                           h.76
                                           rdi
                                           rbp
                                           a.69
                                           tmp-ra.42))
                                         (rdi
                                          (i.77
                                           r15
                                           nfv.44
                                           nfv.43
                                           r9
                                           r8
                                           rcx
                                           rdx
                                           rsi
                                           b.70
                                           c.71
                                           d.72
                                           e.73
                                           f.74
                                           g.75
                                           h.76
                                           rbp
                                           tmp-ra.42))
                                         (r15 (nfv.44 nfv.43 r9 r8 rcx rdx rsi rdi rbp))))
                                       (assignment ((tmp-ra.42 fv3) (i.77 fv0))))
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
                     ((locals (tmp-ra.45))
                      (conflicts
                       ((tmp-ra.45 (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp))
                        (rbp (r15 fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi tmp-ra.45))
                        (rdi (r15 fv2 fv1 fv0 r9 r8 rcx rdx rsi tmp-ra.45 rbp))
                        (rsi (r15 fv2 fv1 fv0 r9 r8 rcx rdx tmp-ra.45 rdi rbp))
                        (rdx (r15 fv2 fv1 fv0 r9 r8 rcx tmp-ra.45 rsi rdi rbp))
                        (rcx (r15 fv2 fv1 fv0 r9 r8 tmp-ra.45 rdx rsi rdi rbp))
                        (r8 (r15 fv2 fv1 fv0 r9 tmp-ra.45 rcx rdx rsi rdi rbp))
                        (r9 (r15 fv2 fv1 fv0 tmp-ra.45 r8 rcx rdx rsi rdi rbp))
                        (fv0 (r15 fv2 fv1 tmp-ra.45 r9 r8 rcx rdx rsi rdi rbp))
                        (fv1 (r15 fv2 tmp-ra.45 fv0 r9 r8 rcx rdx rsi rdi rbp))
                        (fv2 (r15 tmp-ra.45 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp))
                        (r15 (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp))))
                      (assignment ()))
                   (define L.*.2
                     ((locals (tmp.24 tmp.27 tmp.2 tmp.25 tmp-ra.39 tmp.23 tmp.1 tmp.26))
                      (conflicts
                       ((tmp.26 (tmp.1 tmp.2 rbp tmp-ra.39))
                        (tmp.1 (tmp.27 tmp.25 tmp.26 tmp.23 tmp.24 tmp.2 rsi rbp tmp-ra.39))
                        (tmp.23 (tmp.2 tmp.1 rbp tmp-ra.39))
                        (tmp-ra.39
                         (rax tmp.27 tmp.25 tmp.26 tmp.23 tmp.24 tmp.2 tmp.1 rdi rsi rbp))
                        (tmp.25 (tmp.2 tmp.1 rbp tmp-ra.39))
                        (tmp.2 (tmp.25 tmp.26 tmp.23 tmp.24 tmp.1 rbp tmp-ra.39))
                        (tmp.27 (rax tmp.1 rbp tmp-ra.39))
                        (tmp.24 (tmp.2 tmp.1 rbp tmp-ra.39))
                        (rbp (rax tmp.27 tmp.25 tmp.26 tmp.23 tmp.24 tmp.2 tmp.1 tmp-ra.39))
                        (rsi (tmp.1 tmp-ra.39))
                        (rdi (tmp-ra.39))
                        (rax (tmp.27 rbp tmp-ra.39))))
                      (assignment ()))
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
                     ((locals (tmp.4 tmp.31 tmp-ra.40 tmp.3 tmp.28 tmp.30 tmp.29))
                      (conflicts
                       ((tmp.29 (tmp.4 tmp.3 rbp tmp-ra.40))
                        (tmp.30 (tmp.3 tmp.4 rbp tmp-ra.40))
                        (tmp.28 (tmp.3 tmp.4 rbp tmp-ra.40))
                        (tmp.3 (tmp.30 tmp.31 tmp.28 tmp.29 tmp.4 rsi rbp tmp-ra.40))
                        (tmp-ra.40 (rax tmp.30 tmp.31 tmp.28 tmp.29 tmp.4 tmp.3 rdi rsi rbp))
                        (tmp.31 (tmp.3 tmp.4 rbp tmp-ra.40))
                        (tmp.4 (rax tmp.30 tmp.31 tmp.28 tmp.29 tmp.3 rbp tmp-ra.40))
                        (rbp (rax tmp.30 tmp.31 tmp.28 tmp.29 tmp.4 tmp.3 tmp-ra.40))
                        (rsi (tmp.3 tmp-ra.40))
                        (rdi (tmp-ra.40))
                        (rax (tmp.4 rbp tmp-ra.40))))
                      (assignment ()))
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
                     ((locals (tmp.34 tmp.37 g.67 tmp.35 tmp.36 tmp.32 h.68 tmp.33))
                      (conflicts
                       ((tmp.33 (rdi tmp-ra.41 a.61 b.62 rbp))
                        (tmp-ra.41
                         (tmp.32
                          tmp.33
                          tmp.34
                          tmp.35
                          tmp.36
                          tmp.37
                          h.68
                          g.67
                          f.66
                          e.65
                          d.64
                          c.63
                          b.62
                          a.61
                          rdi
                          rsi
                          rdx
                          rcx
                          r8
                          r9
                          fv0
                          fv1
                          rbp))
                        (b.62
                         (tmp.33
                          tmp.34
                          tmp.35
                          tmp.36
                          tmp.37
                          h.68
                          g.67
                          f.66
                          e.65
                          d.64
                          c.63
                          rdx
                          rcx
                          r8
                          r9
                          fv0
                          fv1
                          a.61
                          tmp-ra.41
                          rbp))
                        (d.64
                         (tmp.35
                          tmp.36
                          tmp.37
                          h.68
                          g.67
                          f.66
                          e.65
                          r8
                          r9
                          fv0
                          fv1
                          b.62
                          a.61
                          tmp-ra.41
                          c.63
                          rbp))
                        (f.66
                         (tmp.37 h.68 g.67 fv0 fv1 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp))
                        (h.68 (rdi f.66 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 g.67 rbp))
                        (tmp.32 (rdi a.61 tmp-ra.41 rbp))
                        (a.61
                         (tmp.32
                          tmp.33
                          tmp.34
                          tmp.35
                          tmp.36
                          tmp.37
                          h.68
                          g.67
                          f.66
                          e.65
                          d.64
                          c.63
                          b.62
                          rsi
                          rdx
                          rcx
                          r8
                          r9
                          fv0
                          fv1
                          tmp-ra.41
                          rbp))
                        (tmp.36 (rdi d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp))
                        (tmp.35 (rdi c.63 tmp-ra.41 a.61 b.62 d.64 rbp))
                        (g.67 (h.68 fv1 f.66 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp))
                        (tmp.37 (rdi e.65 c.63 tmp-ra.41 a.61 b.62 d.64 f.66 rbp))
                        (tmp.34 (rdi b.62 a.61 tmp-ra.41 c.63 rbp))
                        (c.63
                         (tmp.34
                          tmp.35
                          tmp.36
                          tmp.37
                          h.68
                          g.67
                          f.66
                          e.65
                          d.64
                          rcx
                          r8
                          r9
                          fv0
                          fv1
                          b.62
                          a.61
                          tmp-ra.41
                          rbp))
                        (e.65
                         (tmp.36
                          tmp.37
                          h.68
                          g.67
                          f.66
                          r9
                          fv0
                          fv1
                          d.64
                          b.62
                          a.61
                          tmp-ra.41
                          c.63
                          rbp))
                        (rbp
                         (tmp.32
                          tmp.33
                          tmp.34
                          tmp.35
                          tmp.36
                          tmp.37
                          r15
                          rsi
                          rdi
                          h.68
                          g.67
                          f.66
                          e.65
                          d.64
                          c.63
                          b.62
                          a.61
                          tmp-ra.41))
                        (fv1 (g.67 f.66 e.65 d.64 c.63 b.62 a.61 tmp-ra.41))
                        (fv0 (f.66 e.65 d.64 c.63 b.62 a.61 tmp-ra.41))
                        (r9 (e.65 d.64 c.63 b.62 a.61 tmp-ra.41))
                        (r8 (d.64 c.63 b.62 a.61 tmp-ra.41))
                        (rcx (c.63 b.62 a.61 tmp-ra.41))
                        (rdx (b.62 a.61 tmp-ra.41))
                        (rsi (r15 rdi rbp a.61 tmp-ra.41))
                        (rdi
                         (tmp.32
                          tmp.33
                          tmp.34
                          tmp.35
                          tmp.36
                          tmp.37
                          r15
                          rsi
                          h.68
                          rbp
                          tmp-ra.41))
                        (r15 (rsi rdi rbp))))
                      (assignment
                       ((f.66 fv8)
                        (e.65 fv7)
                        (d.64 fv6)
                        (c.63 fv5)
                        (b.62 fv4)
                        (tmp-ra.41 fv3)
                        (a.61 fv2))))
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
                       (begin
                         (set! rbp (- rbp 72))
                         (return-point L.rp.12
                                       (begin
                                         (set! rdi g.67)
                                         (set! rsi h.68)
                                         (set! r15 L.rp.12)
                                         (jump L.+.1 rbp r15 rdi rsi)))
                         (set! rbp (+ rbp 72)))
                       (set! tmp.37 rax)
                       (begin
                         (set! rbp (- rbp 72))
                         (return-point L.rp.13
                                       (begin
                                         (set! rdi f.66)
                                         (set! rsi tmp.37)
                                         (set! r15 L.rp.13)
                                         (jump L.+.1 rbp r15 rdi rsi)))
                         (set! rbp (+ rbp 72)))
                       (set! tmp.36 rax)
                       (begin
                         (set! rbp (- rbp 72))
                         (return-point L.rp.14
                                       (begin
                                         (set! rdi e.65)
                                         (set! rsi tmp.36)
                                         (set! r15 L.rp.14)
                                         (jump L.+.1 rbp r15 rdi rsi)))
                         (set! rbp (+ rbp 72)))
                       (set! tmp.35 rax)
                       (begin
                         (set! rbp (- rbp 72))
                         (return-point L.rp.15
                                       (begin
                                         (set! rdi d.64)
                                         (set! rsi tmp.35)
                                         (set! r15 L.rp.15)
                                         (jump L.+.1 rbp r15 rdi rsi)))
                         (set! rbp (+ rbp 72)))
                       (set! tmp.34 rax)
                       (begin
                         (set! rbp (- rbp 72))
                         (return-point L.rp.16
                                       (begin
                                         (set! rdi c.63)
                                         (set! rsi tmp.34)
                                         (set! r15 L.rp.16)
                                         (jump L.+.1 rbp r15 rdi rsi)))
                         (set! rbp (+ rbp 72)))
                       (set! tmp.33 rax)
                       (begin
                         (set! rbp (- rbp 72))
                         (return-point L.rp.17
                                       (begin
                                         (set! rdi b.62)
                                         (set! rsi tmp.33)
                                         (set! r15 L.rp.17)
                                         (jump L.+.1 rbp r15 rdi rsi)))
                         (set! rbp (+ rbp 72)))
                       (set! tmp.32 rax)
                       (set! rdi a.61)
                       (set! rsi tmp.32)
                       (set! r15 tmp-ra.41)
                       (jump L.+.1 rbp r15 rdi rsi)))
                   (define L.add-and-multiply.11
                     ((locals (a.69 g.75 sum.78 h.76 c.71 d.72 b.70 f.74 e.73))
                      (conflicts
                       ((nfv.43 (r15 nfv.44 h.76 r9 r8 rcx rdx rsi rdi rbp))
                        (e.73
                         (rcx
                          rdx
                          rsi
                          rdi
                          i.77
                          h.76
                          g.75
                          f.74
                          r9
                          fv0
                          fv1
                          fv2
                          tmp-ra.42
                          a.69
                          b.70
                          c.71
                          d.72
                          rbp))
                        (f.74
                         (r8
                          rcx
                          rdx
                          rsi
                          rdi
                          i.77
                          h.76
                          g.75
                          fv0
                          fv1
                          fv2
                          tmp-ra.42
                          a.69
                          b.70
                          c.71
                          d.72
                          e.73
                          rbp))
                        (b.70
                         (rdi
                          i.77
                          h.76
                          g.75
                          f.74
                          e.73
                          d.72
                          c.71
                          rdx
                          rcx
                          r8
                          r9
                          fv0
                          fv1
                          fv2
                          tmp-ra.42
                          a.69
                          rbp))
                        (d.72
                         (rdx
                          rsi
                          rdi
                          i.77
                          h.76
                          g.75
                          f.74
                          e.73
                          r8
                          r9
                          fv0
                          fv1
                          fv2
                          tmp-ra.42
                          a.69
                          b.70
                          c.71
                          rbp))
                        (nfv.44 (r15 nfv.43 r9 r8 rcx rdx rsi rdi rbp))
                        (c.71
                         (rsi
                          rdi
                          i.77
                          h.76
                          g.75
                          f.74
                          e.73
                          d.72
                          rcx
                          r8
                          r9
                          fv0
                          fv1
                          fv2
                          tmp-ra.42
                          a.69
                          b.70
                          rbp))
                        (tmp-ra.42
                         (sum.78
                          i.77
                          h.76
                          g.75
                          f.74
                          e.73
                          d.72
                          c.71
                          b.70
                          a.69
                          rdi
                          rsi
                          rdx
                          rcx
                          r8
                          r9
                          fv0
                          fv1
                          fv2
                          rbp))
                        (i.77
                         (rdi sum.78 tmp-ra.42 a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76 rbp))
                        (h.76
                         (nfv.43
                          r9
                          r8
                          rcx
                          rdx
                          rsi
                          rdi
                          i.77
                          fv2
                          tmp-ra.42
                          a.69
                          b.70
                          c.71
                          d.72
                          e.73
                          f.74
                          g.75
                          rbp))
                        (sum.78 (i.77 tmp-ra.42 rbp))
                        (g.75
                         (r9
                          r8
                          rcx
                          rdx
                          rsi
                          rdi
                          i.77
                          h.76
                          fv1
                          fv2
                          tmp-ra.42
                          a.69
                          b.70
                          c.71
                          d.72
                          e.73
                          f.74
                          rbp))
                        (a.69
                         (i.77
                          h.76
                          g.75
                          f.74
                          e.73
                          d.72
                          c.71
                          b.70
                          rsi
                          rdx
                          rcx
                          r8
                          r9
                          fv0
                          fv1
                          fv2
                          tmp-ra.42
                          rbp))
                        (rbp
                         (sum.78
                          r15
                          nfv.44
                          nfv.43
                          r9
                          r8
                          rcx
                          rdx
                          rsi
                          rdi
                          i.77
                          h.76
                          g.75
                          f.74
                          e.73
                          d.72
                          c.71
                          b.70
                          a.69
                          tmp-ra.42))
                        (fv2 (h.76 g.75 f.74 e.73 d.72 c.71 b.70 a.69 tmp-ra.42))
                        (fv1 (g.75 f.74 e.73 d.72 c.71 b.70 a.69 tmp-ra.42))
                        (fv0 (f.74 e.73 d.72 c.71 b.70 a.69 tmp-ra.42))
                        (r9
                         (r15
                          nfv.44
                          nfv.43
                          g.75
                          h.76
                          r8
                          rcx
                          rdx
                          rsi
                          rdi
                          rbp
                          e.73
                          d.72
                          c.71
                          b.70
                          a.69
                          tmp-ra.42))
                        (r8
                         (r15
                          nfv.44
                          nfv.43
                          r9
                          f.74
                          g.75
                          h.76
                          rcx
                          rdx
                          rsi
                          rdi
                          rbp
                          d.72
                          c.71
                          b.70
                          a.69
                          tmp-ra.42))
                        (rcx
                         (r15
                          nfv.44
                          nfv.43
                          r9
                          r8
                          e.73
                          f.74
                          g.75
                          h.76
                          rdx
                          rsi
                          rdi
                          rbp
                          c.71
                          b.70
                          a.69
                          tmp-ra.42))
                        (rdx
                         (r15
                          nfv.44
                          nfv.43
                          r9
                          r8
                          rcx
                          d.72
                          e.73
                          f.74
                          g.75
                          h.76
                          rsi
                          rdi
                          rbp
                          b.70
                          a.69
                          tmp-ra.42))
                        (rsi
                         (r15
                          nfv.44
                          nfv.43
                          r9
                          r8
                          rcx
                          rdx
                          c.71
                          d.72
                          e.73
                          f.74
                          g.75
                          h.76
                          rdi
                          rbp
                          a.69
                          tmp-ra.42))
                        (rdi
                         (i.77
                          r15
                          nfv.44
                          nfv.43
                          r9
                          r8
                          rcx
                          rdx
                          rsi
                          b.70
                          c.71
                          d.72
                          e.73
                          f.74
                          g.75
                          h.76
                          rbp
                          tmp-ra.42))
                        (r15 (nfv.44 nfv.43 r9 r8 rcx rdx rsi rdi rbp))))
                      (assignment ((nfv.44 fv5) (nfv.43 fv4) (tmp-ra.42 fv3) (i.77 fv0))))
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
                       (begin
                         (set! rbp (- rbp 32))
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
                         (set! rbp (+ rbp 32)))
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
  (check-equal? (allocate-frames '(module
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
                                         (r15 (rsi rdi rbp))))
                                       (assignment ()))
                                    (define L.f.1
                                      ((new-frames (()))
                                       (locals (tmp.42 tmp.39 tmp.43 tmp.40 tmp.41))
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
                                         (rax (rbp tmp-ra.50))))
                                       (assignment ((tmp-ra.50 fv3) (tmp.38 fv2) (x.1 fv1) (x.2 fv0))))
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
                                        ((tmp-ra.51 (rax rbp)) (rbp (rax tmp-ra.51)) (rax (rbp tmp-ra.51))))
                                       (assignment ()))
                                      (begin (set! tmp-ra.51 r15) (set! rax 8) (jump tmp-ra.51 rbp rax)))
                                    (begin
                                      (set! tmp-ra.52 r15)
                                      (set! rdi 1)
                                      (set! rsi 2)
                                      (set! r15 tmp-ra.52)
                                      (jump L.f.1 rbp r15 rdi rsi))))
                '(module
                     ((locals (tmp-ra.52))
                      (conflicts
                       ((tmp-ra.52 (rsi rdi rbp))
                        (rbp (r15 rsi rdi tmp-ra.52))
                        (rdi (r15 rsi tmp-ra.52 rbp))
                        (rsi (r15 tmp-ra.52 rdi rbp))
                        (r15 (rsi rdi rbp))))
                      (assignment ()))
                   (define L.f.1
                     ((locals (tmp.41 tmp.40 tmp.43 tmp.39 tmp.42))
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
                        (rax (rbp tmp-ra.50))))
                      (assignment ((tmp-ra.50 fv3) (tmp.38 fv2) (x.1 fv1) (x.2 fv0))))
                     (begin
                       (set! tmp-ra.50 r15)
                       (set! x.1 rdi)
                       (set! x.2 rsi)
                       (set! tmp.39 10)
                       (set! tmp.39 (+ tmp.39 6))
                       (begin (set! tmp.38 r12) (set! r12 (+ r12 tmp.39)))
                       (begin
                         (set! rbp (- rbp 32))
                         (return-point L.rp.21 (begin (set! r15 L.rp.21) (jump L.g.1 rbp r15)))
                         (set! rbp (+ rbp 32)))
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
                     ((locals (tmp-ra.51))
                      (conflicts
                       ((tmp-ra.51 (rax rbp)) (rbp (rax tmp-ra.51)) (rax (rbp tmp-ra.51))))
                      (assignment ()))
                     (begin (set! tmp-ra.51 r15) (set! rax 8) (jump tmp-ra.51 rbp rax)))
                   (begin
                     (set! tmp-ra.52 r15)
                     (set! rdi 1)
                     (set! rsi 2)
                     (set! r15 tmp-ra.52)
                     (jump L.f.1 rbp r15 rdi rsi))))

  (check-equal? (interp-asm-pred-lang-v8/framed (allocate-frames
                                                 '(module
                                                      ((new-frames (() () () ()))
                                                       (locals (tmp.93 tmp.92 tmp.94))
                                                       (call-undead (vec.3 tmp.91 tmp-ra.107))
                                                       (undead-out
                                                        ((tmp-ra.107 rbp)
                                                         ((rax tmp-ra.107 rbp) ((r15 rbp) (r15 rbp)))
                                                         (vec.3 tmp-ra.107 rbp)
                                                         (((((rax vec.3 tmp-ra.107 rbp) ((rdi rbp) (rdi r15 rbp) (rdi r15 rbp)))
                                                            (tmp.93 vec.3 tmp-ra.107 rbp)
                                                            ((rax vec.3 tmp-ra.107 rbp) ((rdi rbp) (rdi r15 rbp) (rdi r15 rbp)))
                                                            (tmp.92 vec.3 tmp-ra.107 rbp))
                                                           (vec.3 tmp-ra.107 rbp))
                                                          (vec.3 tmp.91 tmp-ra.107 rbp)
                                                          (vec.3 tmp.91 tmp-ra.107 rbp))
                                                         ((rax tmp.91 tmp-ra.107 rbp) ((rdi rbp) (rdi r15 rbp) (rdi r15 rbp)))
                                                         (tmp.94 tmp.91 tmp-ra.107 rbp)
                                                         (tmp.91 tmp-ra.107 rsi rbp)
                                                         (tmp-ra.107 rsi rdi rbp)
                                                         (rsi rdi r15 rbp)
                                                         (rsi rdi r15 rbp)))
                                                       (conflicts
                                                        ((tmp-ra.107 (rdi rsi tmp.94 tmp.93 tmp.92 tmp.91 vec.3 rbp))
                                                         (vec.3 (tmp.93 tmp.92 tmp.91 rbp tmp-ra.107))
                                                         (tmp.91 (rsi tmp.94 rbp tmp-ra.107 vec.3))
                                                         (tmp.93 (rbp tmp-ra.107 vec.3))
                                                         (tmp.92 (rbp tmp-ra.107 vec.3))
                                                         (tmp.94 (rbp tmp-ra.107 tmp.91))
                                                         (rbp (rsi tmp.94 tmp.93 rdi tmp.92 tmp.91 vec.3 r15 tmp-ra.107))
                                                         (r15 (rsi rdi rbp))
                                                         (rdi (rsi tmp-ra.107 r15 rbp))
                                                         (rsi (r15 rdi rbp tmp-ra.107 tmp.91))))
                                                       (assignment ((tmp-ra.107 fv0) (tmp.91 fv1) (vec.3 fv2))))
                                                    (define L.+.12
                                                      ((new-frames ())
                                                       (locals (tmp.54 tmp.20 tmp.55 tmp-ra.95 tmp.57 tmp.19 tmp.56))
                                                       (undead-out
                                                        ((rdi rsi tmp-ra.95 rbp)
                                                         (rsi tmp.19 tmp-ra.95 rbp)
                                                         (tmp.19 tmp.20 tmp-ra.95 rbp)
                                                         ((((((tmp.55 tmp.19 tmp.20 tmp-ra.95 rbp)
                                                              (tmp.55 tmp.19 tmp.20 tmp-ra.95 rbp))
                                                             (tmp.19 tmp.20 tmp-ra.95 rbp))
                                                            (tmp.54 tmp.19 tmp.20 tmp-ra.95 rbp)
                                                            (tmp.54 tmp.19 tmp.20 tmp-ra.95 rbp))
                                                           (tmp.19 tmp.20 tmp-ra.95 rbp))
                                                          ((((((tmp.57 tmp.19 tmp.20 tmp-ra.95 rbp)
                                                               (tmp.57 tmp.19 tmp.20 tmp-ra.95 rbp))
                                                              (tmp.19 tmp.20 tmp-ra.95 rbp))
                                                             (tmp.56 tmp.19 tmp.20 tmp-ra.95 rbp)
                                                             (tmp.56 tmp.19 tmp.20 tmp-ra.95 rbp))
                                                            (tmp.19 tmp.20 tmp-ra.95 rbp))
                                                           ((tmp.20 rax tmp-ra.95 rbp) (tmp-ra.95 rax rbp) (rax rbp))
                                                           ((tmp-ra.95 rax rbp) (rax rbp)))
                                                          ((tmp-ra.95 rax rbp) (rax rbp)))))
                                                       (call-undead ())
                                                       (conflicts
                                                        ((tmp.54 (rbp tmp-ra.95 tmp.20 tmp.19))
                                                         (tmp.20 (rbp tmp-ra.95 tmp.19 tmp.55 tmp.54 tmp.57 tmp.56 rax))
                                                         (tmp.55 (rbp tmp-ra.95 tmp.20 tmp.19))
                                                         (tmp-ra.95 (tmp.20 tmp.19 rbp rsi rdi tmp.55 tmp.54 tmp.57 tmp.56 rax))
                                                         (tmp.57 (rbp tmp-ra.95 tmp.20 tmp.19))
                                                         (tmp.19 (tmp.20 rbp tmp-ra.95 rsi tmp.55 tmp.54 tmp.57 tmp.56))
                                                         (tmp.56 (rbp tmp-ra.95 tmp.20 tmp.19))
                                                         (rax (tmp.20 rbp tmp-ra.95))
                                                         (rbp (tmp.20 tmp.19 tmp-ra.95 tmp.55 tmp.54 tmp.57 tmp.56 rax))
                                                         (rdi (tmp-ra.95))
                                                         (rsi (tmp.19 tmp-ra.95))))
                                                       (assignment ()))
                                                      (begin
                                                        (set! tmp-ra.95 r15)
                                                        (set! tmp.19 rdi)
                                                        (set! tmp.20 rsi)
                                                        (if (begin
                                                              (if (begin
                                                                    (begin
                                                                      (set! tmp.55 tmp.20)
                                                                      (set! tmp.55 (bitwise-and tmp.55 7)))
                                                                    (= tmp.55 0))
                                                                  (set! tmp.54 14)
                                                                  (set! tmp.54 6))
                                                              (!= tmp.54 6))
                                                            (if (begin
                                                                  (if (begin
                                                                        (begin
                                                                          (set! tmp.57 tmp.19)
                                                                          (set! tmp.57 (bitwise-and tmp.57 7)))
                                                                        (= tmp.57 0))
                                                                      (set! tmp.56 14)
                                                                      (set! tmp.56 6))
                                                                  (!= tmp.56 6))
                                                                (begin
                                                                  (set! rax tmp.19)
                                                                  (set! rax (+ rax tmp.20))
                                                                  (jump tmp-ra.95 rbp rax))
                                                                (begin (set! rax 574) (jump tmp-ra.95 rbp rax)))
                                                            (begin (set! rax 574) (jump tmp-ra.95 rbp rax)))))
                                                    (define L.void?.11
                                                      ((new-frames ())
                                                       (locals (tmp.58 tmp.43 tmp-ra.96))
                                                       (undead-out
                                                        ((rdi tmp-ra.96 rbp)
                                                         (tmp.43 tmp-ra.96 rbp)
                                                         ((((tmp.58 tmp-ra.96 rbp) (tmp.58 tmp-ra.96 rbp)) (tmp-ra.96 rbp))
                                                          ((tmp-ra.96 rax rbp) (rax rbp))
                                                          ((tmp-ra.96 rax rbp) (rax rbp)))))
                                                       (call-undead ())
                                                       (conflicts
                                                        ((tmp.58 (rbp tmp-ra.96))
                                                         (tmp.43 (rbp tmp-ra.96))
                                                         (tmp-ra.96 (tmp.43 rbp rdi tmp.58 rax))
                                                         (rax (rbp tmp-ra.96))
                                                         (rbp (tmp.43 tmp-ra.96 tmp.58 rax))
                                                         (rdi (tmp-ra.96))))
                                                       (assignment ()))
                                                      (begin
                                                        (set! tmp-ra.96 r15)
                                                        (set! tmp.43 rdi)
                                                        (if (begin
                                                              (begin (set! tmp.58 tmp.43) (set! tmp.58 (bitwise-and tmp.58 255)))
                                                              (= tmp.58 30))
                                                            (begin (set! rax 14) (jump tmp-ra.96 rbp rax))
                                                            (begin (set! rax 6) (jump tmp-ra.96 rbp rax)))))
                                                    (define L.unsafe-vector-ref.3
                                                      ((new-frames ())
                                                       (locals
                                                        (tmp.59 tmp.14 tmp.60 tmp.15 tmp-ra.97 tmp.64 tmp.63 tmp.62 tmp.61))
                                                       (undead-out
                                                        ((rdi rsi tmp-ra.97 rbp)
                                                         (rsi tmp.14 tmp-ra.97 rbp)
                                                         (tmp.15 tmp.14 tmp-ra.97 rbp)
                                                         (((((tmp.60 tmp.15 tmp.14 tmp-ra.97 rbp) (tmp.15 tmp.14 tmp-ra.97 rbp))
                                                            (tmp.59 tmp.15 tmp.14 tmp-ra.97 rbp)
                                                            (tmp.59 tmp.15 tmp.14 tmp-ra.97 rbp))
                                                           (tmp.15 tmp.14 tmp-ra.97 rbp))
                                                          ((((tmp.15 tmp.14 tmp-ra.97 rbp)
                                                             (tmp.61 tmp.15 tmp.14 tmp-ra.97 rbp)
                                                             (tmp.61 tmp.15 tmp.14 tmp-ra.97 rbp))
                                                            (tmp.15 tmp.14 tmp-ra.97 rbp))
                                                           ((tmp.64 tmp.14 tmp-ra.97 rbp)
                                                            (tmp.64 tmp.14 tmp-ra.97 rbp)
                                                            (tmp.63 tmp.14 tmp-ra.97 rbp)
                                                            (tmp.63 tmp.14 tmp-ra.97 rbp)
                                                            (tmp.62 tmp.14 tmp-ra.97 rbp)
                                                            (tmp.62 tmp.14 tmp-ra.97 rbp)
                                                            (tmp-ra.97 rax rbp)
                                                            (rax rbp))
                                                           ((tmp-ra.97 rax rbp) (rax rbp)))
                                                          ((tmp-ra.97 rax rbp) (rax rbp)))))
                                                       (call-undead ())
                                                       (conflicts
                                                        ((tmp.59 (rbp tmp-ra.97 tmp.14 tmp.15))
                                                         (tmp.14
                                                          (tmp.15 rbp tmp-ra.97 rsi tmp.60 tmp.59 tmp.61 tmp.62 tmp.63 tmp.64))
                                                         (tmp.60 (rbp tmp-ra.97 tmp.14 tmp.15))
                                                         (tmp.15 (rbp tmp-ra.97 tmp.14 tmp.60 tmp.59 tmp.61))
                                                         (tmp-ra.97
                                                          (tmp.15
                                                           tmp.14
                                                           rbp
                                                           rsi
                                                           rdi
                                                           tmp.60
                                                           tmp.59
                                                           tmp.61
                                                           tmp.62
                                                           tmp.63
                                                           tmp.64
                                                           rax))
                                                         (tmp.64 (rbp tmp-ra.97 tmp.14))
                                                         (tmp.63 (rbp tmp-ra.97 tmp.14))
                                                         (tmp.62 (rbp tmp-ra.97 tmp.14))
                                                         (tmp.61 (rbp tmp-ra.97 tmp.14 tmp.15))
                                                         (rax (rbp tmp-ra.97))
                                                         (rbp
                                                          (tmp.15
                                                           tmp.14
                                                           tmp-ra.97
                                                           tmp.60
                                                           tmp.59
                                                           tmp.61
                                                           tmp.62
                                                           tmp.63
                                                           tmp.64
                                                           rax))
                                                         (rdi (tmp-ra.97))
                                                         (rsi (tmp.14 tmp-ra.97))))
                                                       (assignment ()))
                                                      (begin
                                                        (set! tmp-ra.97 r15)
                                                        (set! tmp.14 rdi)
                                                        (set! tmp.15 rsi)
                                                        (if (begin
                                                              (if (begin (set! tmp.60 (mref tmp.14 -3)) (< tmp.15 tmp.60))
                                                                  (set! tmp.59 14)
                                                                  (set! tmp.59 6))
                                                              (!= tmp.59 6))
                                                            (if (begin
                                                                  (if (>= tmp.15 0) (set! tmp.61 14) (set! tmp.61 6))
                                                                  (!= tmp.61 6))
                                                                (begin
                                                                  (set! tmp.64 tmp.15)
                                                                  (set! tmp.64 (arithmetic-shift-right tmp.64 3))
                                                                  (set! tmp.63 tmp.64)
                                                                  (set! tmp.63 (* tmp.63 8))
                                                                  (set! tmp.62 tmp.63)
                                                                  (set! tmp.62 (+ tmp.62 5))
                                                                  (set! rax (mref tmp.14 tmp.62))
                                                                  (jump tmp-ra.97 rbp rax))
                                                                (begin (set! rax 2878) (jump tmp-ra.97 rbp rax)))
                                                            (begin (set! rax 2878) (jump tmp-ra.97 rbp rax)))))
                                                    (define L.vector-ref.10
                                                      ((new-frames ())
                                                       (locals (tmp.65 tmp.37 tmp.66 tmp-ra.98 tmp.68 tmp.36 tmp.67))
                                                       (undead-out
                                                        ((rdi rsi tmp-ra.98 rbp)
                                                         (rsi tmp.36 tmp-ra.98 rbp)
                                                         (tmp.37 tmp.36 tmp-ra.98 rbp)
                                                         ((((((tmp.66 tmp.37 tmp.36 tmp-ra.98 rbp)
                                                              (tmp.66 tmp.37 tmp.36 tmp-ra.98 rbp))
                                                             (tmp.37 tmp.36 tmp-ra.98 rbp))
                                                            (tmp.65 tmp.37 tmp.36 tmp-ra.98 rbp)
                                                            (tmp.65 tmp.37 tmp.36 tmp-ra.98 rbp))
                                                           (tmp.37 tmp.36 tmp-ra.98 rbp))
                                                          ((((((tmp.68 tmp.37 tmp.36 tmp-ra.98 rbp)
                                                               (tmp.68 tmp.37 tmp.36 tmp-ra.98 rbp))
                                                              (tmp.37 tmp.36 tmp-ra.98 rbp))
                                                             (tmp.67 tmp.37 tmp.36 tmp-ra.98 rbp)
                                                             (tmp.67 tmp.37 tmp.36 tmp-ra.98 rbp))
                                                            (tmp.37 tmp.36 tmp-ra.98 rbp))
                                                           ((tmp.36 tmp-ra.98 rsi rbp)
                                                            (tmp-ra.98 rsi rdi rbp)
                                                            (rsi rdi r15 rbp)
                                                            (rsi rdi r15 rbp))
                                                           ((tmp-ra.98 rax rbp) (rax rbp)))
                                                          ((tmp-ra.98 rax rbp) (rax rbp)))))
                                                       (call-undead ())
                                                       (conflicts
                                                        ((tmp.65 (rbp tmp-ra.98 tmp.36 tmp.37))
                                                         (tmp.37 (rbp tmp-ra.98 tmp.36 tmp.66 tmp.65 tmp.68 tmp.67))
                                                         (tmp.66 (rbp tmp-ra.98 tmp.36 tmp.37))
                                                         (tmp-ra.98 (tmp.37 tmp.36 rbp tmp.66 tmp.65 tmp.68 tmp.67 rdi rsi rax))
                                                         (tmp.68 (rbp tmp-ra.98 tmp.36 tmp.37))
                                                         (tmp.36 (tmp.37 rbp tmp-ra.98 tmp.66 tmp.65 tmp.68 tmp.67 rsi))
                                                         (tmp.67 (rbp tmp-ra.98 tmp.36 tmp.37))
                                                         (rax (rbp tmp-ra.98))
                                                         (rbp
                                                          (tmp.37 tmp.36 tmp-ra.98 tmp.66 tmp.65 tmp.68 tmp.67 r15 rdi rsi rax))
                                                         (rsi (r15 rdi rbp tmp-ra.98 tmp.36))
                                                         (rdi (r15 rbp rsi tmp-ra.98))
                                                         (r15 (rbp rdi rsi))))
                                                       (assignment ()))
                                                      (begin
                                                        (set! tmp-ra.98 r15)
                                                        (set! tmp.36 rdi)
                                                        (set! tmp.37 rsi)
                                                        (if (begin
                                                              (if (begin
                                                                    (begin
                                                                      (set! tmp.66 tmp.37)
                                                                      (set! tmp.66 (bitwise-and tmp.66 7)))
                                                                    (= tmp.66 0))
                                                                  (set! tmp.65 14)
                                                                  (set! tmp.65 6))
                                                              (!= tmp.65 6))
                                                            (if (begin
                                                                  (if (begin
                                                                        (begin
                                                                          (set! tmp.68 tmp.36)
                                                                          (set! tmp.68 (bitwise-and tmp.68 7)))
                                                                        (= tmp.68 3))
                                                                      (set! tmp.67 14)
                                                                      (set! tmp.67 6))
                                                                  (!= tmp.67 6))
                                                                (begin
                                                                  (set! rsi tmp.37)
                                                                  (set! rdi tmp.36)
                                                                  (set! r15 tmp-ra.98)
                                                                  (jump L.unsafe-vector-ref.3 rbp r15 rdi rsi))
                                                                (begin (set! rax 2878) (jump tmp-ra.98 rbp rax)))
                                                            (begin (set! rax 2878) (jump tmp-ra.98 rbp rax)))))
                                                    (define L.unsafe-vector-set!.2
                                                      ((new-frames ())
                                                       (locals
                                                        (tmp.69
                                                         tmp.9
                                                         tmp.70
                                                         tmp.10
                                                         tmp-ra.99
                                                         tmp.74
                                                         tmp.73
                                                         tmp.72
                                                         tmp.11
                                                         tmp.71))
                                                       (undead-out
                                                        ((rdi rsi rdx rbp tmp-ra.99)
                                                         (rsi rdx rbp tmp-ra.99 tmp.9)
                                                         (rdx tmp.10 rbp tmp-ra.99 tmp.9)
                                                         (tmp.10 rbp tmp-ra.99 tmp.11 tmp.9)
                                                         (((((tmp.70 tmp.10 rbp tmp-ra.99 tmp.11 tmp.9)
                                                             (tmp.10 rbp tmp-ra.99 tmp.11 tmp.9))
                                                            (tmp.69 tmp.10 rbp tmp-ra.99 tmp.11 tmp.9)
                                                            (tmp.69 tmp.10 rbp tmp-ra.99 tmp.11 tmp.9))
                                                           (tmp.10 rbp tmp-ra.99 tmp.11 tmp.9))
                                                          ((((tmp.10 rbp tmp-ra.99 tmp.11 tmp.9)
                                                             (tmp.71 tmp.10 rbp tmp-ra.99 tmp.11 tmp.9)
                                                             (tmp.71 tmp.10 rbp tmp-ra.99 tmp.11 tmp.9))
                                                            (tmp.10 rbp tmp-ra.99 tmp.11 tmp.9))
                                                           ((tmp.74 rbp tmp-ra.99 tmp.11 tmp.9)
                                                            (tmp.74 rbp tmp-ra.99 tmp.11 tmp.9)
                                                            (tmp.73 rbp tmp-ra.99 tmp.11 tmp.9)
                                                            (tmp.73 rbp tmp-ra.99 tmp.11 tmp.9)
                                                            (tmp.72 rbp tmp-ra.99 tmp.11 tmp.9)
                                                            (rbp tmp-ra.99 tmp.11 tmp.72 tmp.9)
                                                            (tmp-ra.99 rbp)
                                                            (tmp-ra.99 rax rbp)
                                                            (rax rbp))
                                                           ((tmp-ra.99 rax rbp) (rax rbp)))
                                                          ((tmp-ra.99 rax rbp) (rax rbp)))))
                                                       (call-undead ())
                                                       (conflicts
                                                        ((tmp.69 (tmp.9 tmp.11 tmp-ra.99 rbp tmp.10))
                                                         (tmp.9
                                                          (tmp.11
                                                           tmp.10
                                                           tmp-ra.99
                                                           rbp
                                                           rdx
                                                           rsi
                                                           tmp.70
                                                           tmp.69
                                                           tmp.71
                                                           tmp.72
                                                           tmp.73
                                                           tmp.74))
                                                         (tmp.70 (tmp.9 tmp.11 tmp-ra.99 rbp tmp.10))
                                                         (tmp.10 (tmp.11 tmp.9 tmp-ra.99 rbp rdx tmp.70 tmp.69 tmp.71))
                                                         (tmp-ra.99
                                                          (tmp.11
                                                           tmp.10
                                                           tmp.9
                                                           rbp
                                                           rdx
                                                           rsi
                                                           rdi
                                                           tmp.70
                                                           tmp.69
                                                           tmp.71
                                                           tmp.72
                                                           tmp.73
                                                           tmp.74
                                                           rax))
                                                         (tmp.74 (tmp.9 tmp.11 tmp-ra.99 rbp))
                                                         (tmp.73 (tmp.9 tmp.11 tmp-ra.99 rbp))
                                                         (tmp.72 (tmp.9 tmp.11 tmp-ra.99 rbp))
                                                         (tmp.11
                                                          (tmp.9 tmp-ra.99 rbp tmp.10 tmp.70 tmp.69 tmp.71 tmp.72 tmp.73 tmp.74))
                                                         (tmp.71 (tmp.9 tmp.11 tmp-ra.99 rbp tmp.10))
                                                         (rax (rbp tmp-ra.99))
                                                         (rbp
                                                          (tmp.11
                                                           tmp.10
                                                           tmp.9
                                                           tmp-ra.99
                                                           tmp.70
                                                           tmp.69
                                                           tmp.71
                                                           tmp.72
                                                           tmp.73
                                                           tmp.74
                                                           rax))
                                                         (rdi (tmp-ra.99))
                                                         (rsi (tmp.9 tmp-ra.99))
                                                         (rdx (tmp.10 tmp.9 tmp-ra.99))))
                                                       (assignment ()))
                                                      (begin
                                                        (set! tmp-ra.99 r15)
                                                        (set! tmp.9 rdi)
                                                        (set! tmp.10 rsi)
                                                        (set! tmp.11 rdx)
                                                        (if (begin
                                                              (if (begin (set! tmp.70 (mref tmp.9 -3)) (< tmp.10 tmp.70))
                                                                  (set! tmp.69 14)
                                                                  (set! tmp.69 6))
                                                              (!= tmp.69 6))
                                                            (if (begin
                                                                  (if (>= tmp.10 0) (set! tmp.71 14) (set! tmp.71 6))
                                                                  (!= tmp.71 6))
                                                                (begin
                                                                  (set! tmp.74 tmp.10)
                                                                  (set! tmp.74 (arithmetic-shift-right tmp.74 3))
                                                                  (set! tmp.73 tmp.74)
                                                                  (set! tmp.73 (* tmp.73 8))
                                                                  (set! tmp.72 tmp.73)
                                                                  (set! tmp.72 (+ tmp.72 5))
                                                                  (mset! tmp.9 tmp.72 tmp.11)
                                                                  (set! rax 30)
                                                                  (jump tmp-ra.99 rbp rax))
                                                                (begin (set! rax 2622) (jump tmp-ra.99 rbp rax)))
                                                            (begin (set! rax 2622) (jump tmp-ra.99 rbp rax)))))
                                                    (define L.vector-set!.9
                                                      ((new-frames ())
                                                       (locals (tmp.75 tmp.34 tmp.76 tmp.35 tmp-ra.100 tmp.78 tmp.33 tmp.77))
                                                       (undead-out
                                                        ((rdi rsi rdx tmp-ra.100 rbp)
                                                         (rsi rdx tmp.33 tmp-ra.100 rbp)
                                                         (rdx tmp.34 tmp.33 tmp-ra.100 rbp)
                                                         (tmp.35 tmp.34 tmp.33 tmp-ra.100 rbp)
                                                         ((((((tmp.76 tmp.35 tmp.34 tmp.33 tmp-ra.100 rbp)
                                                              (tmp.76 tmp.35 tmp.34 tmp.33 tmp-ra.100 rbp))
                                                             (tmp.35 tmp.34 tmp.33 tmp-ra.100 rbp))
                                                            (tmp.75 tmp.35 tmp.34 tmp.33 tmp-ra.100 rbp)
                                                            (tmp.75 tmp.35 tmp.34 tmp.33 tmp-ra.100 rbp))
                                                           (tmp.35 tmp.34 tmp.33 tmp-ra.100 rbp))
                                                          ((((((tmp.78 tmp.35 tmp.34 tmp.33 tmp-ra.100 rbp)
                                                               (tmp.78 tmp.35 tmp.34 tmp.33 tmp-ra.100 rbp))
                                                              (tmp.35 tmp.34 tmp.33 tmp-ra.100 rbp))
                                                             (tmp.77 tmp.35 tmp.34 tmp.33 tmp-ra.100 rbp)
                                                             (tmp.77 tmp.35 tmp.34 tmp.33 tmp-ra.100 rbp))
                                                            (tmp.35 tmp.34 tmp.33 tmp-ra.100 rbp))
                                                           ((tmp.34 tmp.33 tmp-ra.100 rdx rbp)
                                                            (tmp.33 tmp-ra.100 rdx rsi rbp)
                                                            (tmp-ra.100 rdx rsi rdi rbp)
                                                            (rdx rsi rdi r15 rbp)
                                                            (rdx rsi rdi r15 rbp))
                                                           ((tmp-ra.100 rax rbp) (rax rbp)))
                                                          ((tmp-ra.100 rax rbp) (rax rbp)))))
                                                       (call-undead ())
                                                       (conflicts
                                                        ((tmp.75 (rbp tmp-ra.100 tmp.33 tmp.34 tmp.35))
                                                         (tmp.34 (tmp.35 rbp tmp-ra.100 tmp.33 tmp.76 tmp.75 tmp.78 tmp.77 rdx))
                                                         (tmp.76 (rbp tmp-ra.100 tmp.33 tmp.34 tmp.35))
                                                         (tmp.35 (rbp tmp-ra.100 tmp.33 tmp.34 tmp.76 tmp.75 tmp.78 tmp.77))
                                                         (tmp-ra.100
                                                          (tmp.35 tmp.34 tmp.33 rbp tmp.76 tmp.75 tmp.78 tmp.77 rdi rsi rdx rax))
                                                         (tmp.78 (rbp tmp-ra.100 tmp.33 tmp.34 tmp.35))
                                                         (tmp.33
                                                          (tmp.35 tmp.34 rbp tmp-ra.100 tmp.76 tmp.75 tmp.78 tmp.77 rsi rdx))
                                                         (tmp.77 (rbp tmp-ra.100 tmp.33 tmp.34 tmp.35))
                                                         (rax (rbp tmp-ra.100))
                                                         (rbp
                                                          (tmp.35
                                                           tmp.34
                                                           tmp.33
                                                           tmp-ra.100
                                                           tmp.76
                                                           tmp.75
                                                           tmp.78
                                                           tmp.77
                                                           r15
                                                           rdi
                                                           rsi
                                                           rdx
                                                           rax))
                                                         (rdx (r15 rdi rsi rbp tmp-ra.100 tmp.33 tmp.34))
                                                         (rsi (r15 rdi rbp rdx tmp-ra.100 tmp.33))
                                                         (rdi (r15 rbp rsi rdx tmp-ra.100))
                                                         (r15 (rbp rdi rsi rdx))))
                                                       (assignment ()))
                                                      (begin
                                                        (set! tmp-ra.100 r15)
                                                        (set! tmp.33 rdi)
                                                        (set! tmp.34 rsi)
                                                        (set! tmp.35 rdx)
                                                        (if (begin
                                                              (if (begin
                                                                    (begin
                                                                      (set! tmp.76 tmp.34)
                                                                      (set! tmp.76 (bitwise-and tmp.76 7)))
                                                                    (= tmp.76 0))
                                                                  (set! tmp.75 14)
                                                                  (set! tmp.75 6))
                                                              (!= tmp.75 6))
                                                            (if (begin
                                                                  (if (begin
                                                                        (begin
                                                                          (set! tmp.78 tmp.33)
                                                                          (set! tmp.78 (bitwise-and tmp.78 7)))
                                                                        (= tmp.78 3))
                                                                      (set! tmp.77 14)
                                                                      (set! tmp.77 6))
                                                                  (!= tmp.77 6))
                                                                (begin
                                                                  (set! rdx tmp.35)
                                                                  (set! rsi tmp.34)
                                                                  (set! rdi tmp.33)
                                                                  (set! r15 tmp-ra.100)
                                                                  (jump L.unsafe-vector-set!.2 rbp r15 rdi rsi rdx))
                                                                (begin (set! rax 2622) (jump tmp-ra.100 rbp rax)))
                                                            (begin (set! rax 2622) (jump tmp-ra.100 rbp rax)))))
                                                    (define L.vector-init-loop.7
                                                      ((new-frames ())
                                                       (locals (tmp.79 i.8 len.6 vec.7 tmp-ra.101 tmp.83 tmp.80 tmp.81 tmp.82))
                                                       (undead-out
                                                        ((rdi rsi rdx tmp-ra.101 rbp)
                                                         (rsi rdx len.6 tmp-ra.101 rbp)
                                                         (rdx i.8 len.6 tmp-ra.101 rbp)
                                                         (i.8 len.6 vec.7 tmp-ra.101 rbp)
                                                         ((((i.8 len.6 vec.7 tmp-ra.101 rbp)
                                                            (tmp.79 i.8 len.6 vec.7 tmp-ra.101 rbp)
                                                            (tmp.79 i.8 len.6 vec.7 tmp-ra.101 rbp))
                                                           (i.8 len.6 vec.7 tmp-ra.101 rbp))
                                                          ((tmp-ra.101 rax rbp) (rax rbp))
                                                          ((tmp.82 rbp tmp-ra.101 len.6 i.8 vec.7)
                                                           (tmp.82 rbp tmp-ra.101 len.6 i.8 vec.7)
                                                           (tmp.81 rbp tmp-ra.101 len.6 i.8 vec.7)
                                                           (tmp.81 rbp tmp-ra.101 len.6 i.8 vec.7)
                                                           (tmp.80 rbp tmp-ra.101 len.6 i.8 vec.7)
                                                           (rbp tmp-ra.101 len.6 i.8 tmp.80 vec.7)
                                                           (i.8 vec.7 len.6 tmp-ra.101 rbp)
                                                           (tmp.83 vec.7 len.6 tmp-ra.101 rbp)
                                                           (vec.7 tmp.83 len.6 tmp-ra.101 rbp)
                                                           (tmp.83 len.6 tmp-ra.101 rdx rbp)
                                                           (len.6 tmp-ra.101 rdx rsi rbp)
                                                           (tmp-ra.101 rdx rsi rdi rbp)
                                                           (rdx rsi rdi r15 rbp)
                                                           (rdx rsi rdi r15 rbp)))))
                                                       (call-undead ())
                                                       (conflicts
                                                        ((tmp.79 (rbp tmp-ra.101 vec.7 len.6 i.8))
                                                         (i.8 (vec.7 rbp tmp-ra.101 len.6 rdx tmp.79 tmp.80 tmp.81 tmp.82))
                                                         (len.6
                                                          (vec.7 i.8 rbp tmp-ra.101 tmp.79 rsi rdx tmp.83 tmp.80 tmp.81 tmp.82))
                                                         (vec.7 (rbp tmp-ra.101 len.6 i.8 tmp.79 tmp.83 tmp.80 tmp.81 tmp.82))
                                                         (tmp-ra.101
                                                          (vec.7
                                                           i.8
                                                           len.6
                                                           rbp
                                                           tmp.79
                                                           rax
                                                           rdi
                                                           rsi
                                                           rdx
                                                           tmp.83
                                                           tmp.80
                                                           tmp.81
                                                           tmp.82))
                                                         (tmp.83 (rdx rbp tmp-ra.101 len.6 vec.7))
                                                         (tmp.80 (vec.7 i.8 len.6 tmp-ra.101 rbp))
                                                         (tmp.81 (vec.7 i.8 len.6 tmp-ra.101 rbp))
                                                         (tmp.82 (i.8 vec.7 len.6 tmp-ra.101 rbp))
                                                         (rbp
                                                          (vec.7
                                                           i.8
                                                           len.6
                                                           tmp-ra.101
                                                           tmp.79
                                                           rax
                                                           r15
                                                           rdi
                                                           rsi
                                                           rdx
                                                           tmp.83
                                                           tmp.80
                                                           tmp.81
                                                           tmp.82))
                                                         (rdx (i.8 r15 rdi rsi rbp tmp-ra.101 len.6 tmp.83))
                                                         (rsi (r15 rdi rbp rdx tmp-ra.101 len.6))
                                                         (rdi (r15 rbp rsi rdx tmp-ra.101))
                                                         (r15 (rbp rdi rsi rdx))
                                                         (rax (rbp tmp-ra.101))))
                                                       (assignment ()))
                                                      (begin
                                                        (set! tmp-ra.101 r15)
                                                        (set! len.6 rdi)
                                                        (set! i.8 rsi)
                                                        (set! vec.7 rdx)
                                                        (if (begin
                                                              (if (= len.6 i.8) (set! tmp.79 14) (set! tmp.79 6))
                                                              (!= tmp.79 6))
                                                            (begin (set! rax vec.7) (jump tmp-ra.101 rbp rax))
                                                            (begin
                                                              (set! tmp.82 i.8)
                                                              (set! tmp.82 (arithmetic-shift-right tmp.82 3))
                                                              (set! tmp.81 tmp.82)
                                                              (set! tmp.81 (* tmp.81 8))
                                                              (set! tmp.80 tmp.81)
                                                              (set! tmp.80 (+ tmp.80 5))
                                                              (mset! vec.7 tmp.80 0)
                                                              (set! tmp.83 i.8)
                                                              (set! tmp.83 (+ tmp.83 8))
                                                              (set! rdx vec.7)
                                                              (set! rsi tmp.83)
                                                              (set! rdi len.6)
                                                              (set! r15 tmp-ra.101)
                                                              (jump L.vector-init-loop.7 rbp r15 rdi rsi rdx)))))
                                                    (define L.make-init-vector.1
                                                      ((new-frames ())
                                                       (locals
                                                        (tmp.84 tmp.4 tmp-ra.102 tmp.5 tmp.53 tmp.85 tmp.86 tmp.87 tmp.88))
                                                       (undead-out
                                                        ((rdi r12 rbp tmp-ra.102)
                                                         (r12 rbp tmp-ra.102 tmp.4)
                                                         ((((r12 rbp tmp-ra.102 tmp.4)
                                                            (tmp.84 r12 rbp tmp-ra.102 tmp.4)
                                                            (tmp.84 r12 rbp tmp-ra.102 tmp.4))
                                                           (r12 rbp tmp-ra.102 tmp.4))
                                                          ((tmp.88 r12 rbp tmp-ra.102 tmp.4)
                                                           (tmp.88 r12 rbp tmp-ra.102 tmp.4)
                                                           (tmp.88 tmp.87 r12 rbp tmp-ra.102 tmp.4)
                                                           (tmp.87 r12 rbp tmp-ra.102 tmp.4)
                                                           (tmp.86 r12 rbp tmp-ra.102 tmp.4)
                                                           (tmp.86 r12 rbp tmp-ra.102 tmp.4)
                                                           ((tmp.86 r12 tmp.85 rbp tmp-ra.102 tmp.4)
                                                            (tmp.85 rbp tmp-ra.102 tmp.4))
                                                           (tmp.53 rbp tmp-ra.102 tmp.4)
                                                           (rbp tmp-ra.102 tmp.4 tmp.53)
                                                           (tmp.53 tmp.4 tmp-ra.102 rbp)
                                                           (tmp.5 tmp.4 tmp-ra.102 rbp)
                                                           (tmp.4 tmp-ra.102 rdx rbp)
                                                           (tmp.4 tmp-ra.102 rdx rsi rbp)
                                                           (tmp-ra.102 rdx rsi rdi rbp)
                                                           (rdx rsi rdi r15 rbp)
                                                           (rdx rsi rdi r15 rbp))
                                                          ((tmp-ra.102 rax rbp) (rax rbp)))))
                                                       (call-undead ())
                                                       (conflicts
                                                        ((tmp.84 (tmp.4 tmp-ra.102 rbp r12))
                                                         (tmp.4
                                                          (tmp-ra.102
                                                           rbp
                                                           tmp.84
                                                           rsi
                                                           rdx
                                                           tmp.5
                                                           tmp.53
                                                           tmp.85
                                                           r12
                                                           tmp.86
                                                           tmp.87
                                                           tmp.88))
                                                         (tmp-ra.102
                                                          (tmp.4
                                                           rbp
                                                           tmp.84
                                                           rdi
                                                           rsi
                                                           rdx
                                                           tmp.5
                                                           tmp.53
                                                           tmp.85
                                                           r12
                                                           tmp.86
                                                           tmp.87
                                                           tmp.88
                                                           rax))
                                                         (tmp.5 (rbp tmp-ra.102 tmp.4))
                                                         (tmp.53 (tmp.4 tmp-ra.102 rbp))
                                                         (tmp.85 (tmp.4 tmp-ra.102 rbp tmp.86 r12))
                                                         (tmp.86 (tmp.85 tmp.4 tmp-ra.102 rbp r12))
                                                         (tmp.87 (tmp.4 tmp-ra.102 rbp r12 tmp.88))
                                                         (tmp.88 (tmp.87 tmp.4 tmp-ra.102 rbp r12))
                                                         (rax (rbp tmp-ra.102))
                                                         (rbp
                                                          (tmp.4
                                                           tmp-ra.102
                                                           tmp.84
                                                           r15
                                                           rdi
                                                           rsi
                                                           rdx
                                                           tmp.5
                                                           tmp.53
                                                           tmp.85
                                                           r12
                                                           tmp.86
                                                           tmp.87
                                                           tmp.88
                                                           rax))
                                                         (r12 (tmp.84 tmp.4 tmp-ra.102 rbp tmp.85 tmp.86 tmp.87 tmp.88))
                                                         (rdx (r15 rdi rsi rbp tmp-ra.102 tmp.4))
                                                         (rsi (r15 rdi rbp rdx tmp-ra.102 tmp.4))
                                                         (rdi (r15 rbp rsi rdx tmp-ra.102))
                                                         (r15 (rbp rdi rsi rdx))))
                                                       (assignment ()))
                                                      (begin
                                                        (set! tmp-ra.102 r15)
                                                        (set! tmp.4 rdi)
                                                        (if (begin
                                                              (if (>= tmp.4 0) (set! tmp.84 14) (set! tmp.84 6))
                                                              (!= tmp.84 6))
                                                            (begin
                                                              (set! tmp.88 tmp.4)
                                                              (set! tmp.88 (arithmetic-shift-right tmp.88 3))
                                                              (set! tmp.87 1)
                                                              (set! tmp.87 (+ tmp.87 tmp.88))
                                                              (set! tmp.86 tmp.87)
                                                              (set! tmp.86 (* tmp.86 8))
                                                              (begin (set! tmp.85 r12) (set! r12 (+ r12 tmp.86)))
                                                              (set! tmp.53 tmp.85)
                                                              (set! tmp.53 (+ tmp.53 3))
                                                              (mset! tmp.53 -3 tmp.4)
                                                              (set! tmp.5 tmp.53)
                                                              (set! rdx tmp.5)
                                                              (set! rsi 0)
                                                              (set! rdi tmp.4)
                                                              (set! r15 tmp-ra.102)
                                                              (jump L.vector-init-loop.7 rbp r15 rdi rsi rdx))
                                                            (begin (set! rax 3134) (jump tmp-ra.102 rbp rax)))))
                                                    (define L.make-vector.8
                                                      ((new-frames ())
                                                       (locals (tmp.89 tmp.31 tmp.90 tmp-ra.103))
                                                       (undead-out
                                                        ((rdi tmp-ra.103 rbp)
                                                         (tmp.31 tmp-ra.103 rbp)
                                                         ((((((tmp.90 tmp.31 tmp-ra.103 rbp) (tmp.90 tmp.31 tmp-ra.103 rbp))
                                                             (tmp.31 tmp-ra.103 rbp))
                                                            (tmp.89 tmp.31 tmp-ra.103 rbp)
                                                            (tmp.89 tmp.31 tmp-ra.103 rbp))
                                                           (tmp.31 tmp-ra.103 rbp))
                                                          ((tmp-ra.103 rdi rbp) (rdi r15 rbp) (rdi r15 rbp))
                                                          ((tmp-ra.103 rax rbp) (rax rbp)))))
                                                       (call-undead ())
                                                       (conflicts
                                                        ((tmp.89 (rbp tmp-ra.103 tmp.31))
                                                         (tmp.31 (rbp tmp-ra.103 tmp.90 tmp.89))
                                                         (tmp.90 (rbp tmp-ra.103 tmp.31))
                                                         (tmp-ra.103 (tmp.31 rbp tmp.90 tmp.89 rdi rax))
                                                         (rax (rbp tmp-ra.103))
                                                         (rbp (tmp.31 tmp-ra.103 tmp.90 tmp.89 r15 rdi rax))
                                                         (rdi (r15 rbp tmp-ra.103))
                                                         (r15 (rbp rdi))))
                                                       (assignment ()))
                                                      (begin
                                                        (set! tmp-ra.103 r15)
                                                        (set! tmp.31 rdi)
                                                        (if (begin
                                                              (if (begin
                                                                    (begin
                                                                      (set! tmp.90 tmp.31)
                                                                      (set! tmp.90 (bitwise-and tmp.90 7)))
                                                                    (= tmp.90 0))
                                                                  (set! tmp.89 14)
                                                                  (set! tmp.89 6))
                                                              (!= tmp.89 6))
                                                            (begin
                                                              (set! rdi tmp.31)
                                                              (set! r15 tmp-ra.103)
                                                              (jump L.make-init-vector.1 rbp r15 rdi))
                                                            (begin (set! rax 2110) (jump tmp-ra.103 rbp rax)))))
                                                    (define L.v.4
                                                      ((new-frames ())
                                                       (locals (tmp-ra.104))
                                                       (undead-out
                                                        ((tmp-ra.104 rbp) (tmp-ra.104 rdi rbp) (rdi r15 rbp) (rdi r15 rbp)))
                                                       (call-undead ())
                                                       (conflicts
                                                        ((tmp-ra.104 (rdi rbp))
                                                         (rbp (r15 rdi tmp-ra.104))
                                                         (rdi (r15 rbp tmp-ra.104))
                                                         (r15 (rbp rdi))))
                                                       (assignment ()))
                                                      (begin
                                                        (set! tmp-ra.104 r15)
                                                        (set! rdi 24)
                                                        (set! r15 tmp-ra.104)
                                                        (jump L.make-vector.8 rbp r15 rdi)))
                                                    (define L.set-first.5
                                                      ((new-frames ())
                                                       (locals (tmp-ra.105 vec.1))
                                                       (undead-out
                                                        ((rdi tmp-ra.105 rbp)
                                                         (vec.1 tmp-ra.105 rbp)
                                                         (vec.1 tmp-ra.105 rdx rbp)
                                                         (vec.1 tmp-ra.105 rdx rsi rbp)
                                                         (tmp-ra.105 rdx rsi rdi rbp)
                                                         (rdx rsi rdi r15 rbp)
                                                         (rdx rsi rdi r15 rbp)))
                                                       (call-undead ())
                                                       (conflicts
                                                        ((tmp-ra.105 (rsi rdx vec.1 rbp rdi))
                                                         (vec.1 (rsi rdx rbp tmp-ra.105))
                                                         (rdi (r15 rbp rsi rdx tmp-ra.105))
                                                         (rbp (r15 rdi rsi rdx vec.1 tmp-ra.105))
                                                         (rdx (r15 rdi rsi rbp tmp-ra.105 vec.1))
                                                         (rsi (r15 rdi rbp rdx tmp-ra.105 vec.1))
                                                         (r15 (rbp rdi rsi rdx))))
                                                       (assignment ()))
                                                      (begin
                                                        (set! tmp-ra.105 r15)
                                                        (set! vec.1 rdi)
                                                        (set! rdx 336)
                                                        (set! rsi 0)
                                                        (set! rdi vec.1)
                                                        (set! r15 tmp-ra.105)
                                                        (jump L.vector-set!.9 rbp r15 rdi rsi rdx)))
                                                    (define L.get-first.6
                                                      ((new-frames ())
                                                       (locals (tmp-ra.106 vec.2))
                                                       (undead-out
                                                        ((rdi tmp-ra.106 rbp)
                                                         (vec.2 tmp-ra.106 rbp)
                                                         (vec.2 tmp-ra.106 rsi rbp)
                                                         (tmp-ra.106 rsi rdi rbp)
                                                         (rsi rdi r15 rbp)
                                                         (rsi rdi r15 rbp)))
                                                       (call-undead ())
                                                       (conflicts
                                                        ((tmp-ra.106 (rsi vec.2 rbp rdi))
                                                         (vec.2 (rsi rbp tmp-ra.106))
                                                         (rdi (r15 rbp rsi tmp-ra.106))
                                                         (rbp (r15 rdi rsi vec.2 tmp-ra.106))
                                                         (rsi (r15 rdi rbp tmp-ra.106 vec.2))
                                                         (r15 (rbp rdi rsi))))
                                                       (assignment ()))
                                                      (begin
                                                        (set! tmp-ra.106 r15)
                                                        (set! vec.2 rdi)
                                                        (set! rsi 0)
                                                        (set! rdi vec.2)
                                                        (set! r15 tmp-ra.106)
                                                        (jump L.vector-ref.10 rbp r15 rdi rsi)))
                                                    (begin
                                                      (set! tmp-ra.107 r15)
                                                      (return-point L.rp.13 (begin (set! r15 L.rp.13) (jump L.v.4 rbp r15)))
                                                      (set! vec.3 rax)
                                                      (if (begin
                                                            (begin
                                                              (return-point L.rp.14
                                                                            (begin
                                                                              (set! rdi vec.3)
                                                                              (set! r15 L.rp.14)
                                                                              (jump L.set-first.5 rbp r15 rdi)))
                                                              (set! tmp.93 rax)
                                                              (return-point L.rp.15
                                                                            (begin
                                                                              (set! rdi tmp.93)
                                                                              (set! r15 L.rp.15)
                                                                              (jump L.void?.11 rbp r15 rdi)))
                                                              (set! tmp.92 rax))
                                                            (!= tmp.92 6))
                                                          (set! tmp.91 0)
                                                          (set! tmp.91 318))
                                                      (return-point L.rp.16
                                                                    (begin
                                                                      (set! rdi vec.3)
                                                                      (set! r15 L.rp.16)
                                                                      (jump L.get-first.6 rbp r15 rdi)))
                                                      (set! tmp.94 rax)
                                                      (set! rsi tmp.94)
                                                      (set! rdi tmp.91)
                                                      (set! r15 tmp-ra.107)
                                                      (jump L.+.12 rbp r15 rdi rsi)))))
                (interp-asm-pred-lang-v8/framed '(module
                                                     ((locals (tmp.94 tmp.92 tmp.93))
                                                      (conflicts
                                                       ((tmp-ra.107 (rdi rsi tmp.94 tmp.93 tmp.92 tmp.91 vec.3 rbp))
                                                        (vec.3 (tmp.93 tmp.92 tmp.91 rbp tmp-ra.107))
                                                        (tmp.91 (rsi tmp.94 rbp tmp-ra.107 vec.3))
                                                        (tmp.93 (rbp tmp-ra.107 vec.3))
                                                        (tmp.92 (rbp tmp-ra.107 vec.3))
                                                        (tmp.94 (rbp tmp-ra.107 tmp.91))
                                                        (rbp (rsi tmp.94 tmp.93 rdi tmp.92 tmp.91 vec.3 r15 tmp-ra.107))
                                                        (r15 (rsi rdi rbp))
                                                        (rdi (rsi tmp-ra.107 r15 rbp))
                                                        (rsi (r15 rdi rbp tmp-ra.107 tmp.91))))
                                                      (assignment ((tmp-ra.107 fv0) (tmp.91 fv1) (vec.3 fv2))))
                                                   (define L.+.12
                                                     ((locals (tmp.56 tmp.19 tmp.57 tmp-ra.95 tmp.55 tmp.20 tmp.54))
                                                      (conflicts
                                                       ((tmp.54 (rbp tmp-ra.95 tmp.20 tmp.19))
                                                        (tmp.20 (rbp tmp-ra.95 tmp.19 tmp.55 tmp.54 tmp.57 tmp.56 rax))
                                                        (tmp.55 (rbp tmp-ra.95 tmp.20 tmp.19))
                                                        (tmp-ra.95 (tmp.20 tmp.19 rbp rsi rdi tmp.55 tmp.54 tmp.57 tmp.56 rax))
                                                        (tmp.57 (rbp tmp-ra.95 tmp.20 tmp.19))
                                                        (tmp.19 (tmp.20 rbp tmp-ra.95 rsi tmp.55 tmp.54 tmp.57 tmp.56))
                                                        (tmp.56 (rbp tmp-ra.95 tmp.20 tmp.19))
                                                        (rax (tmp.20 rbp tmp-ra.95))
                                                        (rbp (tmp.20 tmp.19 tmp-ra.95 tmp.55 tmp.54 tmp.57 tmp.56 rax))
                                                        (rdi (tmp-ra.95))
                                                        (rsi (tmp.19 tmp-ra.95))))
                                                      (assignment ()))
                                                     (begin
                                                       (set! tmp-ra.95 r15)
                                                       (set! tmp.19 rdi)
                                                       (set! tmp.20 rsi)
                                                       (if (begin
                                                             (if (begin
                                                                   (begin
                                                                     (set! tmp.55 tmp.20)
                                                                     (set! tmp.55 (bitwise-and tmp.55 7)))
                                                                   (= tmp.55 0))
                                                                 (set! tmp.54 14)
                                                                 (set! tmp.54 6))
                                                             (!= tmp.54 6))
                                                           (if (begin
                                                                 (if (begin
                                                                       (begin
                                                                         (set! tmp.57 tmp.19)
                                                                         (set! tmp.57 (bitwise-and tmp.57 7)))
                                                                       (= tmp.57 0))
                                                                     (set! tmp.56 14)
                                                                     (set! tmp.56 6))
                                                                 (!= tmp.56 6))
                                                               (begin
                                                                 (set! rax tmp.19)
                                                                 (set! rax (+ rax tmp.20))
                                                                 (jump tmp-ra.95 rbp rax))
                                                               (begin (set! rax 574) (jump tmp-ra.95 rbp rax)))
                                                           (begin (set! rax 574) (jump tmp-ra.95 rbp rax)))))
                                                   (define L.void?.11
                                                     ((locals (tmp-ra.96 tmp.43 tmp.58))
                                                      (conflicts
                                                       ((tmp.58 (rbp tmp-ra.96))
                                                        (tmp.43 (rbp tmp-ra.96))
                                                        (tmp-ra.96 (tmp.43 rbp rdi tmp.58 rax))
                                                        (rax (rbp tmp-ra.96))
                                                        (rbp (tmp.43 tmp-ra.96 tmp.58 rax))
                                                        (rdi (tmp-ra.96))))
                                                      (assignment ()))
                                                     (begin
                                                       (set! tmp-ra.96 r15)
                                                       (set! tmp.43 rdi)
                                                       (if (begin
                                                             (begin (set! tmp.58 tmp.43) (set! tmp.58 (bitwise-and tmp.58 255)))
                                                             (= tmp.58 30))
                                                           (begin (set! rax 14) (jump tmp-ra.96 rbp rax))
                                                           (begin (set! rax 6) (jump tmp-ra.96 rbp rax)))))
                                                   (define L.unsafe-vector-ref.3
                                                     ((locals
                                                       (tmp.61 tmp.62 tmp.63 tmp.64 tmp-ra.97 tmp.15 tmp.60 tmp.14 tmp.59))
                                                      (conflicts
                                                       ((tmp.59 (rbp tmp-ra.97 tmp.14 tmp.15))
                                                        (tmp.14
                                                         (tmp.15 rbp tmp-ra.97 rsi tmp.60 tmp.59 tmp.61 tmp.62 tmp.63 tmp.64))
                                                        (tmp.60 (rbp tmp-ra.97 tmp.14 tmp.15))
                                                        (tmp.15 (rbp tmp-ra.97 tmp.14 tmp.60 tmp.59 tmp.61))
                                                        (tmp-ra.97
                                                         (tmp.15
                                                          tmp.14
                                                          rbp
                                                          rsi
                                                          rdi
                                                          tmp.60
                                                          tmp.59
                                                          tmp.61
                                                          tmp.62
                                                          tmp.63
                                                          tmp.64
                                                          rax))
                                                        (tmp.64 (rbp tmp-ra.97 tmp.14))
                                                        (tmp.63 (rbp tmp-ra.97 tmp.14))
                                                        (tmp.62 (rbp tmp-ra.97 tmp.14))
                                                        (tmp.61 (rbp tmp-ra.97 tmp.14 tmp.15))
                                                        (rax (rbp tmp-ra.97))
                                                        (rbp
                                                         (tmp.15
                                                          tmp.14
                                                          tmp-ra.97
                                                          tmp.60
                                                          tmp.59
                                                          tmp.61
                                                          tmp.62
                                                          tmp.63
                                                          tmp.64
                                                          rax))
                                                        (rdi (tmp-ra.97))
                                                        (rsi (tmp.14 tmp-ra.97))))
                                                      (assignment ()))
                                                     (begin
                                                       (set! tmp-ra.97 r15)
                                                       (set! tmp.14 rdi)
                                                       (set! tmp.15 rsi)
                                                       (if (begin
                                                             (if (begin (set! tmp.60 (mref tmp.14 -3)) (< tmp.15 tmp.60))
                                                                 (set! tmp.59 14)
                                                                 (set! tmp.59 6))
                                                             (!= tmp.59 6))
                                                           (if (begin
                                                                 (if (>= tmp.15 0) (set! tmp.61 14) (set! tmp.61 6))
                                                                 (!= tmp.61 6))
                                                               (begin
                                                                 (set! tmp.64 tmp.15)
                                                                 (set! tmp.64 (arithmetic-shift-right tmp.64 3))
                                                                 (set! tmp.63 tmp.64)
                                                                 (set! tmp.63 (* tmp.63 8))
                                                                 (set! tmp.62 tmp.63)
                                                                 (set! tmp.62 (+ tmp.62 5))
                                                                 (set! rax (mref tmp.14 tmp.62))
                                                                 (jump tmp-ra.97 rbp rax))
                                                               (begin (set! rax 2878) (jump tmp-ra.97 rbp rax)))
                                                           (begin (set! rax 2878) (jump tmp-ra.97 rbp rax)))))
                                                   (define L.vector-ref.10
                                                     ((locals (tmp.67 tmp.36 tmp.68 tmp-ra.98 tmp.66 tmp.37 tmp.65))
                                                      (conflicts
                                                       ((tmp.65 (rbp tmp-ra.98 tmp.36 tmp.37))
                                                        (tmp.37 (rbp tmp-ra.98 tmp.36 tmp.66 tmp.65 tmp.68 tmp.67))
                                                        (tmp.66 (rbp tmp-ra.98 tmp.36 tmp.37))
                                                        (tmp-ra.98 (tmp.37 tmp.36 rbp tmp.66 tmp.65 tmp.68 tmp.67 rdi rsi rax))
                                                        (tmp.68 (rbp tmp-ra.98 tmp.36 tmp.37))
                                                        (tmp.36 (tmp.37 rbp tmp-ra.98 tmp.66 tmp.65 tmp.68 tmp.67 rsi))
                                                        (tmp.67 (rbp tmp-ra.98 tmp.36 tmp.37))
                                                        (rax (rbp tmp-ra.98))
                                                        (rbp
                                                         (tmp.37 tmp.36 tmp-ra.98 tmp.66 tmp.65 tmp.68 tmp.67 r15 rdi rsi rax))
                                                        (rsi (r15 rdi rbp tmp-ra.98 tmp.36))
                                                        (rdi (r15 rbp rsi tmp-ra.98))
                                                        (r15 (rbp rdi rsi))))
                                                      (assignment ()))
                                                     (begin
                                                       (set! tmp-ra.98 r15)
                                                       (set! tmp.36 rdi)
                                                       (set! tmp.37 rsi)
                                                       (if (begin
                                                             (if (begin
                                                                   (begin
                                                                     (set! tmp.66 tmp.37)
                                                                     (set! tmp.66 (bitwise-and tmp.66 7)))
                                                                   (= tmp.66 0))
                                                                 (set! tmp.65 14)
                                                                 (set! tmp.65 6))
                                                             (!= tmp.65 6))
                                                           (if (begin
                                                                 (if (begin
                                                                       (begin
                                                                         (set! tmp.68 tmp.36)
                                                                         (set! tmp.68 (bitwise-and tmp.68 7)))
                                                                       (= tmp.68 3))
                                                                     (set! tmp.67 14)
                                                                     (set! tmp.67 6))
                                                                 (!= tmp.67 6))
                                                               (begin
                                                                 (set! rsi tmp.37)
                                                                 (set! rdi tmp.36)
                                                                 (set! r15 tmp-ra.98)
                                                                 (jump L.unsafe-vector-ref.3 rbp r15 rdi rsi))
                                                               (begin (set! rax 2878) (jump tmp-ra.98 rbp rax)))
                                                           (begin (set! rax 2878) (jump tmp-ra.98 rbp rax)))))
                                                   (define L.unsafe-vector-set!.2
                                                     ((locals
                                                       (tmp.71
                                                        tmp.11
                                                        tmp.72
                                                        tmp.73
                                                        tmp.74
                                                        tmp-ra.99
                                                        tmp.10
                                                        tmp.70
                                                        tmp.9
                                                        tmp.69))
                                                      (conflicts
                                                       ((tmp.69 (tmp.9 tmp.11 tmp-ra.99 rbp tmp.10))
                                                        (tmp.9
                                                         (tmp.11
                                                          tmp.10
                                                          tmp-ra.99
                                                          rbp
                                                          rdx
                                                          rsi
                                                          tmp.70
                                                          tmp.69
                                                          tmp.71
                                                          tmp.72
                                                          tmp.73
                                                          tmp.74))
                                                        (tmp.70 (tmp.9 tmp.11 tmp-ra.99 rbp tmp.10))
                                                        (tmp.10 (tmp.11 tmp.9 tmp-ra.99 rbp rdx tmp.70 tmp.69 tmp.71))
                                                        (tmp-ra.99
                                                         (tmp.11
                                                          tmp.10
                                                          tmp.9
                                                          rbp
                                                          rdx
                                                          rsi
                                                          rdi
                                                          tmp.70
                                                          tmp.69
                                                          tmp.71
                                                          tmp.72
                                                          tmp.73
                                                          tmp.74
                                                          rax))
                                                        (tmp.74 (tmp.9 tmp.11 tmp-ra.99 rbp))
                                                        (tmp.73 (tmp.9 tmp.11 tmp-ra.99 rbp))
                                                        (tmp.72 (tmp.9 tmp.11 tmp-ra.99 rbp))
                                                        (tmp.11
                                                         (tmp.9 tmp-ra.99 rbp tmp.10 tmp.70 tmp.69 tmp.71 tmp.72 tmp.73 tmp.74))
                                                        (tmp.71 (tmp.9 tmp.11 tmp-ra.99 rbp tmp.10))
                                                        (rax (rbp tmp-ra.99))
                                                        (rbp
                                                         (tmp.11
                                                          tmp.10
                                                          tmp.9
                                                          tmp-ra.99
                                                          tmp.70
                                                          tmp.69
                                                          tmp.71
                                                          tmp.72
                                                          tmp.73
                                                          tmp.74
                                                          rax))
                                                        (rdi (tmp-ra.99))
                                                        (rsi (tmp.9 tmp-ra.99))
                                                        (rdx (tmp.10 tmp.9 tmp-ra.99))))
                                                      (assignment ()))
                                                     (begin
                                                       (set! tmp-ra.99 r15)
                                                       (set! tmp.9 rdi)
                                                       (set! tmp.10 rsi)
                                                       (set! tmp.11 rdx)
                                                       (if (begin
                                                             (if (begin (set! tmp.70 (mref tmp.9 -3)) (< tmp.10 tmp.70))
                                                                 (set! tmp.69 14)
                                                                 (set! tmp.69 6))
                                                             (!= tmp.69 6))
                                                           (if (begin
                                                                 (if (>= tmp.10 0) (set! tmp.71 14) (set! tmp.71 6))
                                                                 (!= tmp.71 6))
                                                               (begin
                                                                 (set! tmp.74 tmp.10)
                                                                 (set! tmp.74 (arithmetic-shift-right tmp.74 3))
                                                                 (set! tmp.73 tmp.74)
                                                                 (set! tmp.73 (* tmp.73 8))
                                                                 (set! tmp.72 tmp.73)
                                                                 (set! tmp.72 (+ tmp.72 5))
                                                                 (mset! tmp.9 tmp.72 tmp.11)
                                                                 (set! rax 30)
                                                                 (jump tmp-ra.99 rbp rax))
                                                               (begin (set! rax 2622) (jump tmp-ra.99 rbp rax)))
                                                           (begin (set! rax 2622) (jump tmp-ra.99 rbp rax)))))
                                                   (define L.vector-set!.9
                                                     ((locals (tmp.77 tmp.33 tmp.78 tmp-ra.100 tmp.35 tmp.76 tmp.34 tmp.75))
                                                      (conflicts
                                                       ((tmp.75 (rbp tmp-ra.100 tmp.33 tmp.34 tmp.35))
                                                        (tmp.34 (tmp.35 rbp tmp-ra.100 tmp.33 tmp.76 tmp.75 tmp.78 tmp.77 rdx))
                                                        (tmp.76 (rbp tmp-ra.100 tmp.33 tmp.34 tmp.35))
                                                        (tmp.35 (rbp tmp-ra.100 tmp.33 tmp.34 tmp.76 tmp.75 tmp.78 tmp.77))
                                                        (tmp-ra.100
                                                         (tmp.35 tmp.34 tmp.33 rbp tmp.76 tmp.75 tmp.78 tmp.77 rdi rsi rdx rax))
                                                        (tmp.78 (rbp tmp-ra.100 tmp.33 tmp.34 tmp.35))
                                                        (tmp.33
                                                         (tmp.35 tmp.34 rbp tmp-ra.100 tmp.76 tmp.75 tmp.78 tmp.77 rsi rdx))
                                                        (tmp.77 (rbp tmp-ra.100 tmp.33 tmp.34 tmp.35))
                                                        (rax (rbp tmp-ra.100))
                                                        (rbp
                                                         (tmp.35
                                                          tmp.34
                                                          tmp.33
                                                          tmp-ra.100
                                                          tmp.76
                                                          tmp.75
                                                          tmp.78
                                                          tmp.77
                                                          r15
                                                          rdi
                                                          rsi
                                                          rdx
                                                          rax))
                                                        (rdx (r15 rdi rsi rbp tmp-ra.100 tmp.33 tmp.34))
                                                        (rsi (r15 rdi rbp rdx tmp-ra.100 tmp.33))
                                                        (rdi (r15 rbp rsi rdx tmp-ra.100))
                                                        (r15 (rbp rdi rsi rdx))))
                                                      (assignment ()))
                                                     (begin
                                                       (set! tmp-ra.100 r15)
                                                       (set! tmp.33 rdi)
                                                       (set! tmp.34 rsi)
                                                       (set! tmp.35 rdx)
                                                       (if (begin
                                                             (if (begin
                                                                   (begin
                                                                     (set! tmp.76 tmp.34)
                                                                     (set! tmp.76 (bitwise-and tmp.76 7)))
                                                                   (= tmp.76 0))
                                                                 (set! tmp.75 14)
                                                                 (set! tmp.75 6))
                                                             (!= tmp.75 6))
                                                           (if (begin
                                                                 (if (begin
                                                                       (begin
                                                                         (set! tmp.78 tmp.33)
                                                                         (set! tmp.78 (bitwise-and tmp.78 7)))
                                                                       (= tmp.78 3))
                                                                     (set! tmp.77 14)
                                                                     (set! tmp.77 6))
                                                                 (!= tmp.77 6))
                                                               (begin
                                                                 (set! rdx tmp.35)
                                                                 (set! rsi tmp.34)
                                                                 (set! rdi tmp.33)
                                                                 (set! r15 tmp-ra.100)
                                                                 (jump L.unsafe-vector-set!.2 rbp r15 rdi rsi rdx))
                                                               (begin (set! rax 2622) (jump tmp-ra.100 rbp rax)))
                                                           (begin (set! rax 2622) (jump tmp-ra.100 rbp rax)))))
                                                   (define L.vector-init-loop.7
                                                     ((locals (tmp.82 tmp.81 tmp.80 tmp.83 tmp-ra.101 vec.7 len.6 i.8 tmp.79))
                                                      (conflicts
                                                       ((tmp.79 (rbp tmp-ra.101 vec.7 len.6 i.8))
                                                        (i.8 (vec.7 rbp tmp-ra.101 len.6 rdx tmp.79 tmp.80 tmp.81 tmp.82))
                                                        (len.6
                                                         (vec.7 i.8 rbp tmp-ra.101 tmp.79 rsi rdx tmp.83 tmp.80 tmp.81 tmp.82))
                                                        (vec.7 (rbp tmp-ra.101 len.6 i.8 tmp.79 tmp.83 tmp.80 tmp.81 tmp.82))
                                                        (tmp-ra.101
                                                         (vec.7
                                                          i.8
                                                          len.6
                                                          rbp
                                                          tmp.79
                                                          rax
                                                          rdi
                                                          rsi
                                                          rdx
                                                          tmp.83
                                                          tmp.80
                                                          tmp.81
                                                          tmp.82))
                                                        (tmp.83 (rdx rbp tmp-ra.101 len.6 vec.7))
                                                        (tmp.80 (vec.7 i.8 len.6 tmp-ra.101 rbp))
                                                        (tmp.81 (vec.7 i.8 len.6 tmp-ra.101 rbp))
                                                        (tmp.82 (i.8 vec.7 len.6 tmp-ra.101 rbp))
                                                        (rbp
                                                         (vec.7
                                                          i.8
                                                          len.6
                                                          tmp-ra.101
                                                          tmp.79
                                                          rax
                                                          r15
                                                          rdi
                                                          rsi
                                                          rdx
                                                          tmp.83
                                                          tmp.80
                                                          tmp.81
                                                          tmp.82))
                                                        (rdx (i.8 r15 rdi rsi rbp tmp-ra.101 len.6 tmp.83))
                                                        (rsi (r15 rdi rbp rdx tmp-ra.101 len.6))
                                                        (rdi (r15 rbp rsi rdx tmp-ra.101))
                                                        (r15 (rbp rdi rsi rdx))
                                                        (rax (rbp tmp-ra.101))))
                                                      (assignment ()))
                                                     (begin
                                                       (set! tmp-ra.101 r15)
                                                       (set! len.6 rdi)
                                                       (set! i.8 rsi)
                                                       (set! vec.7 rdx)
                                                       (if (begin
                                                             (if (= len.6 i.8) (set! tmp.79 14) (set! tmp.79 6))
                                                             (!= tmp.79 6))
                                                           (begin (set! rax vec.7) (jump tmp-ra.101 rbp rax))
                                                           (begin
                                                             (set! tmp.82 i.8)
                                                             (set! tmp.82 (arithmetic-shift-right tmp.82 3))
                                                             (set! tmp.81 tmp.82)
                                                             (set! tmp.81 (* tmp.81 8))
                                                             (set! tmp.80 tmp.81)
                                                             (set! tmp.80 (+ tmp.80 5))
                                                             (mset! vec.7 tmp.80 0)
                                                             (set! tmp.83 i.8)
                                                             (set! tmp.83 (+ tmp.83 8))
                                                             (set! rdx vec.7)
                                                             (set! rsi tmp.83)
                                                             (set! rdi len.6)
                                                             (set! r15 tmp-ra.101)
                                                             (jump L.vector-init-loop.7 rbp r15 rdi rsi rdx)))))
                                                   (define L.make-init-vector.1
                                                     ((locals
                                                       (tmp.88 tmp.87 tmp.86 tmp.85 tmp.53 tmp.5 tmp-ra.102 tmp.4 tmp.84))
                                                      (conflicts
                                                       ((tmp.84 (tmp.4 tmp-ra.102 rbp r12))
                                                        (tmp.4
                                                         (tmp-ra.102
                                                          rbp
                                                          tmp.84
                                                          rsi
                                                          rdx
                                                          tmp.5
                                                          tmp.53
                                                          tmp.85
                                                          r12
                                                          tmp.86
                                                          tmp.87
                                                          tmp.88))
                                                        (tmp-ra.102
                                                         (tmp.4
                                                          rbp
                                                          tmp.84
                                                          rdi
                                                          rsi
                                                          rdx
                                                          tmp.5
                                                          tmp.53
                                                          tmp.85
                                                          r12
                                                          tmp.86
                                                          tmp.87
                                                          tmp.88
                                                          rax))
                                                        (tmp.5 (rbp tmp-ra.102 tmp.4))
                                                        (tmp.53 (tmp.4 tmp-ra.102 rbp))
                                                        (tmp.85 (tmp.4 tmp-ra.102 rbp tmp.86 r12))
                                                        (tmp.86 (tmp.85 tmp.4 tmp-ra.102 rbp r12))
                                                        (tmp.87 (tmp.4 tmp-ra.102 rbp r12 tmp.88))
                                                        (tmp.88 (tmp.87 tmp.4 tmp-ra.102 rbp r12))
                                                        (rax (rbp tmp-ra.102))
                                                        (rbp
                                                         (tmp.4
                                                          tmp-ra.102
                                                          tmp.84
                                                          r15
                                                          rdi
                                                          rsi
                                                          rdx
                                                          tmp.5
                                                          tmp.53
                                                          tmp.85
                                                          r12
                                                          tmp.86
                                                          tmp.87
                                                          tmp.88
                                                          rax))
                                                        (r12 (tmp.84 tmp.4 tmp-ra.102 rbp tmp.85 tmp.86 tmp.87 tmp.88))
                                                        (rdx (r15 rdi rsi rbp tmp-ra.102 tmp.4))
                                                        (rsi (r15 rdi rbp rdx tmp-ra.102 tmp.4))
                                                        (rdi (r15 rbp rsi rdx tmp-ra.102))
                                                        (r15 (rbp rdi rsi rdx))))
                                                      (assignment ()))
                                                     (begin
                                                       (set! tmp-ra.102 r15)
                                                       (set! tmp.4 rdi)
                                                       (if (begin
                                                             (if (>= tmp.4 0) (set! tmp.84 14) (set! tmp.84 6))
                                                             (!= tmp.84 6))
                                                           (begin
                                                             (set! tmp.88 tmp.4)
                                                             (set! tmp.88 (arithmetic-shift-right tmp.88 3))
                                                             (set! tmp.87 1)
                                                             (set! tmp.87 (+ tmp.87 tmp.88))
                                                             (set! tmp.86 tmp.87)
                                                             (set! tmp.86 (* tmp.86 8))
                                                             (begin (set! tmp.85 r12) (set! r12 (+ r12 tmp.86)))
                                                             (set! tmp.53 tmp.85)
                                                             (set! tmp.53 (+ tmp.53 3))
                                                             (mset! tmp.53 -3 tmp.4)
                                                             (set! tmp.5 tmp.53)
                                                             (set! rdx tmp.5)
                                                             (set! rsi 0)
                                                             (set! rdi tmp.4)
                                                             (set! r15 tmp-ra.102)
                                                             (jump L.vector-init-loop.7 rbp r15 rdi rsi rdx))
                                                           (begin (set! rax 3134) (jump tmp-ra.102 rbp rax)))))
                                                   (define L.make-vector.8
                                                     ((locals (tmp-ra.103 tmp.90 tmp.31 tmp.89))
                                                      (conflicts
                                                       ((tmp.89 (rbp tmp-ra.103 tmp.31))
                                                        (tmp.31 (rbp tmp-ra.103 tmp.90 tmp.89))
                                                        (tmp.90 (rbp tmp-ra.103 tmp.31))
                                                        (tmp-ra.103 (tmp.31 rbp tmp.90 tmp.89 rdi rax))
                                                        (rax (rbp tmp-ra.103))
                                                        (rbp (tmp.31 tmp-ra.103 tmp.90 tmp.89 r15 rdi rax))
                                                        (rdi (r15 rbp tmp-ra.103))
                                                        (r15 (rbp rdi))))
                                                      (assignment ()))
                                                     (begin
                                                       (set! tmp-ra.103 r15)
                                                       (set! tmp.31 rdi)
                                                       (if (begin
                                                             (if (begin
                                                                   (begin
                                                                     (set! tmp.90 tmp.31)
                                                                     (set! tmp.90 (bitwise-and tmp.90 7)))
                                                                   (= tmp.90 0))
                                                                 (set! tmp.89 14)
                                                                 (set! tmp.89 6))
                                                             (!= tmp.89 6))
                                                           (begin
                                                             (set! rdi tmp.31)
                                                             (set! r15 tmp-ra.103)
                                                             (jump L.make-init-vector.1 rbp r15 rdi))
                                                           (begin (set! rax 2110) (jump tmp-ra.103 rbp rax)))))
                                                   (define L.v.4
                                                     ((locals (tmp-ra.104))
                                                      (conflicts
                                                       ((tmp-ra.104 (rdi rbp))
                                                        (rbp (r15 rdi tmp-ra.104))
                                                        (rdi (r15 rbp tmp-ra.104))
                                                        (r15 (rbp rdi))))
                                                      (assignment ()))
                                                     (begin
                                                       (set! tmp-ra.104 r15)
                                                       (set! rdi 24)
                                                       (set! r15 tmp-ra.104)
                                                       (jump L.make-vector.8 rbp r15 rdi)))
                                                   (define L.set-first.5
                                                     ((locals (vec.1 tmp-ra.105))
                                                      (conflicts
                                                       ((tmp-ra.105 (rsi rdx vec.1 rbp rdi))
                                                        (vec.1 (rsi rdx rbp tmp-ra.105))
                                                        (rdi (r15 rbp rsi rdx tmp-ra.105))
                                                        (rbp (r15 rdi rsi rdx vec.1 tmp-ra.105))
                                                        (rdx (r15 rdi rsi rbp tmp-ra.105 vec.1))
                                                        (rsi (r15 rdi rbp rdx tmp-ra.105 vec.1))
                                                        (r15 (rbp rdi rsi rdx))))
                                                      (assignment ()))
                                                     (begin
                                                       (set! tmp-ra.105 r15)
                                                       (set! vec.1 rdi)
                                                       (set! rdx 336)
                                                       (set! rsi 0)
                                                       (set! rdi vec.1)
                                                       (set! r15 tmp-ra.105)
                                                       (jump L.vector-set!.9 rbp r15 rdi rsi rdx)))
                                                   (define L.get-first.6
                                                     ((locals (vec.2 tmp-ra.106))
                                                      (conflicts
                                                       ((tmp-ra.106 (rsi vec.2 rbp rdi))
                                                        (vec.2 (rsi rbp tmp-ra.106))
                                                        (rdi (r15 rbp rsi tmp-ra.106))
                                                        (rbp (r15 rdi rsi vec.2 tmp-ra.106))
                                                        (rsi (r15 rdi rbp tmp-ra.106 vec.2))
                                                        (r15 (rbp rdi rsi))))
                                                      (assignment ()))
                                                     (begin
                                                       (set! tmp-ra.106 r15)
                                                       (set! vec.2 rdi)
                                                       (set! rsi 0)
                                                       (set! rdi vec.2)
                                                       (set! r15 tmp-ra.106)
                                                       (jump L.vector-ref.10 rbp r15 rdi rsi)))
                                                   (begin
                                                     (set! tmp-ra.107 r15)
                                                     (begin
                                                       (set! rbp (- rbp 24))
                                                       (return-point L.rp.13 (begin (set! r15 L.rp.13) (jump L.v.4 rbp r15)))
                                                       (set! rbp (+ rbp 24)))
                                                     (set! vec.3 rax)
                                                     (if (begin
                                                           (begin
                                                             (begin
                                                               (set! rbp (- rbp 24))
                                                               (return-point L.rp.14
                                                                             (begin
                                                                               (set! rdi vec.3)
                                                                               (set! r15 L.rp.14)
                                                                               (jump L.set-first.5 rbp r15 rdi)))
                                                               (set! rbp (+ rbp 24)))
                                                             (set! tmp.93 rax)
                                                             (begin
                                                               (set! rbp (- rbp 24))
                                                               (return-point L.rp.15
                                                                             (begin
                                                                               (set! rdi tmp.93)
                                                                               (set! r15 L.rp.15)
                                                                               (jump L.void?.11 rbp r15 rdi)))
                                                               (set! rbp (+ rbp 24)))
                                                             (set! tmp.92 rax))
                                                           (!= tmp.92 6))
                                                         (set! tmp.91 0)
                                                         (set! tmp.91 318))
                                                     (begin
                                                       (set! rbp (- rbp 24))
                                                       (return-point L.rp.16
                                                                     (begin
                                                                       (set! rdi vec.3)
                                                                       (set! r15 L.rp.16)
                                                                       (jump L.get-first.6 rbp r15 rdi)))
                                                       (set! rbp (+ rbp 24)))
                                                     (set! tmp.94 rax)
                                                     (set! rsi tmp.94)
                                                     (set! rdi tmp.91)
                                                     (set! r15 tmp-ra.107)
                                                     (jump L.+.12 rbp r15 rdi rsi))))))
