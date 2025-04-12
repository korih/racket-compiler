#lang racket

(require
  rackunit
  "../passes/implement-fvars.rkt"
  cpsc411/langs/v8)

(module+ test
  (check-equal? (implement-fvars '(module (begin
                                            (set! fv0 r15)
                                            (set! rbp (- rbp 8))
                                            (set! r15 fv0)
                                            (set! rbp (+ rbp 8))
                                            (jump fv0))))
                '(module
                     (begin
                       (set! (rbp - 0) r15)
                       (set! rbp (- rbp 8))
                       (set! r15 (rbp - -8))
                       (set! rbp (+ rbp 8))
                       (jump (rbp - 0)))))
  (check-equal? (implement-fvars '(module (begin
                                            (set! fv0 r15)
                                            (set! rbp (+ rbp 8))
                                            (set! rbp (* rbp 3))
                                            (set! r15 fv0)
                                            (jump fv0))))
                '(module
                     (begin
                       (set! (rbp - 0) r15)
                       (set! rbp (+ rbp 8))
                       (set! rbp (* rbp 3))
                       (set! r15 (rbp - 24))
                       (jump (rbp - 24)))))
  (check-equal? (implement-fvars '(module (begin
                                            (set! fv0 r15)
                                            (set! rbp (- rbp 32))
                                            (return-point L.rp.1
                                                          (if (< fv0 fv1)
                                                              (jump fv0)
                                                              (begin
                                                                (set! fv0 (- fv0 1))
                                                                (jump L.rp.1))))
                                            (set! rbp (+ rbp 32))
                                            (jump L.rp.1))))
                '(module
                     (begin
                       (set! (rbp - 0) r15)
                       (set! rbp (- rbp 32))
                       (return-point L.rp.1
                                     (if (< (rbp - -32) (rbp - -24))
                                         (jump (rbp - -32))
                                         (begin (set! (rbp - -32) (- (rbp - -32) 1)) (jump L.rp.1))))
                       (set! rbp (+ rbp 32))
                       (jump L.rp.1))))
  (check-equal? (implement-fvars '(module
                                      (define L.swap.1
                                        (begin
                                          (set! fv2 r15)
                                          (set! r14 fv0)
                                          (set! r15 fv1)
                                          (if (< r15 r14)
                                              (begin (set! rax r14) (jump fv2))
                                              (begin
                                                (begin
                                                  (set! rbp (- rbp 24))
                                                  (return-point L.rp.6
                                                                (begin
                                                                  (set! fv4 r14)
                                                                  (set! fv3 r15)
                                                                  (set! r15 L.rp.6)
                                                                  (jump L.swap.1)))
                                                  (set! rbp (+ rbp 24)))
                                                (set! r15 rax)
                                                (set! rax r15)
                                                (jump fv2)))))
                                    (begin
                                      (set! r15 r15)
                                      (set! fv1 2)
                                      (set! fv0 1)
                                      (set! r15 r15)
                                      (jump L.swap.1))))
                '(module
                     (define L.swap.1
                       (begin
                         (set! (rbp - 16) r15)
                         (set! r14 (rbp - 0))
                         (set! r15 (rbp - 8))
                         (if (< r15 r14)
                             (begin (set! rax r14) (jump (rbp - 16)))
                             (begin
                               (begin
                                 (set! rbp (- rbp 24))
                                 (return-point L.rp.6
                                               (begin
                                                 (set! (rbp - 8) r14)
                                                 (set! (rbp - 0) r15)
                                                 (set! r15 L.rp.6)
                                                 (jump L.swap.1)))
                                 (set! rbp (+ rbp 24)))
                               (set! r15 rax)
                               (set! rax r15)
                               (jump (rbp - 16))))))
                   (begin
                     (set! r15 r15)
                     (set! (rbp - 8) 2)
                     (set! (rbp - 0) 1)
                     (set! r15 r15)
                     (jump L.swap.1))))
  (check-equal? (implement-fvars '(module
                                      (define L.f.1
                                        (begin
                                          (set! rsp r15)
                                          (set! rcx rdi)
                                          (set! rdx 1)
                                          (set! rbx 2)
                                          (set! rdx rdx)
                                          (set! rdx (bitwise-and rdx rcx))
                                          (set! rbx rbx)
                                          (set! rbx (bitwise-ior rbx rcx))
                                          (set! rdx (bitwise-xor rdx rbx))
                                          (set! rax rdx)
                                          (set! rax (arithmetic-shift-right rax 3))
                                          (jump rsp)))
                                    (begin
                                      (set! rbx r15)
                                      (set! rcx 10)
                                      (if (begin (set! rsp 100) (not (!= rcx rsp)))
                                          (begin (set! rdi rcx) (set! r15 rbx) (jump L.f.1))
                                          (begin (set! rdi 1000) (set! r15 rbx) (jump L.f.2))))))
                '(module
                     (define L.f.1
                       (begin
                         (set! rsp r15)
                         (set! rcx rdi)
                         (set! rdx 1)
                         (set! rbx 2)
                         (set! rdx rdx)
                         (set! rdx (bitwise-and rdx rcx))
                         (set! rbx rbx)
                         (set! rbx (bitwise-ior rbx rcx))
                         (set! rdx (bitwise-xor rdx rbx))
                         (set! rax rdx)
                         (set! rax (arithmetic-shift-right rax 3))
                         (jump rsp)))
                   (begin
                     (set! rbx r15)
                     (set! rcx 10)
                     (if (begin (set! rsp 100) (not (!= rcx rsp)))
                         (begin (set! rdi rcx) (set! r15 rbx) (jump L.f.1))
                         (begin (set! rdi 1000) (set! r15 rbx) (jump L.f.2))))))
  (check-equal? (implement-fvars '(module
                                      (define L.f.1
                                        (begin
                                          (set! fv3 r15)
                                          (set! fv1 rdi)
                                          (set! fv0 rsi)
                                          (set! rsp 10)
                                          (set! rsp (+ rsp 6))
                                          (begin (set! fv2 r12) (set! r12 (+ r12 rsp)))
                                          (begin
                                            (set! rbp (- rbp 32))
                                            (return-point L.rp.21 (begin (set! r15 L.rp.21) (jump L.g.1)))
                                            (set! rbp (+ rbp 32)))
                                          (set! rsp rax)
                                          (if (true) (mset! fv2 rsp fv1) (mset! fv2 rsp fv0))
                                          (set! rbx 10)
                                          (set! rbx (+ rbx 6))
                                          (begin (set! rsp r12) (set! r12 (+ r12 rbx)))
                                          (set! rbx 8)
                                          (set! rbx (bitwise-and rbx 8))
                                          (set! rax (mref rsp rbx))
                                          (jump fv3)))
                                    (define L.g.1 (begin (set! rsp r15) (set! rax 8) (jump rsp)))
                                    (begin (set! rsp r15) (set! rdi 1) (set! rsi 2) (set! r15 rsp) (jump L.f.1))))
                '(module
                     (define L.f.1
                       (begin
                         (set! (rbp - 24) r15)
                         (set! (rbp - 8) rdi)
                         (set! (rbp - 0) rsi)
                         (set! rsp 10)
                         (set! rsp (+ rsp 6))
                         (begin (set! (rbp - 16) r12) (set! r12 (+ r12 rsp)))
                         (begin
                           (set! rbp (- rbp 32))
                           (return-point L.rp.21 (begin (set! r15 L.rp.21) (jump L.g.1)))
                           (set! rbp (+ rbp 32)))
                         (set! rsp rax)
                         (if (true)
                             (mset! (rbp - 16) rsp (rbp - 8))
                             (mset! (rbp - 16) rsp (rbp - 0)))
                         (set! rbx 10)
                         (set! rbx (+ rbx 6))
                         (begin (set! rsp r12) (set! r12 (+ r12 rbx)))
                         (set! rbx 8)
                         (set! rbx (bitwise-and rbx 8))
                         (set! rax (mref rsp rbx))
                         (jump (rbp - 24))))
                   (define L.g.1 (begin (set! rsp r15) (set! rax 8) (jump rsp)))
                   (begin (set! rsp r15) (set! rdi 1) (set! rsi 2) (set! r15 rsp) (jump L.f.1))))

  (check-equal? (interp-nested-asm-lang-v8 (implement-fvars '(module (begin
                                                                       (set! r15 r15)
                                                                       (set! r13 rdi)
                                                                       (set! r14 rsi)
                                                                       (if (begin
                                                                             (if (begin (set! r9 10) (set! r9 (bitwise-and r9 7)) (= r9 0))
                                                                                 (set! r9 14)
                                                                                 (set! r9 6))
                                                                             (!= r9 6))
                                                                           (if (begin
                                                                                 (if (begin (set! r9 r13) (set! r9 (bitwise-and r9 7)) (= r9 0))
                                                                                     (set! r9 14)
                                                                                     (set! r9 6))
                                                                                 (!= r9 6))
                                                                               (begin (set! rax r13) (set! rax (+ rax r14)) (jump r15))
                                                                               (begin (set! rax 574) (jump r15)))
                                                                           (begin (set! rax 574) (jump r15)))))))
                (interp-nested-asm-lang-v8 '(module
                                                (begin
                                                  (set! r15 r15)
                                                  (set! r13 rdi)
                                                  (set! r14 rsi)
                                                  (if (begin
                                                        (if (begin (set! r9 10) (set! r9 (bitwise-and r9 7)) (= r9 0))
                                                            (set! r9 14)
                                                            (set! r9 6))
                                                        (!= r9 6))
                                                      (if (begin
                                                            (if (begin (set! r9 r13) (set! r9 (bitwise-and r9 7)) (= r9 0))
                                                                (set! r9 14)
                                                                (set! r9 6))
                                                            (!= r9 6))
                                                          (begin (set! rax r13) (set! rax (+ rax r14)) (jump r15))
                                                          (begin (set! rax 574) (jump r15)))
                                                      (begin (set! rax 574) (jump r15)))))))

  (check-equal? (interp-nested-asm-lang-v8 (implement-fvars '(module
                                                                 (define L.+.31
                                                                   (begin
                                                                     (set! r15 r15)
                                                                     (set! r13 rdi)
                                                                     (set! r14 10)
                                                                     (if (begin
                                                                           (if (begin (set! r9 r14) (set! r9 (bitwise-and r9 7)) (= r9 0))
                                                                               (set! r9 14)
                                                                               (set! r9 6))
                                                                           (!= r9 6))
                                                                         (if (begin
                                                                               (if (begin (set! r9 r13) (set! r9 (bitwise-and r9 7)) (= r9 0))
                                                                                   (set! r9 14)
                                                                                   (set! r9 6))
                                                                               (!= r9 6))
                                                                             (begin (set! rax r13) (set! rax (+ rax r14)) (jump r15))
                                                                             (begin (set! rax 574) (jump r15)))
                                                                         (begin (set! rax 574) (jump r15)))))
                                                               (define L.F.6
                                                                 (begin
                                                                   (set! fv1 r15)
                                                                   (set! r15 rdi)
                                                                   (set! r14 rsi)
                                                                   (set! r13 rdx)
                                                                   (set! rcx rcx)
                                                                   (set! r8 r8)
                                                                   (set! r9 r9)
                                                                   (set! rbx fv0)
                                                                   (begin
                                                                     (set! rbp (- rbp 16))
                                                                     (return-point L.rp.47
                                                                                   (begin
                                                                                     (set! rdi r15)
                                                                                     (set! rsi r14)
                                                                                     (set! rdx r13)
                                                                                     (set! rcx rcx)
                                                                                     (set! r8 r8)
                                                                                     (set! r9 r9)
                                                                                     (set! r13 rbx)
                                                                                     (set! r14 64)
                                                                                     (set! r15 L.rp.47)
                                                                                     (jump L.G.7)))
                                                                     (set! rbp (+ rbp 16)))
                                                                   (set! r15 rax)
                                                                   (set! rdi 80)
                                                                   (set! rsi r15)
                                                                   (set! r15 fv1)
                                                                   (jump L.+.31)))
                                                               (define L.G.7
                                                                 (begin
                                                                   (set! r15 r15)
                                                                   (set! r14 rdi)
                                                                   (set! r13 rsi)
                                                                   (set! rdx rdx)
                                                                   (set! rcx rcx)
                                                                   (set! r8 r8)
                                                                   (set! r9 r9)
                                                                   (set! rbx fv0)
                                                                   (set! rsp fv1)
                                                                   (set! rdi r14)
                                                                   (set! rsi r13)
                                                                   (set! rdx rdx)
                                                                   (set! rcx rcx)
                                                                   (set! r8 r8)
                                                                   (set! r9 r9)
                                                                   (set! fv0 rbx)
                                                                   (set! fv1 rsp)
                                                                   (set! fv2 72)
                                                                   (set! r15 r15)
                                                                   (jump L.H.8)))
                                                               (define L.H.8
                                                                 (begin
                                                                   (set! fv3 r15)
                                                                   (set! r15 rdi)
                                                                   (set! r14 rsi)
                                                                   (set! fv8 rdx)
                                                                   (set! fv7 rcx)
                                                                   (set! fv6 r8)
                                                                   (set! fv5 r9)
                                                                   (set! fv4 fv0)
                                                                   (set! fv1 fv1)
                                                                   (set! fv0 fv2)
                                                                   (begin
                                                                     (set! rbp (- rbp 72))
                                                                     (return-point L.rp.48
                                                                                   (begin
                                                                                     (set! rdi r15)
                                                                                     (set! rsi r14)
                                                                                     (set! r15 L.rp.48)
                                                                                     (jump L.+.31)))
                                                                     (set! rbp (+ rbp 72)))
                                                                   (set! r15 rax)
                                                                   (begin
                                                                     (set! rbp (- rbp 72))
                                                                     (return-point L.rp.49
                                                                                   (begin
                                                                                     (set! rdi r15)
                                                                                     (set! rsi fv8)
                                                                                     (set! r15 L.rp.49)
                                                                                     (jump L.+.31)))
                                                                     (set! rbp (+ rbp 72)))
                                                                   (set! r15 rax)
                                                                   (begin
                                                                     (set! rbp (- rbp 72))
                                                                     (return-point L.rp.50
                                                                                   (begin
                                                                                     (set! rdi r15)
                                                                                     (set! rsi fv7)
                                                                                     (set! r15 L.rp.50)
                                                                                     (jump L.+.31)))
                                                                     (set! rbp (+ rbp 72)))
                                                                   (set! r15 rax)
                                                                   (begin
                                                                     (set! rbp (- rbp 72))
                                                                     (return-point L.rp.51
                                                                                   (begin
                                                                                     (set! rdi r15)
                                                                                     (set! rsi fv6)
                                                                                     (set! r15 L.rp.51)
                                                                                     (jump L.+.31)))
                                                                     (set! rbp (+ rbp 72)))
                                                                   (set! r15 rax)
                                                                   (begin
                                                                     (set! rbp (- rbp 72))
                                                                     (return-point L.rp.52
                                                                                   (begin
                                                                                     (set! rdi r15)
                                                                                     (set! rsi fv5)
                                                                                     (set! r15 L.rp.52)
                                                                                     (jump L.+.31)))
                                                                     (set! rbp (+ rbp 72)))
                                                                   (set! r15 rax)
                                                                   (begin
                                                                     (set! rbp (- rbp 72))
                                                                     (return-point L.rp.53
                                                                                   (begin
                                                                                     (set! rdi r15)
                                                                                     (set! rsi fv4)
                                                                                     (set! r15 L.rp.53)
                                                                                     (jump L.+.31)))
                                                                     (set! rbp (+ rbp 72)))
                                                                   (set! r15 rax)
                                                                   (begin
                                                                     (set! rbp (- rbp 72))
                                                                     (return-point L.rp.54
                                                                                   (begin
                                                                                     (set! rdi r15)
                                                                                     (set! rsi fv1)
                                                                                     (set! r15 L.rp.54)
                                                                                     (jump L.+.31)))
                                                                     (set! rbp (+ rbp 72)))
                                                                   (set! r15 rax)
                                                                   (set! rdi r15)
                                                                   (set! rsi fv0)
                                                                   (set! r15 fv3)
                                                                   (jump L.+.31)))
                                                               (begin
                                                                 (set! r15 r15)
                                                                 (set! rdi 8)
                                                                 (set! rsi 16)
                                                                 (set! rdx 24)
                                                                 (set! rcx 32)
                                                                 (set! r8 40)
                                                                 (set! r9 48)
                                                                 (set! fv0 56)
                                                                 (set! r15 r15)
                                                                 (jump L.F.6)))))
                (interp-nested-asm-lang-v8 '(module
                                                (define L.+.31
                                                  (begin
                                                    (set! r15 r15)
                                                    (set! r13 rdi)
                                                    (set! r14 10)
                                                    (if (begin
                                                          (if (begin (set! r9 r14) (set! r9 (bitwise-and r9 7)) (= r9 0))
                                                              (set! r9 14)
                                                              (set! r9 6))
                                                          (!= r9 6))
                                                        (if (begin
                                                              (if (begin (set! r9 r13) (set! r9 (bitwise-and r9 7)) (= r9 0))
                                                                  (set! r9 14)
                                                                  (set! r9 6))
                                                              (!= r9 6))
                                                            (begin (set! rax r13) (set! rax (+ rax r14)) (jump r15))
                                                            (begin (set! rax 574) (jump r15)))
                                                        (begin (set! rax 574) (jump r15)))))
                                              (define L.F.6
                                                (begin
                                                  (set! (rbp - 8) r15)
                                                  (set! r15 rdi)
                                                  (set! r14 rsi)
                                                  (set! r13 rdx)
                                                  (set! rcx rcx)
                                                  (set! r8 r8)
                                                  (set! r9 r9)
                                                  (set! rbx (rbp - 0))
                                                  (begin
                                                    (set! rbp (- rbp 16))
                                                    (return-point L.rp.47
                                                                  (begin
                                                                    (set! rdi r15)
                                                                    (set! rsi r14)
                                                                    (set! rdx r13)
                                                                    (set! rcx rcx)
                                                                    (set! r8 r8)
                                                                    (set! r9 r9)
                                                                    (set! r13 rbx)
                                                                    (set! r14 64)
                                                                    (set! r15 L.rp.47)
                                                                    (jump L.G.7)))
                                                    (set! rbp (+ rbp 16)))
                                                  (set! r15 rax)
                                                  (set! rdi 80)
                                                  (set! rsi r15)
                                                  (set! r15 (rbp - 8))
                                                  (jump L.+.31)))
                                              (define L.G.7
                                                (begin
                                                  (set! r15 r15)
                                                  (set! r14 rdi)
                                                  (set! r13 rsi)
                                                  (set! rdx rdx)
                                                  (set! rcx rcx)
                                                  (set! r8 r8)
                                                  (set! r9 r9)
                                                  (set! rbx (rbp - 0))
                                                  (set! rsp (rbp - 8))
                                                  (set! rdi r14)
                                                  (set! rsi r13)
                                                  (set! rdx rdx)
                                                  (set! rcx rcx)
                                                  (set! r8 r8)
                                                  (set! r9 r9)
                                                  (set! (rbp - 0) rbx)
                                                  (set! (rbp - 8) rsp)
                                                  (set! (rbp - 16) 72)
                                                  (set! r15 r15)
                                                  (jump L.H.8)))
                                              (define L.H.8
                                                (begin
                                                  (set! (rbp - 24) r15)
                                                  (set! r15 rdi)
                                                  (set! r14 rsi)
                                                  (set! (rbp - 64) rdx)
                                                  (set! (rbp - 56) rcx)
                                                  (set! (rbp - 48) r8)
                                                  (set! (rbp - 40) r9)
                                                  (set! (rbp - 32) (rbp - 0))
                                                  (set! (rbp - 8) (rbp - 8))
                                                  (set! (rbp - 0) (rbp - 16))
                                                  (begin
                                                    (set! rbp (- rbp 72))
                                                    (return-point L.rp.48
                                                                  (begin
                                                                    (set! rdi r15)
                                                                    (set! rsi r14)
                                                                    (set! r15 L.rp.48)
                                                                    (jump L.+.31)))
                                                    (set! rbp (+ rbp 72)))
                                                  (set! r15 rax)
                                                  (begin
                                                    (set! rbp (- rbp 72))
                                                    (return-point L.rp.49
                                                                  (begin
                                                                    (set! rdi r15)
                                                                    (set! rsi (rbp - -8))
                                                                    (set! r15 L.rp.49)
                                                                    (jump L.+.31)))
                                                    (set! rbp (+ rbp 72)))
                                                  (set! r15 rax)
                                                  (begin
                                                    (set! rbp (- rbp 72))
                                                    (return-point L.rp.50
                                                                  (begin
                                                                    (set! rdi r15)
                                                                    (set! rsi (rbp - -16))
                                                                    (set! r15 L.rp.50)
                                                                    (jump L.+.31)))
                                                    (set! rbp (+ rbp 72)))
                                                  (set! r15 rax)
                                                  (begin
                                                    (set! rbp (- rbp 72))
                                                    (return-point L.rp.51
                                                                  (begin
                                                                    (set! rdi r15)
                                                                    (set! rsi (rbp - -24))
                                                                    (set! r15 L.rp.51)
                                                                    (jump L.+.31)))
                                                    (set! rbp (+ rbp 72)))
                                                  (set! r15 rax)
                                                  (begin
                                                    (set! rbp (- rbp 72))
                                                    (return-point L.rp.52
                                                                  (begin
                                                                    (set! rdi r15)
                                                                    (set! rsi (rbp - -32))
                                                                    (set! r15 L.rp.52)
                                                                    (jump L.+.31)))
                                                    (set! rbp (+ rbp 72)))
                                                  (set! r15 rax)
                                                  (begin
                                                    (set! rbp (- rbp 72))
                                                    (return-point L.rp.53
                                                                  (begin
                                                                    (set! rdi r15)
                                                                    (set! rsi (rbp - -40))
                                                                    (set! r15 L.rp.53)
                                                                    (jump L.+.31)))
                                                    (set! rbp (+ rbp 72)))
                                                  (set! r15 rax)
                                                  (begin
                                                    (set! rbp (- rbp 72))
                                                    (return-point L.rp.54
                                                                  (begin
                                                                    (set! rdi r15)
                                                                    (set! rsi (rbp - -64))
                                                                    (set! r15 L.rp.54)
                                                                    (jump L.+.31)))
                                                    (set! rbp (+ rbp 72)))
                                                  (set! r15 rax)
                                                  (set! rdi r15)
                                                  (set! rsi (rbp - 0))
                                                  (set! r15 (rbp - 24))
                                                  (jump L.+.31)))
                                              (begin
                                                (set! r15 r15)
                                                (set! rdi 8)
                                                (set! rsi 16)
                                                (set! rdx 24)
                                                (set! rcx 32)
                                                (set! r8 40)
                                                (set! r9 48)
                                                (set! (rbp - 0) 56)
                                                (set! r15 r15)
                                                (jump L.F.6))))))
