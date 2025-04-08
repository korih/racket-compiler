#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8
  rackunit
  "../passes/optimize-predicates.rkt")

(module+ test
  (check-equal? (optimize-predicates '(module
                                          (begin
                                            (set! r15 r15)
                                            (set! r13 rdi)
                                            (set! r14 rsi)
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
                                                (begin (set! rax 574) (jump r15))))))
                '(module
                     (begin
                       (set! r15 r15)
                       (set! r13 rdi)
                       (set! r14 rsi)
                       (begin
                         (begin
                           (set! r9 r14)
                           (set! r9 (bitwise-and r9 7))
                           (if (= r9 0)
                               (set! r9 14)
                               (set! r9 6)))
                         (if (!= r9 6)
                             (begin
                               (begin
                                 (set! r9 r13)
                                 (set! r9 (bitwise-and r9 7))
                                 (if (= r9 0)
                                     (set! r9 14)
                                     (set! r9 6)))
                               (if (!= r9 6)
                                   (begin
                                     (set! rax r13)
                                     (set! rax (+ rax r14))
                                     (jump r15))
                                   (begin
                                     (set! rax 574)
                                     (jump r15))))
                             (begin (set! rax 574)
                                    (jump r15)))))))
  (check-equal? (interp-nested-asm-lang-fvars-v8 (optimize-predicates '(module
                                                                           (begin
                                                                             (set! fv2 0)
                                                                             (set! fv0 0)
                                                                             (set! fv1 fv2)
                                                                             (set! fv0 (+ fv0 fv2))
                                                                             (set! fv0 (+ fv0 fv1))
                                                                             (begin (set! rax fv0) (jump r15))))))
                (interp-nested-asm-lang-fvars-v8 '(module
                                                      (begin
                                                        (set! fv2 0)
                                                        (set! fv0 0)
                                                        (set! fv1 fv2)
                                                        (set! fv0 (+ fv0 fv2))
                                                        (set! fv0 (+ fv0 fv1))
                                                        (begin (set! rax fv0) (jump r15))))))

  (check-equal? (optimize-predicates '(module (define L.f.1 (jump done))
                                        (jump L.f.1)))
                '(module (define L.f.1 (jump done)) (jump L.f.1)))
  (check-equal? (optimize-predicates '(module
                                          (define L.f.1 (begin (set! rsp 1) (set! rsp (* rsp 2)) (jump done)))
                                        (jump L.f.1)))
                '(module (define L.f.1 (begin (set! rsp 1) (set! rsp (* rsp 2)) (jump done)))
                   (jump L.f.1)))
  (check-equal? (optimize-predicates '(module
                                          (define L.f.1
                                            (begin
                                              (set! rsi rdi)
                                              (set! rdx rsi)
                                              (set! rcx rdx)
                                              (set! rbx rcx)
                                              (set! rsp r8)
                                              (set! rdi r9)
                                              (set! rsi (+ rsi rdx))
                                              (set! rsi (+ rsi rcx))
                                              (set! rsi (+ rsi rbx))
                                              (set! rsi (+ rsi rsp))
                                              (set! rsi (+ rsi rdi))
                                              (jump done)))
                                        (begin
                                          (set! r9 6)
                                          (set! r8 5)
                                          (set! rcx 4)
                                          (set! rdx 3)
                                          (set! rsi 2)
                                          (set! rdi 1)
                                          (jump L.f.1))))
                '(module
                     (define L.f.1
                       (begin
                         (set! rsi rdi)
                         (set! rdx rsi)
                         (set! rcx rdx)
                         (set! rbx rcx)
                         (set! rsp r8)
                         (set! rdi r9)
                         (set! rsi (+ rsi rdx))
                         (set! rsi (+ rsi rcx))
                         (set! rsi (+ rsi rbx))
                         (set! rsi (+ rsi rsp))
                         (set! rsi (+ rsi rdi))
                         (jump done)))
                   (begin
                     (set! r9 6)
                     (set! r8 5)
                     (set! rcx 4)
                     (set! rdx 3)
                     (set! rsi 2)
                     (set! rdi 1)
                     (jump L.f.1))))
  (check-equal? (optimize-predicates '(module
                                          (define L.f.1 (begin (set! rsp rdi) (jump done)))
                                        (define L.g.1
                                          (begin
                                            (set! rbx rdi)
                                            (set! rsp rsi)
                                            (set! rsp rdx)
                                            (set! rdi rbx)
                                            (jump L.f.1)))
                                        (if (true)
                                            (begin (set! rdx 3) (set! rsi 2) (set! rdi 1) (jump L.g.1))
                                            (begin (set! rdi 1) (jump L.f.1)))))
                '(module
                     (define L.f.1 (begin (set! rsp rdi) (jump done)))
                   (define L.g.1
                     (begin
                       (set! rbx rdi)
                       (set! rsp rsi)
                       (set! rsp rdx)
                       (set! rdi rbx)
                       (jump L.f.1)))
                   (begin (set! rdx 3) (set! rsi 2) (set! rdi 1) (jump L.g.1))))
  (check-equal? (optimize-predicates '(module (if (true) (jump done) (jump done)))) '(module (jump done)))
  (check-equal? (optimize-predicates '(module (if (false) (begin (set! rax 1) (jump done)) (jump done))))
                '(module (jump done)))
  (check-equal? (optimize-predicates '(module (if (not (true)) (jump done) (begin (set! rax 1) (jump done)))))
                '(module (begin (set! rax 1) (jump done))))
  (check-equal? (optimize-predicates '(module (if (if (true) (false) (true)) (jump done) (jump done))))
                '(module (jump done)))
  (check-equal? (optimize-predicates '(module (begin (set! rax 1) (if (> rax 0) (jump done) (jump done)))))
                '(module (begin (set! rax 1) (jump done))))
  (check-equal? (optimize-predicates '(module (begin (set! rbx 1) (if (< rbx 1) (jump done) (jump done)))))
                '(module (begin (set! rbx 1) (jump done))))
  (check-equal? (optimize-predicates `(module (begin (set! rbx rax) (if (<= rbx ,(max-int 64)) (jump done) (jump done)))))
                '(module
                     (begin
                       (set! rbx rax)
                       (if (<= rbx 9223372036854775807) (jump done) (jump done)))))
  (check-equal? (optimize-predicates '(module (begin (set! rbx rax)
                                                     (if (= rbx rax)
                                                         (begin (set! rax rcx) (jump done))
                                                         (jump done)))))
                '(module
                     (begin
                       (set! rbx rax)
                       (if (= rbx rax) (begin (set! rax rcx) (jump done)) (jump done)))))
  (check-equal? (optimize-predicates '(module (begin (set! rbx 33) (if (= rbx 44) (jump done) (jump done)))))
                '(module (begin (set! rbx 33) (jump done))))
  (check-equal? (optimize-predicates '(module (begin (set! rbx rcx) (if (= rbx 5) (jump done) (jump done)))))
                '(module (begin (set! rbx rcx) (if (= rbx 5) (jump done) (jump done)))))
  (check-equal? (optimize-predicates '(module (begin (set! rbx 33) (if (= rbx 44) (jump done) (jump done)))))
                '(module (begin (set! rbx 33) (jump done))))
  (check-equal? (optimize-predicates `(module (begin (set! rcx rbx)
                                                     (if (begin (set! rcx (+ rcx 10))
                                                                (< rcx ,(+ (min-int 64) 10)))
                                                         (jump done)
                                                         (jump done)))))
                '(module
                     (begin
                       (set! rcx rbx)
                       (begin (set! rcx (+ rcx 10))
                              (if (< rcx -9223372036854775798)
                                  (jump done)
                                  (jump done))))))
  (check-equal? (interp-nested-asm-lang-fvars-v8 (optimize-predicates '(module
                                                                           (define L.f.1 (begin (set! rbx 3) (set! rax rbx) (jump L.f.2)))
                                                                         (define L.f.2 (jump done))
                                                                         (begin
                                                                           (set! rbx 5)
                                                                           (set! rcx 4)
                                                                           (set! rax rcx)
                                                                           (jump L.f.2)))))
                (interp-nested-asm-lang-fvars-v8 '(module
                                                      (define L.f.1 (begin (set! rbx 3) (set! rax rbx) (jump L.f.2)))
                                                    (define L.f.2 (jump done))
                                                    (begin (set! rbx 5) (set! rcx 4) (set! rax rcx) (jump L.f.2)))))
  (check-equal? (optimize-predicates '(module (define L.func.1 (begin (set! rax 1) (jump L.func.2)))
                                        (define L.func.2 (begin (if (> rax 0) (jump done) (jump done))))
                                        (begin
                                          (set! rax -1)
                                          (jump L.func.2))))
                '(module
                     (define L.func.1 (begin (set! rax 1) (jump L.func.2)))
                   (define L.func.2 (begin (if (> rax 0) (jump done) (jump done))))
                   (begin (set! rax -1) (jump L.func.2))))
  (check-equal? (optimize-predicates
                 '(module (begin (set! rbx rax)
                                 (set! rbx (+ rbx 10))
                                 (if (> rbx 0)
                                     (jump done)
                                     (jump done)))))
                '(module
                     (begin
                       (set! rbx rax)
                       (set! rbx (+ rbx 10))
                       (if (> rbx 0) (jump done) (jump done)))))
  (check-equal? (optimize-predicates
                 '(module (begin (set! rax 1)
                                 (if (> rax 0)
                                     (jump done)
                                     (begin (set! rax -1)
                                            (jump done))))))
                '(module (begin (set! rax 1) (jump done))))

  (check-equal? (optimize-predicates '(module
                                          (begin (set! r8 0)
                                                 (set! r9 0)
                                                 (if
                                                  (not (if (true) (> r8 5) (< r9 6)))
                                                  (set! r12 15)
                                                  (set! r12 90))
                                                 (jump done))))
                '(module (begin (set! r8 0) (set! r9 0) (set! r12 15) (jump done))))

  (check-equal? (interp-nested-asm-lang-fvars-v8 (optimize-predicates '(module
                                                                           (begin
                                                                             (set! rsp r15) (set! rbx 5)
                                                                             (if (true)
                                                                                 (begin (set! rbx rbx) (set! rbx (+ rbx 17)) (set! rbx 12))
                                                                                 (begin (set! rbx 15)))
                                                                             (set! rax rbx)
                                                                             (jump rsp)))))
                (interp-nested-asm-lang-fvars-v8 '(module
                                                      (begin
                                                        (set! rsp r15) (set! rbx 5)
                                                        (if (true)
                                                            (begin (set! rbx rbx) (set! rbx (+ rbx 17)) (set! rbx 12))
                                                            (begin (set! rbx 15)))
                                                        (set! rax rbx)
                                                        (jump rsp)))))
  (check-equal? (optimize-predicates '(module
                                          (begin
                                            (set! r15 r15)
                                            (set! r13 rdi)
                                            (set! r14 rsi)
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
                                                (begin (set! rax 574) (jump r15))))))
                '(module
                     (begin
                       (set! r15 r15)
                       (set! r13 rdi)
                       (set! r14 rsi)
                       (begin
                         (begin
                           (set! r9 r14)
                           (set! r9 (bitwise-and r9 7))
                           (if (= r9 0)
                               (set! r9 14)
                               (set! r9 6)))
                         (if (!= r9 6)
                             (begin
                               (begin
                                 (set! r9 r13)
                                 (set! r9 (bitwise-and r9 7))
                                 (if (= r9 0)
                                     (set! r9 14)
                                     (set! r9 6)))
                               (if (!= r9 6)
                                   (begin (set! rax r13)
                                          (set! rax (+ rax r14))
                                          (jump r15))
                                   (begin (set! rax 574)
                                          (jump r15))))
                             (begin (set! rax 574)
                                    (jump r15)))))))
  (check-equal? (optimize-predicates '(module
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
                         (mset! fv2 rsp fv1)
                         (set! rbx 10)
                         (set! rbx (+ rbx 6))
                         (begin (set! rsp r12) (set! r12 (+ r12 rbx)))
                         (set! rbx 8)
                         (set! rbx (bitwise-and rbx 8))
                         (set! rax (mref rsp rbx))
                         (jump fv3)))
                   (define L.g.1 (begin (set! rsp r15) (set! rax 8) (jump rsp)))
                   (begin (set! rsp r15) (set! rdi 1) (set! rsi 2) (set! r15 rsp) (jump L.f.1))))

  (check-equal? (interp-nested-asm-lang-fvars-v8 (optimize-predicates '(module
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
                (interp-nested-asm-lang-fvars-v8 '(module
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

  (check-equal? (interp-nested-asm-lang-fvars-v8 (optimize-predicates
                                                  '(module
                                                       (define L.+.12
                                                         (begin
                                                           (set! r15 r15)
                                                           (set! r13 rdi)
                                                           (set! r14 10)
                                                           (if (begin
                                                                 (if (begin
                                                                       (begin (set! r9 r14) (set! r9 (bitwise-and r9 7)))
                                                                       (= r9 0))
                                                                     (set! r9 14)
                                                                     (set! r9 6))
                                                                 (!= r9 6))
                                                               (if (begin
                                                                     (if (begin
                                                                           (begin (set! r9 r13) (set! r9 (bitwise-and r9 7)))
                                                                           (= r9 0))
                                                                         (set! r9 14)
                                                                         (set! r9 6))
                                                                     (!= r9 6))
                                                                   (begin (set! rax r13) (set! rax (+ rax r14)) (jump r15))
                                                                   (begin (set! rax 574) (jump r15)))
                                                               (begin (set! rax 574) (jump r15)))))
                                                     (define L.void?.11
                                                       (begin
                                                         (set! r15 r15)
                                                         (set! r14 rdi)
                                                         (if (begin
                                                               (begin (set! r14 r14) (set! r14 (bitwise-and r14 255)))
                                                               (= r14 30))
                                                             (begin (set! rax 14) (jump r15))
                                                             (begin (set! rax 6) (jump r15)))))
                                                     (define L.unsafe-vector-ref.3
                                                       (begin
                                                         (set! r15 r15)
                                                         (set! r14 rdi)
                                                         (set! r13 rsi)
                                                         (if (begin
                                                               (if (begin (set! r9 (mref r14 -3)) (< r13 r9))
                                                                   (set! r9 14)
                                                                   (set! r9 6))
                                                               (!= r9 6))
                                                             (if (begin (if (>= r13 0) (set! r9 14) (set! r9 6)) (!= r9 6))
                                                                 (begin
                                                                   (set! r13 r13)
                                                                   (set! r13 (arithmetic-shift-right r13 3))
                                                                   (set! r13 r13)
                                                                   (set! r13 (* r13 8))
                                                                   (set! r13 r13)
                                                                   (set! r13 (+ r13 5))
                                                                   (set! rax (mref r14 r13))
                                                                   (jump r15))
                                                                 (begin (set! rax 2878) (jump r15)))
                                                             (begin (set! rax 2878) (jump r15)))))
                                                     (define L.vector-ref.10
                                                       (begin
                                                         (set! r15 r15)
                                                         (set! r14 rdi)
                                                         (set! r13 rsi)
                                                         (if (begin
                                                               (if (begin
                                                                     (begin (set! r9 r13) (set! r9 (bitwise-and r9 7)))
                                                                     (= r9 0))
                                                                   (set! r9 14)
                                                                   (set! r9 6))
                                                               (!= r9 6))
                                                             (if (begin
                                                                   (if (begin
                                                                         (begin (set! r9 r14) (set! r9 (bitwise-and r9 7)))
                                                                         (= r9 3))
                                                                       (set! r9 14)
                                                                       (set! r9 6))
                                                                   (!= r9 6))
                                                                 (begin
                                                                   (set! rsi r13)
                                                                   (set! rdi r14)
                                                                   (set! r15 r15)
                                                                   (jump L.unsafe-vector-ref.3))
                                                                 (begin (set! rax 2878) (jump r15)))
                                                             (begin (set! rax 2878) (jump r15)))))
                                                     (define L.unsafe-vector-set!.2
                                                       (begin
                                                         (set! r15 r15)
                                                         (set! r14 rdi)
                                                         (set! r9 rsi)
                                                         (set! r13 rdx)
                                                         (if (begin
                                                               (if (begin (set! r8 (mref r14 -3)) (< r9 r8))
                                                                   (set! r8 14)
                                                                   (set! r8 6))
                                                               (!= r8 6))
                                                             (if (begin (if (>= r9 0) (set! r8 14) (set! r8 6)) (!= r8 6))
                                                                 (begin
                                                                   (set! r9 r9)
                                                                   (set! r9 (arithmetic-shift-right r9 3))
                                                                   (set! r9 r9)
                                                                   (set! r9 (* r9 8))
                                                                   (set! r9 r9)
                                                                   (set! r9 (+ r9 5))
                                                                   (mset! r14 r9 r13)
                                                                   (set! rax 30)
                                                                   (jump r15))
                                                                 (begin (set! rax 2622) (jump r15)))
                                                             (begin (set! rax 2622) (jump r15)))))
                                                     (define L.vector-set!.9
                                                       (begin
                                                         (set! r15 r15)
                                                         (set! r14 rdi)
                                                         (set! r13 rsi)
                                                         (set! r9 rdx)
                                                         (if (begin
                                                               (if (begin
                                                                     (begin (set! r8 r13) (set! r8 (bitwise-and r8 7)))
                                                                     (= r8 0))
                                                                   (set! r8 14)
                                                                   (set! r8 6))
                                                               (!= r8 6))
                                                             (if (begin
                                                                   (if (begin
                                                                         (begin (set! r8 r14) (set! r8 (bitwise-and r8 7)))
                                                                         (= r8 3))
                                                                       (set! r8 14)
                                                                       (set! r8 6))
                                                                   (!= r8 6))
                                                                 (begin
                                                                   (set! rdx r9)
                                                                   (set! rsi r13)
                                                                   (set! rdi r14)
                                                                   (set! r15 r15)
                                                                   (jump L.unsafe-vector-set!.2))
                                                                 (begin (set! rax 2622) (jump r15)))
                                                             (begin (set! rax 2622) (jump r15)))))
                                                     (define L.vector-init-loop.7
                                                       (begin
                                                         (set! r15 r15)
                                                         (set! r14 rdi)
                                                         (set! r13 rsi)
                                                         (set! r9 rdx)
                                                         (if (begin (if (= r14 r13) (set! r8 14) (set! r8 6)) (!= r8 6))
                                                             (begin (set! rax r9) (jump r15))
                                                             (begin
                                                               (set! r8 r13)
                                                               (set! r8 (arithmetic-shift-right r8 3))
                                                               (set! r8 r8)
                                                               (set! r8 (* r8 8))
                                                               (set! r8 r8)
                                                               (set! r8 (+ r8 5))
                                                               (mset! r9 r8 0)
                                                               (set! r13 r13)
                                                               (set! r13 (+ r13 8))
                                                               (set! rdx r9)
                                                               (set! rsi r13)
                                                               (set! rdi r14)
                                                               (set! r15 r15)
                                                               (jump L.vector-init-loop.7)))))
                                                     (define L.make-init-vector.1
                                                       (begin
                                                         (set! r15 r15)
                                                         (set! r14 rdi)
                                                         (if (begin (if (>= r14 0) (set! r13 14) (set! r13 6)) (!= r13 6))
                                                             (begin
                                                               (set! r9 r14)
                                                               (set! r9 (arithmetic-shift-right r9 3))
                                                               (set! r13 1)
                                                               (set! r13 (+ r13 r9))
                                                               (set! r9 r13)
                                                               (set! r9 (* r9 8))
                                                               (begin (set! r13 r12) (set! r12 (+ r12 r9)))
                                                               (set! r13 r13)
                                                               (set! r13 (+ r13 3))
                                                               (mset! r13 -3 r14)
                                                               (set! r13 r13)
                                                               (set! rdx r13)
                                                               (set! rsi 0)
                                                               (set! rdi r14)
                                                               (set! r15 r15)
                                                               (jump L.vector-init-loop.7))
                                                             (begin (set! rax 3134) (jump r15)))))
                                                     (define L.make-vector.8
                                                       (begin
                                                         (set! r15 r15)
                                                         (set! r14 rdi)
                                                         (if (begin
                                                               (if (begin
                                                                     (begin (set! r13 r14) (set! r13 (bitwise-and r13 7)))
                                                                     (= r13 0))
                                                                   (set! r13 14)
                                                                   (set! r13 6))
                                                               (!= r13 6))
                                                             (begin (set! rdi r14) (set! r15 r15) (jump L.make-init-vector.1))
                                                             (begin (set! rax 2110) (jump r15)))))
                                                     (define L.v.4
                                                       (begin (set! r15 r15) (set! rdi 24) (set! r15 r15) (jump L.make-vector.8)))
                                                     (define L.set-first.5
                                                       (begin
                                                         (set! r15 r15)
                                                         (set! r14 rdi)
                                                         (set! rdx 336)
                                                         (set! rsi 0)
                                                         (set! rdi r14)
                                                         (set! r15 r15)
                                                         (jump L.vector-set!.9)))
                                                     (define L.get-first.6
                                                       (begin
                                                         (set! r15 r15)
                                                         (set! r14 rdi)
                                                         (set! rsi 0)
                                                         (set! rdi r14)
                                                         (set! r15 r15)
                                                         (jump L.vector-ref.10)))
                                                     (begin
                                                       (set! fv0 r15)
                                                       (begin
                                                         (set! rbp (- rbp 24))
                                                         (return-point L.rp.13 (begin (set! r15 L.rp.13) (jump L.v.4)))
                                                         (set! rbp (+ rbp 24)))
                                                       (set! fv2 rax)
                                                       (if (begin
                                                             (begin
                                                               (begin
                                                                 (set! rbp (- rbp 24))
                                                                 (return-point L.rp.14
                                                                               (begin (set! rdi fv2) (set! r15 L.rp.14) (jump L.set-first.5)))
                                                                 (set! rbp (+ rbp 24)))
                                                               (set! r15 rax)
                                                               (begin
                                                                 (set! rbp (- rbp 24))
                                                                 (return-point L.rp.15
                                                                               (begin (set! rdi r15) (set! r15 L.rp.15) (jump L.void?.11)))
                                                                 (set! rbp (+ rbp 24)))
                                                               (set! r15 rax))
                                                             (!= r15 6))
                                                           (set! fv1 0)
                                                           (set! fv1 318))
                                                       (begin
                                                         (set! rbp (- rbp 24))
                                                         (return-point L.rp.16
                                                                       (begin (set! rdi fv2) (set! r15 L.rp.16) (jump L.get-first.6)))
                                                         (set! rbp (+ rbp 24)))
                                                       (set! r15 rax)
                                                       (set! rsi r15)
                                                       (set! rdi fv1)
                                                       (set! r15 fv0)
                                                       (jump L.+.12)))))
                (interp-nested-asm-lang-fvars-v8 '(module
                                                      (define L.+.12
                                                        (begin
                                                          (set! r15 r15)
                                                          (set! r13 rdi)
                                                          (set! r14 10)
                                                          (if (begin
                                                                (if (begin
                                                                      (begin (set! r9 r14) (set! r9 (bitwise-and r9 7)))
                                                                      (= r9 0))
                                                                    (set! r9 14)
                                                                    (set! r9 6))
                                                                (!= r9 6))
                                                              (if (begin
                                                                    (if (begin
                                                                          (begin (set! r9 r13) (set! r9 (bitwise-and r9 7)))
                                                                          (= r9 0))
                                                                        (set! r9 14)
                                                                        (set! r9 6))
                                                                    (!= r9 6))
                                                                  (begin (set! rax r13) (set! rax (+ rax r14)) (jump r15))
                                                                  (begin (set! rax 574) (jump r15)))
                                                              (begin (set! rax 574) (jump r15)))))
                                                    (define L.void?.11
                                                      (begin
                                                        (set! r15 r15)
                                                        (set! r14 rdi)
                                                        (if (begin
                                                              (begin (set! r14 r14) (set! r14 (bitwise-and r14 255)))
                                                              (= r14 30))
                                                            (begin (set! rax 14) (jump r15))
                                                            (begin (set! rax 6) (jump r15)))))
                                                    (define L.unsafe-vector-ref.3
                                                      (begin
                                                        (set! r15 r15)
                                                        (set! r14 rdi)
                                                        (set! r13 rsi)
                                                        (if (begin
                                                              (if (begin (set! r9 (mref r14 -3)) (< r13 r9))
                                                                  (set! r9 14)
                                                                  (set! r9 6))
                                                              (!= r9 6))
                                                            (if (begin (if (>= r13 0) (set! r9 14) (set! r9 6)) (!= r9 6))
                                                                (begin
                                                                  (set! r13 r13)
                                                                  (set! r13 (arithmetic-shift-right r13 3))
                                                                  (set! r13 r13)
                                                                  (set! r13 (* r13 8))
                                                                  (set! r13 r13)
                                                                  (set! r13 (+ r13 5))
                                                                  (set! rax (mref r14 r13))
                                                                  (jump r15))
                                                                (begin (set! rax 2878) (jump r15)))
                                                            (begin (set! rax 2878) (jump r15)))))
                                                    (define L.vector-ref.10
                                                      (begin
                                                        (set! r15 r15)
                                                        (set! r14 rdi)
                                                        (set! r13 rsi)
                                                        (if (begin
                                                              (if (begin
                                                                    (begin (set! r9 r13) (set! r9 (bitwise-and r9 7)))
                                                                    (= r9 0))
                                                                  (set! r9 14)
                                                                  (set! r9 6))
                                                              (!= r9 6))
                                                            (if (begin
                                                                  (if (begin
                                                                        (begin (set! r9 r14) (set! r9 (bitwise-and r9 7)))
                                                                        (= r9 3))
                                                                      (set! r9 14)
                                                                      (set! r9 6))
                                                                  (!= r9 6))
                                                                (begin
                                                                  (set! rsi r13)
                                                                  (set! rdi r14)
                                                                  (set! r15 r15)
                                                                  (jump L.unsafe-vector-ref.3))
                                                                (begin (set! rax 2878) (jump r15)))
                                                            (begin (set! rax 2878) (jump r15)))))
                                                    (define L.unsafe-vector-set!.2
                                                      (begin
                                                        (set! r15 r15)
                                                        (set! r14 rdi)
                                                        (set! r9 rsi)
                                                        (set! r13 rdx)
                                                        (if (begin
                                                              (if (begin (set! r8 (mref r14 -3)) (< r9 r8))
                                                                  (set! r8 14)
                                                                  (set! r8 6))
                                                              (!= r8 6))
                                                            (if (begin (if (>= r9 0) (set! r8 14) (set! r8 6)) (!= r8 6))
                                                                (begin
                                                                  (set! r9 r9)
                                                                  (set! r9 (arithmetic-shift-right r9 3))
                                                                  (set! r9 r9)
                                                                  (set! r9 (* r9 8))
                                                                  (set! r9 r9)
                                                                  (set! r9 (+ r9 5))
                                                                  (mset! r14 r9 r13)
                                                                  (set! rax 30)
                                                                  (jump r15))
                                                                (begin (set! rax 2622) (jump r15)))
                                                            (begin (set! rax 2622) (jump r15)))))
                                                    (define L.vector-set!.9
                                                      (begin
                                                        (set! r15 r15)
                                                        (set! r14 rdi)
                                                        (set! r13 rsi)
                                                        (set! r9 rdx)
                                                        (if (begin
                                                              (if (begin
                                                                    (begin (set! r8 r13) (set! r8 (bitwise-and r8 7)))
                                                                    (= r8 0))
                                                                  (set! r8 14)
                                                                  (set! r8 6))
                                                              (!= r8 6))
                                                            (if (begin
                                                                  (if (begin
                                                                        (begin (set! r8 r14) (set! r8 (bitwise-and r8 7)))
                                                                        (= r8 3))
                                                                      (set! r8 14)
                                                                      (set! r8 6))
                                                                  (!= r8 6))
                                                                (begin
                                                                  (set! rdx r9)
                                                                  (set! rsi r13)
                                                                  (set! rdi r14)
                                                                  (set! r15 r15)
                                                                  (jump L.unsafe-vector-set!.2))
                                                                (begin (set! rax 2622) (jump r15)))
                                                            (begin (set! rax 2622) (jump r15)))))
                                                    (define L.vector-init-loop.7
                                                      (begin
                                                        (set! r15 r15)
                                                        (set! r14 rdi)
                                                        (set! r13 rsi)
                                                        (set! r9 rdx)
                                                        (if (begin (if (= r14 r13) (set! r8 14) (set! r8 6)) (!= r8 6))
                                                            (begin (set! rax r9) (jump r15))
                                                            (begin
                                                              (set! r8 r13)
                                                              (set! r8 (arithmetic-shift-right r8 3))
                                                              (set! r8 r8)
                                                              (set! r8 (* r8 8))
                                                              (set! r8 r8)
                                                              (set! r8 (+ r8 5))
                                                              (mset! r9 r8 0)
                                                              (set! r13 r13)
                                                              (set! r13 (+ r13 8))
                                                              (set! rdx r9)
                                                              (set! rsi r13)
                                                              (set! rdi r14)
                                                              (set! r15 r15)
                                                              (jump L.vector-init-loop.7)))))
                                                    (define L.make-init-vector.1
                                                      (begin
                                                        (set! r15 r15)
                                                        (set! r14 rdi)
                                                        (if (begin (if (>= r14 0) (set! r13 14) (set! r13 6)) (!= r13 6))
                                                            (begin
                                                              (set! r9 r14)
                                                              (set! r9 (arithmetic-shift-right r9 3))
                                                              (set! r13 1)
                                                              (set! r13 (+ r13 r9))
                                                              (set! r9 r13)
                                                              (set! r9 (* r9 8))
                                                              (begin (set! r13 r12) (set! r12 (+ r12 r9)))
                                                              (set! r13 r13)
                                                              (set! r13 (+ r13 3))
                                                              (mset! r13 -3 r14)
                                                              (set! r13 r13)
                                                              (set! rdx r13)
                                                              (set! rsi 0)
                                                              (set! rdi r14)
                                                              (set! r15 r15)
                                                              (jump L.vector-init-loop.7))
                                                            (begin (set! rax 3134) (jump r15)))))
                                                    (define L.make-vector.8
                                                      (begin
                                                        (set! r15 r15)
                                                        (set! r14 rdi)
                                                        (if (begin
                                                              (if (begin
                                                                    (begin (set! r13 r14) (set! r13 (bitwise-and r13 7)))
                                                                    (= r13 0))
                                                                  (set! r13 14)
                                                                  (set! r13 6))
                                                              (!= r13 6))
                                                            (begin (set! rdi r14) (set! r15 r15) (jump L.make-init-vector.1))
                                                            (begin (set! rax 2110) (jump r15)))))
                                                    (define L.v.4
                                                      (begin (set! r15 r15) (set! rdi 24) (set! r15 r15) (jump L.make-vector.8)))
                                                    (define L.set-first.5
                                                      (begin
                                                        (set! r15 r15)
                                                        (set! r14 rdi)
                                                        (set! rdx 336)
                                                        (set! rsi 0)
                                                        (set! rdi r14)
                                                        (set! r15 r15)
                                                        (jump L.vector-set!.9)))
                                                    (define L.get-first.6
                                                      (begin
                                                        (set! r15 r15)
                                                        (set! r14 rdi)
                                                        (set! rsi 0)
                                                        (set! rdi r14)
                                                        (set! r15 r15)
                                                        (jump L.vector-ref.10)))
                                                    (begin
                                                      (set! fv0 r15)
                                                      (begin
                                                        (set! rbp (- rbp 24))
                                                        (return-point L.rp.13 (begin (set! r15 L.rp.13) (jump L.v.4)))
                                                        (set! rbp (+ rbp 24)))
                                                      (set! fv2 rax)
                                                      (if (begin
                                                            (begin
                                                              (begin
                                                                (set! rbp (- rbp 24))
                                                                (return-point L.rp.14
                                                                              (begin (set! rdi fv2) (set! r15 L.rp.14) (jump L.set-first.5)))
                                                                (set! rbp (+ rbp 24)))
                                                              (set! r15 rax)
                                                              (begin
                                                                (set! rbp (- rbp 24))
                                                                (return-point L.rp.15
                                                                              (begin (set! rdi r15) (set! r15 L.rp.15) (jump L.void?.11)))
                                                                (set! rbp (+ rbp 24)))
                                                              (set! r15 rax))
                                                            (!= r15 6))
                                                          (set! fv1 0)
                                                          (set! fv1 318))
                                                      (begin
                                                        (set! rbp (- rbp 24))
                                                        (return-point L.rp.16
                                                                      (begin (set! rdi fv2) (set! r15 L.rp.16) (jump L.get-first.6)))
                                                        (set! rbp (+ rbp 24)))
                                                      (set! r15 rax)
                                                      (set! rsi r15)
                                                      (set! rdi fv1)
                                                      (set! r15 fv0)
                                                      (jump L.+.12))))))
