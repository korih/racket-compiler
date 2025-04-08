#lang racket

(require
  rackunit
  "../passes/expose-basic-blocks.rkt"
  cpsc411/langs/v8)

(module+ test
  (check-equal? (expose-basic-blocks '(module (begin (jump rax))))
                '(module (define L.tmp.1 (jump rax))))
  (check-equal? (expose-basic-blocks '(module (begin (begin (set! rax 1)) (jump rax))))
                '(module (define L.tmp.2 (begin (set! rax 1) (jump rax)))))
  (check-equal? (expose-basic-blocks '(module (begin (begin (begin (begin (set! rax 1)))) (jump rax))))
                '(module (define L.tmp.3 (begin (set! rax 1) (jump rax)))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (if (true) (set! rax 5) (set! rax 6))
                                                (jump rax))))
                '(module
                     (define L.tmp.4 (if (true) (jump L.tmp.6) (jump L.tmp.7)))
                   (define L.tmp.5 (jump rax))
                   (define L.tmp.7 (begin (set! rax 6) (jump L.tmp.5)))
                   (define L.tmp.6 (begin (set! rax 5) (jump L.tmp.5)))))

  (check-equal? (expose-basic-blocks '(module (jump rax)))
                '(module (define L.tmp.8 (jump rax))))
  (check-equal? (expose-basic-blocks '(module (if (true)
                                                  (begin (set! rbx 5) (jump rbx))
                                                  (begin (set! rbx 6) (jump rbx)))))
                '(module
                     (define L.tmp.9 (if (true) (jump L.tmp.10) (jump L.tmp.11)))
                   (define L.tmp.11 (begin (set! rbx 6) (jump rbx)))
                   (define L.tmp.10 (begin (set! rbx 5) (jump rbx)))))
  (check-equal? (expose-basic-blocks '(module (if (not (true))
                                                  (begin (set! rbx 5) (jump rbx))
                                                  (begin (set! rbx 6) (jump rbx)))))
                '(module
                     (define L.tmp.12 (if (true) (jump L.tmp.14) (jump L.tmp.13)))
                   (define L.tmp.14 (begin (set! rbx 6) (jump rbx)))
                   (define L.tmp.13 (begin (set! rbx 5) (jump rbx)))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (set! rax 5)
                                                (set! rax (+ rax rax))
                                                (jump rax))))
                '(module
                     (define L.tmp.15 (begin (set! rax 5) (set! rax (+ rax rax)) (jump rax)))))
  (check-equal? (expose-basic-blocks '(module (if (if (true) (true) (false))
                                                  (begin (set! rcx 5) (jump rcx))
                                                  (begin (set! rcx 6) (jump rcx)))))
                '(module
                     (define L.tmp.16 (if (true) (jump L.tmp.19) (jump L.tmp.20)))
                   (define L.tmp.19 (if (true) (jump L.tmp.17) (jump L.tmp.18)))
                   (define L.tmp.20 (if (false) (jump L.tmp.17) (jump L.tmp.18)))
                   (define L.tmp.18 (begin (set! rcx 6) (jump rcx)))
                   (define L.tmp.17 (begin (set! rcx 5) (jump rcx)))))
  (check-equal? (expose-basic-blocks '(module (if (begin
                                                    (set! rax 5)
                                                    (if (true) (true) (false)))
                                                  (begin (set! r13 5) (jump r13))
                                                  (jump rax))))
                '(module
                     (define L.tmp.21
                       (begin (set! rax 5) (if (true) (jump L.tmp.24) (jump L.tmp.25))))
                   (define L.tmp.24 (if (true) (jump L.tmp.22) (jump L.tmp.23)))
                   (define L.tmp.25 (if (false) (jump L.tmp.22) (jump L.tmp.23)))
                   (define L.tmp.23 (jump rax))
                   (define L.tmp.22 (begin (set! r13 5) (jump r13)))))
  (check-equal? (expose-basic-blocks '(module (if (begin
                                                    (set! rax 5)
                                                    (if (true) (true) (false)))
                                                  (begin
                                                    (set! rax (+ rax rax))
                                                    (jump rax))
                                                  (begin
                                                    (set! rax (* rax rax))
                                                    (jump rax)))))
                '(module
                     (define L.tmp.26
                       (begin (set! rax 5) (if (true) (jump L.tmp.29) (jump L.tmp.30))))
                   (define L.tmp.29 (if (true) (jump L.tmp.27) (jump L.tmp.28)))
                   (define L.tmp.30 (if (false) (jump L.tmp.27) (jump L.tmp.28)))
                   (define L.tmp.28 (begin (set! rax (* rax rax)) (jump rax)))
                   (define L.tmp.27 (begin (set! rax (+ rax rax)) (jump rax)))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (set! rax 5)
                                                (set! rax (+ rax rax))
                                                (if (true) (jump rax) (begin (set! r12 0) (jump r12))))))
                '(module
                     (define L.tmp.31
                       (begin
                         (set! rax 5)
                         (set! rax (+ rax rax))
                         (if (true) (jump L.tmp.32) (jump L.tmp.33))))
                   (define L.tmp.33 (begin (set! r12 0) (jump r12)))
                   (define L.tmp.32 (jump rax))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (set! rax 5)
                                                (set! rax (+ rax rax))
                                                (if (true)
                                                    (begin
                                                      (set! rax (+ rax 1))
                                                      (jump rax))
                                                    (begin
                                                      (set! rax (+ rax 2))
                                                      (jump rax))))))
                '(module
                     (define L.tmp.34
                       (begin
                         (set! rax 5)
                         (set! rax (+ rax rax))
                         (if (true) (jump L.tmp.35) (jump L.tmp.36))))
                   (define L.tmp.36 (begin (set! rax (+ rax 2)) (jump rax)))
                   (define L.tmp.35 (begin (set! rax (+ rax 1)) (jump rax)))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (set! rax 5)
                                                (set! rax (+ rax rax))
                                                (if (true)
                                                    (begin
                                                      (set! rax (+ rax 1))
                                                      (if (false) (begin (set! rax 0) (jump rax)) (jump rax)))
                                                    (begin
                                                      (set! rax (+ rax 2))
                                                      (if (true) (begin (set! rbx 0) (jump rbx)) (jump rax)))))))
                '(module
                     (define L.tmp.37
                       (begin
                         (set! rax 5)
                         (set! rax (+ rax rax))
                         (if (true) (jump L.tmp.38) (jump L.tmp.39))))
                   (define L.tmp.39
                     (begin (set! rax (+ rax 2)) (if (true) (jump L.tmp.42) (jump L.tmp.43))))
                   (define L.tmp.43 (jump rax))
                   (define L.tmp.42 (begin (set! rbx 0) (jump rbx)))
                   (define L.tmp.38
                     (begin (set! rax (+ rax 1)) (if (false) (jump L.tmp.40) (jump L.tmp.41))))
                   (define L.tmp.41 (jump rax))
                   (define L.tmp.40 (begin (set! rax 0) (jump rax)))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (if (true) (begin (set! rbx 1) (jump rbx)) (begin (set! rbx 2) (jump rbx))))))
                '(module
                     (define L.tmp.44 (if (true) (jump L.tmp.45) (jump L.tmp.46)))
                   (define L.tmp.46 (begin (set! rbx 2) (jump rbx)))
                   (define L.tmp.45 (begin (set! rbx 1) (jump rbx)))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (if (not (true))
                                                    (begin (set! r12 1) (jump r12))
                                                    (begin (set! r12 2) (jump r12))))))
                '(module
                     (define L.tmp.47 (if (true) (jump L.tmp.49) (jump L.tmp.48) ))
                   (define L.tmp.49 (begin (set! r12 2) (jump r12)))
                   (define L.tmp.48 (begin (set! r12 1) (jump r12)))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (if (if (not (true)) (true) (false))
                                                    (begin (set! r8 1) (jump r8))
                                                    (begin (set! r8 2) (jump r8))))))
                '(module
                     (define L.tmp.50 (if (true) (jump L.tmp.54) (jump L.tmp.53) ))
                   (define L.tmp.53 (if (true) (jump L.tmp.51) (jump L.tmp.52)))
                   (define L.tmp.54 (if (false) (jump L.tmp.51) (jump L.tmp.52)))
                   (define L.tmp.52 (begin (set! r8 2) (jump r8)))
                   (define L.tmp.51 (begin (set! r8 1) (jump r8)))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (if (if (true) (true) (false))
                                                    (begin (set! r9 1) (jump r9))
                                                    (begin (set! r9 2) (jump r9))))))
                '(module
                     (define L.tmp.55 (if (true) (jump L.tmp.58) (jump L.tmp.59)))
                   (define L.tmp.58 (if (true) (jump L.tmp.56) (jump L.tmp.57)))
                   (define L.tmp.59 (if (false) (jump L.tmp.56) (jump L.tmp.57)))
                   (define L.tmp.57 (begin (set! r9 2) (jump r9)))
                   (define L.tmp.56 (begin (set! r9 1) (jump r9)))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (if (true)
                                                    (set! rax 5)
                                                    (set! rax 2))
                                                (set! rax (+ rax rax))
                                                (set! rax (* rax 2))
                                                (if (false)
                                                    (set! rax (+ rax 1))
                                                    (set! rax (+ rax 2)))
                                                (jump rax))))
                '(module
                     (define L.tmp.60 (if (true) (jump L.tmp.65) (jump L.tmp.66)))
                   (define L.tmp.64
                     (begin
                       (set! rax (+ rax rax))
                       (set! rax (* rax 2))
                       (if (false) (jump L.tmp.62) (jump L.tmp.63))))
                   (define L.tmp.66 (begin (set! rax 2) (jump L.tmp.64)))
                   (define L.tmp.65 (begin (set! rax 5) (jump L.tmp.64)))
                   (define L.tmp.61 (jump rax))
                   (define L.tmp.63 (begin (set! rax (+ rax 2)) (jump L.tmp.61)))
                   (define L.tmp.62 (begin (set! rax (+ rax 1)) (jump L.tmp.61)))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (set! rax 10)
                                                (set! rax (+ rax rax))
                                                (if (> rax 10) (set! rax 2) (set! rax 5))
                                                (if (true) (jump rax) (begin (set! rcx 3) (jump rcx))))))
                '(module
                     (define L.tmp.67
                       (begin
                         (set! rax 10)
                         (set! rax (+ rax rax))
                         (if (> rax 10) (jump L.tmp.71) (jump L.tmp.72))))
                   (define L.tmp.70 (if (true) (jump L.tmp.68) (jump L.tmp.69)))
                   (define L.tmp.72 (begin (set! rax 5) (jump L.tmp.70)))
                   (define L.tmp.71 (begin (set! rax 2) (jump L.tmp.70)))
                   (define L.tmp.69 (begin (set! rcx 3) (jump rcx)))
                   (define L.tmp.68 (jump rax))))
  (check-equal? (expose-basic-blocks '(module (if (false)
                                                  (begin (set! r13 1) (jump r13))
                                                  (begin (set! r13 2) (jump r13)))))
                '(module
                     (define L.tmp.73 (if (false) (jump L.tmp.74) (jump L.tmp.75)))
                   (define L.tmp.75 (begin (set! r13 2) (jump r13)))
                   (define L.tmp.74 (begin (set! r13 1) (jump r13)))))
  (check-equal? (expose-basic-blocks '(module (if (not (false))
                                                  (begin (set! rax 1) (jump rax))
                                                  (begin (set! rax 2) (jump rax)))))
                '(module
                     (define L.tmp.76 (if (false) (jump L.tmp.78) (jump L.tmp.77) ))
                   (define L.tmp.78 (begin (set! rax 2) (jump rax)))
                   (define L.tmp.77 (begin (set! rax 1) (jump rax)))))
  (check-equal? (expose-basic-blocks '(module (if (not (if (true)
                                                           (> r8 5)
                                                           (< r9 5)))
                                                  (begin (set! r8 1) (jump r8))
                                                  (begin (set! r8 2) (jump r8)))))
                '(module
                     (define L.tmp.79 (if (true) (jump L.tmp.82) (jump L.tmp.83)))
                   (define L.tmp.82 (if (> r8 5) (jump L.tmp.81) (jump L.tmp.80)))
                   (define L.tmp.83 (if (< r9 5) (jump L.tmp.81) (jump L.tmp.80)))
                   (define L.tmp.81 (begin (set! r8 2) (jump r8)))
                   (define L.tmp.80 (begin (set! r8 1) (jump r8)))))
  (check-equal? (expose-basic-blocks '(module (define L.fun.1 (begin (set! r8 1) (jump r8)))
                                        (begin
                                          (set! rax 10)
                                          (set! rax (+ rax rax))
                                          (if (> rax 10) (set! rax 2) (set! rax 5))
                                          (if (true) (jump rax) (begin (set! r8 3) (jump r8))))))
                '(module
                     (define L.tmp.84
                       (begin
                         (set! rax 10)
                         (set! rax (+ rax rax))
                         (if (> rax 10) (jump L.tmp.88) (jump L.tmp.89))))
                   (define L.tmp.87 (if (true) (jump L.tmp.85) (jump L.tmp.86)))
                   (define L.tmp.89 (begin (set! rax 5) (jump L.tmp.87)))
                   (define L.tmp.88 (begin (set! rax 2) (jump L.tmp.87)))
                   (define L.tmp.86 (begin (set! r8 3) (jump r8)))
                   (define L.tmp.85 (jump rax))
                   (define L.fun.1 (begin (set! r8 1) (jump r8)))))

  (check-equal? (expose-basic-blocks '(module
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
                                                      (return-point L.rp.1
                                                                    (begin
                                                                      (set! (rbp - 8) r14)
                                                                      (set! (rbp - 0) r15)
                                                                      (set! r15 L.rp.1)
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
                '(module
                     (define L.tmp.92
                       (begin
                         (set! r15 r15)
                         (set! (rbp - 8) 2)
                         (set! (rbp - 0) 1)
                         (set! r15 r15)
                         (jump L.swap.1)))
                   (define L.swap.1
                     (begin
                       (set! (rbp - 16) r15)
                       (set! r14 (rbp - 0))
                       (set! r15 (rbp - 8))
                       (if (< r15 r14) (jump L.tmp.90) (jump L.tmp.91))))
                   (define L.tmp.91
                     (begin
                       (set! rbp (- rbp 24))
                       (set! (rbp - 8) r14)
                       (set! (rbp - 0) r15)
                       (set! r15 L.rp.1)
                       (jump L.swap.1)))
                   (define L.rp.1
                     (begin
                       (set! rbp (+ rbp 24))
                       (set! r15 rax)
                       (set! rax r15)
                       (jump (rbp - 16))))
                   (define L.tmp.90 (begin (set! rax r14) (jump (rbp - 16))))))

  (check-equal? (expose-basic-blocks '(module
                                          (define L.swap.1
                                            (if (< r15 r14)
                                                (begin (set! rax r14) (jump (rbp - 16)))
                                                (begin
                                                  (begin
                                                    (set! rbp (- rbp 24))
                                                    (return-point L.rp.1
                                                                  (begin
                                                                    (set! (rbp - 8) r14)
                                                                    (set! (rbp - 0) r15)
                                                                    (set! r15 L.rp.1)
                                                                    (jump L.swap.1)))
                                                    (set! rbp (+ rbp 24)))
                                                  (set! r15 rax)
                                                  (set! rax r15)
                                                  (jump (rbp - 16)))))
                                        (begin
                                          (set! r15 r15)
                                          (set! (rbp - 8) 2)
                                          (set! (rbp - 0) 1)
                                          (set! r15 r15)
                                          (jump L.swap.1))))
                '(module
                     (define L.tmp.95
                       (begin
                         (set! r15 r15)
                         (set! (rbp - 8) 2)
                         (set! (rbp - 0) 1)
                         (set! r15 r15)
                         (jump L.swap.1)))
                   (define L.swap.1 (if (< r15 r14) (jump L.tmp.93) (jump L.tmp.94)))
                   (define L.tmp.94
                     (begin
                       (set! rbp (- rbp 24))
                       (set! (rbp - 8) r14)
                       (set! (rbp - 0) r15)
                       (set! r15 L.rp.1)
                       (jump L.swap.1)))
                   (define L.rp.1
                     (begin
                       (set! rbp (+ rbp 24))
                       (set! r15 rax)
                       (set! rax r15)
                       (jump (rbp - 16))))
                   (define L.tmp.93 (begin (set! rax r14) (jump (rbp - 16))))))

  (check-equal? (expose-basic-blocks '(module
                                          (if (begin (set! rax 5)
                                                     (return-point L.ret.1 (begin (set! rax 6)
                                                                                  (jump L.func.1)))
                                                     (set! rax 7)
                                                     (true))
                                              (jump (rbp - 8))
                                              (jump (rbp - 16)))))
                '(module
                     (define L.tmp.96 (begin (set! rax 5) (set! rax 6) (jump L.func.1)))
                   (define L.ret.1
                     (begin (set! rax 7) (if (true) (jump L.tmp.97) (jump L.tmp.98))))
                   (define L.tmp.98 (jump (rbp - 16)))
                   (define L.tmp.97 (jump (rbp - 8)))))
  (check-equal? (expose-basic-blocks '(module
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
                     (define L.tmp.99
                       (begin
                         (set! rbx r15)
                         (set! rcx 10)
                         (set! rsp 100)
                         (if (!= rcx rsp) (jump L.tmp.101) (jump L.tmp.100))))
                   (define L.tmp.101 (begin (set! rdi 1000) (set! r15 rbx) (jump L.f.2)))
                   (define L.tmp.100 (begin (set! rdi rcx) (set! r15 rbx) (jump L.f.1)))
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
                       (jump rsp)))))
  (check-equal? (expose-basic-blocks '(module
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
                '(module
                     (define L.tmp.105
                       (begin
                         (set! rsp r15)
                         (set! rdi 1)
                         (set! rsi 2)
                         (set! r15 rsp)
                         (jump L.f.1)))
                   (define L.g.1 (begin (set! rsp r15) (set! rax 8) (jump rsp)))
                   (define L.f.1
                     (begin
                       (set! (rbp - 24) r15)
                       (set! (rbp - 8) rdi)
                       (set! (rbp - 0) rsi)
                       (set! rsp 10)
                       (set! rsp (+ rsp 6))
                       (set! (rbp - 16) r12)
                       (set! r12 (+ r12 rsp))
                       (set! rbp (- rbp 32))
                       (set! r15 L.rp.21)
                       (jump L.g.1)))
                   (define L.rp.21
                     (begin
                       (set! rbp (+ rbp 32))
                       (set! rsp rax)
                       (if (true) (jump L.tmp.103) (jump L.tmp.104))))
                   (define L.tmp.102
                     (begin
                       (set! rbx 10)
                       (set! rbx (+ rbx 6))
                       (set! rsp r12)
                       (set! r12 (+ r12 rbx))
                       (set! rbx 8)
                       (set! rbx (bitwise-and rbx 8))
                       (set! rax (mref rsp rbx))
                       (jump (rbp - 24))))
                   (define L.tmp.104 (begin (mset! (rbp - 16) rsp (rbp - 0)) (jump L.tmp.102)))
                   (define L.tmp.103
                     (begin (mset! (rbp - 16) rsp (rbp - 8)) (jump L.tmp.102)))))

  (check-equal? (interp-block-pred-lang-v8 (expose-basic-blocks '(module
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
                                                                           (begin (set! rax 574) (jump r15)))))))
                (interp-block-pred-lang-v8 '(module
                                                (define L.__main.14
                                                  (begin
                                                    (set! r15 r15)
                                                    (set! r13 rdi)
                                                    (set! r14 rsi)
                                                    (set! r9 10)
                                                    (set! r9 (bitwise-and r9 7))
                                                    (if (= r9 0) (jump L.tmp.11) (jump L.tmp.12))))
                                              (define L.tmp.11 (begin (set! r9 14) (jump L.tmp.13)))
                                              (define L.tmp.12 (begin (set! r9 6) (jump L.tmp.13)))
                                              (define L.tmp.13 (if (!= r9 6) (jump L.__nested.4) (jump L.__nested.5)))
                                              (define L.tmp.8 (begin (set! r9 14) (jump L.tmp.10)))
                                              (define L.tmp.9 (begin (set! r9 6) (jump L.tmp.10)))
                                              (define L.tmp.10 (if (!= r9 6) (jump L.__nested.6) (jump L.__nested.7)))
                                              (define L.__nested.6
                                                (begin (set! rax r13) (set! rax (+ rax r14)) (jump r15)))
                                              (define L.__nested.7 (begin (set! rax 574) (jump r15)))
                                              (define L.__nested.4
                                                (begin
                                                  (set! r9 r13)
                                                  (set! r9 (bitwise-and r9 7))
                                                  (if (= r9 0) (jump L.tmp.8) (jump L.tmp.9))))
                                              (define L.__nested.5 (begin (set! rax 574) (jump r15)))))))
