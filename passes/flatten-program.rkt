#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8
  rackunit)

(provide flatten-program)

;; block-asm-lang-v8 -> para-asm-lang-v8
;; interp. flatten begin statements in the program
(define/contract (flatten-program p)
  (-> block-asm-lang-v8? para-asm-lang-v8?)

  ;; block-asm-lang-v8.b -> (list para-asm-lang-v8.s)
  ;; convert b expressions to flattened s expressions
  (define (flatten-program/b b)
    (match b
      [`(define ,label ,tail)
       (define compiled-s (flatten-program/tail tail))
       (cons `(with-label ,label ,(first compiled-s)) (rest compiled-s))]))

  ;; block-asm-lang-v8.tail -> (list para-asm-lang-v8.s)
  ;; interp. flattens the tail expression into a list of s statements
  (define (flatten-program/tail tail)
    (match tail
      [`(halt ,opand) (list `(halt, opand))]
      [`(jump ,trg) (list `(jump ,trg))]
      [`(begin ,fx ... ,tail)
       ;; Note that block-asm-lang-v8.effect expressions are already para-asm-lang-v8.s expressions
       (append fx (flatten-program/tail tail))]
      [`(if (,relop ,loc ,opand) (jump ,trg1) (jump ,trg2))
       (list `(compare ,loc ,opand) `(jump-if ,relop ,trg1) `(jump ,trg2))]))

  (match p
    [`(module ,bs ...)
     (define compiled-s (for/fold ([s empty])
                                  ([b bs])
                          (append s (flatten-program/b b))))
     `(begin ,@compiled-s)]))

(module+ test
  (let ([x (fresh-label)])
    (check-equal? (flatten-program `(module (define ,x (jump ,x)))) `(begin (with-label ,x (jump ,x)))))
  (let ([x (fresh-label)])
    (check-equal? (flatten-program `(module (define ,x (begin (set! rbx 1) (jump ,x)))))
                  `(begin (with-label ,x (set! rbx 1)) (jump ,x))))
  (let ([x (fresh-label)])
    (check-equal? (flatten-program `(module (define ,x (begin (set! rax 0) (begin (set! rbx 1) (jump ,x))))))
                  `(begin (with-label ,x (set! rax 0)) (set! rbx 1) (jump ,x))))
  (let ([x (fresh-label)])
    (check-equal? (flatten-program `(module (define ,x (begin (begin (set! rax 0) (jump ,x))))))
                  `(begin (with-label ,x (set! rax 0)) (jump ,x))))
  (let ([x (fresh-label)]
        [y (fresh-label)])
    (check-equal? (flatten-program `(module (define ,x (jump ,y)) (define ,y (jump ,x))))
                  `(begin (with-label ,x (jump ,y)) (with-label ,y (jump ,x)))))
  (let ([x (fresh-label)]
        [y (fresh-label)]
        [z (fresh-label)])
    (check-equal? (flatten-program `(module (define ,x (if (< r9 0) (jump ,y) (jump ,z)))
                                      (define ,y (jump r8))
                                      (define ,z (begin (set! r9 (+ r9 -1)) (jump ,x)))))
                  `(begin (with-label ,x (compare r9 0))
                          (jump-if < ,y)
                          (jump ,z)
                          (with-label ,y (jump r8))
                          (with-label ,z (set! r9 (+ r9 -1)))
                          (jump ,x))))
  (let ([x (fresh-label)]
        [y (fresh-label)]
        [z (fresh-label)])
    (check-equal? (flatten-program `(module (define ,x (begin (set! r9 0) (jump ,y)))
                                      (define ,y (begin (set! r13 0) (jump ,z)))
                                      (define ,z (jump r8))))
                  `(begin (with-label ,x (set! r9 0))
                          (jump ,y)
                          (with-label ,y (set! r13 0))
                          (jump ,z)
                          (with-label ,z (jump r8)))))
  (check-equal? (flatten-program '(module
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
                '(begin
                   (with-label L.tmp.99 (set! rbx r15))
                   (set! rcx 10)
                   (set! rsp 100)
                   (compare rcx rsp)
                   (jump-if != L.tmp.101)
                   (jump L.tmp.100)
                   (with-label L.tmp.101 (set! rdi 1000))
                   (set! r15 rbx)
                   (jump L.f.2)
                   (with-label L.tmp.100 (set! rdi rcx))
                   (set! r15 rbx)
                   (jump L.f.1)
                   (with-label L.f.1 (set! rsp r15))
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
  (check-equal? (flatten-program '(module
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
                                      (begin (set! rbp (+ rbp 32)) (set! rsp rax) (jump L.tmp.103)))
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
                                    (define L.tmp.103 (begin (mset! (rbp - 16) rsp (rbp - 8)) (jump L.tmp.102)))))
                '(begin
                   (with-label L.tmp.105 (set! rsp r15))
                   (set! rdi 1)
                   (set! rsi 2)
                   (set! r15 rsp)
                   (jump L.f.1)
                   (with-label L.g.1 (set! rsp r15))
                   (set! rax 8)
                   (jump rsp)
                   (with-label L.f.1 (set! (rbp - 24) r15))
                   (set! (rbp - 8) rdi)
                   (set! (rbp - 0) rsi)
                   (set! rsp 10)
                   (set! rsp (+ rsp 6))
                   (set! (rbp - 16) r12)
                   (set! r12 (+ r12 rsp))
                   (set! rbp (- rbp 32))
                   (set! r15 L.rp.21)
                   (jump L.g.1)
                   (with-label L.rp.21 (set! rbp (+ rbp 32)))
                   (set! rsp rax)
                   (jump L.tmp.103)
                   (with-label L.tmp.102 (set! rbx 10))
                   (set! rbx (+ rbx 6))
                   (set! rsp r12)
                   (set! r12 (+ r12 rbx))
                   (set! rbx 8)
                   (set! rbx (bitwise-and rbx 8))
                   (set! rax (mref rsp rbx))
                   (jump (rbp - 24))
                   (with-label L.tmp.104 (mset! (rbp - 16) rsp (rbp - 0)))
                   (jump L.tmp.102)
                   (with-label L.tmp.103 (mset! (rbp - 16) rsp (rbp - 8)))
                   (jump L.tmp.102))))
