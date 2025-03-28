#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/2c-run-time
  cpsc411/langs/v8
  rackunit)

(provide resolve-predicates)

;; block-pred-lang-v8 -> block-asm-lang-v8
;; compiles p to Block-asm-lang v8 by manipulating the branches of if statements
;; to resolve branches
(define/contract (resolve-predicates p)
  (-> block-pred-lang-v8? block-asm-lang-v8?)

  ;; block-pred-lang-v8.b -> block-asm-lang-v8.b
  (define (resolve-predicates-b b)
    (match b
      [`(define ,label ,tail)
       `(define ,label ,(resolve-predicates-tail tail))]))

  ;; block-pred-lang-v8.tail -> block-asm-lang-v8.tail
  (define (resolve-predicates-tail t)
    (match t
      [`(if ,pred (jump ,trg1) (jump ,trg2))
       (resolve-predicates-pred pred `(jump ,trg1) `(jump ,trg2))]
      [`(begin ,e ... ,tail)
       `(begin ,@e ,(resolve-predicates-tail tail))]
      ;; Using a wildcard collapse case as it captures all other well-formed
      ;; expressions without any needed transformations
      [_ t]))

  ;; block-pred-lang-v8.pred block-pred-lang-v8.tail block-pred-lang-v8.tail -> block-asm-lang-v8.tail
  (define (resolve-predicates-pred p t f)
    (match p
      [`(,relop ,loc ,opand)
       `(if ,p ,t ,f)]
      ['(true) t]
      ['(false) f]
      [`(not ,pred) (resolve-predicates-pred pred f t)]))

  (match p
    [`(module ,b ...)
     `(module ,@(map resolve-predicates-b b))]))

(module+ test
  (check-equal? (resolve-predicates '(module
                                         (define L.test.1
                                           (if (true) (jump L.true.1) (jump L.false.1)))))
                '(module (define L.test.1 (jump L.true.1))))
  (check-equal? (resolve-predicates '(module
                                         (define L.test.1
                                           (if (false) (jump L.true.1) (jump L.false.1)))))
                '(module (define L.test.1 (jump L.false.1))))
  (check-equal? (resolve-predicates '(module
                                         (define L.test.1
                                           (if (not (true)) (jump L.true.1) (jump L.false.1)))))
                '(module (define L.test.1 (jump L.false.1))))
  (check-equal? (resolve-predicates '(module
                                         (define L.test.1
                                           (if (not (false)) (jump L.true.1) (jump L.false.1)))))
                '(module (define L.test.1 (jump L.true.1))))
  (check-equal? (resolve-predicates '(module
                                         (define L.test.1
                                           (if (> rax 1) (jump L.true.1) (jump L.false.1)))))
                '(module
                     (define L.test.1
                       (if (> rax 1) (jump L.true.1) (jump L.false.1)))))
  (check-equal? (resolve-predicates '(module
                                         (define L.test.1
                                           (if (not (> rax 1)) (jump L.true.1) (jump L.false.1)))))
                '(module
                     (define L.test.1
                       (if (> rax 1) (jump L.false.1) (jump L.true.1)))))
  (check-equal? (resolve-predicates '(module
                                         (define L.true.1
                                           (if (true) (jump rax) (jump r12)))
                                       (define L.false.1
                                         (if (false) (jump r12) (jump rax)))
                                       (define L.test.1
                                         (if (not (> rax 1)) (jump L.true.1) (jump L.false.1)))))
                '(module
                     (define L.true.1 (jump rax))
                   (define L.false.1 (jump rax))
                   (define L.test.1
                     (if (> rax 1) (jump L.false.1) (jump L.true.1)))))
  (check-equal? (resolve-predicates '(module
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
  (check-equal? (resolve-predicates '(module
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

  (check-equal? (interp-block-asm-lang-v8 (resolve-predicates '(module
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
                                                                 (define L.__nested.5 (begin (set! rax 574) (jump r15))))))
                (interp-block-asm-lang-v8 '(module
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
