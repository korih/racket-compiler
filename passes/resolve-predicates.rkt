#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/2c-run-time
  cpsc411/langs/v7
  rackunit)

(provide resolve-predicates)

;; block-pred-lang-v7 -> block-asm-lang-v7
;; compiles p to Block-asm-lang v7 by manipulating the branches of if statements
;; to resolve branches
(define/contract (resolve-predicates p)
  (-> block-pred-lang-v7? block-asm-lang-v7?)

  ;; block-pred-lang-v7.b -> block-asm-lang-v7.b
  (define (resolve-predicates-b b)
    (match b
      [`(define ,label ,tail)
       `(define ,label ,(resolve-predicates-tail tail))]))

  ;; block-pred-lang-v7.tail -> block-asm-lang-v7.tail
  (define (resolve-predicates-tail t)
    (match t
      [`(if ,pred (jump ,trg1) (jump ,trg2))
       (resolve-predicates-pred pred `(jump ,trg1) `(jump ,trg2))]
      [`(begin ,e ... ,tail)
       `(begin ,@e ,(resolve-predicates-tail tail))]
      ;; Using a wildcard collapse case as it captures all other well-formed
      ;; expressions without any needed transformations
      [_ t]))

  ;; block-pred-lang-v7.pred block-pred-lang-v7.tail block-pred-lang-v7.tail -> block-asm-lang-v7.tail
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
                       (jump rsp))))))
