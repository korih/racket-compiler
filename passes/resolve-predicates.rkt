#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/2c-run-time
  cpsc411/langs/v4
  rackunit)

(provide resolve-predicates)

;; Exercise 9
;; block-pred-lang-v4 -> block-asm-lang-v4
;; compiles p to Block-asm-lang v4 by manipulating the branches of if statements
;; to resolve branches
(define/contract (resolve-predicates p)
  (-> block-pred-lang-v4? block-asm-lang-v4?)

  ;; block-pred-lang-v4.b -> block-asm-lang-v4.b
  (define (resolve-predicates-b b)
    (match b
      [`(define ,label ,tail)
       `(define ,label ,(resolve-predicates-tail tail))]))

  ;; block-pred-lang-v4.tail -> block-asm-lang-v4.tail
  (define (resolve-predicates-tail t)
    (match t
      [`(if ,pred (jump ,trg1) (jump ,trg2))
       (resolve-predicates-pred pred `(jump ,trg1) `(jump ,trg2))]
      [`(begin ,e ... ,tail)
       `(begin ,@e ,(resolve-predicates-tail tail))]
      ;; Using a wildcard collapse case as it captures all other well-formed
      ;; expressions without any needed transformations
      [_ t]))

  ;; block-pred-lang-v4.pred block-pred-lang-v4.tail block-pred-lang-v4.tail -> block-asm-lang-v4.tail
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
                     (if (> rax 1) (jump L.false.1) (jump L.true.1))))))