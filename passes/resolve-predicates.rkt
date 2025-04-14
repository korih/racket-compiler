#lang racket

(require
  cpsc411/langs/v8)

(provide resolve-predicates)

;; block-pred-lang-v8 -> block-asm-lang-v8
;; compiles p to Block-asm-lang v8 by manipulating the branches of if statements
;; to resolve branches
(define/contract (resolve-predicates p)
  (-> block-pred-lang-v8? block-asm-lang-v8?)

  ;; block-pred-lang-v8.b -> block-asm-lang-v8.b
  ;; interp. resolves predicates in a basic block by rewriting its tail
  (define (resolve-predicates-b b)
    (match b
      [`(define ,label ,tail)
       `(define ,label ,(resolve-predicates-tail tail))]))

  ;; block-pred-lang-v8.tail -> block-asm-lang-v8.tail
  ;; interp. recursively rewrites a tail expression to eliminate complex predicates,
  ;; simplifying branches to direct conditional jumps
  (define (resolve-predicates-tail tail)
    (match tail
      [`(if ,pred (jump ,trg1) (jump ,trg2))
       (resolve-predicates-pred pred `(jump ,trg1) `(jump ,trg2))]
      [`(begin ,e ... ,tail)
       `(begin ,@e ,(resolve-predicates-tail tail))]
      ;; Using a wildcard collapse case as it captures all other well-formed
      ;; expressions without any needed transformations
      [_ tail]))

  ;; block-pred-lang-v8.pred block-pred-lang-v8.tail block-pred-lang-v8.tail -> block-asm-lang-v8.tail
  ;; interp. rewrites a predicate into a canonical if expression that jumps to
  ;; either the true or false target based on its evaluation
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

