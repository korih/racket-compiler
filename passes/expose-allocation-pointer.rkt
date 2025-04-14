#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide expose-allocation-pointer)

;; asm-alloc-lang-v8 -> asm-pred-lang-v8
;; compiles p to Asm-pred-lang v8 by implementing the allocation primitive in
;; terms of pointer arithmetic on the current-heap-base-pointer-register
(define/contract (expose-allocation-pointer p)
  (-> asm-alloc-lang-v8? asm-pred-lang-v8?)

  ;; func is `(define ,label ,info ,tail)
  ;; interp. a function definition

  ;; func -> func
  ;; interp. lowers all alloc expressions inside the tail of a function
  ;; definition
  (define (expose-allocation-pointer-func func)
    (match func
      [`(define ,label ,info ,tail)
       `(define ,label ,info ,(expose-allocation-pointer-tail tail))]))

  ;; asm-alloc-lang-v8.tail -> asm-pred-lang-v8.tail
  ;; interp. transforms allocation inside tail expressions with pointer
  ;; arithmetic on the heap base pointer register
  (define (expose-allocation-pointer-tail tail)
    (match tail
      [`(begin ,es ... ,t)
       `(begin ,@(map expose-allocation-pointer-effect es) ,(expose-allocation-pointer-tail t))]
      [`(if ,pred ,t1 ,t2)
       `(if ,(expose-allocation-pointer-pred pred)
            ,(expose-allocation-pointer-tail t1)
            ,(expose-allocation-pointer-tail t2))]
      [`(jump ,trg ,locs ...) tail]))

  ;; asm-alloc-lang-v8.effect -> asm-pred-lang-v8.effect
  ;; transforms effects to replace (alloc n) with heap pointer operations
  (define (expose-allocation-pointer-effect effect)
    (match effect
      [`(set! ,loc (alloc ,index))
       (define hbp (current-heap-base-pointer-register))
       `(begin
          (set! ,loc ,hbp)
          (set! ,hbp (+ ,hbp ,index)))]
      [`(begin ,es ...)
       `(begin ,@(map expose-allocation-pointer-effect es))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(expose-allocation-pointer-pred pred)
            ,(expose-allocation-pointer-effect e1)
            ,(expose-allocation-pointer-effect e2))]
      [`(return-point ,label ,tail)
       `(return-point ,label ,(expose-allocation-pointer-tail tail))]
      ;; Using wildcard collapse case because in all other cases, the
      ;; expression is already in asm-pred-lang-v8.effect form
      [_ effect]))

  ;; asm-alloc-lang-v8.pred -> asm-pred-lang-v8.pred
  ;; transforms predicates by recursively exposing allocations in sub-effects
  (define (expose-allocation-pointer-pred pred)
    (match pred
      [`(not ,p)
       `(not ,(expose-allocation-pointer-pred p))]
      [`(begin ,es ... ,p)
       `(begin ,@(map expose-allocation-pointer-effect es) ,(expose-allocation-pointer-pred p))]
      [`(if ,p1 ,p2 ,p3)
       `(if ,(expose-allocation-pointer-pred p1)
            ,(expose-allocation-pointer-pred p2)
            ,(expose-allocation-pointer-pred p3))]
      ;; Using wildcard collapse case because in all other cases, the
      ;; expression is already in asm-pred-lang-v8.pred form
      [_ pred]))

  (match p
    [`(module ,info ,funcs ... ,tail)
     `(module ,info ,@(map expose-allocation-pointer-func funcs) ,(expose-allocation-pointer-tail tail))]))

