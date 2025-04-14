#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide uncover-locals)

;; asm-pred-lang-v8 -> asm-pred-lang-v8/locals
;; compiles p to to Asm-pred-lang v8/locals by analysing which abstract
;; locations are used in the module and decorating the module with the set of
;; variables in an info field.
(define/contract (uncover-locals p)
  (-> asm-pred-lang-v8? asm-pred-lang-v8/locals?)

  ;; func-info is `(define ,label ,info ,tail)
  ;; interp. a function definition that has metadata

  ;; unique-alocs is (Set-of aloc)
  ;; interp. keeps track of unique abstract locations
  (define unique-alocs (mutable-set))

  ;; func-info -> func-info
  ;; interp. collects alocs used in the tail of the function and adds them to
  ;; its info
  ;; EFFECTS: clears the unique-alocs set
  (define (uncover-locals-func func)
    (set-clear! unique-alocs)
    (match func
      [`(define ,label ,info ,tail)
       (uncover-locals-tail tail)
       `(define ,label ,(info-set info 'locals (set->list unique-alocs)) ,tail)]))

  ;; asm-pred-lang-v8.tail -> void
  ;; interp. recursively collects alocs from tail position
  ;; EFFECTS: adds alocs from tail t to unique-alocs
  (define (uncover-locals-tail tail)
    (match tail
      [`(begin ,ef ... ,ta)
       (for-each uncover-locals-effect ef)
       (uncover-locals-tail ta)]
      [`(jump ,trg ,locs ...)
       (uncover-locals-trg trg)
       (for-each uncover-locals-loc locs)]
      [`(if ,pred ,tail1 ,tail2)
       (uncover-locals-pred pred)
       (for-each uncover-locals-tail (list tail1 tail2))]))

  ;; asm-pred-lang-v8.effect -> void
  ;; interp. recursively collects alocs from effects 
  ;; EFFECTS: adds alocs from effect e to unique-alocs
  (define (uncover-locals-effect effect)
    (match effect
      [`(set! ,loc1 (mref ,loc2 ,index))
       (for-each uncover-locals-loc (list loc1 loc2))
       (uncover-locals-opand index)]
      [`(set! ,loc (,binop ,loc ,op))
       (uncover-locals-loc loc)
       (uncover-locals-opand op)]
      [`(set! ,loc ,triv)
       (uncover-locals-loc loc)
       (uncover-locals-triv triv)]
      [`(mset! ,loc ,index ,triv)
       (uncover-locals-loc loc)
       (uncover-locals-opand index)
       (uncover-locals-triv triv)]
      [`(begin ,ef ...)
       (for-each uncover-locals-effect ef)]
      [`(if ,pred ,e1 ,e2)
       (uncover-locals-pred pred)
       (for-each uncover-locals-effect (list e1 e2))]
      [`(return-point ,label ,tail)
       (uncover-locals-tail tail)]))

  ;; asm-pred-lang-v8.pred -> void
  ;; interp. recursively collects alocs from predicates
  ;; EFFECTS: adds alocs from pred p to unique-alocs
  (define (uncover-locals-pred pred)
    (match pred
      [`(not ,pred)
       (uncover-locals-pred pred)]
      [`(begin ,e ... ,pred)
       (for-each uncover-locals-effect e)
       (uncover-locals-pred pred)]
      [`(if ,pred1 ,pred2 ,pred3)
       (for-each uncover-locals-pred (list pred1 pred2 pred3))]
      [`(,relop ,loc ,op)
       (uncover-locals-loc loc)
       (uncover-locals-opand op)]
      ['(true) (void)]
      ['(false) (void)]))

  ;; asm-pred-lang-v8.triv -> void
  ;; interp. collects alocs from trivials
  ;; EFFECTS: adds alocs from triv t to unique-alocs
  (define (uncover-locals-triv triv)
    (match triv
      [label #:when (label? label) (void)]
      [opand (uncover-locals-opand opand)]))

  ;; asm-pred-lang-v8.opand -> void
  ;; interp. collects alocs from operands
  ;; EFFECTS: adds alocs from opand op to unique-alocs
  (define (uncover-locals-opand op)
    (match op
      [int64 #:when (int64? int64) (void)]
      [loc (uncover-locals-loc loc)]))

  ;; asm-pred-lang-v8.loc -> void
  ;; interp. adds aloc to set if it's not a register
  ;; EFFECTS: adds alocs from loc to unique-alocs
  (define (uncover-locals-loc loc)
    (match loc
      [aloc #:when (aloc? aloc) (set-add! unique-alocs aloc)]
      [rloc #:when (rloc? rloc) (void)]))

  ;; asm-pred-lang-v8.trg -> void
  ;; interp. collects alocs from jump targets
  ;; EFFECTS: adds alocs from trg to unique-alocs
  (define (uncover-locals-trg trg)
    (match trg
      [label #:when (label? label) (void)]
      [loc (uncover-locals-loc loc)]))

  (match p
    [`(module ,info ,funcs ... ,t)
     (define uncovered-funcs (map uncover-locals-func funcs))
     (set-clear! unique-alocs)
     (uncover-locals-tail t)
     `(module ,(info-set info 'locals (set->list unique-alocs)) ,@uncovered-funcs ,t)]))

