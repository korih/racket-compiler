#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide replace-locations)

;; asm-lang-v8/assignments -> nested-asm-lang-fvars-v8
;; compiles p to Nested-asm-lang v8 by replacing all abstract location with
;; physical locations using the assignment described in the assignment info
;; field, and dropping any register-allocation-related metadata from the program
(define (replace-locations p)
  (-> asm-pred-lang-v8/assignments? nested-asm-lang-fvars-v8?)

  ;; func is `(define ,label ,tail)
  ;; interp. a function definition that does not have metadata

  ;; asm-lang-v8/assignments -> (Dict-of aloc rloc)
  ;; interp. creates a dictionary of assignments
  (define (make-assignments-dict assignments)
    (for/fold ([acc (hash)])
              ([pair assignments])
      (dict-set acc (first pair) (second pair))))

  ;; asm-pred-lang-v8/assignments.label asm-pred-lang-v8/assignments.info asm-pred-lang-v8/assignments.tail -> func
  (define (replace-locations-func label info tail)
    (define assignments (make-assignments-dict (info-ref info 'assignment)))
    `(define ,label ,(replace-locations-tail tail assignments)))

  ;; asm-lang-v8/assignments.tail (Dict-of aloc rloc) -> nested-asm-lang-v8.tail
  (define (replace-locations-tail t assignments)
    (match t
      [`(begin ,fx ... ,tail)
       (define compiled-fx (for/list ([e fx])
                             (replace-locations-effect e assignments)))
       (define compiled-tail (replace-locations-tail tail assignments))
       `(begin ,@compiled-fx ,compiled-tail)]
      [`(if ,pred ,t1 ,t2)
       (define pred^ (replace-locations-pred pred assignments))
       (define t1^ (replace-locations-tail t1 assignments))
       (define t2^ (replace-locations-tail t2 assignments))
       `(if ,pred^ ,t1^ ,t2^)]
      [`(jump ,trg ,locs ...)
       `(jump ,(replace-locations-trg trg assignments))]))

  ;; asm-lang-v8/assignments.effect (Dict-of aloc rloc) -> nested-asm-lang-v8.effect
  (define (replace-locations-effect e assignments)
    (match e
      [`(set! ,loc1 (mref ,loc2 ,index))
       (define loc1^ (replace-locations-loc loc1 assignments))
       (define loc2^ (replace-locations-loc loc2 assignments))
       (define index^ (replace-locations-opand index assignments))
       `(set! ,loc1^ (mref ,loc2^ ,index^))]
      [`(set! ,loc (,binop ,loc ,op))
       (define loc^ (replace-locations-loc loc assignments))
       (define op^ (replace-locations-opand op assignments))
       `(set! ,loc^ (,binop ,loc^ ,op^))]
      [`(set! ,loc ,triv)
       (define loc^ (replace-locations-loc loc assignments))
       (define triv^ (replace-locations-triv triv assignments))
       `(set! ,loc^ ,triv^)]
      [`(mset! ,loc ,index ,triv)
       (define loc^ (replace-locations-loc loc assignments))
       (define index^ (replace-locations-opand index assignments))
       (define triv^ (replace-locations-triv triv assignments))
       `(mset! ,loc^ ,index^ ,triv^)]
      [`(begin ,fx ... ,e)
       (define compiled-fx (for/list ([e fx]) (replace-locations-effect e assignments)))
       (define compiled-e (replace-locations-effect e assignments))
       `(begin ,@compiled-fx ,compiled-e)]
      [`(if ,pred ,e1 ,e2)
       (define pred^ (replace-locations-pred pred assignments))
       (define e1^ (replace-locations-effect e1 assignments))
       (define e2^ (replace-locations-effect e2 assignments))
       `(if ,pred^ ,e1^ ,e2^)]
      [`(return-point ,label ,tail) `(return-point ,label ,(replace-locations-tail tail assignments))]))

  ;; asm-lang-v8/assignments.pred (Dict-of aloc rloc) -> nested-asm-lang-v8.pred
  (define (replace-locations-pred p assignments)
    (match p
      ['(true) p]
      ['(false) p]
      [`(begin ,effects ... ,pred)
       (define effects^
         (for/list ([effect effects])
           (replace-locations-effect effect assignments)))
       (define pred^ (replace-locations-pred pred assignments))
       `(begin ,@effects^ ,pred^)]
      [`(not ,pred) `(not ,(replace-locations-pred pred assignments))]
      [`(,relop ,loc ,op)
       (define loc^ (replace-locations-loc loc assignments))
       (define op^ (replace-locations-opand op assignments))
       `(,relop ,loc^ ,op^)]
      [`(if ,p1 ,p2 ,p3)
       (define p1^ (replace-locations-pred p1 assignments))
       (define p2^ (replace-locations-pred p2 assignments))
       (define p3^ (replace-locations-pred p3 assignments))
       `(if ,p1^ ,p2^ ,p3^)]))

  ;; asm-lang-v8/assignments.triv (Dict-of aloc rloc) -> nested-asm-lang-v8.triv
  (define (replace-locations-triv t assignments)
    (match t
      [label #:when (label? label) label]
      [op (replace-locations-opand op assignments)]))

  ;; asm-lang-v8/assignments.opand (Dict-of aloc rloc) -> nested-asm-lang-v8.opand
  (define (replace-locations-opand op assignments)
    (match op
      [int64 #:when (int64? int64) int64]
      [loc (replace-locations-loc loc assignments)]))

  ;; asm-lang-v8/assignments.loc (Dict-of aloc rloc) -> nested-asm-lang-v8.loc
  (define (replace-locations-loc loc assignments)
    (match loc
      [aloc #:when (aloc? aloc) (dict-ref assignments aloc)]
      [rloc #:when (rloc? rloc) rloc]))

  ;; asm-lang-v8/assignments.trg (Dict-of aloc rloc) -> nested-asm-lang-v8.trg
  (define (replace-locations-trg trg assignments)
    (match trg
      [label #:when (label? label) label]
      [loc (replace-locations-loc loc assignments)]))

  (match p
    [`(module ,info (define ,labels ,infos ,tails) ... ,tail)
     `(module ,@(for/list ([label labels] [info infos] [tail tails])
                  (replace-locations-func label info tail))
        ,(replace-locations-tail tail (make-assignments-dict (info-ref info 'assignment))))]))

