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

  ;; func is `(define ,label ,info ,tail)
  ;; interp. a function definition that has metadata

  ;; func is `(define ,label ,tail)
  ;; interp. a function definition that does not have metadata

  ;; assignments is (Dict-of aloc rloc)
  ;; interp. 

  ;; asm-lang-v8/assignments -> (Dict-of aloc rloc)
  ;; interp. creates a dictionary of assignments
  (define (make-assignments-dict assignments)
    (for/fold ([acc (hash)])
              ([pair assignments])
      (dict-set acc (first pair) (second pair))))

  ;; func-info -> func
  ;; interp. replaces locations in a function body using its assignment table
  (define (replace-locations-func func)
    (match func
      [`(define ,label ,info ,tail)
       (define assignments (make-assignments-dict (info-ref info 'assignment)))
       `(define ,label ,(replace-locations-tail tail assignments))]))

  ;; asm-lang-v8/assignments.tail (Dict-of aloc rloc) -> nested-asm-lang-v8.tail
  ;; interp. replaces all alocs in the tail with physical locations
  ;; ACCUMULATOR: assignments is (Dict-of aloc rloc)
  ;; INVARIANT: all alocs in the tail are present as keys in assignments
  (define (replace-locations-tail tail assignments)
    (match tail
      [`(begin ,fx ... ,tail)
       `(begin
          ,@(map (lambda (e) (replace-locations-effect e assignments)) fx)
          ,(replace-locations-tail tail assignments))]
      [`(if ,pred ,t1 ,t2)
       `(if ,(replace-locations-pred pred assignments)
            ,(replace-locations-tail t1 assignments)
            ,(replace-locations-tail t2 assignments))]
      [`(jump ,trg ,locs ...)
       `(jump ,(replace-locations-trg trg assignments))]))

  ;; asm-lang-v8/assignments.effect (Dict-of aloc rloc) -> nested-asm-lang-v8.effect
  ;; interp. replaces all alocs in the effect expression with physical locations
  ;; ACCUMULATOR: assignments is (Dict-of aloc rloc)
  ;; INVARIANT: all alocs in the effect are present as keys in assignments
  (define (replace-locations-effect effect assignments)
    (match effect
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
      [`(begin ,effects ...)
       `(begin ,@(map (lambda (eff) (replace-locations-effect eff assignments)) effects))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(replace-locations-pred pred assignments)
            ,(replace-locations-effect e1 assignments)
            ,(replace-locations-effect e2 assignments))]
      [`(return-point ,label ,tail)
       `(return-point ,label ,(replace-locations-tail tail assignments))]))

  ;; asm-lang-v8/assignments.pred (Dict-of aloc rloc) -> nested-asm-lang-v8.pred
  ;; interp. replaces all alocs in the predicate with physical locations
  ;; ACCUMULATOR: assignments is (Dict-of aloc rloc)
  ;; INVARIANT: all alocs in the pred are present as keys in assignments
  (define (replace-locations-pred pred assignments)
    (match pred
      ['(true) pred]
      ['(false) pred]
      [`(begin ,effects ... ,pred)
       `(begin
          ,@(map (lambda (effect) (replace-locations-effect effect assignments)) effects)
          ,(replace-locations-pred pred assignments))]
      [`(not ,pred)
       `(not ,(replace-locations-pred pred assignments))]
      [`(,relop ,loc ,op)
       (define loc^ (replace-locations-loc loc assignments))
       (define op^ (replace-locations-opand op assignments))
       `(,relop ,loc^ ,op^)]
      [`(if ,p1 ,p2 ,p3)
       `(if ,(replace-locations-pred p1 assignments)
            ,(replace-locations-pred p2 assignments)
            ,(replace-locations-pred p3 assignments))]))

  ;; asm-lang-v8/assignments.triv (Dict-of aloc rloc) -> nested-asm-lang-v8.triv
  ;; interp. replaces all alocs in a triv with physical locations
  ;; ACCUMULATOR: assignments is (Dict-of aloc rloc)
  ;; INVARIANT: all alocs in the triv are present as keys in assignments
  (define (replace-locations-triv t assignments)
    (match t
      [label #:when (label? label) label]
      [op (replace-locations-opand op assignments)]))

  ;; asm-lang-v8/assignments.opand (Dict-of aloc rloc) -> nested-asm-lang-v8.opand
  ;; interp. replaces all alocs in an operand with physical locations
  ;; ACCUMULATOR: assignments is (Dict-of aloc rloc)
  ;; INVARIANT: all alocs in the opand are present as keys in assignments
  (define (replace-locations-opand op assignments)
    (match op
      [int64 #:when (int64? int64) int64]
      [loc (replace-locations-loc loc assignments)]))

  ;; asm-lang-v8/assignments.loc (Dict-of aloc rloc) -> nested-asm-lang-v8.loc
  ;; interp. looks up and replaces an abstract location using the assignments table
  ;; ACCUMULATOR: assignments is (Dict-of aloc rloc)
  ;; INVARIANT: all alocs in the program are keys in assignments
  (define (replace-locations-loc loc assignments)
    (match loc
      [aloc #:when (aloc? aloc) (dict-ref assignments aloc)]
      [rloc #:when (rloc? rloc) rloc]))

  ;; asm-lang-v8/assignments.trg (Dict-of aloc rloc) -> nested-asm-lang-v8.trg
  ;; interp. replaces locations in the jump target if needed
  ;; ACCUMULATOR: assignments is (Dict-of aloc rloc)
  ;; INVARIANT: all alocs in the trg are present as keys in assignments
  (define (replace-locations-trg trg assignments)
    (match trg
      [label #:when (label? label) label]
      [loc (replace-locations-loc loc assignments)]))

  (match p
    [`(module ,info ,funcs ... ,tail)
     (define assignments (make-assignments-dict (info-ref info 'assignment)))
     `(module ,@(map replace-locations-func funcs) ,(replace-locations-tail tail assignments))]))

