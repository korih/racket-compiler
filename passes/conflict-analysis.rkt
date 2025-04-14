#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v8)

(provide conflict-analysis)

;; asm-pred-lang-v8/undead -> asm-pred-lang-v8/conflicts
;; compiles p to Asm-pred-lang v5/conflicts by performing conflict analysis and
;; decorating programs with their conflict graph
(define/contract (conflict-analysis p)
  (-> asm-pred-lang-v8/undead? asm-pred-lang-v8/conflicts?)

  ;; func-info is `(define ,label ,info ,tail)
  ;; interp. a function definition that has metadata

  ;; undead-set-tree is one-of:
  ;; - undead-set
  ;; - (List-of undead-set-tree)
  ;; interp. represents undeadness information at each control-flow node

  ;; conflict-graph is (Graph-of aloc)
  ;; interp. the conflict graph of abstract locations
  (define conflict-graph (void))

  ;; func-info -> func-info
  ;; interp. annotates a function with a conflict graph based on its undead-out info
  ;; EFFECTS: updates conflict-graph with a new graph consisting of all the
  ;; local variables
  (define (conflict-analysis-func func)
    (match func
      [`(define ,label ,info ,tail)
       (set! conflict-graph (new-graph (info-ref info 'locals)))
       (define udt (info-ref info 'undead-out))
       (define analyzed-tail (conflict-analysis-tail udt tail))
       (define updated-info (info-set info
                                      'conflicts
                                      conflict-graph))
       `(define ,label ,updated-info ,tail)]))

  ;; undead-set-tree asm-pred-lang-v8/undead.tail -> asm-pred-lang-v8/conflicts.tail
  ;; interp. performs conflict analysis on a tail using its undead-out set tree
  ;; EFFECTS: mutates conflict-graph by adding edges from tail-level operations
  (define (conflict-analysis-tail udt tail)
    (match (cons udt tail)
      [(cons `(,undead-set-trees ... ,undead-set-tree-tail) `(begin ,fx ... ,inner-tail))
       (define compiled-effects (for/list ([e fx]
                                           [udt-e undead-set-trees])
                                  (conflict-analysis-effect udt-e e)))
       `(begin ,@compiled-effects ,(conflict-analysis-tail undead-set-tree-tail inner-tail))]
      [(cons `(,undead-out ...) `(jump ,trg ,locs ...))
       (define analyzed-trg (extract-loc-from-trg trg))
       (when (not (void? analyzed-trg))
         (set! conflict-graph (add-edges conflict-graph analyzed-trg (remove analyzed-trg undead-out))))
       (for ([loc locs])
         (set! conflict-graph (add-edges conflict-graph loc (remove loc undead-out))))
       `(jump ,trg ,@locs)]
      [(cons `(,undead-set-tree-p ,undead-set-tree-t ,undead-set-tree-f) `(if ,pred ,tail-t ,tail-f))
       `(if ,(conflict-analysis-pred undead-set-tree-p pred)
            ,(conflict-analysis-tail undead-set-tree-t tail-t)
            ,(conflict-analysis-tail undead-set-tree-f tail-f))]))

  ;; undead-set-tree asm-pred-lang-v8/undead.effect -> asm-pred-lang-v8/conflicts.effect
  ;; interp. performs conflict analysis on an effect using its undead-out set
  ;; EFFECTS: mutates conflict-graph by adding edges from set! and mref instructions
  (define (conflict-analysis-effect udt effect)
    (match (cons udt effect)
      [(cons `(,undead-set-trees ...) `(begin ,effects ...))
       `(begin ,@(map (lambda (ust eff) (conflict-analysis-effect ust eff)) undead-set-trees effects))]
      [(cons `(,undead-out ...) `(set! ,loc1 (mref ,loc2 ,index)))
       (set! conflict-graph (add-edges conflict-graph loc1 (set-subtract undead-out (list loc1))))
       `(set! ,loc1 (mref ,loc2 ,index))]
      [(cons `(,undead-out ...) `(set! ,loc (,binop ,loc ,op)))
       (set! conflict-graph (add-edges conflict-graph loc (set-subtract udt (list loc))))
       `(set! ,loc (,binop ,loc ,op))]
      [(cons `(,undead-out ...) `(set! ,loc ,triv))
       (define analyzed-triv (extract-loc-from-triv triv))
       (define extracted-loc (if (void? analyzed-triv)
                                 empty
                                 (list analyzed-triv)))
       (set! conflict-graph (add-edges conflict-graph loc (set-subtract udt (list loc) extracted-loc)))
       `(set! ,loc ,triv)]
      [(cons `(,undead-out ...) `(mset! ,loc ,index ,triv))
       `(mset! ,loc ,index ,triv)]
      [(cons `(,udt-p ,udt-t ,udt-f) `(if ,pred ,t-e ,f-e))
       `(if ,(conflict-analysis-pred udt-p pred)
            ,(conflict-analysis-effect udt-t t-e)
            ,(conflict-analysis-effect udt-f f-e))]
      [(cons `(,udt-inner ,udt-outer) `(return-point ,label ,tail))
       `(return-point ,label ,(conflict-analysis-tail udt-outer tail))]))

  ;; undead-set-tree asm-pred-lang-v8/undead.pred -> asm-pred-lang-v8/conflicts.pred
  ;; interp. performs conflict analysis on a predicate using its undead-out set
  (define (conflict-analysis-pred udt pred)
    (match (cons udt pred)
      [(cons undead-set-tree '(true)) '(true)]
      [(cons undead-set-tree '(false)) '(false)]
      [(cons undead-set-tree `(not ,pred))
       `(not ,(conflict-analysis-pred undead-set-tree pred))]
      [(cons `(,udts ... ,udt-p) `(begin ,fx ... ,pred))
       `(begin ,@(for/list ([e fx]
                            [udt-e udts])
                   (conflict-analysis-effect udt-e e))
               ,(conflict-analysis-pred udt-p pred))]
      [(cons `(,udt-p ,udt-t ,udt-f) `(if ,pred ,t-pred ,f-pred))
       `(if ,(conflict-analysis-pred udt-p pred)
            ,(conflict-analysis-pred udt-t t-pred)
            ,(conflict-analysis-pred udt-f f-pred))]
      [(cons undead-set-tree `(,relop ,aloc ,triv))
       `(,relop ,aloc ,triv)]))

  ;; triv -> (or/c loc void)
  ;; interp. extracts the loc from a triv if one exists, otherwise returns void
  (define (extract-loc-from-triv triv)
    (match triv
      [label #:when (label? label) (void)]
      [opand (extract-loc-from-opand opand)]))

  ;; loc -> loc
  ;; interp. returns the loc itself after validating it is a loc
  (define (extract-loc-from-loc loc)
    (match loc
      [rloc #:when (rloc? rloc) rloc]
      [aloc #:when (aloc? aloc) aloc]))

  ;; trg -> (or/c loc void)
  ;; interp. extracts the loc from a jump target if one exists, otherwise
  ;; returns void
  (define (extract-loc-from-trg trg)
    (match trg
      [label #:when (label? label) (void)]
      [loc (extract-loc-from-loc loc)]))

  ;; opand -> (or/c loc void)
  ;; interp. extracts the loc from an operand if one exists, otherwise
  ;; returns void
  (define (extract-loc-from-opand op)
    (match op
      [int64 #:when (int64? int64) (void)]
      [loc (extract-loc-from-loc loc)]))

  (match p
    [`(module ,info ,funcs ... ,tail)
     (define updated-funcs (map conflict-analysis-func funcs))
     (set! conflict-graph (new-graph (info-ref info 'locals)))
     (define udt (info-ref info 'undead-out))
     (define analyzed-tail (conflict-analysis-tail udt tail))
     (define updated-info (info-set info
                                    'conflicts
                                    conflict-graph))
     `(module ,updated-info ,@updated-funcs ,tail)]))

