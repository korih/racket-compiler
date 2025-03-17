#lang racket

(require "replace-locations.rkt"
         "assign-registers.rkt"
         "conflict-analysis.rkt"
         "undead-analysis.rkt"
         "uncover-locals.rkt")

(require
  cpsc411/langs/v2
  rackunit)

(provide assign-homes-opt)

;; Exercise 4
;; asm-lang-v2 -> nested-asm-lang-v2
;; compiles p to nested-asm-lang-v2 by replacing each abstract location with a
;; physical location through a graph-colouring register allocation algorithm
(define/contract (assign-homes-opt p)
  (-> asm-lang-v2? nested-asm-lang-v2?)

  (replace-locations
   (assign-registers
    (conflict-analysis
     (undead-analysis
      (uncover-locals p))))))