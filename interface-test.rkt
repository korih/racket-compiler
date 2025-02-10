#lang racket

(require rackunit
         cpsc411/graph-lib)

(module+ test
  (require
    (only-in "compiler.rkt"
             check-values-lang
             uniquify
             sequentialize-let
             normalize-bind
             select-instructions
             uncover-locals
             undead-analysis
             conflict-analysis
             assign-registers
             replace-locations
             assign-homes-opt
             assign-homes
             flatten-begins
             patch-instructions
             implement-fvars
             generate-x64

             compile-m2
             compile-m3))
  
  (require
    cpsc411/test-suite/utils)

  (check-import-list
   "compiler.rkt"
   '(check-values-lang
     uniquify
     sequentialize-let
     normalize-bind
     select-instructions
     uncover-locals
     undead-analysis
     conflict-analysis
     assign-registers
     replace-locations
     assign-homes-opt
     assign-homes
     flatten-begins
     patch-instructions
     implement-fvars
     generate-x64

     compile-m2
     compile-m3)))
