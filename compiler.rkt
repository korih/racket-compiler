#lang racket

(require
  "passes/assign-fvars.rkt"
  "passes/assign-homes-opt.rkt"
  "passes/assign-homes.rkt"
  "passes/assign-call-undead-variables.rkt"
  "passes/assign-registers.rkt"
  "passes/conflict-analysis.rkt"
  "passes/flatten-begins.rkt"
  "passes/implement-fvars.rkt"
  "passes/normalize-bind.rkt"
  "passes/patch-instructions.rkt"
  "passes/replace-locations.rkt"
  "passes/select-instructions.rkt"
  "passes/sequentialize-let.rkt"
  "passes/uncover-locals.rkt"
  "passes/undead-analysis.rkt"
  "passes/uniquify.rkt"
  "passes/generate-x64.rkt"
  "passes/resolve-predicates.rkt"
  "passes/optimize-predicates.rkt"
  "passes/flatten-program.rkt"
  "passes/expose-basic-blocks.rkt"
  "passes/link-paren-x64.rkt"
  "passes/interp-paren-x64.rkt"
  "passes/impose-calling-conventions.rkt"
  "passes/assign-call-undead-variables.rkt"
  "passes/allocate-frames.rkt"
  "passes/assign-frame-variables.rkt")

(require
 cpsc411/compiler-lib
 cpsc411/2c-run-time)

(provide
 allocate-frames
 assign-frame-variables)

;; TODO: Fill in.
;; You'll want to merge milestone-5 code in

;; Stubs; remove or replace with your definitions.

(module+ test
  (require
   rackunit
   rackunit/text-ui
   cpsc411/langs/v6
   cpsc411/test-suite/public/v6)

  ;; You can modify this pass list, e.g., by adding other
  ;; optimization, debugging, or validation passes.
  ;; Doing this may provide additional debugging info when running the rest
  ;; suite.
  (define pass-map
    (list
     (cons uniquify interp-values-lang-v6)
     (cons sequentialize-let interp-values-unique-lang-v6)
     (cons normalize-bind interp-imp-mf-lang-v6)
     (cons impose-calling-conventions interp-proc-imp-cmf-lang-v6)
     (cons select-instructions interp-imp-cmf-lang-v6)
     (cons uncover-locals interp-asm-pred-lang-v6)
     (cons undead-analysis interp-asm-pred-lang-v6/locals)
     (cons conflict-analysis interp-asm-pred-lang-v6/undead)
     (cons assign-call-undead-variables interp-asm-pred-lang-v6/conflicts)
     (cons allocate-frames interp-asm-pred-lang-v6/pre-framed)
     (cons assign-registers interp-asm-pred-lang-v6/framed)
     (cons assign-frame-variables interp-asm-pred-lang-v6/spilled)
     (cons replace-locations interp-asm-pred-lang-v6/assignments)
     (cons optimize-predicates interp-nested-asm-lang-fvars-v6)
     (cons implement-fvars interp-nested-asm-lang-fvars-v6)
     (cons expose-basic-blocks interp-nested-asm-lang-v6)
     (cons resolve-predicates interp-block-pred-lang-v6)
     (cons flatten-program interp-block-asm-lang-v6)
     (cons patch-instructions interp-para-asm-lang-v6)
     (cons generate-x64 interp-paren-x64-v6)
     (cons wrap-x64-boilerplate #f)
     (cons wrap-x64-run-time #f)))

  (current-pass-list
   (map car pass-map))

  (run-tests
   (v6-public-test-suite
    (current-pass-list)
    (map cdr pass-map))))
