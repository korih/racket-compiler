#lang racket

(require
 cpsc411/compiler-lib
 cpsc411/ptr-run-time)

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
  "passes/assign-frame-variables.rkt"
  "passes/implement-safe-primops.rkt"
  "passes/specify-representation.rkt"
  "passes/remove-complex-opera.rkt")

(module+ test
  (require
   rackunit
   rackunit/text-ui
   cpsc411/langs/v7
   cpsc411/test-suite/public/v7)

  ;; You can modify this pass list, e.g., by adding other
  ;; optimization, debugging, or validation passes.
  ;; Doing this may provide additional debugging info when running the rest
  ;; suite.
  (define pass-map
    (list
     #;(cons check-exprs-lang #f)
     (cons uniquify interp-exprs-lang-v7)
     (cons implement-safe-primops interp-exprs-unique-lang-v7)
     (cons specify-representation interp-exprs-unsafe-data-lang-v7)
     (cons remove-complex-opera* interp-exprs-bits-lang-v7)
     (cons sequentialize-let interp-values-bits-lang-v7)
     (cons normalize-bind interp-imp-mf-lang-v7)
     (cons impose-calling-conventions interp-proc-imp-cmf-lang-v7)
     (cons select-instructions interp-imp-cmf-lang-v7)
     (cons uncover-locals interp-asm-pred-lang-v7)
     (cons undead-analysis interp-asm-pred-lang-v7/locals)
     (cons conflict-analysis interp-asm-pred-lang-v7/undead)
     (cons assign-call-undead-variables interp-asm-pred-lang-v7/conflicts)
     (cons allocate-frames interp-asm-pred-lang-v7/pre-framed)
     (cons assign-registers interp-asm-pred-lang-v7/framed)
     (cons assign-frame-variables interp-asm-pred-lang-v7/spilled)
     (cons replace-locations interp-asm-pred-lang-v7/assignments)
     (cons optimize-predicates interp-nested-asm-lang-fvars-v7)
     (cons implement-fvars interp-nested-asm-lang-fvars-v7)
     (cons expose-basic-blocks interp-nested-asm-lang-v7)
     (cons resolve-predicates interp-block-pred-lang-v7)
     (cons flatten-program interp-block-asm-lang-v7)
     (cons patch-instructions interp-para-asm-lang-v7)
     (cons generate-x64 interp-paren-x64-v7)
     (cons wrap-x64-boilerplate #f)
     (cons wrap-x64-run-time #f)))

  (current-pass-list
   (map car pass-map))

  (run-tests
   (v7-public-test-suite
    (current-pass-list)
    (map cdr pass-map))))
