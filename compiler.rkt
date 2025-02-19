#lang racket

(require
  "passes/assign-fvars.rkt"
  "passes/assign-homes-opt.rkt"
  "passes/assign-homes.rkt"
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
  "passes/generate-x64.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/2c-run-time
  cpsc411/langs/v2
  cpsc411/langs/v3
  rackunit)

(provide
 check-values-lang
 compile-m2
 compile-m3)

;; STUBS; delete when you've begun to implement the passes or replaced them with
;; your own stubs.
(define-values (check-values-lang)
  (values
   values))

;; Exercise 5
;; values-unique-lang-v3 -> string
;; interp. compile values-lang-v3 to x64 assembly without register allocation
(define/contract (compile-m2 p)
  (-> values-unique-lang-v3? string?)
  (parameterize ([current-pass-list (list uniquify
                                          sequentialize-let
                                          normalize-bind
                                          select-instructions
                                          assign-homes
                                          flatten-begins
                                          patch-instructions
                                          implement-fvars
                                          generate-x64
                                          wrap-x64-run-time
                                          wrap-x64-boilerplate)])
    (compile p)))

;; values-unique-lang-v3 -> string
;; interp. compile values-lang-v3 to x64 assembly with register allocation
(define/contract (compile-m3 p)
  (-> values-unique-lang-v3? string?)
  (parameterize ([current-pass-list (list uniquify
                                          sequentialize-let
                                          normalize-bind
                                          select-instructions
                                          assign-homes-opt
                                          flatten-begins
                                          patch-instructions
                                          implement-fvars
                                          generate-x64
                                          wrap-x64-run-time
                                          wrap-x64-boilerplate)])
    (compile p)))

(module+ test
  (require
    rackunit/text-ui
    cpsc411/langs/v3
    cpsc411/langs/v2
    cpsc411/test-suite/public/v3
    cpsc411/test-suite/public/v2-reg-alloc)

  ;; You can modify this pass list, e.g., by adding check-assignment, or other
  ;; debugging and validation passes.
  ;; Doing this may provide additional debugging info when running the rest
  ;; suite.
  ;; If you modify, you must modify the corresponding interpreter in the
  ;; interp-ls, at least by interesting #f as the interpreter for the new pass.
  ;; See the documentation for v3-public-test-suite for details on the structure
  ;; of the interpreter list.
  (current-pass-list (list
                      check-values-lang
                      uniquify
                      sequentialize-let
                      normalize-bind
                      select-instructions
                      assign-homes-opt
                      flatten-begins
                      patch-instructions
                      implement-fvars
                      generate-x64
                      wrap-x64-run-time
                      wrap-x64-boilerplate))

  (define interp-ls (list
                     interp-values-lang-v3
                     interp-values-lang-v3
                     interp-values-unique-lang-v3
                     interp-imp-mf-lang-v3
                     interp-imp-cmf-lang-v3
                     interp-asm-lang-v2
                     interp-nested-asm-lang-v2
                     interp-para-asm-lang-v2
                     interp-paren-x64-fvars-v2
                     interp-paren-x64-v2
                     #f #f))

  (run-tests (v3-public-test-sutie (current-pass-list) interp-ls))
  (run-tests (v2-reg-alloc-public-test-suite undead-analysis conflict-analysis assign-registers)))
