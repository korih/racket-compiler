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
  "passes/generate-x64.rkt"
  "passes/resolve-predicates.rkt"
  "passes/optimize-predicates.rkt"
  "passes/flatten-program.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/2c-run-time
  cpsc411/langs/v3)

#;
(provide
 check-values-lang
 compile-m2
 compile-m3)

(provide
 link-paren-x64
 interp-paren-x64
 interp-values-lang
 check-values-lang
 expose-basic-blocks
 compile-m2
 compile-m3)

;; Template support macro; feel free to delete
(define-syntax-rule (.... stx ...)
  (error "Unfinished template"))

;; Stubs; remove or replace with your definitions.
(define-values (check-values-lang
                interp-values-lang
                expose-basic-blocks)
  (values
   values
   values
   values))

;; Milestone 4 template
(define (link-paren-x64 p)
  (TODO "Design and implement link-paren-x64 for Exercise 2."))

;; Exercise 3
(define (interp-paren-x64 p)

  ;; dict-of(loc -> int64) Natural (listof statement) statement -> int64
  ;; Runs statement `s`, which is expected to be the `pc`th instruction of
  ;; `los`, modifying the environment and incrementing the program counter,
  ;; before executing the next instruction in `los`.
  (define (eval-statement env pc los s)
    (....
     (eval-program (.... env) (.... (add1 pc)) los)))

  ;; dict-of(loc -> int64) Natural (listof statements) -> int64
  ;; Runs the program represented by `los` starting from instruction number
  ;; indicated by the program counter `pc`, represented as a natural number.
  ;; Program is finished when `pc` reaches the final instruction of `los`.
  (define (eval-program env pc los)
    (if (= pc (length los))
        (dict-ref env 'rax)
        (eval-statement env pc los (list-ref los pc))))

  (TODO "Redesign and implement interp-paren-x64 for Exercise 3."))

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
   rackunit
   rackunit/text-ui
   cpsc411/langs/v4
   cpsc411/test-suite/public/v4
   racket/engine)

  ;; Milliseconds (any/c -> any_1) (() -> any_2) -> any_1 or any_2
  ;; Runs proc in an engine, returning its result, or calling the failure
  ;; continuation of proc fails to finish before timeout-ms milliseconds.
  (define (run-with-timeout timeout-ms proc
                            [fail-k (lambda () (error "Timed out"))])
    (let* ([e (engine proc)]
           [res (engine-run timeout-ms e)])
      (unless res
        (fail-k))
      (engine-result e)))

  ;; (any/c -> any/c) Milliseconds -> void
  ;; Checks that th *does* timeout after ms milliseconds
  ;; Silently passes or fails with (fail-check) if the test fails
  (define-check (check-timeout? th ms)
    (when (run-with-timeout ms th (lambda () #t))
      (fail-check)))

  (check-timeout?
   (lambda (_)
    (interp-paren-x64
     '(begin
        (with-label L.f.10 (jump L.f.10)))))
   2000)

  ;; You can modify this pass list, e.g., by adding check-assignment, or other
  ;; debugging and validation passes.
  ;; Doing this may provide additional debugging info when running the rest
  ;; suite.
  (define pass-map
    (list
     (cons check-values-lang interp-values-lang-v4)
     (cons uniquify interp-values-lang-v4)
     (cons sequentialize-let interp-values-unique-lang-v4)
     (cons normalize-bind interp-imp-mf-lang-v4)
     (cons select-instructions interp-imp-cmf-lang-v4)

     (cons uncover-locals interp-asm-pred-lang-v4)
     (cons undead-analysis interp-asm-pred-lang-v4/locals)
     (cons conflict-analysis interp-asm-pred-lang-v4/undead)
     (cons assign-registers interp-asm-pred-lang-v4/conflicts)
     (cons replace-locations interp-asm-pred-lang-v4/assignments)

     (cons optimize-predicates interp-nested-asm-lang-v4)
     (cons expose-basic-blocks interp-nested-asm-lang-v4)
     (cons resolve-predicates interp-block-pred-lang-v4)
     (cons flatten-program interp-block-asm-lang-v4)
     (cons patch-instructions interp-para-asm-lang-v4)
     (cons implement-fvars interp-paren-x64-fvars-v4)
     (cons generate-x64 interp-paren-x64-v4)
     (cons wrap-x64-run-time #f)
     (cons wrap-x64-boilerplate #f)))

  (current-pass-list
   (map car pass-map))

  (run-tests
   (v4-public-test-suite
    (current-pass-list)
    (map cdr pass-map)

    #f #;link-paren-x64
    #f #;interp-paren-x64
    #f #;interp-values-lang
    #f #;check-values-lang)))
