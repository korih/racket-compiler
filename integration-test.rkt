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
  "passes/assign-frame-variables.rkt"
  "passes/implement-safe-primops.rkt"
  "passes/specify-representation.rkt"
  "passes/remove-complex-opera.rkt"
  "passes/expose-allocation-pointer.rkt"
  "passes/implement-mops.rkt")

(module+ test
  (require cpsc411/compiler-lib
           cpsc411/langs/v8
           cpsc411/ptr-run-time
           cpsc411/test-suite/public/v8
           rackunit)

  (define tests (set
                 '(module
                      (define add (lambda (x y) (call + x y)))
                    (call add 10 20))
                 '(module
                      (define abs (lambda (x) (if (call < x 0) (call - 0 x) x)))
                    (call abs -5))
                 '(module
                      (define check-empty (lambda (x) (if (call empty? x) #t (error 1))))
                    (call check-empty empty))
                 '(module
                      (define v (lambda () (call make-vector 3)))
                    (define set-first (lambda (vec) (call vector-set! vec 0 42)))
                    (define get-first (lambda (vec) (call vector-ref vec 0)))

                    (let ([vec (call v)])
                      (call + (if (call void? (call set-first vec)) 0 (error 1))
                            (call get-first vec))))
                 '(module
                      (define factorial (lambda (n)
                                          (if (call <= n 1)
                                              1
                                              (call * n (call factorial (call - n 1))))))
                    (call factorial 5))
                 ))

  (parameterize ([current-pass-list (list
                                     uniquify
                                     implement-safe-primops
                                     specify-representation
                                     remove-complex-opera*
                                     sequentialize-let
                                     normalize-bind
                                     impose-calling-conventions
                                     select-instructions
                                     expose-allocation-pointer
                                     uncover-locals
                                     undead-analysis
                                     conflict-analysis
                                     assign-call-undead-variables
                                     allocate-frames
                                     assign-registers
                                     assign-frame-variables
                                     replace-locations
                                     optimize-predicates
                                     implement-fvars
                                     expose-basic-blocks
                                     resolve-predicates
                                     flatten-program
                                     patch-instructions
                                     implement-mops
                                     generate-x64
                                     wrap-x64-boilerplate
                                     wrap-x64-run-time)])
    (for ([test tests])
      (check-equal? (execute test) (interp-exprs-lang-v8 test) (format "input: ~a" test)))))
