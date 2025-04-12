#lang racket

(require
  "passes/assign-call-undead-variables.rkt"
  "passes/assign-registers.rkt"
  "passes/conflict-analysis.rkt"
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
  "passes/impose-calling-conventions.rkt"
  "passes/assign-call-undead-variables.rkt"
  "passes/allocate-frames.rkt"
  "passes/assign-frame-variables.rkt"
  "passes/implement-safe-primops.rkt"
  "passes/specify-representation.rkt"
  "passes/remove-complex-opera.rkt"
  "passes/expose-allocation-pointer.rkt"
  "passes/implement-mops.rkt"
  "passes/implement-safe-call.rkt"
  "passes/define-letrec.rkt"
  "passes/optimize-direct-calls.rkt"
  "passes/dox-lambdas.rkt"
  "passes/uncover-free.rkt"
  "passes/convert-closures.rkt"
  "passes/optimize-known-calls.rkt"
  "passes/hoist-lambdas.rkt"
  "passes/implement-closures.rkt"
  "passes/expand-macros.rkt")

(module+ test
  (require cpsc411/compiler-lib
           cpsc411/langs/v8
           cpsc411/langs/v9
           cpsc411/langs/v11
           cpsc411/ptr-run-time
           cpsc411/test-suite/utils
           rackunit
           rackunit/text-ui)

  (register-test-programs!
   interp-racketish-surface
   `(("quicksort"
      (module
          (define filter
            (lambda (f ls)
              (if (empty? ls)
                  ls
                  (let ([x (car ls)])
                    ((lambda (y)
                       (if (f x)
                           (cons x y)
                           y))
                     (filter f (cdr ls)))))))

        (define append
          (lambda (ls1 ls2)
            (if (empty? ls1)
                ls2
                (cons (car ls1) (append (cdr ls1) ls2)))))

        (define quicksort
          (lambda (ls)
            (if (or (empty? ls)
                    (empty? (cdr ls)))
                ls
                (let ([pivot (car ls)]
                      [rst (cdr ls)])
                  (append
                   (append
                    (quicksort
                     (filter (lambda (x) (< x pivot)) rst))
                    (cons pivot empty))
                   (quicksort
                    (filter (lambda (x) (>= x pivot)) rst)))))))

        (define mod
          (lambda (x y)
            (if (>= x y)
                (mod (- x y) y)
                x)))

        (define build-list
          (lambda (f len)
            (if (eq? len 0)
                empty
                (cons (f len) (build-list f (- len 1))))))

        (define sorted?
          (lambda (ls)
            (if (eq? empty ls)
                #t
                (if (eq? empty (cdr ls))
                    #t
                    (and (< (car ls) (car (cdr ls)))
                         (sorted? (cdr (cdr ls))))))))

        (let ([x (make-vector 1)])
          (let ([random (lambda ()
                          (let ([A 12312]
                                [C 1]
                                [MAX 100])
                            (let ([p (mod
                                      (+ C (* A (vector-ref x 0)))
                                      MAX)])
                              (begin
                                (vector-set! x 0 p)
                                p))))])
            (sorted? (quicksort (build-list (lambda (x) (random)) 20)))))))

     ("basic define"
      (module
          (define x (lambda () 42))
        42))

     ("nested function calls"
      (module
          (define add1 (lambda (x) (+ x 1)))
        (call add1 5)))

     ("let and if"
      (module
          (define max (lambda (a b)
                        (if (< a b) b a)))
        (call max 3 7)))

     ("quoted s-expression"
      (module
          (define quoted (lambda () (quote (1 2 3))))
        (call quoted)))

     ("macro and"
      (module
          (define always-true (lambda () (and #t #t)))
        (call always-true)))

     ("make-vector and vector-ref"
      (module
          (define vec (lambda ()
                        (let ([v (make-vector 3)])
                          (begin
                            (vector-set! v 0 99)
                            (vector-ref v 0)))))
        (call vec)))
     ))

  (define pass-map
    (list
     (cons expand-macros interp-racketish-surface)
     (cons uniquify interp-exprs-lang-v9)
     (cons implement-safe-primops interp-exprs-unique-lang-v9)
     (cons implement-safe-call interp-exprs-unsafe-data-lang-v9)
     (cons define->letrec interp-exprs-unsafe-lang-v9)
     (cons optimize-direct-calls interp-just-exprs-lang-v9)
     (cons dox-lambdas interp-just-exprs-lang-v9)
     (cons uncover-free interp-lam-opticon-lang-v9)
     (cons convert-closures interp-lam-free-lang-v9)
     (cons optimize-known-calls interp-closure-lang-v9)
     (cons hoist-lambdas interp-closure-lang-v9)
     (cons implement-closures interp-hoisted-lang-v9)
     (cons specify-representation interp-proc-exposed-lang-v9)
     (cons remove-complex-opera* interp-exprs-bits-lang-v8)
     (cons sequentialize-let interp-values-bits-lang-v8)
     (cons normalize-bind interp-imp-mf-lang-v8)
     (cons impose-calling-conventions interp-proc-imp-cmf-lang-v8)
     (cons select-instructions interp-imp-cmf-lang-v8)
     (cons expose-allocation-pointer interp-asm-alloc-lang-v8)
     (cons uncover-locals interp-asm-pred-lang-v8)
     (cons undead-analysis interp-asm-pred-lang-v8/locals)
     (cons conflict-analysis interp-asm-pred-lang-v8/undead)
     (cons assign-call-undead-variables interp-asm-pred-lang-v8/conflicts)
     (cons allocate-frames interp-asm-pred-lang-v8/pre-framed)
     (cons assign-registers interp-asm-pred-lang-v8/framed)
     (cons assign-frame-variables interp-asm-pred-lang-v8/spilled)
     (cons replace-locations interp-asm-pred-lang-v8/assignments)
     (cons optimize-predicates interp-nested-asm-lang-fvars-v8)
     (cons implement-fvars interp-nested-asm-lang-fvars-v8)
     (cons expose-basic-blocks interp-nested-asm-lang-v8)
     (cons resolve-predicates interp-block-pred-lang-v8)
     (cons flatten-program interp-block-asm-lang-v8)
     (cons patch-instructions interp-para-asm-lang-v8)
     (cons implement-mops interp-paren-x64-mops-v8)
     (cons generate-x64 interp-paren-x64-v8)
     (cons wrap-x64-boilerplate #f)
     (cons wrap-x64-run-time #f)))

  (define pass-list (map car pass-map))
  (define interp-list (map cdr pass-map))

  (run-tests
   (test-suite
    "integration test suite"
    (compiler-testomatic pass-list interp-list))))
