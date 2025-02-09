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
    cpsc411/langs/v2
    cpsc411/langs/v3
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
     compile-m3))

  ;; -----------------------------------
  ;; sequentialize-let tests
  ;; -----------------------------------
  (test-case
   "sequentialize-let"
   (check-equal? (sequentialize-let '(module (let ([x.1 3]) x.1))) '(module (begin (set! x.1 3) x.1)))
   (check-equal? (sequentialize-let '(module (let ([x.1 0]
                                                   [x.2 1])
                                               (+ x.1 x.2))))
                 '(module (begin (set! x.1 0) (set! x.2 1) (+ x.1 x.2))))
   (check-equal? (sequentialize-let '(module (let ([x.1 (let ([x.7 5]) (* 5 x.7))]) (+ x.1 -1))))
                 '(module (begin (set! x.1 (begin (set! x.7 5) (* 5 x.7))) (+ x.1 -1)))))

  ;; -----------------------------------
  ;; normalize-bind tests
  ;; -----------------------------------
  (test-case
   "normalize-bind"
   (check-equal? (normalize-bind '(module x1)) '(module x1))
   (check-equal? (normalize-bind '(module (begin (set! x1 1) x1))) '(module (begin (set! x1 1) x1)))
   (check-equal? (normalize-bind '(module (begin (set! x2 2) (begin (set! x3 4) x3))))
                 '(module (begin (set! x2 2) (begin (set! x3 4) x3))))
   (check-equal? (normalize-bind '(module (begin (set! x4 1) (set! x5 1) (+ x4 x5))))
                 '(module (begin (set! x4 1) (set! x5 1) (+ x4 x5))))
   (check-equal? (normalize-bind '(module (begin (set! x5 (begin (set! x6 5) -2)) (+ x5 1))))
                 '(module (begin (begin (set! x6 5) (set! x5 -2)) (+ x5 1))))
   (check-equal? (normalize-bind '(module (begin (set! x0 0) (set! x1 (begin (set! x2 2) (+ x2 1))) x0)))
                 '(module (begin (set! x0 0) (begin (set! x2 2) (set! x1 (+ x2 1))) x0))))

  ;; -----------------------------------
  ;; select-instructions tests
  ;; -----------------------------------
  (test-case
   "select-instructions"
   (define cmf-lang-v3-1 '(module (+ 2 2)))
   (check-equal? (interp-imp-cmf-lang-v3 cmf-lang-v3-1) (interp-asm-lang-v2 (select-instructions cmf-lang-v3-1)))

   (check-equal? (select-instructions '(module (begin (set! x.1 5) x.1)))
                 '(module () (begin (set! x.1 5) (halt x.1))))
   (check-equal? (select-instructions '(module (begin (set! x.1 (+ 2 2)) x.1)))
                 '(module () (begin (set! x.1 2) (set! x.1 (+ x.1 2)) (halt x.1))))

   (define cmf-lang-v3-2 '(module (begin (set! x.1 2) (set! x.2 2) (+ x.1 x.2))))
   (check-equal? (interp-imp-cmf-lang-v3 cmf-lang-v3-2) (interp-asm-lang-v2 (select-instructions cmf-lang-v3-2)))

   (check-equal? (select-instructions '(module (begin (begin (set! x.1 1)) x.1)))
                 '(module () (begin (begin (set! x.1 1)) (halt x.1))))

   (define cmf-lang-v3-3 '(module (begin (set! foo.12 1) (begin (begin (set! x.14 1) (set! bar.13 (+ x.14 5)))
                                                                (+ foo.12 bar.13)))))
   (check-equal? (interp-imp-cmf-lang-v3 cmf-lang-v3-3) (interp-asm-lang-v2 (select-instructions cmf-lang-v3-3)))))
