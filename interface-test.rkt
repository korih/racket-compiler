#lang racket

(require rackunit)

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
    cpsc411/compiler-lib
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
                 '(module (begin (set! x0 0) (begin (set! x2 2) (set! x1 (+ x2 1))) x0)))
   (check-equal? (normalize-bind '(module (begin 0))) '(module (begin 0)))
   (check-equal? (normalize-bind '(module (begin (set! x.1 1)
                                                 (begin (set! x.2 -2)
                                                        (set! x.3 3)
                                                        (set! x.4 (+ x.2 x.3))
                                                        (begin (set! x.5 0)
                                                               (begin (set! x.6 7)
                                                                      x.6))))))
                 '(module (begin (set! x.1 1)
                                 (begin (set! x.2 -2)
                                        (set! x.3 3)
                                        (set! x.4 (+ x.2 x.3))
                                        (begin (set! x.5 0)
                                               (begin (set! x.6 7)
                                                      x.6))))))
   (check-equal? (normalize-bind '(module (begin (set! x.0 -3)
                                                 (set! x.1 (begin (set! x.2 2)
                                                                  (set! x.3 (begin (set! x.4 4) x.4)) (+ x.2 x.3))) x.0)))
                 '(module (begin (set! x.0 -3)
                                 (begin (set! x.2 2)
                                        (begin (set! x.4 4)
                                               (set! x.3 x.4))
                                        (set! x.1 (+ x.2 x.3))) x.0))))

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
   (check-equal? (interp-imp-cmf-lang-v3 cmf-lang-v3-3) (interp-asm-lang-v2 (select-instructions cmf-lang-v3-3))))

  ;; -----------------------------------
  ;; flatten-begins tests
  ;; -----------------------------------
  (test-case
   "flatten-begins"
   (check-equal? (flatten-begins '(halt 1)) '(begin (halt 1)))
   (check-equal? (flatten-begins '(begin (set! rbx 1) (halt rbx))) '(begin (set! rbx 1) (halt rbx)))
   (check-equal? (flatten-begins '(begin (set! rax 0) (begin (set! rbx 1) (halt rbx))))
                 '(begin (set! rax 0) (set! rbx 1) (halt rbx)))
   (check-equal? (flatten-begins '(begin (begin (set! rax 0)) (halt rax))) '(begin (set! rax 0) (halt rax))))

  ;; -----------------------------------
  ;; implement-fvars tests
  ;; -----------------------------------
  (test-case
   "implement-fvars"
   (check-equal? (implement-fvars '(begin (set! fv0 0))) '(begin (set! (rbp - 0) 0)))
   (check-equal? (implement-fvars `(begin (set! fv0 0) (set! fv1 ,(max-int 32))))
                 `(begin (set! (rbp - 0) 0) (set! (rbp - 8) ,(max-int 32))))
   (check-equal? (implement-fvars '(begin (set! fv0 5) (set! rax fv0))) '(begin (set! (rbp - 0) 5)
                                                                                (set! rax (rbp - 0)))))

  ;; -----------------------------------
  ;; compile-m2 and compile-m3 tests
  ;; -----------------------------------
  (define values-lang-v3-1 '(module 1))
  (define values-lang-v3-2 '(module (let ([x.1 1]) x.1)))
  (define values-lang-v3-3 '(module (let ([x.1 (let ([x.2 -1]) (+ x.2 1))]) (* x.1 0))))
  (define values-lang-v3-4 '(module (let ([x.1 1]
                                          [x.2 2]
                                          [x.3 3]
                                          [x.4 4]
                                          [x.5 5]
                                          [x.6 6])
                                      (let ([x.7 (* x.1 x.2)]
                                            [x.8 (* x.3 x.4)]
                                            [x.9 (+ x.5 x.6)])
                                        (let ([x.10 (* x.7 x.8)])
                                          (let ([x.11 (+ x.9 x.10)])
                                            x.11))))))

  (test-case
   "compile-m2 & compile-m3"
   (for ([test-case (in-list (list values-lang-v3-1 values-lang-v3-2 values-lang-v3-3 values-lang-v3-4))])
     (define m2 (compile-m2 test-case))
     (define m3 (compile-m3 test-case))
     (displayln (format "test case: ~a" test-case))
     (check-equal? (nasm-run/print-number m2) (interp-values-lang-v3 test-case))
     (check-equal? (nasm-run/print-number m3) (interp-values-lang-v3 test-case))
     (time (nasm-run/exit-code m2))
     (time (nasm-run/exit-code m3)))))
