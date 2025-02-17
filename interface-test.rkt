#lang racket

(require rackunit)

(module+ test
  (require
    (only-in "compiler.rkt"
             compile-m2
             compile-m3))

  (require
    cpsc411/compiler-lib
    cpsc411/langs/v2
    cpsc411/langs/v3
    cpsc411/test-suite/utils)

  (check-import-list
   "compiler.rkt"
   '(compile-m2
     compile-m3))

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
