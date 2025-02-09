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
     compile-m3))

  ;; -----------------------------------
  ;; undead-analysis tests
  ;; -----------------------------------
  (test-case
   "undead-analysis tests"
   (check-equal? (undead-analysis
                  '(module ((locals ()))
                     (halt x.1)))
                 '(module
                      ((locals ()) (undead-out ()))
                    (halt x.1)))
   (check-equal? (undead-analysis
                  '(module ((locals ()))
                     (begin
                       (set! x.1 1)
                       (halt x.1))))
                 '(module
                      ((locals ()) (undead-out ((x.1) ())))
                    (begin
                      (set! x.1 1)
                      (halt x.1))))
   (check-equal? (undead-analysis
                  '(module ((locals (x.1)))
                     (begin
                       (set! x.1 42)
                       (halt x.1))))
                 '(module
                      ((locals (x.1)) (undead-out ((x.1) ())))
                    (begin (set! x.1 42) (halt x.1)))
                 "Testing basic small program")
   (check-equal? (undead-analysis '(module
                                       ((locals (x.1 y.2)))
                                     (begin
                                       (set! y.2 1)
                                       (set! x.1 2)
                                       (set! y.2 (* y.2 x.1))
                                       (begin
                                         (set! x.1 (+ x.1 -1))
                                         (set! y.2 (* y.2 x.1))
                                         (begin
                                           (set! x.1 (+ x.1 -1))
                                           (set! y.2 (* y.2 x.1))))
                                       (halt y.2))))
                 '(module
                      ((locals (x.1 y.2))
                       (undead-out ((y.2)
                                    (y.2 x.1)
                                    (x.1 y.2)
                                    ((y.2 x.1)
                                     (x.1 y.2)
                                     ((x.1 y.2)
                                      (y.2)))
                                    ())))
                    (begin
                      (set! y.2 1)
                      (set! x.1 2)
                      (set! y.2 (* y.2 x.1))
                      (begin
                        (set! x.1 (+ x.1 -1))
                        (set! y.2 (* y.2 x.1))
                        (begin
                          (set! x.1 (+ x.1 -1))
                          (set! y.2 (* y.2 x.1))))
                      (halt y.2))))
   (check-equal? (undead-analysis
                  '(module ((locals ()))
                     (halt 1)))
                 '(module
                      ((locals ()) (undead-out ()))
                    (halt 1)))
   (check-equal? (undead-analysis
                  '(module ((locals (x.1 y.2 z.3)))
                     (begin
                       (set! x.1 42)
                       (set! x.1 x.1)
                       (set! z.3 x.1)
                       (set! z.3 z.3)
                       (set! z.3 (+ z.3 z.3))
                       (halt z.3))))
                 '(module
                      ((locals (x.1 y.2 z.3))
                       (undead-out ((x.1) (x.1) (z.3) (z.3) (z.3) ())))
                    (begin
                      (set! x.1 42)
                      (set! x.1 x.1)
                      (set! z.3 x.1)
                      (set! z.3 z.3)
                      (set! z.3 (+ z.3 z.3))
                      (halt z.3))))
   (check-equal? (undead-analysis
                  '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1)))
                     (begin
                       (set! v.1 1)
                       (set! w.2 46)
                       (set! x.3 v.1)
                       (set! p.1 7)
                       (set! x.3 (+ x.3 p.1))
                       (set! y.4 x.3)
                       (set! p.1 4)
                       (set! y.4 (+ y.4 p.1))
                       (set! z.5 x.3)
                       (set! z.5 (+ z.5 w.2))
                       (set! t.6 y.4)
                       (set! p.1 -1)
                       (set! t.6 (* t.6 p.1))
                       (set! z.5 (+ z.5 t.6))
                       (halt z.5))))
                 '(module
                      ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                       (undead-out
                        ((v.1)
                         (v.1 w.2)
                         (x.3 w.2)
                         (p.1 x.3 w.2)
                         (x.3 w.2)
                         (y.4 x.3 w.2)
                         (p.1 y.4 x.3 w.2)
                         (x.3 w.2 y.4)
                         (w.2 z.5 y.4)
                         (y.4 z.5)
                         (t.6 z.5)
                         (p.1 t.6 z.5)
                         (t.6 z.5)
                         (z.5)
                         ())))
                    (begin
                      (set! v.1 1)
                      (set! w.2 46)
                      (set! x.3 v.1)
                      (set! p.1 7)
                      (set! x.3 (+ x.3 p.1))
                      (set! y.4 x.3)
                      (set! p.1 4)
                      (set! y.4 (+ y.4 p.1))
                      (set! z.5 x.3)
                      (set! z.5 (+ z.5 w.2))
                      (set! t.6 y.4)
                      (set! p.1 -1)
                      (set! t.6 (* t.6 p.1))
                      (set! z.5 (+ z.5 t.6))
                      (halt z.5)))))

  ;; -----------------------------------
  ;; conflict-analysis tests
  ;; -----------------------------------
  (test-case
   "conflict-analysis"
   (check-equal? (conflict-analysis '(module ((locals (x.1)) (undead-out ((x.1) ()))) (begin (set! x.1 42) (halt x.1))))
                 '(module ((locals (x.1)) (conflicts ((x.1 ())))) (begin (set! x.1 42) (halt x.1))))
   (match (conflict-analysis '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                                       (undead-out ((v.1)
                                                    (v.1 w.2)
                                                    (w.2 x.3)
                                                    (p.1 w.2 x.3)
                                                    (w.2 x.3)
                                                    (y.4 w.2 x.3)
                                                    (p.1 y.4 w.2 x.3)
                                                    (y.4 w.2 x.3)
                                                    (z.5 y.4 w.2)
                                                    (z.5 y.4)
                                                    (t.6 z.5)
                                                    (t.6 z.5 p.1)
                                                    (t.6 z.5)
                                                    (z.5)
                                                    ())))
                                (begin (set! v.1 1)
                                       (set! w.2 46)
                                       (set! x.3 v.1)
                                       (set! p.1 7)
                                       (set! x.3 (+ x.3 p.1))
                                       (set! y.4 x.3)
                                       (set! p.1 4)
                                       (set! y.4 (+ y.4 p.1))
                                       (set! z.5 x.3)
                                       (set! z.5 (+ z.5 w.2))
                                       (set! t.6 y.4)
                                       (set! p.1 -1)
                                       (set! t.6 (* t.6 p.1))
                                       (set! z.5 (+ z.5 t.6))
                                       (halt z.5))))
     [`(module ((locals ,ls) (conflicts ,conflicts)) ,tail)
      (check-true (set=? (get-neighbors conflicts 'v.1) (list 'w.2)))
      (check-true (set=? (get-neighbors conflicts 'w.2) (list 'z.5 'y.4 'p.1 'x.3 'v.1)))
      (check-true (set=? (get-neighbors conflicts 'x.3) (list 'y.4 'p.1 'w.2)))
      (check-true (set=? (get-neighbors conflicts 'y.4) (list 'z.5 'x.3 'p.1 'w.2)))
      (check-true (set=? (get-neighbors conflicts 'z.5) (list 'p.1 't.6 'w.2 'y.4)))
      (check-true (set=? (get-neighbors conflicts 't.6) (list 'p.1 'z.5)))
      (check-true (set=? (get-neighbors conflicts 'p.1) (list 'z.5 't.6 'y.4 'x.3 'w.2)))]))

  ;; -----------------------------------
  ;; assign-registers tests
  ;; -----------------------------------
  (test-case
   "assign-registers"
   (check-true #t))

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
                 '(module (begin (set! x0 0) (begin (set! x2 2) (set! x1 (+ x2 1))) x0)))))
