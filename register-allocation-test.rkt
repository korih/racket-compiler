#lang racket

(module+ test
  (require "register-allocation.rkt")

  (require cpsc411/graph-lib)

  (require rackunit)

  ;; ------------------------------
  ;; uncover-locals tests
  ;; ------------------------------
  (test-case
   "uncover-locals"
   (check-equal? (uncover-locals '(module () (begin (set! x.1 0) (halt x.1))))
                 '(module ((locals (x.1))) (begin (set! x.1 0) (halt x.1))))
   (match-let ([`(module ((locals (,ls ...))) ,_) (uncover-locals '(module () (begin (set! x.1 0)
                                                                                     (set! y.1 x.1)
                                                                                     (set! y.1 (+ y.1 x.1))
                                                                                     (halt y.1))))])
     (check-equal? (list->set ls) (set 'x.1 'y.1))))

  ;; ------------------------------
  ;; assign-fvars tests
  ;; ------------------------------
  (test-case
   "assign-fvars"
   (check-equal? (assign-fvars '(module ((locals (x.1))) (begin (set! x.1 0) (halt x.1))))
                 '(module ((locals (x.1)) (assignment ((x.1 fv0)))) (begin (set! x.1 0) (halt x.1))))
   (check-equal? (assign-fvars '(module ((locals (x.1 y.1 w.1))) (begin (set! x.1 0)
                                                                        (set! y.1 w.1)
                                                                        (set! w.1 1)
                                                                        (set! w.1 (+ w.1 y.1))
                                                                        (halt w.1))))
                 '(module ((locals (x.1 y.1 w.1)) (assignment ((x.1 fv0) (y.1 fv1) (w.1 fv2)))) (begin (set! x.1 0)
                                                                                                       (set! y.1 w.1)
                                                                                                       (set! w.1 1)
                                                                                                       (set! w.1 (+ w.1 y.1))
                                                                                                       (halt w.1)))))

  ;; ------------------------------
  ;; replace-locations tests
  ;; ------------------------------
  (test-case
   "replace-locations"
   (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                       (begin (set! x.1 (+ x.1 1)) (halt x.1))))
                 '(begin (set! rax (+ rax 1)) (halt rax)))
   (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax)))) (begin (set! x.1 0) (halt x.1))))
                 '(begin (set! rax 0) (halt rax)))
   (check-equal? (replace-locations '(module ((locals (x.1 y.1 w.1)) (assignment ((x.1 rax) (y.1 rbx) (w.1 r9))))
                                       (begin (set! x.1 0)
                                              (set! y.1 x.1)
                                              (set! w.1 1)
                                              (set! w.1 (+ w.1 y.1))
                                              (halt w.1))))
                 '(begin (set! rax 0) (set! rbx rax) (set! r9 1) (set! r9 (+ r9 rbx)) (halt r9))))

  ;; ------------------------------
  ;; assign-homes tests
  ;; ------------------------------
  (test-case
   "assign-homes"
   (check-equal? (assign-homes '(module () (begin (set! x.1 0) (halt x.1)))) '(begin (set! fv0 0) (halt fv0)))
   (check-equal? (assign-homes '(module () (begin (set! x.1 0)
                                                  (set! y.1 x.1)
                                                  (set! w.1 1)
                                                  (set! w.1 (+ w.1 y.1))
                                                  (halt w.1))))
                 '(begin (set! fv2 0) (set! fv1 fv2) (set! fv0 1) (set! fv0 (+ fv0 fv1)) (halt fv0))))

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
   (check-true #t)))
