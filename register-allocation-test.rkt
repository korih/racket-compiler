#lang racket

(module+ test
  (require "register-allocation.rkt")

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
                '(begin (set! fv2 0) (set! fv1 fv2) (set! fv0 1) (set! fv0 (+ fv0 fv1)) (halt fv0)))))
