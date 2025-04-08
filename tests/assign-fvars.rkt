#lang racket

(require
  "../passes/assign-fvars.rkt"
  cpsc411/langs/v8
  rackunit)

(module+ test
  (check-equal? (assign-fvars '(module ((locals (x.1)))
                                 (begin (set! x.1 0) (halt x.1))))
                '(module ((locals (x.1)) (assignment ((x.1 fv0))))
                   (begin (set! x.1 0) (halt x.1))))
  (check-equal? (assign-fvars '(module ((locals (x.1 y.1 w.1)))
                                 (begin (set! x.1 0)
                                        (set! y.1 w.1)
                                        (set! w.1 1)
                                        (set! w.1 (+ w.1 y.1))
                                        (halt w.1))))
                '(module ((locals (x.1 y.1 w.1))
                          (assignment ((x.1 fv0) (y.1 fv1) (w.1 fv2))))
                   (begin (set! x.1 0)
                          (set! y.1 w.1)
                          (set! w.1 1)
                          (set! w.1 (+ w.1 y.1))
                          (halt w.1)))))
