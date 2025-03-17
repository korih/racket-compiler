#lang racket

(require "replace-locations.rkt"
         "assign-fvars.rkt"
         "uncover-locals.rkt")

(require cpsc411/langs/v2
         rackunit)

(provide assign-homes)

;; asm-lang-v2 -> nested-asm-lang-v2
;; interp. compiles p and replaces abstract locations with concrete locations
(define (assign-homes p)
  (-> asm-lang-v2? nested-asm-lang-v2?)
  
  (replace-locations
   (assign-fvars
    (uncover-locals p))))

(module+ test
  (check-equal? (assign-homes '(module () (begin (set! x.1 0) (halt x.1)))) '(begin (set! fv0 0) (halt fv0)))
  (check-equal? (assign-homes '(module () (begin (set! x.1 0)
                                                 (set! y.1 x.1)
                                                 (set! w.1 1)
                                                 (set! w.1 (+ w.1 y.1))
                                                 (halt w.1))))
                '(begin (set! fv2 0) (set! fv1 fv2) (set! fv0 1) (set! fv0 (+ fv0 fv1)) (halt fv0))))