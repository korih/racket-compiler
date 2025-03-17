#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2
  rackunit)

(provide assign-fvars)

;; asm-lang-v2/locals -> asm-lang-v2/assignments
;; compiles p to asm-lang-v2/assignments by assigning each abstract location
;; from the locals info field to a fresh frame variable
(define/contract (assign-fvars p)
  (-> asm-lang-v2/locals? asm-lang-v2/assignments?)
  
  (match p
    [`(module ,info ,tail)
     (define assignments (for/list ([l (info-ref info 'locals)]
                                    [i (in-naturals)])
                           `(,l ,(make-fvar i))))
     (define updated-info (info-set info 'assignment assignments))
     `(module ,updated-info ,tail)]))

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