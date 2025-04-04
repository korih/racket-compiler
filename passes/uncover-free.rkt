#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9
  rackunit)

(provide uncover-free)

;; lam-opticon-lang-v9 -> lam-free-lang-v9
;; compiles p to Lam-free-lang v9 by explicitly annotate procedures with their
;; free variable sets.
(define/contract (uncover-free p)
  (-> lam-opticon-lang-v9? lam-free-lang-v9?)

  (void))

(module+ test)