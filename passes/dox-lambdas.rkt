#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9
  rackunit)

(provide dox-lambdas)

;; just-exprs-lang-v9 -> lam-opticon-lang-v9
;; compiles p to Lam-opticon-lang-v9 by explicitly binds all procedures to
;; abstract locations
(define/contract (dox-lambdas p)
  (-> just-exprs-lang-v9? lam-opticon-lang-v9?)

  (void))

(module+ test)