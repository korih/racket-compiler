#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9
  rackunit)

(provide optimize-direct-calls)

;; just-exprs-lang-v9 -> just-exprs-lang-v9
;; optimizes p by inlining all direct calls to first-class procedures
(define/contract (optimize-direct-calls p)
  (-> just-exprs-lang-v9? just-exprs-lang-v9?)

  (void))

(module+ test)