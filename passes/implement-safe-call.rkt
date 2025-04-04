#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9
  rackunit)

(provide implement-safe-call)

;; exprs-unsafe-data-lang-v9 -> exprs-unsafe-lang-v9
;; compiles p to Exprs-unsafe-lang v9 by implementing call as an unsafe
;; procedure call with dynamic checks
(define/contract (implement-safe-call p)
  (-> exprs-unsafe-data-lang-v9? exprs-unsafe-lang-v9?)

  (void))

(module+ test)