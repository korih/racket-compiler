#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9
  rackunit)

(provide optimize-known-calls)

;; closure-lang-v9 -> closure-lang-v9
;; optimizes p by optimizing calls to known closures
(define/contract (optimize-known-calls p)
  (-> closure-lang-v9? closure-lang-v9?)

  (void))

(module+ test)