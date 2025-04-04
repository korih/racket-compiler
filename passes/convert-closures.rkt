#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9
  rackunit)

(provide convert-closures)

;; lam-free-lang-v9 -> closure-lang-v9
;; compiles p to Closure-lang v9 by performing closure conversion, converting
;; all procedures into explicit closures
(define/contract (convert-closures p)
  (-> lam-free-lang-v9? closure-lang-v9?)

  (void))

(module+ test)