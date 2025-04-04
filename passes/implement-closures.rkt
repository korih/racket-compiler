#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9
  rackunit)

(provide implement-closures)

;; hoisted-lang-v9 -> proc-exposed-lang-v9
;; compiles p to Proc-exposed-lang v9 by implementing closures in terms of the
;; procedure data structure
(define/contract (implement-closures p)
  (-> hoisted-lang-v9? proc-exposed-lang-v9?)

  (void))

(module+ test)