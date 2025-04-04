#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9
  rackunit)

(provide define->letrec)

;; exprs-unsafe-lang-v9 -> just-exprs-lang-v9
;; compiles p to Just-exprs-lang v9 by  transforming all top-level bindings into
;; local bindings
(define/contract (define->letrec p)
  (-> exprs-unsafe-lang-v9? just-exprs-lang-v9?)

  (void))

(module+ test)