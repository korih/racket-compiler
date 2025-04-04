#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9
  rackunit)

(provide hoist-lambdas)

;; closure-lang-v9 -> hoisted-lang-v9
;; compiles p to Hoisted-lang v9 hoisting code to the top-level definitions
(define/contract (hoist-lambdas p)
  (-> closure-lang-v9? hoisted-lang-v9?)

  (void))

(module+ test)