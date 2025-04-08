#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2)

(provide flatten-begins)

;; nested-asm-lang-v2 -> para-asm-lang-v2
;; compiles p to para-asm-lang-v2 by flattening all the nested begin expressions
(define/contract (flatten-begins p)
  (-> nested-asm-lang-v2? para-asm-lang-v2?)

  ;; nested-asm-lang-v2.effect -> (list para-asm-lang-v2.effect)
  ;; interp. flatten begin statements in the program into a list of effect
  ;; statements
  (define (flatten-begins-effect e)
    (match e
      [`(set! ,loc ,triv) (list `(set! ,loc ,triv))]
      [`(set! ,loc (,binop ,loc ,triv)) (list `(set! ,loc (,binop ,loc ,triv)))]
      [`(begin ,ef ...) (for/fold ([ef-acc empty])
                                  ([e ef])
                          (append (flatten-begins-effect e) ef-acc))]))

  (match p
    [`(halt ,triv) `(begin (halt ,triv))]
    [`(begin ,fx ... ,tail)
     (define compiled-fx (for/foldr ([fx-acc empty])
                           ([e fx])
                           (append (flatten-begins-effect e) fx-acc)))
     (make-begin compiled-fx (flatten-begins tail))]))

