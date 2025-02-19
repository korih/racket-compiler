#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2
  rackunit)

(provide flatten-begins)

;; nested-asm-lang-v2 -> para-asm-lang-v2
;; compiles p to para-asm-lang-v2 by flattening all the nested begin expressions
(define/contract (flatten-begins p)
  (-> nested-asm-lang-v2? para-asm-lang-v2?)

  ;; nested-asm-lang-v2.effect -> para-asm-lang-v2.effect
  (define (flatten-begins-effect e)
    (match e
      [`(set! ,loc ,triv) `(set! ,loc ,triv)]
      [`(set! ,loc (,binop ,loc ,triv)) `(set! ,loc (,binop ,loc ,triv))]
      [`(begin ,ef ...) (make-begin (map flatten-begins-effect ef) '(begin))]))

  (match p
    [`(halt ,triv) `(begin (halt ,triv))]
    [`(begin ,fx ... ,tail)
     (define compiled-fx (for/foldr ([fx-acc empty])
                           ([e fx])
                           (append (flatten-begins-effect e) fx-acc)))
     (make-begin compiled-fx (flatten-begins tail))]))

(module+ test
  (check-equal? (flatten-begins '(halt 1)) '(begin (halt 1)))
  (check-equal? (flatten-begins '(begin (set! rbx 1) (halt rbx))) '(begin (set! rbx 1) (halt rbx)))
  (check-equal? (flatten-begins '(begin (set! rax 0) (begin (set! rbx 1) (halt rbx))))
                '(begin (set! rax 0) (set! rbx 1) (halt rbx)))
  (check-equal? (flatten-begins '(begin (begin (set! rax 0)) (halt rax))) '(begin (set! rax 0) (halt rax))))
