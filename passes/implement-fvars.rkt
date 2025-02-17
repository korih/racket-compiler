#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2
  rackunit)

(provide implement-fvars)

;; paren-x64-fvars-v2 -> paren-x64-v2
;; interp. convert fvars into displacement mode operands
(define/contract (implement-fvars p)
  (-> paren-x64-fvars-v2? paren-x64-v2?)

  ;; fvar -> addr
  ;; convert fvar into displacement mode operand
  (define (fvar->addr fvar)
    `(,(current-frame-base-pointer-register) - ,(* (fvar->index fvar) (current-word-size-bytes))))

  ;; (param-asm-fvars-v2-s) -> (paren-x64-v2-s)
  ;; interp. convert fvars into displacement mode operands
  (define (implement-fvars/s s)
    (match s
      [`(set! ,fvar ,v)
       #:when (fvar? fvar)
       `(set! ,(fvar->addr fvar) ,v)]
      [`(set! ,x ,fvar)
       #:when (fvar? fvar)
       `(set! ,x ,(fvar->addr fvar))]
      [`(set! ,x (,binop ,x ,fvar))
       #:when (fvar? fvar)
       `(set! ,x (,binop ,x ,(fvar->addr fvar)))]
      [_ s]))

  (match p
    [`(begin ,ss ...)
     (define compiled-s (for/list ([s ss]) (implement-fvars/s s)))
     `(begin ,@compiled-s)]))

(test-case
 "implement-fvars"
 (check-equal? (implement-fvars '(begin (set! fv0 0))) '(begin (set! (rbp - 0) 0)))
 (check-equal? (implement-fvars `(begin (set! fv0 0) (set! fv1 ,(max-int 32))))
               `(begin (set! (rbp - 0) 0) (set! (rbp - 8) ,(max-int 32))))
 (check-equal? (implement-fvars '(begin (set! fv0 5) (set! rax fv0))) '(begin (set! (rbp - 0) 5)
                                                                              (set! rax (rbp - 0)))))