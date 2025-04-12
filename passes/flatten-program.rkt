#lang racket

(require
  cpsc411/langs/v8)

(provide flatten-program)

;; block-asm-lang-v8 -> para-asm-lang-v8
;; interp. flatten begin statements in the program
(define/contract (flatten-program p)
  (-> block-asm-lang-v8? para-asm-lang-v8?)

  ;; block-asm-lang-v8.b -> (list para-asm-lang-v8.s)
  ;; convert b expressions to flattened s expressions
  (define (flatten-program/b b)
    (match b
      [`(define ,label ,tail)
       (define compiled-s (flatten-program/tail tail))
       (cons `(with-label ,label ,(first compiled-s)) (rest compiled-s))]))

  ;; block-asm-lang-v8.tail -> (list para-asm-lang-v8.s)
  ;; interp. flattens the tail expression into a list of s statements
  (define (flatten-program/tail tail)
    (match tail
      [`(halt ,opand) (list `(halt, opand))]
      [`(jump ,trg) (list `(jump ,trg))]
      [`(begin ,fx ... ,tail)
       ;; Note that block-asm-lang-v8.effect expressions are already para-asm-lang-v8.s expressions
       (append fx (flatten-program/tail tail))]
      [`(if (,relop ,loc ,opand) (jump ,trg1) (jump ,trg2))
       (list `(compare ,loc ,opand) `(jump-if ,relop ,trg1) `(jump ,trg2))]))

  (match p
    [`(module ,bs ...)
     (define compiled-s (for/fold ([s empty])
                                  ([b bs])
                          (append s (flatten-program/b b))))
     `(begin ,@compiled-s)]))


