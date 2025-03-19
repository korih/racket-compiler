#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v6
  rackunit)

(provide flatten-program)

;; block-asm-lang-v4 -> para-asm-lang-v4
;; interp. flatten begin statements in the program
(define/contract (flatten-program p)
  (-> block-asm-lang-v6? para-asm-lang-v6?)

  ;; block-asm-lang-v4-b -> (list para-asm-lang-v4-s)
  ;; convert b expressions to flattened s expressions
  (define (flatten-program/b b)
    (match b
      [`(define ,label ,tail)
       (define compiled-s (flatten-program/tail tail))
       (cons `(with-label ,label ,(first compiled-s)) (rest compiled-s))]))

  ;; block-asm-lang-v4-tail -> (list para-asm-lang-v4-s)
  ;; interp. flattens the tail expression into a list of s statements
  (define (flatten-program/tail tail)
    (match tail
      [`(halt ,opand) (list `(halt, opand))]
      [`(jump ,trg) (list `(jump ,trg))]
      [`(begin ,fx ... ,tail)
       ;; Note that block-asm-lang-v4-effect expressions are already para-asm-lang-v4-s expressions
       (append fx (flatten-program/tail tail))]
      [`(if (,relop ,loc ,opand) (jump ,trg1) (jump ,trg2))
       (list `(compare ,loc ,opand) `(jump-if ,relop ,trg1) `(jump ,trg2))]))

  (match p
    [`(module ,bs ...)
     (define compiled-s (for/fold ([s empty])
                                  ([b bs])
                          (append s (flatten-program/b b))))
     `(begin ,@compiled-s)]))

(test-case
 "flatten-program"
 (let ([x (fresh-label)])
   (check-equal? (flatten-program `(module (define ,x (jump ,x)))) `(begin (with-label ,x (jump ,x)))))
 (let ([x (fresh-label)])
   (check-equal? (flatten-program `(module (define ,x (begin (set! rbx 1) (jump ,x)))))
                 `(begin (with-label ,x (set! rbx 1)) (jump ,x))))
 (let ([x (fresh-label)])
   (check-equal? (flatten-program `(module (define ,x (begin (set! rax 0) (begin (set! rbx 1) (jump ,x))))))
                 `(begin (with-label ,x (set! rax 0)) (set! rbx 1) (jump ,x))))
 (let ([x (fresh-label)])
   (check-equal? (flatten-program `(module (define ,x (begin (begin (set! rax 0) (jump ,x))))))
                 `(begin (with-label ,x (set! rax 0)) (jump ,x))))
 (let ([x (fresh-label)]
       [y (fresh-label)])
   (check-equal? (flatten-program `(module (define ,x (jump ,y)) (define ,y (jump ,x))))
                 `(begin (with-label ,x (jump ,y)) (with-label ,y (jump ,x)))))
 (let ([x (fresh-label)]
       [y (fresh-label)]
       [z (fresh-label)])
   (check-equal? (flatten-program `(module (define ,x (if (< r9 0) (jump ,y) (jump ,z)))
                                     (define ,y (jump r8))
                                     (define ,z (begin (set! r9 (+ r9 -1)) (jump ,x)))))
                 `(begin (with-label ,x (compare r9 0))
                         (jump-if < ,y)
                         (jump ,z)
                         (with-label ,y (jump r8))
                         (with-label ,z (set! r9 (+ r9 -1)))
                         (jump ,x))))
 (let ([x (fresh-label)]
       [y (fresh-label)]
       [z (fresh-label)])
   (check-equal? (flatten-program `(module (define ,x (begin (set! r9 0) (jump ,y)))
                                     (define ,y (begin (set! r13 0) (jump ,z)))
                                     (define ,z (jump r8))))
                 `(begin (with-label ,x (set! r9 0))
                         (jump ,y)
                         (with-label ,y (set! r13 0))
                         (jump ,z)
                         (with-label ,z (jump r8))))))
