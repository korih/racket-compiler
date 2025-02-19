#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4
  rackunit)

(provide flatten-begins)

;; block-asm-lang-v4 -> para-asm-lang-v4
;; interp. flatten begin statements in the program
(define/contract (flatten-begins p)
  (-> block-asm-lang-v4? para-asm-lang-v4?)

  ;; interp. flatten begin statements in the program into a list of effect
  ;; statements
  (define (flatten-begins/effect e)
    (match e
      [`(set! ,x (,binop ,x ,v)) (list `(set! ,x (,binop ,x ,v)))]
      [`(set! ,x ,v) (list `(set! ,x ,v))]
      [`(begin ,fx ... ,e)
       (define compiled-fx (for/foldr ([fx-acc empty])
                             ([e fx])
                             (append (flatten-begins/effect e) fx-acc)))
       (append compiled-fx (flatten-begins/effect e))]))

  (match p
    [`(halt ,triv) `(begin (halt ,triv))]
    [`(begin ,fx ... ,tail)
     (define compiled-fx (for/foldr ([fx-acc empty])
                           ([e fx])
                           (append (flatten-begins/effect e) fx-acc)))
     (make-begin compiled-fx (flatten-begins tail))]))

(test-case
 "flatten-begins"
 (check-equal? (flatten-begins '(module (halt 1))) '(begin (halt 1)))
 (let ([x (symbol->string (gensym))])
   (check-equal? (flatten-begins `(module (define ,x (begin (set! rbx 1) (halt rbx))))
                                '(begin (with-label ,x (set! rbx 1)) (halt rbx)))))
 (let ([x (symbol->string (gensym))])
   (check-equal? (flatten-begins `(module (define ,x (begin (set! rax 0) (begin (set! rbx 1) (halt rbx))))))
                 `(begin (with-label ,x (set! rax 0)) (set! rbx 1) (halt rbx))))
 (let ([x (symbol->string (gensym))])
   (check-equal? (flatten-begins `(module (define ,x (begin (begin (set! rax 0)) (halt rax))))
                                 `(begin (with-label ,x (set! rax 0)) (halt rax)))))
 (let ([x (symbol->string (gensym))]
       [y (symbol->string (gensym))])
   (check-equal? (flatten-begins `(module (define ,x (jump ,y)) (define ,y (halt 0))))
                 `(begin (with-label ,x (jump ,y)) (with-label ,y (halt 0))))))
