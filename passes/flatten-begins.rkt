#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2
  rackunit)

(provide flatten-begins)

;; nested-asm-lang-v2 -> para-asm-lang-v2
;; interp. flatten begin statements in the program
(define/contract (flatten-begins p)
  (-> nested-asm-lang-v2? para-asm-lang-v2?)

  ;; nested-asm-lang-v2-effect -> (list param-asm-lang-v2-effect)
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
 (check-equal? (flatten-begins '(halt 1)) '(begin (halt 1)))
 (check-equal? (flatten-begins '(begin (set! rbx 1) (halt rbx))) '(begin (set! rbx 1) (halt rbx)))
 (check-equal? (flatten-begins '(begin (set! rax 0) (begin (set! rbx 1) (halt rbx))))
               '(begin (set! rax 0) (set! rbx 1) (halt rbx)))
 (check-equal? (flatten-begins '(begin (begin (set! rax 0)) (halt rax))) '(begin (set! rax 0) (halt rax))))
