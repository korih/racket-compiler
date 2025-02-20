#lang racket

(provide optimize-predicates)

(require
 cpsc411/langs/v4)

;; Exercise 11
;; nested-asm-lang-v4 -> nested-asm-lang-v4
;; simplifies the predicates in the program
(define/contract (optimize-predicates p)
  (-> nested-asm-lang-v4? nested-asm-lang-v4?)
  (void))

(module+ test
  (require rackunit)

  (check-equal? (optimize-predicates '(module (if (true) (halt 0) (halt 1)))) '(module (halt 0)))
  (check-equal? (optimize-predicates '(module (if (false) (begin (set! rax 1) (halt rax)) (halt 0))))
                '(module (halt 0)))
  (check-equal? (optimize-predicates '(module (if (not (true)) (halt 0) (begin (set! rax 1) (halt rax)))))
                '(module (begin (set! rax 1) (halt rax))))
  (check-equal? (optimize-predicates '(module (if (if (true) (false) (true)) (halt 0) (halt 1))))
                '(module (halt 1)))
  (check-equal? (optimize-predicates '(module (begin (set! rax 1) (if (> rax 0) (halt 0) (halt 1)))))
                '(module (begin (set! rax 1) (halt 0)))))
