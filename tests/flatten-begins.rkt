#lang racket

(require
  rackunit
  "../passes/flatten-begins.rkt")

(module+ test
  (check-equal? (flatten-begins '(halt 1)) '(begin (halt 1)))
  (check-equal? (flatten-begins '(begin (set! rbx 1) (halt rbx))) '(begin (set! rbx 1) (halt rbx)))
  (check-equal? (flatten-begins '(begin (set! rax 0) (begin (set! rbx 1) (halt rbx))))
                '(begin (set! rax 0) (set! rbx 1) (halt rbx)))
  (check-equal? (flatten-begins '(begin (begin (set! rax 0)) (halt rax))) '(begin (set! rax 0) (halt rax))))
