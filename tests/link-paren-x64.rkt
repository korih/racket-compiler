#lang racket

(require
  "../passes/link-paren-x64.rkt"
  rackunit)

(module+ test
  (check-equal? (link-paren-x64 '(begin
                                   (set! rax 1)
                                   (with-label L.tmp.1 (set! rax 2))
                                   (compare rax 2)
                                   (jump-if > L.tmp.1)))
                '(begin (set! rax 1) (set! rax 2) (compare rax 2) (jump-if > 1)))
  (check-equal? (link-paren-x64 '(begin
                                   (with-label L.tmp.1
                                     (with-label L.tmp.2 (jump L.tmp.1)))
                                   (jump L.tmp.1)))
                '(begin (jump 0) (jump 0)))
  (check-equal? (link-paren-x64 '(begin
                                   (with-label L.tmp.1
                                     (with-label L.tmp.2
                                       (with-label L.tmp.3 (jump L.tmp.1))))
                                   (jump L.tmp.1)))
                '(begin (jump 0) (jump 0)))
  (check-equal? (link-paren-x64 '(begin
                                   (with-label L.tmp.1
                                     (with-label L.tmp.2
                                       (with-label L.tmp.3 (set! rax 1))))
                                   (jump L.tmp.1)))
                '(begin (set! rax 1) (jump 0)))
  (check-equal? (link-paren-x64 '(begin
                                   (with-label L.tmp.1 (set! rax 1))
                                   (with-label L.tmp.2 (jump L.tmp.3))
                                   (with-label L.tmp.3 (jump L.tmp.1))
                                   (with-label L.tmp.4
                                     (with-label L.tmp.5 (jump L.tmp.6)))
                                   (with-label L.tmp.6 (set! rax 1))
                                   (jump L.tmp.3)
                                   (compare rax 2)
                                   (jump-if > L.tmp.4)))
                '(begin
                   (set! rax 1)
                   (jump 2)
                   (jump 0)
                   (jump 4)
                   (set! rax 1)
                   (jump 2)
                   (compare rax 2)
                   (jump-if > 3))))
