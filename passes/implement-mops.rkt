#lang racket

(require
  cpsc411/langs/v8)

(provide implement-mops)

;; paren-x64-mops-v8 -> paren-x64-v8
;; Compiles mops to instructions on pointers with index-
;; and displacement-mode operands.
(define/contract (implement-mops p)
  (-> paren-x64-mops-v8? paren-x64-v8?)

  ;; paren-x64-mops-v8.s -> paren-x64-v8.s
  ;; Translate mops into index-mode and displacement-mode operands
  (define (compile-s s)
    (match s
      [`(set! ,reg_1 (mref ,reg_2 ,index)) `(set! ,reg_1 (,reg_2 + ,index))]
      [`(mset! ,reg_1 ,index ,int32) `(set! (,reg_1 + ,index) ,int32)]
      [`(mset! ,reg_1 ,index ,trg) `(set! (,reg_1 + ,index) ,trg)]

      ;; Wildcard, rest of s are not mops that would require a transformation
      [_ s]))

  (match p
    [`(begin ,s ...) `(begin ,@(map compile-s s))]))

(module+ test
  (require rackunit)
  (check-equal? (implement-mops '(begin (set! rax 0)))
                '(begin (set! rax 0)))
  (check-equal? (implement-mops '(begin (mset! rax 0 1)))
                '(begin (set! (rax + 0) 1)))
  (check-equal? (implement-mops '(begin (mset! rax 0 rax)))
                '(begin (set! (rax + 0) rax)))
  (check-equal? (implement-mops '(begin (set! rax (mref rax 0))))
                '(begin (set! rax (rax + 0))))
  (check-equal? (implement-mops '(begin
                                   (set! rbx 10)
                                   (set! rbp 0)
                                   (set! rdi 0)
                                   (set! rsi 4)
                                   (with-label L.tmp.1 (set! rbx (- rbx 1)))
                                   (jump-if = L.tmp.2)
                                   (set! rbp (+ rbp rbx))
                                   (mset! rdi 0 rbp)
                                   (set! rdi (+ rdi rsi))
                                   (jump L.tmp.1)
                                   (with-label L.tmp.2 (set! rdi 0))
                                   (set! rax (mref rdi 0))))
                '(begin
                   (set! rbx 10)
                   (set! rbp 0)
                   (set! rdi 0)
                   (set! rsi 4)
                   (with-label L.tmp.1 (set! rbx (- rbx 1)))
                   (jump-if = L.tmp.2)
                   (set! rbp (+ rbp rbx))
                   (set! (rdi + 0) rbp)
                   (set! rdi (+ rdi rsi))
                   (jump L.tmp.1)
                   (with-label L.tmp.2 (set! rdi 0))
                   (set! rax (rdi + 0)))))
