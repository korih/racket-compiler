#lang racket

(require "link-paren-x64.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4
  rackunit)

(provide interp-paren-x64)

;; Template support macro; feel free to delete
(define-syntax-rule (.... stx ...)
  (error "Unfinished template"))

;; Exercise 3
;; paren-x64-v4 -> int64
;; interpret the Paren-x64 v4 program p as a value, returning the final value of
;; rax
(define/contract (interp-paren-x64 p)
  (-> paren-x64-v4? int64?)

  ;; dict-of(loc -> int64) Natural (listof statement) statement -> int64
  ;; Runs statement `s`, which is expected to be the `pc`th instruction of
  ;; `los`, modifying the environment and incrementing the program counter,
  ;; before executing the next instruction in `los`.
  (define (eval-statement env pc los s)
    (....
     (eval-program (.... env) (.... (add1 pc)) los)))

  ;; dict-of(loc -> int64) Natural (listof statements) -> int64
  ;; Runs the program represented by `los` starting from instruction number
  ;; indicated by the program counter `pc`, represented as a natural number.
  ;; Program is finished when `pc` reaches the final instruction of `los`.
  (define (eval-program env pc los)
    (if (= pc (length los))
        (dict-ref env 'rax)
        (eval-statement env pc los (list-ref los pc))))

  (TODO "Redesign and implement interp-paren-x64 for Exercise 3."))

(module+ test
  (check-equal? (interp-paren-x64 '(begin
                                     (with-label L.tmp.1 (set! rax (+ rax rax)))
                                     (set! rax 1)
                                     (with-label L.tmp.2 (jump L.tmp.3))
                                     (with-label L.tmp.3 (set! rax (* rax 10)))
                                     (compare rax 2)
                                     (jump-if != L.tmp.2)))
                #f))