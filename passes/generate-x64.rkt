#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2
  rackunit)

(provide generate-x64)

;; paren-x64-v2 -> string
;; compiles p into a valid sequence of x64 instructions, represented as a string
(define/contract (generate-x64 p)
  (-> paren-x64-v2? string?)

  ;; paren-x64-v2 -> string
  (define (program->x64 p)
    (match p
      [`(begin ,s ...)
       (string-join (map statement->x64 s) "\n")]))

  ;; paren-x64-v2.s -> string
  (define (statement->x64 s)
    (match s
      [`(set! (,fbp - ,offset) ,int32)
       #:when (int32? int32)
       (format "mov QWORD [~a - ~a], ~a" fbp offset int32)]
      [`(set! (,fbp - ,offset) ,reg)
       #:when (register? reg)
       (format "mov QWORD [~a - ~a], ~a" fbp offset reg)]
      [`(set! ,reg ,loc)
       #:when (or (register? loc) (addr? loc))
       (format "mov ~a, ~a" reg (loc->x64 loc))]
      [`(set! ,reg ,triv)
       #:when (or (register? triv) (int64? triv))
       (format "mov ~a, ~a" reg triv)]
      [`(set! ,reg (,binop ,reg ,int32))
       #:when (int32? int32)
       (format "~a ~a, ~a" (binop->ins binop) reg int32)]
      [`(set! ,reg (,binop ,reg ,loc))
       #:when (or (register? loc) (addr? loc))
       (format "~a ~a, ~a" (binop->ins binop) reg (loc->x64 loc))]))

  ;; paren-x64-v2.loc -> string
  (define (loc->x64 loc)
    (match loc
      [reg #:when (register? reg) reg]
      [`(,fbp - ,offset) (format "QWORD [~a - ~a]" fbp offset)]))

  ;; paren-x64-v2.binop -> string
  (define (binop->ins b)
    (match b
      ['+ "add"]
      ['* "imul"]))

  (program->x64 p))

(module+ test
  (check-equal? (generate-x64 '(begin))
                "")
  (check-equal? (generate-x64 '(begin (set! (rbp - 8) 42)))
                "mov QWORD [rbp - 8], 42")
  (check-equal? (generate-x64 '(begin (set! (rbp - 16) rax)))
                "mov QWORD [rbp - 16], rax")
  (check-equal? (generate-x64 '(begin (set! rax rbx)))
                "mov rax, rbx")
  (check-equal? (generate-x64 '(begin (set! rax (rbp - 8))))
                "mov rax, QWORD [rbp - 8]")
  (check-equal? (generate-x64 '(begin (set! rax 170679)))
                "mov rax, 170679")
  (check-equal? (generate-x64 '(begin (set! rax (+ rax 5))))
                "add rax, 5")
  (check-equal? (generate-x64 '(begin (set! rax (+ rax rbx))))
                "add rax, rbx")
  (check-equal? (generate-x64 '(begin (set! rax (+ rax (rbp - 8)))))
                "add rax, QWORD [rbp - 8]")
  (check-equal?
   (generate-x64
    '(begin
       (set! rax 1)
       (set! rax (+ rax 2))
       (set! rax (+ rax 3))
       (set! rax (+ rax 4))
       (set! rax (+ rax 5))))
   (string-join
    (list "mov rax, 1"
          "add rax, 2"
          "add rax, 3"
          "add rax, 4"
          "add rax, 5")
    "\n"))
  (check-equal?
   (generate-x64
    '(begin
       (set! rax (rbp - 8))
       (set! rbx (rbp - 16))
       (set! rax (+ rax (rbp - 8)))
       (set! rax (+ rax rbx))))
   (string-join
    (list "mov rax, QWORD [rbp - 8]"
          "mov rbx, QWORD [rbp - 16]"
          "add rax, QWORD [rbp - 8]"
          "add rax, rbx")
    "\n")))