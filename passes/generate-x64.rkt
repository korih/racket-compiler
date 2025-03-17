#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v6
  rackunit)

(provide generate-x64)

;; paren-x64-v6 -> string
;; compiles p into a valid sequence of x64 instructions, represented as a string
(define/contract (generate-x64 p)
  (-> paren-x64-v6? string?)

  ;; paren-x64-v6 -> string
  (define (program->x64 p)
    (match p
      [`(begin ,s ...)
       (string-join (map statement->x64 s) "\n")]))

  ;; paren-x64-v6.s -> string
  (define (statement->x64 s)
    (match s
      [`(set! (,fbp - ,offset) ,int32)
       #:when (int32? int32)
       (format "mov QWORD [~a - ~a], ~a" fbp offset int32)]
      [`(set! (,fbp - ,offset) ,trg)
       (format "mov QWORD [~a - ~a], ~a" fbp offset (trg->x64 trg))]
      [`(set! ,reg (,binop ,reg ,int32))
       #:when (int32? int32)
       (format "~a ~a, ~a" (binop->ins binop) reg int32)]
      [`(set! ,reg (,binop ,reg ,loc))
       #:when (or (register? loc) (addr? loc))
       (format "~a ~a, ~a" (binop->ins binop) reg (loc->x64 loc))]
      [`(set! ,reg ,loc)
       #:when (or (register? loc) (addr? loc))
       (format "mov ~a, ~a" reg (loc->x64 loc))]
      [`(set! ,reg ,triv)
       (cond
         [(label? triv) (format "lea ~a, [rel ~a]" reg (triv->x64 triv))]
         [else (format "mov ~a, ~a" reg (triv->x64 triv))])]
      [`(with-label ,label ,s)
       (format "~a:\n~a" (symbol->string label) (statement->x64 s))]
      [`(jump ,trg)
       (format "jmp ~a" trg)]
      [`(compare ,reg ,op)
       (format "cmp ~a, ~a" reg op)]
      [`(jump-if ,relop ,label)
       (format "~a ~a" (relop->ins relop) (symbol->string label))]))

  ;; paren-x64-v6.trg -> string
  (define (trg->x64 trg)
    (match trg
      [label #:when (label? label) (sanitize-label label)]
      [reg #:when (register? reg) reg]))

  ;; paren-x64-v6.triv -> string
  (define (triv->x64 triv)
    (match triv
      [int64 #:when (int64? int64) int64]
      [trg (trg->x64 trg)]))

  ;; paren-x64-v6.opand -> string
  (define (opand->x64 op)
    (match op
      [int64 #:when (int64? int64) int64]
      [reg #:when (register? reg) reg]))

  ;; paren-x64-v6.loc -> string
  (define (loc->x64 loc)
    (match loc
      [reg #:when (register? reg) reg]
      [`(,fbp - ,offset) (format "QWORD [~a - ~a]" fbp offset)]))

  ;; paren-x64-v6.binop -> string
  (define (binop->ins b)
    (match b
      ['+ "add"]
      ['* "imul"]
      ['- "sub"]))

  ;; paren-x64-v4.relop -> string
  (define (relop->ins relop)
    (match relop
      [`< "jl"]
      [`<= "jle"]
      [`= "je"]
      [`> "jg"]
      [`>= "jge"]
      [`!= "jne"]))

  (program->x64 p))

(module+ test
  (check-equal? (generate-x64 '(begin (set! rax (- rax 1))))
                "sub rax, 1")
  (check-equal? (generate-x64 '(begin (set! rsi L.label.1) (with-label L.label.1 (set! rbx 18)) (set! rax rbx) (jump done)))
                (string-join
                 (list "lea rsi, [rel L.label.1]"
                       "L.label.1:"
                       "mov rbx, 18"
                       "mov rax, rbx"
                       "jmp done")
                 "\n"))
  (check-equal? (generate-x64 '(begin (set! (rbp - 8) L.label.1) (with-label L.label.1 (set! rbx 18)) (set! rax rbx) (jump done)))
                (string-join
                 (list "mov QWORD [rbp - 8], L.label.1"
                       "L.label.1:"
                       "mov rbx, 18"
                       "mov rax, rbx"
                       "jmp done")
                 "\n"))
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
    "\n"))
  (check-equal? (generate-x64 '(begin (jump rax)))
                "jmp rax")
  (check-equal? (generate-x64 '(begin (jump-if > L.start.1)))
                "jg L.start.1")
  (check-equal? (generate-x64 '(begin (with-label L.start.1 (jump L.start.1))))
                "L.start.1:\njmp L.start.1")
  (check-equal? (generate-x64 '(begin (compare rax 2)))
                "cmp rax, 2")
  (check-equal? (generate-x64 '(begin 
                                 (set! rbx 10)
                                 (compare rbx 0)
                                 (with-label L.start.1 (jump-if = L.end.1))
                                 (set! rbx (+ rbx -1))
                                 (jump L.start.1)
                                 (with-label L.end.1 (set! rax rbx))))
                (string-join
                 (list "mov rbx, 10"
                       "cmp rbx, 0"
                       "L.start.1:"
                       "je L.end.1"
                       "add rbx, -1"
                       "jmp L.start.1"
                       "L.end.1:"
                       "mov rax, rbx")
                 "\n")))