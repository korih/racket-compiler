#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v7
  rackunit)

(provide generate-x64)

;; paren-x64-v7 -> string
;; compiles p into a valid sequence of x64 instructions, represented as a string
(define/contract (generate-x64 p)
  (-> paren-x64-v7? string?)

  ;; paren-x64-v7 -> string
  (define (program->x64 p)
    (match p
      [`(begin ,s ...)
       (string-join (map statement->x64 s) "\n")]))

  ;; paren-x64-v7.s -> string
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
       (format "~a:\n~a" (sanitize-label label) (statement->x64 s))]
      [`(jump ,trg)
       (format "jmp ~a" (trg->x64 trg))]
      [`(compare ,reg ,op)
       (format "cmp ~a, ~a" reg (opand->x64 op))]
      [`(jump-if ,relop ,label)
       (format "~a ~a" (relop->ins relop) (sanitize-label label))]))

  ;; paren-x64-v7.trg -> string
  (define (trg->x64 trg)
    (match trg
      [label #:when (label? label) (sanitize-label label)]
      [reg #:when (register? reg) reg]))

  ;; paren-x64-v7.triv -> string
  (define (triv->x64 triv)
    (match triv
      [int64 #:when (int64? int64) int64]
      [trg (trg->x64 trg)]))

  ;; paren-x64-v7.opand -> string
  (define (opand->x64 op)
    (match op
      [int64 #:when (int64? int64) int64]
      [reg #:when (register? reg) reg]))

  ;; paren-x64-v7.loc -> string
  (define (loc->x64 loc)
    (match loc
      [reg #:when (register? reg) reg]
      [`(,fbp - ,offset) (format "QWORD [~a - ~a]" fbp offset)]))

  ;; paren-x64-v7.binop -> string
  (define (binop->ins b)
    (match b
      ['+ "add"]
      ['* "imul"]
      ['- "sub"]
      ['bitwise-and "and"]
      ['bitwise-ior "or"]
      ['bitwise-xor "xor"]
      ['arithmetic-shift-right "sar"]))

  ;; paren-x64-v7.relop -> string
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
                 "\n"))
  (check-equal? (generate-x64 '(begin
                                 (with-label L.tmp.99 (set! rbx r15))
                                 (set! rcx 10)
                                 (set! rsp 100)
                                 (compare rcx rsp)
                                 (jump-if != L.tmp.101)
                                 (jump L.tmp.100)
                                 (with-label L.tmp.101 (set! rdi 1000))
                                 (set! r15 rbx)
                                 (jump L.f.2)
                                 (with-label L.tmp.100 (set! rdi rcx))
                                 (set! r15 rbx)
                                 (jump L.f.1)
                                 (with-label L.f.1 (set! rsp r15))
                                 (set! rcx rdi)
                                 (set! rdx 1)
                                 (set! rbx 2)
                                 (set! rdx rdx)
                                 (set! rdx (bitwise-and rdx rcx))
                                 (set! rbx rbx)
                                 (set! rbx (bitwise-ior rbx rcx))
                                 (set! rdx (bitwise-xor rdx rbx))
                                 (set! rax rdx)
                                 (set! rax (arithmetic-shift-right rax 3))
                                 (jump rsp)))
                (string-join
                 (list "L.tmp.99:"
                       "mov rbx, r15"
                       "mov rcx, 10"
                       "mov rsp, 100"
                       "cmp rcx, rsp"
                       "jne L.tmp.101"
                       "jmp L.tmp.100"
                       "L.tmp.101:"
                       "mov rdi, 1000"
                       "mov r15, rbx"
                       "jmp L.f.2"
                       "L.tmp.100:"
                       "mov rdi, rcx"
                       "mov r15, rbx"
                       "jmp L.f.1"
                       "L.f.1:"
                       "mov rsp, r15"
                       "mov rcx, rdi"
                       "mov rdx, 1"
                       "mov rbx, 2"
                       "mov rdx, rdx"
                       "and rdx, rcx"
                       "mov rbx, rbx"
                       "or rbx, rcx"
                       "xor rdx, rbx"
                       "mov rax, rdx"
                       "sar rax, 3"
                       "jmp rsp")
                 "\n"))
  (check-equal? (generate-x64 '(begin (with-label L.tmp.89 (set! (rbp - 8) r15)) (set! rbp (- rbp 16)) (set! rdi 40) (set! rsi 48) (set! r15 L.rp.19) (jump L.+.16) (with-label L.rp.19 (set! rbp (+ rbp 16))) (set! (rbp - 0) rax) (set! rbp (- rbp 16)) (set! rdi 32) (set! rsi 40) (set! r15 L.rp.20) (jump L.*.17) (with-label L.rp.20 (set! rbp (+ rbp 16))) (set! rsp rax) (set! rdi (rbp - 0)) (set! rsi rsp) (set! r15 (rbp - 8)) (jump L.+.16) (with-label L.+.16 (set! rcx r15)) (set! rsp rdi) (set! rdx rsi) (set! rbx rdx) (set! rbx (bitwise-and rbx 7)) (compare rbx 0) (jump-if = L.tmp.87) (jump L.tmp.88) (with-label L.tmp.86 (compare rbx 6)) (jump-if != L.tmp.79) (jump L.tmp.80) (with-label L.tmp.88 (set! rbx 6)) (jump L.tmp.86) (with-label L.tmp.87 (set! rbx 14)) (jump L.tmp.86) (with-label L.tmp.80 (set! rax 574)) (jump rcx) (with-label L.tmp.79 (set! rbx rsp)) (set! rbx (bitwise-and rbx 7)) (compare rbx 0) (jump-if = L.tmp.84) (jump L.tmp.85) (with-label L.tmp.83 (compare rbx 6)) (jump-if != L.tmp.81) (jump L.tmp.82) (with-label L.tmp.85 (set! rbx 6)) (jump L.tmp.83) (with-label L.tmp.84 (set! rbx 14)) (jump L.tmp.83) (with-label L.tmp.82 (set! rax 574)) (jump rcx) (with-label L.tmp.81 (set! rax rsp)) (set! rax (+ rax rdx)) (jump rcx) (with-label L.*.17 (set! rdx r15)) (set! rbx rdi) (set! rcx rsi) (set! rsp rcx) (set! rsp (bitwise-and rsp 7)) (compare rsp 0) (jump-if = L.tmp.77) (jump L.tmp.78) (with-label L.tmp.76 (compare rsp 6)) (jump-if != L.tmp.69) (jump L.tmp.70) (with-label L.tmp.78 (set! rsp 6)) (jump L.tmp.76) (with-label L.tmp.77 (set! rsp 14)) (jump L.tmp.76) (with-label L.tmp.70 (set! rax 318)) (jump rdx) (with-label L.tmp.69 (set! rsp rbx)) (set! rsp (bitwise-and rsp 7)) (compare rsp 0) (jump-if = L.tmp.74) (jump L.tmp.75) (with-label L.tmp.73 (compare rsp 6)) (jump-if != L.tmp.71) (jump L.tmp.72) (with-label L.tmp.75 (set! rsp 6)) (jump L.tmp.73) (with-label L.tmp.74 (set! rsp 14)) (jump L.tmp.73) (with-label L.tmp.72 (set! rax 318)) (jump rdx) (with-label L.tmp.71 (set! rsp rcx)) (set! rsp (arithmetic-shift-right rsp 3)) (set! rax rbx) (set! rax (* rax rsp)) (jump rdx)))
                (string-join
                 (list
                  "L.tmp.89:"
                  "mov QWORD [rbp - 8], r15"
                  "sub rbp, 16"
                  "mov rdi, 40"
                  "mov rsi, 48"
                  "lea r15, [rel L.rp.19]"
                  "jmp L.$2b$.16"
                  "L.rp.19:"
                  "add rbp, 16"
                  "mov QWORD [rbp - 0], rax"
                  "sub rbp, 16"
                  "mov rdi, 32"
                  "mov rsi, 40"
                  "lea r15, [rel L.rp.20]"
                  "jmp L.$2a$.17"
                  "L.rp.20:"
                  "add rbp, 16"
                  "mov rsp, rax"
                  "mov rdi, QWORD [rbp - 0]"
                  "mov rsi, rsp"
                  "mov r15, QWORD [rbp - 8]"
                  "jmp L.$2b$.16"
                  "L.$2b$.16:"
                  "mov rcx, r15"
                  "mov rsp, rdi"
                  "mov rdx, rsi"
                  "mov rbx, rdx"
                  "and rbx, 7"
                  "cmp rbx, 0"
                  "je L.tmp.87"
                  "jmp L.tmp.88"
                  "L.tmp.86:"
                  "cmp rbx, 6"
                  "jne L.tmp.79"
                  "jmp L.tmp.80"
                  "L.tmp.88:"
                  "mov rbx, 6"
                  "jmp L.tmp.86"
                  "L.tmp.87:"
                  "mov rbx, 14"
                  "jmp L.tmp.86"
                  "L.tmp.80:"
                  "mov rax, 574"
                  "jmp rcx"
                  "L.tmp.79:"
                  "mov rbx, rsp"
                  "and rbx, 7"
                  "cmp rbx, 0"
                  "je L.tmp.84"
                  "jmp L.tmp.85"
                  "L.tmp.83:"
                  "cmp rbx, 6"
                  "jne L.tmp.81"
                  "jmp L.tmp.82"
                  "L.tmp.85:"
                  "mov rbx, 6"
                  "jmp L.tmp.83"
                  "L.tmp.84:"
                  "mov rbx, 14"
                  "jmp L.tmp.83"
                  "L.tmp.82:"
                  "mov rax, 574"
                  "jmp rcx"
                  "L.tmp.81:"
                  "mov rax, rsp"
                  "add rax, rdx"
                  "jmp rcx"
                  "L.$2a$.17:"
                  "mov rdx, r15"
                  "mov rbx, rdi"
                  "mov rcx, rsi"
                  "mov rsp, rcx"
                  "and rsp, 7"
                  "cmp rsp, 0"
                  "je L.tmp.77"
                  "jmp L.tmp.78"
                  "L.tmp.76:"
                  "cmp rsp, 6"
                  "jne L.tmp.69"
                  "jmp L.tmp.70"
                  "L.tmp.78:"
                  "mov rsp, 6"
                  "jmp L.tmp.76"
                  "L.tmp.77:"
                  "mov rsp, 14"
                  "jmp L.tmp.76"
                  "L.tmp.70:"
                  "mov rax, 318"
                  "jmp rdx"
                  "L.tmp.69:"
                  "mov rsp, rbx"
                  "and rsp, 7"
                  "cmp rsp, 0"
                  "je L.tmp.74"
                  "jmp L.tmp.75"
                  "L.tmp.73:"
                  "cmp rsp, 6"
                  "jne L.tmp.71"
                  "jmp L.tmp.72"
                  "L.tmp.75:"
                  "mov rsp, 6"
                  "jmp L.tmp.73"
                  "L.tmp.74:"
                  "mov rsp, 14"
                  "jmp L.tmp.73"
                  "L.tmp.72:"
                  "mov rax, 318"
                  "jmp rdx"
                  "L.tmp.71:"
                  "mov rsp, rcx"
                  "sar rsp, 3"
                  "mov rax, rbx"
                  "imul rax, rsp"
                  "jmp rdx")
                 "\n")))