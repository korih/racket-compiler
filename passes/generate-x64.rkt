#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide generate-x64)

;; paren-x64-v8 -> string
;; compiles p into a valid sequence of x64 instructions, represented as a string
(define/contract (generate-x64 p)
  (-> paren-x64-v8? string?)

  ;; paren-x64-v8 -> string
  ;; interp. compiles a Paren-x64 program into a newline-separated string of x64 instructions
  (define (program->x64 p)
    (match p
      [`(begin ,s ...)
       (string-join (map statement->x64 s) "\n")]))

  ;; paren-x64-v8.s -> string
  ;; interp. compiles a single Paren-x64 statement into x64 assembly syntax
  (define (statement->x64 s)
    (match s
      [`(set! ,addr ,int32)
       #:when (and (addr-mop? addr) (int32? int32))
       (format "mov ~a, ~a" (addr->x64 addr) int32)]
      [`(set! ,addr ,trg)
       #:when (addr-mop? addr)
       (format "mov ~a, ~a" (addr->x64 addr) (trg->x64 trg))]
      [`(set! ,reg (,binop ,reg ,int32))
       #:when (int32? int32)
       (format "~a ~a, ~a" (binop->ins binop) reg int32)]
      [`(set! ,reg (,binop ,reg ,loc))
       #:when (or (register? loc) (addr-mop? loc))
       (format "~a ~a, ~a" (binop->ins binop) reg (loc->x64 loc))]
      [`(set! ,reg ,loc)
       #:when (or (register? loc) (addr-mop? loc))
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

  ;; paren-x64-v8.trg -> string
  ;; interp. converts a jump target into its x64 representation
  (define (trg->x64 trg)
    (match trg
      [label #:when (label? label) (sanitize-label label)]
      [reg #:when (register? reg) reg]))

  ;; paren-x64-v8.triv -> string
  ;; interp. converts a trivial operand into x64 syntax
  (define (triv->x64 triv)
    (match triv
      [int64 #:when (int64? int64) int64]
      [trg (trg->x64 trg)]))

  ;; paren-x64-v8.opand -> string
  ;; interp. converts an operand into x64 syntax
  (define (opand->x64 op)
    (match op
      [int64 #:when (int64? int64) int64]
      [reg #:when (register? reg) reg]))

  ;; paren-x64-v8.loc -> string
  ;; interp. converts a location into x64 syntax
  (define (loc->x64 loc)
    (match loc
      [reg #:when (register? reg) reg]
      [addr (addr->x64 addr)]))

  ;; paren-x64-v8.addr -> string
  ;; interp. converts a memory address in Paren-x64 into x64 addressing mode syntax 
  (define (addr->x64 addr)
    (match addr
      [`(,fbp - ,dispoffset)
       #:when (and (frame-base-pointer-register? fbp) (dispoffset? dispoffset))
       (format "QWORD [~a - ~a]" fbp dispoffset)]
      [`(,reg + ,int32)
       #:when (and (register? reg) (int32? int32))
       (format "QWORD [~a + ~a]" reg int32)]
      [`(,reg1 + ,reg2)
       #:when (and (register? reg1) (register? reg2))
       (format "QWORD [~a + ~a]" reg1 reg2)]))

  ;; paren-x64-v8.binop -> string
  ;; interp. maps Paren-x64 binary operators to corresponding x64 instructions
  (define (binop->ins b)
    (match b
      ['+ "add"]
      ['* "imul"]
      ['- "sub"]
      ['bitwise-and "and"]
      ['bitwise-ior "or"]
      ['bitwise-xor "xor"]
      ['arithmetic-shift-right "sar"]))

  ;; paren-x64-v8.relop -> string
  ;; interp. maps Paren-x64 relational operators to x64 conditional jump mnemonics
  (define (relop->ins relop)
    (match relop
      [`< "jl"]
      [`<= "jle"]
      [`= "je"]
      [`> "jg"]
      [`>= "jge"]
      [`!= "jne"]))

  (program->x64 p))

