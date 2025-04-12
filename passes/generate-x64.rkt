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
  (define (program->x64 p)
    (match p
      [`(begin ,s ...)
       (string-join (map statement->x64 s) "\n")]))

  ;; paren-x64-v8.s -> string
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
  (define (trg->x64 trg)
    (match trg
      [label #:when (label? label) (sanitize-label label)]
      [reg #:when (register? reg) reg]))

  ;; paren-x64-v8.triv -> string
  (define (triv->x64 triv)
    (match triv
      [int64 #:when (int64? int64) int64]
      [trg (trg->x64 trg)]))

  ;; paren-x64-v8.opand -> string
  (define (opand->x64 op)
    (match op
      [int64 #:when (int64? int64) int64]
      [reg #:when (register? reg) reg]))

  ;; paren-x64-v8.loc -> string
  (define (loc->x64 loc)
    (match loc
      [reg #:when (register? reg) reg]
      [addr (addr->x64 addr)]))

  ;; paren-x64-v8.addr -> string
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
  (define (relop->ins relop)
    (match relop
      [`< "jl"]
      [`<= "jle"]
      [`= "je"]
      [`> "jg"]
      [`>= "jge"]
      [`!= "jne"]))

  (program->x64 p))

