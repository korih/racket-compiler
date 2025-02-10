#lang racket

(require
  cpsc411/compiler-lib)

(provide generate-x64)

;; paren-x64-v1 -> x64-instruction-sequence
;; interp. a compiler that generates x64 instructions for the paren-x64-v2 language
(define (generate-x64 p)
  ;; paren-x64-v2 -> x64-instruction-sequence
  ;; interp. a compiler that generates x64 instructions for the paren-x64-v2 language
  (define (program->x64 p)
    (match p
      [`(begin ,s ...)
       (for/foldr ([acc ""])
         ([s s])
         (string-append (statement->x64 s) "\n" acc))]))

  ;; interp. predicate if the given value is a paren-x64-v2-loc
  (define/contract (loc? loc)
    (-> any/c boolean?)
    (match loc
      [`(,fbp - ,dispoffset) #t]
      [x #:when (register? x) #t]
      [_ #f]))


  ;; paren-x64-v2-loc -> x64-instruction-sequence
  ;; interp. generates x64 instruction postfix for a paren-x64-v2 location
  (define (loc->x64 loc)
    (match loc
      [`(,fbp - ,dispoffset) (format "QWORD [~a - ~a]" fbp dispoffset)]
      [x x]))

  ;; paren-x64-v2 statement -> x64-instruction-sequence
  ;; interp. a generated x64-instruction-sequence for a paren-x64-v2 statement
  (define (statement->x64 s)
    (match s
      [`(set! (,fbp - ,dispoffset) ,val) (format "mov QWORD [~a - ~a], ~a" fbp dispoffset val)]
      [`(set! ,reg ,val) #:when (int64? val) (format "mov ~a, ~a" reg val)]
      [`(set! ,reg1 ,loc) #:when (loc? loc) (format "mov ~a, ~a" reg1 (loc->x64 loc))]
      [`(set! ,reg_1 (,op ,reg_1 ,val)) #:when (int32? val) (format "~a ~a, ~a" (binop->ins op) reg_1 val)]
      [`(set! ,reg_1 (,op ,reg_1 ,loc)) (format "~a ~a, ~a" (binop->ins op) reg_1 (loc->x64 loc))]))


  ;; paren-x64-v2 binop -> x64-instruction-sequence instruction binop
  ;; interp. a corresponding x64-instruction-sequence binop for a paren-x64-v2 binop
  (define (binop->ins b)
    (match b
      [`+ "add"]
      [`* "imul"]))

  (program->x64 p))
