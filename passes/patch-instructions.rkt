#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide patch-instructions)

;; para-asm-lang-v8 -> paren-x64-mops-v8
;; compiles p to to Paren-x64-mops v8 by patching each instruction that has no
;; x64 analogue into a sequence of instructions using auxiliary register from
;; current-patch-instructions-registers
(define/contract (patch-instructions p)
  (-> para-asm-lang-v8? paren-x64-mops-v8?)

  ;; relop -> relop
  ;; interp. produces the negation of relop
  (define (negate-relop relop)
    (match relop
      [`< `>=]
      [`<= `>]
      [`= `!=]
      [`>= `<]
      [`> `<=]
      [`!= `=]))

  ;; para-asm-lang-v8.loc para-asm-lang-v8.loc para-asm-lang-v8.index -> (List-of paren-x64-mops-v8.s)
  ;; interp. patches memory reference instructions into valid paren-x64 sequences
  (define (patch-mref loc1 loc2 index)
    (define patch-reg-1 (first (current-patch-instructions-registers)))
    (define patch-reg-2 (second (current-patch-instructions-registers)))
    (cond
      ;; Case 1: loc1 is register, loc2 is register, index is register or int32
      [(and (register? loc1)
            (register? loc2)
            (or (register? index) (int32? index)))
       (list `(set! ,loc1 (mref ,loc2 ,index)))]
      ;; Case 2: loc1 is register, loc2 is addr, index is register or int32
      [(and (register? loc1)
            (addr? loc2)
            (or (register? index) (int32? index)))
       `((set! ,patch-reg-1 ,loc2)
         (set! ,loc1 (mref ,patch-reg-1 ,index)))]
      ;; Case 3: loc1 is register, loc2 is register, index is addr
      [(and (register? loc1)
            (register? loc2)
            (addr? index))
       `((set! ,patch-reg-1 ,index)
         (set! ,loc1 (mref ,loc2 ,patch-reg-1)))]
      ;; Case 4: loc1 is register, loc2 and index are addr
      [(and (register? loc1)
            (addr? loc2)
            (addr? index))
       `((set! ,patch-reg-1 ,loc2)
         (set! ,patch-reg-2 ,index)
         (set! ,loc1 (mref ,patch-reg-1 ,patch-reg-2)))]
      ;; Case 5: loc1 is addr
      [(addr? loc1)
       (cond
         ;; Subcase A: loc2 is register, index is register or int32
         [(and (register? loc2) (or (register? index) (int32? index)))
          `((set! ,patch-reg-1 (mref ,loc2 ,index))
            (set! ,loc1 ,patch-reg-1))]
         ;; Subcase B: loc2 is addr, index is register or int32
         [(and (addr? loc2) (or (register? index) (int32? index)))
          `((set! ,patch-reg-1 ,loc2)
            (set! ,patch-reg-1 (mref ,patch-reg-1 ,index))
            (set! ,loc1 ,patch-reg-1))]
         ;; Subcase C: loc2 is register, index is addr
         [(and (register? loc2) (addr? index))
          `((set! ,patch-reg-1 ,index)
            (set! ,patch-reg-1 (mref ,loc2 ,patch-reg-1))
            (set! ,loc1 ,patch-reg-1))]
         ;; Subcase D: loc2 and index are addr
         [(and (addr? loc2) (addr? index))
          `((set! ,patch-reg-1 ,loc2)
            (set! ,patch-reg-2 ,index)
            (set! ,patch-reg-1 (mref ,patch-reg-1 ,patch-reg-2))
            (set! ,loc1 ,patch-reg-1))])]))

  ;; para-asm-lang-v8.loc para-asm-lang-v8.binop para-asm-lang-v8.triv -> (List-of paren-x64-mops-v8.s)
  ;; interp. patches binary operations into valid paren-x64 sequences
  (define (patch-binop loc binop triv)
    (define patch-reg-1 (first (current-patch-instructions-registers)))
    (define patch-reg-2 (second (current-patch-instructions-registers)))
    (cond
      ;; if loc is an addr and triv is a label, loc, or larger than int32,
      ;; then both loc and triv must be stored in temporary registers
      [(and (addr? loc) (not (int32? triv)))
       `((set! ,patch-reg-1 ,loc)
         (set! ,patch-reg-2 ,triv)
         (set! ,patch-reg-1 (,binop ,patch-reg-1 ,patch-reg-2))
         (set! ,loc ,patch-reg-1))]
      ;; if loc is an addr and triv is an int32, then only loc needs to be
      ;; stored in a temporary register
      [(and (addr? loc) (int32? triv))
       `((set! ,patch-reg-1 ,loc)
         (set! ,patch-reg-1 (,binop ,patch-reg-1 ,triv))
         (set! ,loc ,patch-reg-1))]
      ;; if loc is a register and triv is larger than int32, then triv
      ;; needs to be stored in a temporary register
      [(and (not (int32? triv)) (int64? triv))
       `((set! ,patch-reg-1 ,triv)
         (set! ,loc (,binop ,loc ,patch-reg-1)))]
      [else (list `(set! ,loc (,binop ,loc ,triv)))]))

  ;; para-asm-lang-v8.loc para-asm-lang-v8.triv -> (List-of paren-x64-mops-v8.s)
  ;; interp. patches set! instructions with potentially invalid destinations or values
  (define (patch-set loc triv)
    (define patch-reg (first (current-patch-instructions-registers)))
    (if (and (addr? loc)
             (or (and (not (int32? triv)) (int64? triv))
                 (addr? triv)
                 (label? triv)))
        `((set! ,patch-reg ,triv)
          (set! ,loc ,patch-reg))
        (list `(set! ,loc ,triv))))

  ;; para-asm-lang-v8.loc para-asm-lang-v8.index para-asm-lang-v8.triv -> (List-of paren-x64-mops-v8.s)
  ;; interp. patches mset! instructions to ensure valid operands
  (define (patch-mset loc index triv)
    (define patch-reg-1 (first (current-patch-instructions-registers)))
    (define patch-reg-2 (second (current-patch-instructions-registers)))
    (cond
      [(and (addr? loc) (int64? index) (int64? triv))
       `((set! ,patch-reg-1 ,loc)
         (mset! ,patch-reg-1 ,index ,triv))]
      [(and (register? loc) (int64? index) (label? triv))
       `((set! ,patch-reg-1 ,triv)
         (mset! ,loc ,index ,patch-reg-1))]
      [(and (addr? loc) (addr? index) (or (addr? triv) (label? triv)))
       `((set! ,patch-reg-1 ,loc)
         (set! ,patch-reg-2 ,index)
         (set! ,patch-reg-1 (+ ,patch-reg-1 ,patch-reg-2))
         (set! ,patch-reg-2 ,triv)
         (mset! ,patch-reg-1 0 ,patch-reg-2))]
      [(and (register? loc) (addr? index) (label? triv))
       `((set! ,patch-reg-1 ,triv)
         (set! ,patch-reg-2 ,index)
         (mset! ,loc ,patch-reg-2 ,patch-reg-1))]
      [(and (register? loc) (register? index) (label? triv))
       `((set! ,patch-reg-1 ,triv)
         (mset! ,loc ,index ,patch-reg-1))]
      [(and (addr? loc) (register? index) (label? triv))
       `((set! ,patch-reg-1 ,triv)
         (set! ,patch-reg-2 ,loc)
         (mset! ,patch-reg-2 ,index ,patch-reg-1))]
      [(and (addr? loc) (int64? index) (label? triv))
       `((set! ,patch-reg-1 ,triv)
         (set! ,patch-reg-2 ,loc)
         (mset! ,patch-reg-2 ,index ,patch-reg-1))]
      [(and (register? loc) (addr? index) (addr? triv))
       `((set! ,patch-reg-1 ,triv)
         (set! ,patch-reg-2 ,index)
         (mset! ,loc ,patch-reg-2 ,patch-reg-1))]
      [(and (addr? loc) (addr? index) (int64? triv))
       `((set! ,patch-reg-1 ,loc)
         (set! ,patch-reg-2 ,index)
         (mset! ,patch-reg-1 ,patch-reg-2 ,triv))]
      [(and (addr? loc) (or (int64? triv) (addr? triv)))
       `((set! ,patch-reg-1 ,triv)
         (set! ,patch-reg-2 ,loc)
         (mset! ,patch-reg-2 ,index ,patch-reg-1))]
      [(or (int64? triv) (addr? triv))
       `((set! ,patch-reg-1 ,triv)
         (mset! ,loc ,index ,patch-reg-1))]
      [else (list `(mset! ,loc ,index ,triv))]))

  ;; para-asm-lang-v8.loc para-asm-lang-v8.opand -> (List-of paren-x64-mops-v8.s)
  ;; interp. patches compare instructions with address operands
  (define (patch-compare loc op)
    (define reg (first (current-patch-instructions-registers)))
    (define reg2 (second (current-patch-instructions-registers)))
    (cond
      [(and (addr? loc) (addr? op)) `((set! ,reg2 ,op)(set! ,reg ,loc) (compare ,reg ,reg2))]
      [(addr? loc) `((set! ,reg ,loc) (compare ,reg ,op))]
      [(addr? op) `((set! ,reg ,op) (compare ,loc ,reg))]
      [else `((compare ,loc ,op))]))

  ;; para-asm-lang-v8.relop para-asm-lang-v8.trg -> (List-of paren-x64-mops-v8.s)
  ;; interp. patches jump-if instructions with address targets
  (define (patch-jump-if relop trg)
    (define label (fresh-label))
    (define reg (first (current-patch-instructions-registers)))
    (cond
      [(addr? trg)
       `((set! ,reg ,trg)
         (jump-if ,(negate-relop relop) ,label)
         (jump ,reg)
         (with-label ,label (set! ,reg ,reg)))]
      [(label? trg)
       `((jump-if ,relop ,trg))]
      [else
       `((jump-if ,(negate-relop relop) ,label)
         (jump ,trg)
         (with-label ,label (set! ,reg ,reg)))]))

  ;; para-asm-lang-v8.s -> (List-of paren-x64-mops-v8.s)
  ;; interp. transforms a single para-asm statement into one or more valid
  ;; paren-x64 instructions
  (define (patch-instructions-s s)
    (match s
      [`(set! ,loc1 (mref ,loc2 ,index))
       (patch-mref loc1 loc2 index)]
      [`(set! ,loc (,binop ,loc ,triv))
       (patch-binop loc binop triv)]
      [`(set! ,loc ,triv)
       (patch-set loc triv)]
      [`(mset! ,loc ,index ,triv)
       (patch-mset loc index triv)]
      [`(with-label ,label ,s)
       (define s-compiled (patch-instructions-s s))
       (if (empty? (rest s-compiled))
           `((with-label ,label ,(first s-compiled)))
           `((with-label ,label ,(first s-compiled)) ,@(rest s-compiled)))]
      [`(jump ,trg)
       (define reg (first (current-patch-instructions-registers)))
       (if (addr? trg)
           `((set! ,reg ,trg) (jump ,reg))
           `((jump ,trg)))]
      [`(compare ,loc ,op)
       (patch-compare loc op)]
      [`(jump-if ,relop ,trg)
       (patch-jump-if relop trg)]
      [`(halt ,op)
       (define set-rax `(set! ,(current-return-value-register) ,op))
       `(,@(patch-instructions-s set-rax)
         (jump done))]))

  (match p
    [`(begin ,stmts ...)
     (define stmts^ (for/list ([stmt stmts])
                      (patch-instructions-s stmt)))
     `(begin ,@(apply append stmts^))]))


