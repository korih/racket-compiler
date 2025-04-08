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
  ;; produces the negation of relop
  (define (negate-relop relop)
    (match relop
      [`< `>=]
      [`<= `>]
      [`= `!=]
      [`>= `<]
      [`> `<=]
      [`!= `=]))

  ;; para-asm-lang-v8.s -> paren-x64-v8.s
  (define (compile-s s)
    (match s
      [`(set! ,loc1 (mref ,loc2 ,index))
       `((set! ,loc1 (mref ,loc2 ,index)))]
      [`(set! ,loc (,binop ,loc ,triv))
       (cond
         ;; if loc is an addr and triv is a label, loc, or larger than int32,
         ;; then both loc and triv must be stored in temporary registers
         [(and (addr? loc) (not (int32? triv)))
          (define patch-reg-1 (first (current-patch-instructions-registers)))
          (define patch-reg-2 (second (current-patch-instructions-registers)))
          `((set! ,patch-reg-1 ,loc)
            (set! ,patch-reg-2 ,triv)
            (set! ,patch-reg-1 (,binop ,patch-reg-1 ,patch-reg-2))
            (set! ,loc ,patch-reg-1))]

         ;; if loc is an addr and triv is an int32, then only loc needs to be
         ;; stored in a temporary register
         [(and (addr? loc) (int32? triv))
          (define patch-reg-1 (first (current-patch-instructions-registers)))
          `((set! ,patch-reg-1 ,loc)
            (set! ,patch-reg-1 (,binop ,patch-reg-1 ,triv))
            (set! ,loc ,patch-reg-1))]

         ;; if loc is a register and triv is larger than int32, then triv
         ;; needs to be stored in a temporary register
         [(and (not (int32? triv)) (int64? triv))
          (define patch-reg-1 (first (current-patch-instructions-registers)))
          `((set! ,patch-reg-1 ,triv)
            (set! ,loc (,binop ,loc ,patch-reg-1)))]
         [else (list `(set! ,loc (,binop ,loc ,triv)))])]
      [`(set! ,loc ,triv)
       (cond
         ;; if loc is an addr and triv is a label, loc, or larger than int32,
         ;; then triv must be stored in a temporary register
         [(and (addr? loc)
               (or (and (not (int32? triv)) (int64? triv))
                   (addr? triv)
                   (label? triv)))
          (define patch-reg (first (current-patch-instructions-registers)))
          `((set! ,patch-reg ,triv)
            (set! ,loc ,patch-reg))]
         [else (list `(set! ,loc ,triv))])]
      [`(mset! ,loc ,index ,triv)
       (cond
         [(and (addr? loc) (or (int64? triv) (addr? triv)))
          (define patch-reg-1 (first (current-patch-instructions-registers)))
          (define patch-reg-2 (second (current-patch-instructions-registers)))
          `((set! ,patch-reg-1 ,triv)
            (set! ,patch-reg-2 ,loc)
            (mset! ,patch-reg-2 ,index ,patch-reg-1))]
         [(or (int64? triv) (addr? triv))
          (define patch-reg (first (current-patch-instructions-registers)))
          `((set! ,patch-reg ,triv)
            (mset! ,loc ,index ,patch-reg))]
         [else (list `(mset! ,loc ,index ,triv))])]
      [`(with-label ,label ,s)
       (define s-compiled (compile-s s))
       (if (empty? (rest s-compiled))
           `((with-label ,label ,(first s-compiled)))
           `((with-label ,label ,(first s-compiled)) ,@(rest s-compiled)))]
      [`(jump ,trg)
       (define reg (first (current-patch-instructions-registers)))
       (if (addr? trg)
           `((set! ,reg ,trg) (jump ,reg))
           `((jump ,trg)))]
      [`(compare ,loc ,op)
       (define reg (first (current-patch-instructions-registers)))
       (define reg2 (second (current-patch-instructions-registers)))
       (cond
         [(and (addr? loc) (addr? op)) `((set! ,reg2 ,op)(set! ,reg ,loc) (compare ,reg ,reg2))]
         [(addr? loc) `((set! ,reg ,loc) (compare ,reg ,op))]
         [(addr? op) `((set! ,reg ,op) (compare ,loc ,reg))]
         [else `((compare ,loc ,op))])]
      [`(jump-if ,relop ,trg)
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
            (with-label ,label (set! ,reg ,reg)))])]
      [`(halt ,op)
       (define set-rax `(set! ,(current-return-value-register) ,op))
       `(,@(compile-s set-rax)
         (jump done))]))

  (match p
    [`(begin ,effects ...)
     (define effects^ (for/list ([effect effects])
                        (compile-s effect)))
     `(begin ,@(apply append effects^))]))


