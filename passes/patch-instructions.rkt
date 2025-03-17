#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v6
  rackunit)

(provide patch-instructions)

;; para-asm-lang-v6 -> paren-x64-v6
;; compiles p to to Paren-x64 v6 by patching each instruction that has no
;; x64 analogue into a sequence of instructions using auxiliary register from
;; current-patch-instructions-registers
(define/contract (patch-instructions p)
  (-> para-asm-lang-v6? paren-x64-v6?)

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

  ;; para-asm-lang-v6.s -> paren-x64-v6.s
  (define (compile-s s)
    (match s
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


(module+ test
  (check-equal? (patch-instructions '(begin
                                       (with-label L.tmp.1 (set! rax 10))
                                       (set! (rbp - 8) 2)
                                       (compare rax (rbp - 8))
                                       (jump-if != L.tmp.1)))
                '(begin
                   (with-label L.tmp.1 (set! rax 10))
                   (set! (rbp - 8) 2)
                   (set! r10 (rbp - 8))
                   (compare rax r10)
                   (jump-if != L.tmp.1)))
  (check-equal? (patch-instructions '(begin
                                       (set! (rbp - 0) 0)
                                       (set! (rbp - 8) 1)
                                       (set! r11 (rbp - 8))
                                       (set! r10 (rbp - 0))
                                       (compare r10 r11)
                                       (jump-if > L.foo.1)
                                       (jump done)
                                       (with-label L.foo.1 (jump done))))
                '(begin
                   (set! fv0 0)
                   (set! fv1 1)
                   (set! r11 fv1)
                   (set! r10 fv0)
                   (compare r10 r11)
                   (jump-if > L.foo.1)
                   (set! rax 0)
                   (jump done)
                   (with-label L.foo.1 (set! rax 1))
                   (jump done)))
  (check-equal? (patch-instructions
                 '(begin
                    (set! rsi L.label.1)
                    (with-label L.label.1
                      (set! rbx 18))
                    (halt rbx)))
                '(begin
                   (set! rsi L.label.1)
                   (with-label L.label.1 (set! rbx 18))
                   (set! rax rbx)
                   (jump done)))
  (check-equal? (patch-instructions
                 '(begin
                    (compare rax 10)
                    (jump-if = done)))
                `(begin (compare rax 10) (jump-if = done)))
  (check-equal? (patch-instructions
                 `(begin (jump done)))
                `(begin (jump done)))
  (check-equal? (patch-instructions
                 `(begin (with-label L.my_label.1 (set! fv0 L.my_label.1))
                         (with-label L.my_label.2 (halt 42))))
                `(begin
                   (with-label L.my_label.1 (set! r10 L.my_label.1))
                   (set! fv0 r10)
                   (with-label L.my_label.2 (set! rax 42))
                   (jump done)))
  (check-equal? (patch-instructions
                 '(begin (set! rbx 42)
                         (set! fv0 L.my_label.1)
                         (set! fv1 L.my_label.2)
                         (compare rbx 42)
                         (jump-if = fv0)
                         (halt rbx)
                         (with-label L.my_label.1 (halt 32))
                         (with-label L.my_label.2 (halt 42))))
                '(begin
                   (set! rbx 42)
                   (set! r10 L.my_label.1)
                   (set! fv0 r10)
                   (set! r10 L.my_label.2)
                   (set! fv1 r10)
                   (compare rbx 42)
                   (set! r10 fv0)
                   (jump-if != L.tmp.4)
                   (jump r10)
                   (with-label L.tmp.4 (set! r10 r10))
                   (set! rax rbx)
                   (jump done)
                   (with-label L.my_label.1 (set! rax 32))
                   (jump done)
                   (with-label L.my_label.2 (set! rax 42))
                   (jump done)))
  (check-equal? (patch-instructions
                 `(begin
                    (jump L.tmp.1)
                    (halt 2)
                    (with-label L.tmp.1 (halt 1))))
                `(begin
                   (jump L.tmp.1)
                   (set! rax 2)
                   (jump done)
                   (with-label L.tmp.1 (set! rax 1))
                   (jump done)))
  (check-equal? (patch-instructions
                 `(begin
                    (jump fv1)
                    (halt 2)
                    (with-label L.tmp.1 (halt 1))))
                `(begin
                   (set! r10 fv1)
                   (jump r10)
                   (set! rax 2)
                   (jump done)
                   (with-label L.tmp.1 (set! rax 1))
                   (jump done)))
  (check-equal? (patch-instructions
                 `(begin
                    (jump rbx)
                    (halt 1)))
                `(begin
                   (jump rbx)
                   (set! rax 1)
                   (jump done)))
  (check-equal? (patch-instructions
                 '(begin (set! rbx 42) (halt rbx)))
                '(begin (set! rbx 42) (set! rax rbx) (jump done)))
  (check-equal? (patch-instructions '(begin (set! rbx 42) (halt rbx)))
                '(begin (set! rbx 42) (set! rax rbx) (jump done)))
  (check-equal? (patch-instructions
                 '(begin
                    (set! fv0 0)
                    (set! fv1 42)
                    (set! fv0 fv1)
                    (halt fv0)))
                '(begin
                   (set! fv0 0)
                   (set! fv1 42)
                   (set! r10 fv1)
                   (set! fv0 r10)
                   (set! rax fv0)
                   (jump done)))
  (check-equal? (patch-instructions
                 '(begin
                    (set! rbx 0)
                    (set! rcx 0)
                    (set! r9 42)
                    (set! rbx rcx)
                    (set! rbx (+ rbx r9))
                    (halt rbx)))
                '(begin
                   (set! rbx 0)
                   (set! rcx 0)
                   (set! r9 42)
                   (set! rbx rcx)
                   (set! rbx (+ rbx r9))
                   (set! rax rbx)
                   (jump done)))
  (check-equal? (patch-instructions
                 '(begin
                    (set! fv0 0)
                    (set! fv0 (+ fv0 1))
                    (halt 1)))
                '(begin
                   (set! fv0 0)
                   (set! r10 fv0)
                   (set! r10 (+ r10 1))
                   (set! fv0 r10)
                   (set! rax 1)
                   (jump done)))
  (check-equal? (patch-instructions
                 '(begin
                    (set! fv1 7)
                    (set! fv2 0)
                    (set! r8 5)
                    (set! r12 fv1)
                    (set! r12 (+ r12 r8))
                    (set! fv2 (+ fv2 4))
                    (halt fv2)))
                '(begin
                   (set! fv1 7)
                   (set! fv2 0)
                   (set! r8 5)
                   (set! r12 fv1)
                   (set! r12 (+ r12 r8))
                   (set! r10 fv2)
                   (set! r10 (+ r10 4))
                   (set! fv2 r10)
                   (set! rax fv2)
                   (jump done)))
  (check-equal? (patch-instructions '(begin
                                       (with-label L.start.1 (set! rax 5))
                                       (jump L.start.1)))
                '(begin (with-label L.start.1 (set! rax 5)) (jump L.start.1)))
  (check-equal? (patch-instructions '(begin
                                       (set! fv0 10)
                                       (set! r9 fv0)
                                       (with-label L.init.1 (set! fv0 100))
                                       (compare fv0 100)))
                '(begin
                   (set! fv0 10)
                   (set! r9 fv0)
                   (with-label L.init.1 (set! fv0 100))
                   (set! r10 fv0)
                   (compare r10 100))))
