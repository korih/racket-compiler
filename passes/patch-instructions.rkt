#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4)

(provide patch-instructions)

;; para-asm-lang -> paren-x64-fvars
;; compile program by patching instructions that have to x64 equivilent
;; into sequences of equivilent instructions
(define/contract (patch-instructions p)
  (-> para-asm-lang-v4? paren-x64-fvars-v4?)

  ;; (para-asm-lang s) -> (paren-x64-fvars effect)
  ;; compiles effectful operations in para-asm-lang-v2 to sequence of
  ;; instructions equivilent in paren-x64-fvars-v2
  (define (compile-s e)
    (match e
      ;; triv is now also a label
      [`(set! ,loc (,binop ,loc ,triv))
       (cond
         ;; check if fvar and triv are valid in there positions
         [(and (fvar? loc) (not (int32? triv)))
          (define patch-reg-1 (first (current-patch-instructions-registers)))
          (define patch-reg-2 (second (current-patch-instructions-registers)))
          `((set! ,patch-reg-1 ,loc)
            (set! ,patch-reg-2 ,triv)
            (set! ,patch-reg-1 (,binop ,patch-reg-1 ,patch-reg-2))
            (set! ,loc ,patch-reg-1))]

         ;; check fvar since we know triv is now int32 when loc is fvar
         [(and (fvar? loc) (int32? triv))
          (define patch-reg-1 (first (current-patch-instructions-registers)))
          `((set! ,patch-reg-1 ,loc)
            (set! ,patch-reg-1 (,binop ,patch-reg-1 ,triv))
            (set! ,loc ,patch-reg-1))]

         ;; check triv since we know loc is not fvar
         [(and (not (int32? triv)) (int64? triv))
          (define patch-reg-1 (first (current-patch-instructions-registers)))
          `((set! ,patch-reg-1 ,triv)
            (set! ,loc (,binop ,loc ,patch-reg-1)))]
         [else (list `(set! ,loc (,binop ,loc ,triv)))])]
      [`(set! ,loc ,triv)
       (cond
         ;; if loc is fvar and we are not moving an int32 or reg there
         [(and (fvar? loc)
               (or (and (not (int32? triv)) (int64? triv))
                   (fvar? triv)))
          (define patch-reg (first (current-patch-instructions-registers)))
          `((set! ,patch-reg ,triv)
            (set! ,loc ,patch-reg))]

         [else (list `(set! ,loc ,triv))])]

      [`(with-label ,label ,s)
       (define s-compiled (compile-s s))
       `((with-label ,label ,(first s-compiled)) ,@(rest s-compiled))]
      [`(jump ,trg) `((jump ,trg))]
      [`(compare ,loc ,op)
       (define reg (first (current-patch-instructions-registers)))
       (if (fvar? loc)
           `((set! ,reg ,loc) (compare ,reg ,op))
           `((compare ,loc ,op)))]
      [`(jump-if ,relop ,trg)
       (define label (fresh-label)) ;; TODO:
       `((jump-if ,(negate-relop relop) ,label) (jump ,trg) (with-label ,label (set! rax rax)))]
      [`(halt ,op)
       (define set-rax `(set! ,(current-return-value-register) ,op))
       `(,@(compile-s set-rax)
         (jump done))] ; halt is now an s
      ))

  ;; negate a relop conditional
  (define (negate-relop relop)
    (match relop
      [`< `>=]
      [`<= `>]
      [`= `!=]
      [`>= `<]
      [`> `<=]
      [`!= `=]))

  ;; compiles para-asm-lang-v2 halt to set return register in paren-x64-fvars-v2
  (define (compile-p p)
    (match p
      [`(halt ,triv)
       (define ret (current-return-value-register))
       `(set! ,ret ,triv)]))

  (match p
    [`(begin ,effects ...)
     (define effects^ (for/list ([effect effects])
                        (compile-s effect)))
     `(begin ,@(apply append effects^))]))


;; v2 tests which no longer work
(module+ test
  (require rackunit)
  (define label (fresh-label))
  (define label2 (fresh-label))
  #;
  (check-equal? (patch-instructions
                 `(begin
                    (set! rbx 1)
                    (compare rbx 0)
                    (jump-if > rbx)
                    (halt 2)
                    (with-label ,label (halt 1))))
                `(begin
                   (set! rbx 1)
                   (compare rbx 0)
                   (jump-if <= ,label2)
                   (jump ,label)
                   (with-label ,label2 (set! rax rax))
                   (set! rax 2)
                   (jump done)
                   (with-label ,label (set! rax 1))
                   (jump done)))
  (check-equal? (patch-instructions
                 `(begin (jump ,label) (with-label ,label (halt 1)) (halt 2)))
                `(begin (jump ,label) (with-label ,label (set! rax 1)) (jump done)(set! rax 2) (jump done)))
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
                   (jump done))))
