#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2
  rackunit)

(provide patch-instructions)

;; para-asm-lang-v2 -> paren-x64-fvars-v2
;; compile program by patching instructions that have to x64 equivilent
;; into sequences of equivilent instructions
(define/contract (patch-instructions p)
  (-> para-asm-lang-v2? paren-x64-fvars-v2?)

  ;; para-asm-lang-v2.effect -> paren-x64-fvars-v2.effect
  (define (patch-instructions-effect e)
    (match e
      [`(set! ,loc (,binop ,loc ,triv))
       (cond
         [(and (fvar? loc) (not (int32? triv)))
          (define patch-reg-1 (first (current-patch-instructions-registers)))
          (define patch-reg-2 (second (current-patch-instructions-registers)))
          (list `(set! ,patch-reg-1 ,loc)
                `(set! ,patch-reg-2 ,triv)
                `(set! ,patch-reg-1 (,binop ,patch-reg-1 ,patch-reg-2))
                `(set! ,loc ,patch-reg-1))]
         [(and (fvar? loc) (int32? triv))
          (define patch-reg-1 (first (current-patch-instructions-registers)))
          (list `(set! ,patch-reg-1 ,loc)
                `(set! ,patch-reg-1 (,binop ,patch-reg-1 ,triv))
                `(set! ,loc ,patch-reg-1))]
         [(and (not (int32? triv)) (int64? triv))
          (define patch-reg-1 (first (current-patch-instructions-registers)))
          (list `(set! ,patch-reg-1 ,triv)
                `(set! ,loc (,binop ,loc ,patch-reg-1)))]
         [else (list `(set! ,loc (,binop ,loc ,triv)))])]
      [`(set! ,loc ,triv)
       (cond
         [(and (fvar? loc) (or (register? triv) (fvar? triv)))
          (define patch-reg (first (current-patch-instructions-registers)))
          (list `(set! ,patch-reg ,triv)
                `(set! ,loc ,patch-reg))]
         [else (list `(set! ,loc ,triv))])]))

  (match p
    [`(begin ,e ... (halt ,triv))
     `(begin
        ,@(apply append (map patch-instructions-effect e))
        (set! ,(current-return-value-register) ,triv))]))

(module+ test
  (check-equal?
   (patch-instructions '(begin (halt rbx)))
   '(begin (set! rax rbx)))
  (check-equal?
   (patch-instructions
    '(begin
       (set! rbx 42)
       (halt rbx)))
   '(begin
      (set! rbx 42)
      (set! rax rbx)))
  (check-equal?
   (patch-instructions
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
      (set! rax fv0)))
  (check-equal?
   (patch-instructions
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
      (set! rax rbx)))
  (check-equal?
   (patch-instructions
    '(begin 
       (set! rbx 10)
       (set! rcx 20)
       (set! fv0 5)
       (set! fv1 15)
       (set! rdi 100)
       (set! rbx (+ rbx rcx))
       (halt rbx)))
   '(begin
      (set! rbx 10)
      (set! rcx 20)
      (set! fv0 5)
      (set! fv1 15)
      (set! rdi 100)
      (set! rbx (+ rbx rcx))
      (set! rax rbx)))
  (check-equal?
   (patch-instructions
    '(begin
       (set! rbx 1)
       (set! rcx 2)
       (set! rdx 3)
       (set! rsi 4)
       (set! rdi 5)
       (set! r8 6)
       (set! r9 7)
       (set! rbp 8)
       (set! rbx (+ rbx rdx))
       (set! rcx (+ rcx rsi))
       (halt rbp)))
   '(begin
      (set! rbx 1)
      (set! rcx 2)
      (set! rdx 3)
      (set! rsi 4)
      (set! rdi 5)
      (set! r8 6)
      (set! r9 7)
      (set! rbp 8)
      (set! rbx (+ rbx rdx))
      (set! rcx (+ rcx rsi))
      (set! rax rbp)))
  (check-equal?
   (patch-instructions
    '(begin
       (set! fv0 (+ fv0 42))
       (halt fv0)))
   '(begin
      (set! r10 fv0)
      (set! r10 (+ r10 42))
      (set! fv0 r10)
      (set! rax fv0)))
  (check-equal?
   (patch-instructions
    '(begin
       (set! fv0 (+ fv0 2147483648))
       (halt fv0)))
   '(begin
      (set! r10 fv0)
      (set! r11 2147483648)
      (set! r10 (+ r10 r11))
      (set! fv0 r10)
      (set! rax fv0))))