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

  ;; compiles effectful operations in para-asm-lang-v2 to sequence of
  ;; instructions equivilent in paren-x64-fvars-v2
  (define (compile-effect e)
    (match e
      [`(set! ,loc (,binop ,loc ,triv))
       #:when (and (fvar? loc) (not (int32? triv)))
       (define patch-reg-1 (first (current-patch-instructions-registers)))
       (define patch-reg-2 (second (current-patch-instructions-registers)))
       `((set! ,patch-reg-1 ,loc)
         (set! ,patch-reg-2 ,triv)
         (set! ,patch-reg-1 (,binop ,patch-reg-1 ,patch-reg-2))
         (set! ,loc ,patch-reg-1))]
      [`(set! ,loc (,binop ,loc ,triv))
       #:when (and (fvar? loc) (int32? triv))
       (define patch-reg-1 (first (current-patch-instructions-registers)))
       `((set! ,patch-reg-1 ,loc)
         (set! ,patch-reg-1 (,binop ,patch-reg-1 ,triv))
         (set! ,loc ,patch-reg-1))]
      [`(set! ,loc (,binop ,loc ,triv))
       #:when (and (not (int32? triv)) (int64? triv))
       (define patch-reg-1 (first (current-patch-instructions-registers)))
       `((set! ,patch-reg-1 ,triv)
         (set! ,loc (,binop ,loc ,patch-reg-1)))]
      [`(set! ,loc (,binop ,loc ,triv))
       `((set! ,loc (,binop ,loc ,triv)))]
      [`(set! ,loc ,triv)
       #:when (and (fvar? loc)
                   (or (and (not (int32? triv)) (int64? triv))
                       (fvar? triv)))
       (define patch-reg (first (current-patch-instructions-registers)))
       `((set! ,patch-reg ,triv)
         (set! ,loc ,patch-reg))]
      [`(set! ,loc ,triv)
       (list `(set! ,loc ,triv))]))

  ;; compiles para-asm-lang-v2 halt to set return register in paren-x64-fvars-v2
  (define (compile-p p)
    (match p
      [`(halt ,triv)
       (define ret (current-return-value-register))
       `(set! ,ret ,triv)]))

  (match p
    [`(begin ,effects ... ,halt)
     (define effects^ (for/list ([effect effects])
                        (compile-effect effect)))
     (define halt^ (compile-p halt))
     `(begin ,@(apply append effects^) ,halt^)]))