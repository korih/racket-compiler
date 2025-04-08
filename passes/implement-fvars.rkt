#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide implement-fvars)

;; nested-asm-lang-fvars-v8 -> nested-asm-lang-v8
;; compiles p to Nested-asm-lang v8 by reifying fvars into displacement mode
;; operands
(define/contract (implement-fvars p)
  (-> nested-asm-lang-fvars-v8? nested-asm-lang-v8?)

  ;; base-pointer-offset is Integer
  ;; interp. keeps track of the frame base pointer offset after
  ;; allocating/deallocating a frame
  (define base-pointer-offset 0)

  ;; fvar -> addr
  ;; convert fvar into displacement mode operand
  (define (fvar->addr fvar)
    `(,(current-frame-base-pointer-register) - ,(+ (* (fvar->index fvar)
                                                      (current-word-size-bytes))
                                                   base-pointer-offset)))

  ;; nested-asm-lang-fvars-v8.binop Integer -> void
  ;; EFFECTS: mutates `base-pointer-offset` by applying the given binop with the
  ;; offset
  (define (update-base-pointer-offset! binop offset)
    (match binop
      ['+ (set! base-pointer-offset (+ base-pointer-offset offset))]
      ['* (set! base-pointer-offset (* base-pointer-offset offset))]
      ['- (set! base-pointer-offset (- base-pointer-offset offset))]

      ;; Wildcard TODO: should not be able to put some binops here
      [_ (set! base-pointer-offset offset)]))

  ;; nested-asm-lang-fvars-v8.tail -> nested-asm-lang-v8.tail
  (define (implement-fvars-tail tail)
    (match tail
      [`(jump ,trg)
       `(jump ,(implement-fvars-trg trg))]
      [`(begin ,es ... ,t)
       `(begin ,@(map implement-fvars-effect es) ,(implement-fvars-tail t))]
      [`(if ,pred ,t1 ,t2)
       `(if ,(implement-fvars-pred pred)
            ,(implement-fvars-tail t1)
            ,(implement-fvars-tail t2))]))

  ;; nested-asm-lang-fvars-v8.effect -> nested-asm-lang-v8.effect
  (define (implement-fvars-effect effect)
    (match effect
      [`(set! ,loc1 (mref ,loc2 ,index))
       (define loc1^ (implement-fvars-loc loc1))
       (define loc2^ (implement-fvars-loc loc2))
       (define index^ (implement-fvars-opand index))
       `(set! ,loc1^ (mref ,loc2^ ,index^))]
      [`(set! ,loc (,binop ,loc ,opand))
       (when (and (eq? (current-frame-base-pointer-register) loc)
                  (int64? opand))
         (update-base-pointer-offset! binop opand))
       (define loc^ (implement-fvars-loc loc))
       `(set! ,loc^ (,binop ,loc^ ,(implement-fvars-opand opand)))]
      [`(set! ,loc ,triv)
       (when (and (eq? (current-frame-base-pointer-register) loc)
                  (int64? triv))
         (update-base-pointer-offset! '= triv))
       `(set! ,(implement-fvars-loc loc) ,(implement-fvars-triv triv))]
      [`(mset! ,loc ,index ,triv)
       `(mset! ,(implement-fvars-loc loc) ,(implement-fvars-opand index) ,(implement-fvars-triv triv))]
      [`(begin ,es ...)
       `(begin ,@(map implement-fvars-effect es))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(implement-fvars-pred pred)
            ,(implement-fvars-effect e1)
            ,(implement-fvars-effect e2))]
      [`(return-point ,label ,tail)
       `(return-point ,label ,(implement-fvars-tail tail))]))

  ;; nested-asm-lang-fvars-v8.pred -> nested-asm-lang-v8.pred
  (define (implement-fvars-pred pred)
    (match pred
      ['(true) pred]
      ['(false) pred]
      [`(not ,p) `(not ,(implement-fvars-pred p))]
      [`(begin ,es ... ,p)
       `(begin ,@(map implement-fvars-effect es) ,(implement-fvars-pred p))]
      [`(if ,p1 ,p2 ,p3)
       `(if ,(implement-fvars-pred p1)
            ,(implement-fvars-pred p2)
            ,(implement-fvars-pred p3))]
      [`(,relop ,loc ,opand)
       `(,relop ,(implement-fvars-loc loc) ,(implement-fvars-opand opand))]))

  ;; nested-asm-lang-fvars-v8.loc -> nested-asm-lang-v8.loc
  (define (implement-fvars-loc loc)
    (match loc
      [reg #:when (register? reg) reg]
      [fvar #:when (fvar? fvar) (fvar->addr fvar)]))

  ;; nested-asm-lang-fvars-v8.trg -> nested-asm-lang-v8.trg
  (define (implement-fvars-trg trg)
    (match trg
      [label #:when (label? label) label]
      [loc (implement-fvars-loc loc)]))

  ;; nested-asm-lang-fvars-v8.opand -> nested-asm-lang-v8.opand
  (define (implement-fvars-opand opand)
    (match opand
      [int64 #:when (int64? int64) int64]
      [loc (implement-fvars-loc loc)]))

  ;; nested-asm-lang-fvars-v8.triv -> nested-asm-lang-v8.triv
  (define (implement-fvars-triv triv)
    (match triv
      [label #:when (label? label) label]
      [opand (implement-fvars-opand opand)]))

  (match p
    [`(module (define ,ls ,ts) ... ,tail)
     (define funcs (map (lambda (l t) `(define ,l ,(implement-fvars-tail t))) ls ts))
     `(module ,@funcs ,(implement-fvars-tail tail))]))

