#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5
  rackunit)

(provide optimize-predicates)

;; nested-asm-lang-v5 -> nested-asm-lang-v5
;; optimizes p by analyzing and simplifying predicates
(define/contract (optimize-predicates p)
  (-> nested-asm-lang-v5? nested-asm-lang-v5?)

  ;; (Env-of nested-asm-lang-v5.loc RangeValue)
  ;; invariant: env contains a mapping of the locations to their currently known value ranges
  (define env empty-env)

  ;; RangeValue is one-of:
  ;; - int64
  ;; - 'unknown

  ;; to get eval to work
  (define ns (make-base-namespace))

  (define (optimize-predicates/func f)
    (match f
      [`(define ,label ,tail)
       `(define ,label ,(optimize-predicates/tail tail))]))

  ;; nested-asm-lang-v5.tail -> nested-asm-lang-v5.tail
  (define (optimize-predicates/tail t)
    (match t
      [`(halt ,triv) `(halt ,(try-optimize-triv/triv triv))]
      [`(begin ,fx ... ,tail)
       `(begin ,@(map optimize-predicates/effect fx) ,(optimize-predicates/tail tail))]
      [`(jump ,trg) `(jump ,trg)]
      [`(if ,pred ,t-tail ,f-tail)
       (optimize-conditional pred
                             (optimize-predicates/tail t-tail)
                             (optimize-predicates/tail f-tail))]))

  ;; nested-asm-lang-v5.pred nested-asm-lang-v5.tail nested-asm-lang-v5.tail -> nested-asm-lang-v5.tail
  ;; OR
  ;; nested-asm-lang-v5.pred nested-asm-lang-v5.effect nested-asm-lang-v5.effect -> nested-asm-lang-v5.effect
  (define (optimize-conditional pred k-t k-f)
    (match pred
      ['(true) k-t]
      ['(false) k-f]
      [`(not ,pred) (optimize-conditional pred k-f k-t)]
      [`(begin ,fx ... ,pred)
       `(begin ,@(map optimize-predicates/effect fx) ,(optimize-conditional pred k-t k-f))]
      [`(if ,pred ,t-pred ,f-pred)
       (optimize-conditional pred
                             (optimize-conditional t-pred k-t k-f)
                             (optimize-conditional f-pred k-t k-f))]
      [`(,relop ,loc ,triv) (interp-relop-conditional relop loc triv k-t k-f)]))

  ;; nested-asm-lang-v5.effect -> nested-asm-lang-v5.effect
  (define (optimize-predicates/effect e)
    (match e
      [`(begin ,fx ...) `(begin ,@(map optimize-predicates/effect fx))]
      [`(if ,pred ,t-e ,f-e)
       (optimize-conditional pred
                             (optimize-predicates/effect t-e)
                             (optimize-predicates/effect f-e))]
      [`(set! ,loc (,binop ,loc ,triv))
       (define triv-rv (interp-triv triv))
       (define updated-rv (interp-binop/range-value binop (interp-triv loc) triv-rv))
       (set! env (extend-env env loc updated-rv))
       `(set! ,loc (,binop ,loc ,(try-optimize-triv/triv triv)))]
      [`(set! ,loc ,triv) (set! env (extend-env env loc (interp-triv triv)))
                          `(set! ,loc ,(try-optimize-triv/triv triv))]))

  ;; nested-asm-lang-v5.binop RangeValue RangeValue -> RangeValue
  ;; interp. the known abstract value resulting from the binary operation
  (define (interp-binop/range-value binop val1 val2)
    (match (cons val1 val2)
      [(cons a b) #:when (and (int64? a) (int64? b))
                  (interp-binop binop a b)]
      ; In all other cases, we don't know the range of the result because an overflow is unpredictable
      [_ 'unknown]))

  ;; nested-asm-lang-v5.binop int64 int64 -> RangeValue
  ;; interp. the known abstract value resulting from the binary operation
  (define (interp-binop binop a b)
    (match binop
      ['* (x64-mul a b)]
      ['+ (x64-add a b)]))

  ;; nested-asm-lang-v5.triv -> RangeValue
  ;; interp. the known value or range of the triv
  (define (interp-triv triv)
    (match triv
      [x #:when (int64? x) x]
      [loc (with-handlers ([exn:fail? (lambda (_) 'unknown)])
             (lookup-env env loc))]))

  ;; nested-asm-lang-v5.relop nested-asm-lang-v5.loc nested-asm-lang-v5.triv nested-asm-lang-v5.tail nested-asm-lang-v5.tail -> nested-asm-lang-v5.tail
  ;; OR
  ;; nested-asm-lang-v5.relop nested-asm-lang-v5.loc nested-asm-lang-v5.triv nested-asm-lang-v5.effect nested-asm-lang-v5.effect -> nested-asm-lang-v5.effect
  (define (interp-relop-conditional relop loc triv k-t k-f)
    (define op1 (interp-triv loc))
    (define op2 (interp-triv triv))
    (cond
      [(interp-relop-optimize-true? relop op1 op2) k-t]
      [(interp-relop-optimize-false? relop op1 op2) k-f]
      [else `(if (,relop ,loc ,triv) ,k-t ,k-f)]))


  ;; nested-asm-lang-v5.relop RangeValue RangeValue -> boolean
  ;; interp. true if the relop can be optimized to true
  (define (interp-relop-optimize-true? relop op1 op2)
    (match (cons op1 op2)
      [(cons a b) #:when (and (int64? a) (int64? b))
                  (eval (list relop a b) ns)]
      ; In all other cases, we don't know the range of the result
      [_ #f]))

  ;; nested-asm-lang-v5.relop nested-asm-lang-v5.triv nested-asm-lang-v5.triv -> boolean
  ;; interp. true if the relop can be optimized to false (the relop is guaranteed to be false)
  (define (interp-relop-optimize-false? relop op1 op2)
    (match (cons op1 op2)
      [(cons a b) #:when (and (int64? a) (int64? b))
                  (not (eval (list relop a b) ns))]
      ; In all other cases, we don't know the range of the result
      [_ #f]))

  ;; nested-asm-lang-v5.triv -> nested-asm-lang-v5.triv
  ;; interp. optimize the triv if possible
  (define (try-optimize-triv/triv triv)
    (match triv
      [label #:when (label? label) label]
      [opand (try-optimize-triv/opand opand)]))

  ;; nested-asm-lang-v5.opand -> nested-asm-lang-v5.triv
  ;; interp. optimize the opand if possible
  (define (try-optimize-triv/opand opand)
    (match opand
      [x #:when (int64? x) x]
      [loc (try-optimize-triv/loc loc)]))

  ;; nested-asm-lang-v5.loc -> nested-asm-lang-v5.triv
  ;; interp. optimize the loc if possible
  (define (try-optimize-triv/loc loc)
    (with-handlers ([exn:fail? (lambda (_) loc)])
      (match (lookup-env env loc)
        ['unknown loc]
        [x #:when (int64? x) x])))

  (match p
    [`(module ,funcs ... ,tail)
     (define optimized-funcs (for/list ([f funcs])
                               (define optimized-f (optimize-predicates/func f))
                               (set! env empty-env)
                               optimized-f))
     `(module ,@optimized-funcs ,(optimize-predicates/tail tail))]))


(module+ test
  (check-equal? (optimize-predicates '(module (define L.f.1 (halt 1))
                                        (jump L.f.1)))
                '(module (define L.f.1 (halt 1)) (jump L.f.1)))
  (check-equal? (optimize-predicates '(module
                                          (define L.f.1 (begin (set! rsp 1) (set! rsp (* rsp 2)) (halt rsp)))
                                        (jump L.f.1)))
                '(module
                     (define L.f.1 (begin (set! rsp 1) (set! rsp (* rsp 2)) (halt 2)))
                   (jump L.f.1)))
  (check-equal? (optimize-predicates '(module
                                          (define L.f.1
                                            (begin
                                              (set! rsi rdi)
                                              (set! rdx rsi)
                                              (set! rcx rdx)
                                              (set! rbx rcx)
                                              (set! rsp r8)
                                              (set! rdi r9)
                                              (set! rsi (+ rsi rdx))
                                              (set! rsi (+ rsi rcx))
                                              (set! rsi (+ rsi rbx))
                                              (set! rsi (+ rsi rsp))
                                              (set! rsi (+ rsi rdi))
                                              (halt rsi)))
                                        (begin
                                          (set! r9 6)
                                          (set! r8 5)
                                          (set! rcx 4)
                                          (set! rdx 3)
                                          (set! rsi 2)
                                          (set! rdi 1)
                                          (jump L.f.1))))
                '(module
                     (define L.f.1
                       (begin
                         (set! rsi rdi)
                         (set! rdx rsi)
                         (set! rcx rdx)
                         (set! rbx rcx)
                         (set! rsp r8)
                         (set! rdi r9)
                         (set! rsi (+ rsi rdx))
                         (set! rsi (+ rsi rcx))
                         (set! rsi (+ rsi rbx))
                         (set! rsi (+ rsi rsp))
                         (set! rsi (+ rsi rdi))
                         (halt rsi)))
                   (begin
                     (set! r9 6)
                     (set! r8 5)
                     (set! rcx 4)
                     (set! rdx 3)
                     (set! rsi 2)
                     (set! rdi 1)
                     (jump L.f.1))))
  (check-equal? (optimize-predicates '(module
                                          (define L.f.1 (begin (set! rsp rdi) (halt rsp)))
                                        (define L.g.1
                                          (begin
                                            (set! rbx rdi)
                                            (set! rsp rsi)
                                            (set! rsp rdx)
                                            (set! rdi rbx)
                                            (jump L.f.1)))
                                        (if (true)
                                            (begin (set! rdx 3) (set! rsi 2) (set! rdi 1) (jump L.g.1))
                                            (begin (set! rdi 1) (jump L.f.1)))))
                '(module
                     (define L.f.1 (begin (set! rsp rdi) (halt rsp)))
                   (define L.g.1
                     (begin
                       (set! rbx rdi)
                       (set! rsp rsi)
                       (set! rsp rdx)
                       (set! rdi rbx)
                       (jump L.f.1)))
                   (begin (set! rdx 3) (set! rsi 2) (set! rdi 1) (jump L.g.1))))
  (check-equal? (optimize-predicates '(module (if (true) (halt 0) (halt 1)))) '(module (halt 0)))
  (check-equal? (optimize-predicates '(module (if (false) (begin (set! rax 1) (halt rax)) (halt 0))))
                '(module (halt 0)))
  (check-equal? (optimize-predicates '(module (if (not (true)) (halt 0) (begin (set! rax 1) (halt rax)))))
                '(module (begin (set! rax 1) (halt 1))))
  (check-equal? (optimize-predicates '(module (if (if (true) (false) (true)) (halt 0) (halt 1))))
                '(module (halt 1)))
  (check-equal? (optimize-predicates '(module (begin (set! rax 1) (if (> rax 0) (halt 0) (halt 1)))))
                '(module (begin (set! rax 1) (halt 0))))
  (check-equal? (optimize-predicates '(module (begin (set! rbx 1) (if (< rbx 1) (halt 0) (halt 3)))))
                '(module (begin (set! rbx 1) (halt 3))))
  (check-equal? (optimize-predicates `(module (begin (set! rbx rax) (if (<= rbx ,(max-int 64)) (halt 12) (halt 133)))))
                '(module
                     (begin
                       (set! rbx rax)
                       (if (<= rbx 9223372036854775807) (halt 12) (halt 133)))))
  (check-equal? (optimize-predicates '(module (begin (set! rbx rax)
                                                     (if (= rbx rax)
                                                         (begin (set! rax rcx) (halt rax))
                                                         (halt rax)))))
                '(module
                     (begin
                       (set! rbx rax)
                       (if (= rbx rax) (begin (set! rax rcx) (halt rax)) (halt rax)))))
  (check-equal? (optimize-predicates '(module (begin (set! rbx 33) (if (= rbx 44) (halt -1) (halt 0)))))
                '(module (begin (set! rbx 33) (halt 0))))
  (check-equal? (optimize-predicates '(module (begin (set! rbx rcx) (if (= rbx 5) (halt 0) (halt 1)))))
                '(module (begin (set! rbx rcx) (if (= rbx 5) (halt 0) (halt 1)))))
  (check-equal? (optimize-predicates '(module (begin (set! rbx 33) (if (= rbx 44) (halt -1) (halt 3)))))
                '(module (begin (set! rbx 33) (halt 3))))
  (check-equal? (optimize-predicates `(module (begin (set! rcx rbx)
                                                     (if (begin (set! rcx (+ rcx 10))
                                                                (< rcx ,(+ (min-int 64) 10)))
                                                         (halt 35)
                                                         (halt 99)))))
                '(module
                     (begin
                       (set! rcx rbx)
                       (begin (set! rcx (+ rcx 10))
                              (if (< rcx -9223372036854775798)
                                  (halt 35)
                                  (halt 99))))))
  (check-equal? (optimize-predicates '(module
                                          (define L.f.1 (begin (set! rbx 3) (set! rax rbx) (jump L.f.2)))
                                        (define L.f.2 (halt rax))
                                        (begin
                                          (set! rbx 5)
                                          (set! rcx 4)
                                          (set! rax rcx)
                                          (jump L.f.2))))
                '(module
                     (define L.f.1 (begin (set! rbx 3) (set! rax 3) (jump L.f.2)))
                   (define L.f.2 (halt rax))
                   (begin (set! rbx 5) (set! rcx 4) (set! rax 4) (jump L.f.2))))
  (check-equal? (optimize-predicates '(module (define L.func.1 (begin (set! rax 1) (jump L.func.2)))
                                        (define L.func.2 (begin (if (> rax 0) (halt -1) (halt 1))))
                                        (begin
                                          (set! rax -1)
                                          (jump L.func.2))))
                '(module
                     (define L.func.1 (begin (set! rax 1) (jump L.func.2)))
                   (define L.func.2 (begin (if (> rax 0) (halt -1) (halt 1))))
                   (begin (set! rax -1) (jump L.func.2))))
  (check-equal? (optimize-predicates
                 '(module (begin (set! rax 1)
                                 (if (> rax 0)
                                     (halt rax)
                                     (begin (set! rax -1)
                                            (halt rax))))))
                '(module (begin (set! rax 1) (halt 1)))))


