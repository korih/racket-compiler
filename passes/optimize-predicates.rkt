#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v7
  rackunit)

(provide optimize-predicates)

;; nested-asm-lang-v7 -> nested-asm-lang-v7
;; optimizes p by analyzing and simplifying predicates
(define/contract (optimize-predicates p)
  (-> nested-asm-lang-fvars-v7? nested-asm-lang-fvars-v7?)

  ;; func is `(define ,label ,tail)
  ;; interp. a function definition

  ;; RangeValue is one-of:
  ;; - int64
  ;; - 'unknown

  ;; to get eval to work
  (define ns (make-base-namespace))

  ;; func is `(define ,label ,tail)
  ;; interp. a function definition
  (define (optimize-predicates/func f)
    (match f
      [`(define ,label ,tail)
       `(define ,label ,(optimize-predicates/tail tail empty-env))]))

  ;; nested-asm-lang-v7.tail -> nested-asm-lang-v7.tail
  (define (optimize-predicates/tail t env)
    (match t
      [`(begin ,fx ... ,tail)

       (define-values (optimized-fx eff-env)
         (for/fold ([fx^ '()]
                    [effect-env empty-env])
                   ([f fx])
           (define-values (f^ env^)
             (optimize-predicates/effect f effect-env))
           (values (cons f^ fx^) env^)))

       `(begin ,@(reverse optimized-fx) ,(optimize-predicates/tail tail eff-env))]
      [`(jump ,trg) `(jump ,trg)]
      [`(if ,pred ,t-tail ,f-tail)
       (optimize-conditional pred
                             (optimize-predicates/tail t-tail env)
                             (optimize-predicates/tail f-tail env)
                             env)]))

  ;; nested-asm-lang-v7.pred nested-asm-lang-v7.tail nested-asm-lang-v7.tail -> nested-asm-lang-v7.tail
  ;; OR
  ;; nested-asm-lang-v7.pred nested-asm-lang-v7.effect nested-asm-lang-v7.effect -> nested-asm-lang-v7.effect
  (define (optimize-conditional pred k-t k-f env)
    (match pred
      ['(true) k-t]
      ['(false) k-f]
      [`(not ,pred) (optimize-conditional pred k-f k-t env)]
      [`(begin ,fx ... ,pred)

       (define-values (optimized-fx eff-env)
         (for/fold ([fx^ '()]
                    [effect-env empty-env])
                   ([f fx])
           (define-values (f^ env^)
             (optimize-predicates/effect f effect-env))
           (values (cons f^ fx^) env^)))

       `(begin ,@(reverse optimized-fx) ,(optimize-conditional pred k-t k-f eff-env))]
      [`(if ,pred ,t-pred ,f-pred)
       (optimize-conditional pred
                             (optimize-conditional t-pred k-t k-f env)
                             (optimize-conditional f-pred k-t k-f env)
                             env)]
      [`(,relop ,loc ,triv)  (interp-relop-conditional relop loc triv k-t k-f env)]))

  ;; nested-asm-lang-v7.effect -> nested-asm-lang-v7.effect
  (define (optimize-predicates/effect e env)
    (match e
      [`(begin ,fx ...)

       (define-values (optimized-fx eff-env)
         (for/fold ([fx^ '()]
                    [effect-env empty-env])
                   ([f fx])
           (define-values (f^ env^)
             (optimize-predicates/effect f effect-env))
           (values (cons f^ fx^) env^)))
       (values `(begin ,@(reverse optimized-fx)) eff-env)
       #;
       `(begin ,@(map (lambda (f) (optimize-predicates/effect f env)) fx))]
      [`(if ,pred ,t-e ,f-e)
       (define-values (t-e^ t-env) (optimize-predicates/effect t-e env))
       (define-values (f-e^ f-env) (optimize-predicates/effect f-e env))
       (values (optimize-conditional pred
                                     t-e^
                                     f-e^
                                     env)
               env)]
      [`(set! ,loc (,binop ,loc ,triv))
       (define triv-rv (interp-triv triv env))
       (define updated-rv (interp-binop/range-value binop (interp-triv loc env) triv-rv))
       (define env^ (extend-env env loc updated-rv))
        ;; don't try to optimize triv any more
       (values `(set! ,loc (,binop ,loc ,triv)) env^)]
      [`(set! ,loc ,triv)
       (define env^ (extend-env env loc (interp-triv triv env)))
        ;; don't try to optimize triv any more
       (values `(set! ,loc ,triv) env^)]
      [`(return-point ,label ,tail) (define tail^ (optimize-predicates/tail tail env))
                                    (values `(return-point ,label ,tail^) env)]))

  ;; nested-asm-lang-v7.binop RangeValue RangeValue -> RangeValue
  ;; interp. the known abstract value resulting from the binary operation
  (define (interp-binop/range-value binop val1 val2)
    (match (cons val1 val2)
      [(cons a b) #:when (and (int64? a) (int64? b))
                  (interp-binop binop a b)]
      ; In all other cases, we don't know the range of the result because an overflow is unpredictable
      [_ 'unknown]))

  ;; nested-asm-lang-v7.binop int64 int64 -> RangeValue
  ;; interp. the known abstract value resulting from the binary operation
  (define (interp-binop binop a b)
    (match binop
      ['* (x64-mul a b)]
      ['+ (x64-add a b)]))

  ;; nested-asm-lang-v7.triv -> RangeValue
  ;; interp. the known value or range of the triv
  (define (interp-triv triv env)
    (match triv
      [x #:when (int64? x) x]
      [loc (with-handlers ([exn:fail? (lambda (_) 'unknown)])
             (lookup-env env loc))]))

  ;; nested-asm-lang-v7.relop nested-asm-lang-v7.loc nested-asm-lang-v7.triv nested-asm-lang-v7.tail nested-asm-lang-v7.tail -> nested-asm-lang-v7.tail
  ;; OR
  ;; nested-asm-lang-v7.relop nested-asm-lang-v7.loc nested-asm-lang-v7.triv nested-asm-lang-v7.effect nested-asm-lang-v7.effect -> nested-asm-lang-v7.effect
  (define (interp-relop-conditional relop loc triv k-t k-f env)
    (define op1 (interp-triv loc env))
    (define op2 (interp-triv triv env))
    (cond
      [(interp-relop-optimize-true? relop op1 op2) k-t]
      [(interp-relop-optimize-false? relop op1 op2) k-f]
      [else `(if (,relop ,loc ,triv) ,k-t ,k-f)]))


  ;; nested-asm-lang-v7.relop RangeValue RangeValue -> boolean
  ;; interp. true if the relop can be optimized to true
  (define (interp-relop-optimize-true? relop op1 op2)
    ;(begin (printf op1))
    (match (cons op1 op2)
      [(cons a b) #:when (and (int64? a) (int64? b))
                  (eval (list relop a b) ns)]
      ; In all other cases, we don't know the range of the result
      [_ #f]))

  ;; nested-asm-lang-v7.relop nested-asm-lang-v7.triv nested-asm-lang-v7.triv -> boolean
  ;; interp. true if the relop can be optimized to false (the relop is guaranteed to be false)
  (define (interp-relop-optimize-false? relop op1 op2)
    (match (cons op1 op2)
      [(cons a b) #:when (and (int64? a) (int64? b))
                  (not (eval (list relop a b) ns))]
      ; In all other cases, we don't know the range of the result
      [_ #f]))

  ;; nested-asm-lang-v7.triv -> nested-asm-lang-v7.triv
  ;; interp. optimize the triv if possible
  (define (try-optimize-triv/triv triv env)
    (match triv
      [label #:when (label? label) label]
      [opand (try-optimize-triv/opand opand env)]))

  ;; nested-asm-lang-v7.opand -> nested-asm-lang-v7.triv
  ;; interp. optimize the opand if possible
  (define (try-optimize-triv/opand opand env)
    (match opand
      [x #:when (int64? x) x]
      [loc (try-optimize-triv/loc loc env)]))

  ;; nested-asm-lang-v7.loc -> nested-asm-lang-v7.triv
  ;; interp. optimize the loc if possible
  (define (try-optimize-triv/loc loc env)
    (with-handlers ([exn:fail? (lambda (_) loc)])
      (match (lookup-env env loc)
        ['unknown loc]
        [x #:when (int64? x) x])))

  (match p
    [`(module ,funcs ... ,tail)
     (define optimized-funcs (for/list ([f funcs])
                               #;
                               (set! env empty-env)
                               (define optimized-f (optimize-predicates/func f))
                               optimized-f))
     `(module ,@optimized-funcs ,(optimize-predicates/tail tail empty-env))]))

(module+ test
  (check-equal? (optimize-predicates '(module
                                          (begin
                                            (set! fv2 0)
                                            (set! fv0 0)
                                            (set! fv1 fv2)
                                            (set! fv0 (+ fv0 fv2))
                                            (set! fv0 (+ fv0 fv1))
                                            (begin (set! rax fv0) (jump r15)))))
                '(module
                     (begin
                       (set! fv2 0)
                       (set! fv0 0)
                       (set! fv1 fv2)
                       (set! fv0 (+ fv0 fv2))
                       (set! fv0 (+ fv0 fv1))
                       (begin (set! rax fv0) (jump r15)))))

  (check-equal? (optimize-predicates '(module (define L.f.1 (jump done))
                                        (jump L.f.1)))
                '(module (define L.f.1 (jump done)) (jump L.f.1)))
  (check-equal? (optimize-predicates '(module
                                          (define L.f.1 (begin (set! rsp 1) (set! rsp (* rsp 2)) (jump done)))
                                        (jump L.f.1)))
                '(module (define L.f.1 (begin (set! rsp 1) (set! rsp (* rsp 2)) (jump done)))
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
                                              (jump done)))
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
                         (jump done)))
                   (begin
                     (set! r9 6)
                     (set! r8 5)
                     (set! rcx 4)
                     (set! rdx 3)
                     (set! rsi 2)
                     (set! rdi 1)
                     (jump L.f.1))))
  (check-equal? (optimize-predicates '(module
                                          (define L.f.1 (begin (set! rsp rdi) (jump done)))
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
                     (define L.f.1 (begin (set! rsp rdi) (jump done)))
                   (define L.g.1
                     (begin
                       (set! rbx rdi)
                       (set! rsp rsi)
                       (set! rsp rdx)
                       (set! rdi rbx)
                       (jump L.f.1)))
                   (begin (set! rdx 3) (set! rsi 2) (set! rdi 1) (jump L.g.1))))
  (check-equal? (optimize-predicates '(module (if (true) (jump done) (jump done)))) '(module (jump done)))
  (check-equal? (optimize-predicates '(module (if (false) (begin (set! rax 1) (jump done)) (jump done))))
                '(module (jump done)))
  (check-equal? (optimize-predicates '(module (if (not (true)) (jump done) (begin (set! rax 1) (jump done)))))
                '(module (begin (set! rax 1) (jump done))))
  (check-equal? (optimize-predicates '(module (if (if (true) (false) (true)) (jump done) (jump done))))
                '(module (jump done)))
  (check-equal? (optimize-predicates '(module (begin (set! rax 1) (if (> rax 0) (jump done) (jump done)))))
                '(module (begin (set! rax 1) (jump done))))
  (check-equal? (optimize-predicates '(module (begin (set! rbx 1) (if (< rbx 1) (jump done) (jump done)))))
                '(module (begin (set! rbx 1) (jump done))))
  (check-equal? (optimize-predicates `(module (begin (set! rbx rax) (if (<= rbx ,(max-int 64)) (jump done) (jump done)))))
                '(module
                     (begin
                       (set! rbx rax)
                       (if (<= rbx 9223372036854775807) (jump done) (jump done)))))
  (check-equal? (optimize-predicates '(module (begin (set! rbx rax)
                                                     (if (= rbx rax)
                                                         (begin (set! rax rcx) (jump done))
                                                         (jump done)))))
                '(module
                     (begin
                       (set! rbx rax)
                       (if (= rbx rax) (begin (set! rax rcx) (jump done)) (jump done)))))
  (check-equal? (optimize-predicates '(module (begin (set! rbx 33) (if (= rbx 44) (jump done) (jump done)))))
                '(module (begin (set! rbx 33) (jump done))))
  (check-equal? (optimize-predicates '(module (begin (set! rbx rcx) (if (= rbx 5) (jump done) (jump done)))))
                '(module (begin (set! rbx rcx) (if (= rbx 5) (jump done) (jump done)))))
  (check-equal? (optimize-predicates '(module (begin (set! rbx 33) (if (= rbx 44) (jump done) (jump done)))))
                '(module (begin (set! rbx 33) (jump done))))
  (check-equal? (optimize-predicates `(module (begin (set! rcx rbx)
                                                     (if (begin (set! rcx (+ rcx 10))
                                                                (< rcx ,(+ (min-int 64) 10)))
                                                         (jump done)
                                                         (jump done)))))
                '(module
                     (begin
                       (set! rcx rbx)
                       (begin (set! rcx (+ rcx 10))
                              (if (< rcx -9223372036854775798)
                                  (jump done)
                                  (jump done))))))
  (check-equal? (optimize-predicates '(module
                                          (define L.f.1 (begin (set! rbx 3) (set! rax rbx) (jump L.f.2)))
                                        (define L.f.2 (jump done))
                                        (begin
                                          (set! rbx 5)
                                          (set! rcx 4)
                                          (set! rax rcx)
                                          (jump L.f.2))))
                '(module
                     (define L.f.1 (begin (set! rbx 3) (set! rax rbx) (jump L.f.2)))
                   (define L.f.2 (jump done))
                   (begin (set! rbx 5) (set! rcx 4) (set! rax rcx) (jump L.f.2))))
  (check-equal? (optimize-predicates '(module (define L.func.1 (begin (set! rax 1) (jump L.func.2)))
                                        (define L.func.2 (begin (if (> rax 0) (jump done) (jump done))))
                                        (begin
                                          (set! rax -1)
                                          (jump L.func.2))))
                '(module
                     (define L.func.1 (begin (set! rax 1) (jump L.func.2)))
                   (define L.func.2 (begin (if (> rax 0) (jump done) (jump done))))
                   (begin (set! rax -1) (jump L.func.2))))
  (check-equal? (optimize-predicates
                 '(module (begin (set! rbx rax)
                                 (set! rbx (+ rbx 10))
                                 (if (> rbx 0)
                                     (jump done)
                                     (jump done)))))
                '(module
                     (begin
                       (set! rbx rax)
                       (set! rbx (+ rbx 10))
                       (if (> rbx 0) (jump done) (jump done)))))
  (check-equal? (optimize-predicates
                 '(module (begin (set! rax 1)
                                 (if (> rax 0)
                                     (jump done)
                                     (begin (set! rax -1)
                                            (jump done))))))
                '(module (begin (set! rax 1) (jump done))))
  (check-equal? (optimize-predicates '(module
                                          (begin (set! r8 0)
                                                 (set! r9 0)
                                                 (if
                                                  (not (if (true) (> r8 5) (< r9 6)))
                                                  (set! r12 15)
                                                  (set! r12 90))
                                                 (jump done))))
                '(module (begin (set! r8 0) (set! r9 0) (set! r12 15) (jump done))))

  (check-equal? (optimize-predicates '(module
                                          (begin
                                            (set! rsp r15) (set! rbx 5)
                                            (if (true)
                                                (begin (set! rbx rbx) (set! rbx (+ rbx 17)) (set! rbx 12))
                                                (begin (set! rbx 15)))
                                            (set! rax rbx)
                                            (jump rsp))))
                '(module
                     (begin
                       (set! rsp r15)
                       (set! rbx 5)
                       (begin (set! rbx rbx) (set! rbx (+ rbx 17)) (set! rbx 12))
                       (set! rax rbx)
                       (jump rsp))))

  )
