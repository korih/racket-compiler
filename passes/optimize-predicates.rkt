#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide optimize-predicates)

;; nested-asm-lang-v8 -> nested-asm-lang-v8
;; optimizes p by analyzing and simplifying predicates
(define/contract (optimize-predicates p)
  (-> nested-asm-lang-fvars-v8? nested-asm-lang-fvars-v8?)

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

  ;; nested-asm-lang-v8.tail -> nested-asm-lang-v8.tail
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

  ;; nested-asm-lang-v8.pred nested-asm-lang-v8.tail nested-asm-lang-v8.tail -> nested-asm-lang-v8.tail
  ;; OR
  ;; nested-asm-lang-v8.pred nested-asm-lang-v8.effect nested-asm-lang-v8.effect -> nested-asm-lang-v8.effect
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

  ;; nested-asm-lang-v8.effect -> nested-asm-lang-v8.effect
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
       (values `(begin ,@(reverse optimized-fx)) eff-env)]
      [`(if ,pred ,t-e ,f-e)
       (define-values (t-e^ t-env) (optimize-predicates/effect t-e env))
       (define-values (f-e^ f-env) (optimize-predicates/effect f-e env))
       (values (optimize-conditional pred
                                     t-e^
                                     f-e^
                                     env)
               env)]
      [`(set! ,loc1 (mref ,loc2 ,index))
       (values `(set! ,loc1 (mref ,loc2 ,index)) (extend-env env loc1 'unknown))]
      [`(set! ,loc (,binop ,loc ,triv))
       (define triv-rv (interp-triv triv env))
       (define updated-rv (interp-binop/range-value binop (interp-triv loc env) triv-rv))
       (define env^ (extend-env env loc updated-rv))
       (values `(set! ,loc (,binop ,loc ,triv)) env^)]
      [`(set! ,loc ,triv)
       (define env^ (extend-env env loc (interp-triv triv env)))
       (values `(set! ,loc ,triv) env^)]
      [`(mset! ,loc ,index ,triv)
       (values `(mset! ,loc ,index ,triv) env)]
      [`(return-point ,label ,tail) (define tail^ (optimize-predicates/tail tail env))
                                    (values `(return-point ,label ,tail^) env)]))

  ;; nested-asm-lang-v8.binop RangeValue RangeValue -> RangeValue
  ;; interp. the known abstract value resulting from the binary operation
  (define (interp-binop/range-value binop val1 val2)
    (match (cons val1 val2)
      [(cons a b) #:when (and (int64? a) (int64? b))
                  (interp-binop binop a b)]
      ; In all other cases, we don't know the range of the result because an overflow is unpredictable
      [_ 'unknown]))

  ;; nested-asm-lang-v8.binop int64 int64 -> RangeValue
  ;; interp. the known abstract value resulting from the binary operation
  (define (interp-binop binop a b)
    (match binop
      ['* (x64-mul a b)]
      ['+ (x64-add a b)]
      ['- (x64-sub a b)]
      ['bitwise-and (bitwise-and a b)]
      ['bitwise-ior (bitwise-ior a b)]
      ['bitwise-xor (bitwise-xor a b)]
      ['arithmetic-shift-right (arithmetic-shift a (- b))]))

  ;; nested-asm-lang-v8.triv -> RangeValue
  ;; interp. the known value or range of the triv
  (define (interp-triv triv env)
    (match triv
      [x #:when (int64? x) x]
      [loc (with-handlers ([exn:fail? (lambda (_) 'unknown)])
             (lookup-env env loc))]))

  ;; nested-asm-lang-v8.relop nested-asm-lang-v8.loc nested-asm-lang-v8.triv nested-asm-lang-v8.tail nested-asm-lang-v8.tail -> nested-asm-lang-v8.tail
  ;; OR
  ;; nested-asm-lang-v8.relop nested-asm-lang-v8.loc nested-asm-lang-v8.triv nested-asm-lang-v8.effect nested-asm-lang-v8.effect -> nested-asm-lang-v8.effect
  (define (interp-relop-conditional relop loc triv k-t k-f env)
    (define op1 (interp-triv loc env))
    (define op2 (interp-triv triv env))
    (cond
      [(interp-relop-optimize-true? relop op1 op2) k-t]
      [(interp-relop-optimize-false? relop op1 op2) k-f]
      [else `(if (,relop ,loc ,triv) ,k-t ,k-f)]))

  (define (match-relop-to-racket relop)
    (match relop
      ['< '<]
      ['<= '<=]
      ['> '>]
      ['>= '>=]
      ['= '=]
      ['!= 'not-equal]))

  ;; nested-asm-lang-v8.relop RangeValue RangeValue -> boolean
  ;; interp. true if the relop can be optimized to true
  (define (interp-relop-optimize-true? relop op1 op2)
    (match (cons op1 op2)
      [(cons a b) #:when (and (int64? a) (int64? b))
                  (if (eq? relop '!=)
                      (eval `(not (= ,a ,b)) ns)
                      (eval `(,relop ,a ,b) ns))]
      ; In all other cases, we don't know the range of the result
      [_ #f]))

  ;; nested-asm-lang-v8.relop nested-asm-lang-v8.triv nested-asm-lang-v8.triv -> boolean
  ;; interp. true if the relop can be optimized to false (the relop is guaranteed to be false)
  (define (interp-relop-optimize-false? relop op1 op2)
    (match (cons op1 op2)
      [(cons a b) #:when (and (int64? a) (int64? b))
                  (if (eq? relop '!=)
                      (eval `(not (= ,a ,b)) ns)
                      (eval `(,relop ,a ,b) ns))]
      ; In all other cases, we don't know the range of the result
      [_ #f]))

  ;; nested-asm-lang-v8.triv -> nested-asm-lang-v8.triv
  ;; interp. optimize the triv if possible
  (define (try-optimize-triv/triv triv env)
    (match triv
      [label #:when (label? label) label]
      [opand (try-optimize-triv/opand opand env)]))

  ;; nested-asm-lang-v8.opand -> nested-asm-lang-v8.triv
  ;; interp. optimize the opand if possible
  (define (try-optimize-triv/opand opand env)
    (match opand
      [x #:when (int64? x) x]
      [loc (try-optimize-triv/loc loc env)]))

  ;; nested-asm-lang-v8.loc -> nested-asm-lang-v8.triv
  ;; interp. optimize the loc if possible
  (define (try-optimize-triv/loc loc env)
    (with-handlers ([exn:fail? (lambda (_) loc)])
      (match (lookup-env env loc)
        ['unknown loc]
        [x #:when (int64? x) x])))

  (match p
    [`(module ,funcs ... ,tail)
     (define optimized-funcs (for/list ([f funcs])
                               (define optimized-f (optimize-predicates/func f))
                               optimized-f))
     `(module ,@optimized-funcs ,(optimize-predicates/tail tail empty-env))]))

