#lang racket

(require
  "common.rkt"
  cpsc411/compiler-lib
  cpsc411/langs/v9)

(provide dox-lambdas)

;; just-exprs-lang-v9 -> lam-opticon-lang-v9
;; compiles p to Lam-opticon-lang-v9 by explicitly binding all procedures to
;; abstract locations
(define/contract (dox-lambdas p)
  (-> just-exprs-lang-v9? lam-opticon-lang-v9?)

  ;; just-exprs-lang-v9.value -> lam-opticon-lang-v9.value
  ;; interp. compiles the values expression, binding all procedures to abstract
  ;; locations in value position
  (define (dox-lambdas/value v)
    (match v
      [`(unsafe-procedure-call ,procV ,argsV ...)
       `(unsafe-procedure-call ,(dox-lambdas/value procV)
                               ,@(map dox-lambdas/value argsV))]
      [`(letrec ([,aloc1 (lambda (,lambdaArgs ...) ,lambdaVs)] ...) ,v)
       (define compiled-lambda-vs (map dox-lambdas/value lambdaVs))
       `(letrec ,(for/foldr ([acc '()])
                   ([compiled-lambda-body compiled-lambda-vs]
                    [args lambdaArgs]
                    [aloc aloc1])
                   (cons `(,aloc (lambda ,args ,compiled-lambda-body)) acc))
          ,(dox-lambdas/value v))]
      [`(let ([,aloc ,v] ...) ,bodyV)
       (define compiled-vs (map dox-lambdas/value v))
       `(let ,(for/foldr ([acc '()])
                ([compiled-v compiled-vs]
                 [a aloc])
                (cons `(,a ,compiled-v) acc))
          ,(dox-lambdas/value bodyV))]
      [`(if ,predV ,cV ,aV)
       `(if ,(dox-lambdas/value predV)
            ,(dox-lambdas/value cV)
            ,(dox-lambdas/value aV))]
      [`(begin ,e ... ,v)
       `(begin ,@(map dox-lambdas/effect e) ,(dox-lambdas/value v))]
      [`(,primop ,v ...)
       #:when (unsafe-primop? primop)
       `(,primop ,@(map dox-lambdas/value v))]
      [triv (dox-lambdas/triv triv)]))

  ;; just-exprs-lang-v9.effect -> lam-opticon-lang-v9.effect
  ;; interp. compiles the effect expression, binding all procedures to abstract
  ;; locations in effect position
  (define (dox-lambdas/effect e)
    (match e
      [`(begin ,e ...)
       `(begin ,@(map dox-lambdas/effect e))]
      [`(,primop ,vs ...)
       `(,primop ,@(map dox-lambdas/value vs))]))

  ;; just-exprs-lang-v9.triv -> lam-opticon-lang-v9.value
  ;; interp. compiles the triv expression, value expressions can replace triv
  ;; expressions in src expression
  (define (dox-lambdas/triv t)
    (match t
      [#f #f]
      [#t #t]
      ['empty 'empty]
      ['(void) '(void)]
      [`(error ,i) `(error ,i)]
      [`(lambda (,alocs ...) ,v)
       (define fn (fresh 'lam))
       `(letrec ([,fn (lambda ,alocs ,(dox-lambdas/value v))])
          ,fn)]
      [i #:when (fixnum? i) i]
      [a #:when (aloc? a) a]
      [c c]))

  (match p
    [`(module ,value) `(module ,(dox-lambdas/value value))]))

