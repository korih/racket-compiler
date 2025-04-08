#lang racket

(require
  cpsc411/langs/v9)

(provide optimize-direct-calls)

;; just-exprs-lang-v9 -> just-exprs-lang-v9
;; optimizes p by inlining all direct calls to first-class procedures
(define/contract (optimize-direct-calls p)
  (-> just-exprs-lang-v9? just-exprs-lang-v9?)

  (define (optimize-direct-calls-value value)
    (match value
      [`(unsafe-procedure-call ,func ,params ...)
       (define func^ (optimize-direct-calls-value func))
       (define params^ (map optimize-direct-calls-value params))
       (match func
         [`(lambda (,args ...) ,v)
          `(let ,(map list args params^) ,(optimize-direct-calls-value v))]
         [_ `(unsafe-procedure-call ,func^ ,@params^)])]
      [`(if ,v1 ,v2 ,v3)
       `(if ,(optimize-direct-calls-value v1)
            ,(optimize-direct-calls-value v2)
            ,(optimize-direct-calls-value v3))]
      [`(begin ,effs ... ,v)
       `(begin ,@(map optimize-direct-calls-effect effs) ,(optimize-direct-calls-value v))]
      [`(letrec ([,alocs (lambda (,args ...) ,bodies)] ...) ,v)
       (define bindings
         (map (lambda (aloc args body)
                (list aloc `(lambda ,args ,(optimize-direct-calls-value body))))
              alocs args bodies))
       `(letrec ,bindings ,(optimize-direct-calls-value v))]
      [`(let ([,alocs ,vs] ...) ,v)
       (define bindings
         (map (lambda (aloc val)
                (list aloc (optimize-direct-calls-value val)))
              alocs vs))
       `(let ,bindings ,(optimize-direct-calls-value v))]
      [`(,primop ,vs ...)
       `(,primop ,@(map optimize-direct-calls-value vs))]
      [triv (optimize-direct-calls-triv triv)]))

  (define (optimize-direct-calls-effect effect)
    (match effect
      [`(begin ,effs ...)
       `(begin ,@(map optimize-direct-calls-effect effs))]
      [`(,primop ,vs ...)
       `(,primop ,@(map optimize-direct-calls-value vs))]))

  (define (optimize-direct-calls-triv triv)
    (match triv
      [`(lambda (,args ...) ,v)
       `(lambda (,@args) ,(optimize-direct-calls-value v))]
      ;; Wildcard collapse cased used because
      [_ triv]))

  (match p
    [`(module ,value)
     `(module ,(optimize-direct-calls-value value))]))

