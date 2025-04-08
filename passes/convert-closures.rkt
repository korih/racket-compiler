#lang racket

(require
  "common.rkt"
  cpsc411/compiler-lib
  cpsc411/langs/v9)

(provide convert-closures)

;; lam-free-lang-v9 -> closure-lang-v9
;; compiles p to Closure-lang v9 by performing closure conversion, converting
;; all procedures into explicit closures
(define/contract (convert-closures p)
  (-> lam-free-lang-v9? closure-lang-v9?)

  ;; lam-free-lang-v9.value -> closure-lang-v9.value
  ;; performs closure conversion on the value
  (define (convert-closures/value value)
    (match value
      [`(unsafe-procedure-call ,fun ,args ...)
       `(closure-call ,fun ,fun ,@(map convert-closures/value args))]
      [`(letrec ([,fun-names (lambda ,infos (,alocs ...) ,vs)] ...) ,value)
       (define-values (letrec-clauses cletrec-clauses)
         (map2 (lambda (name-aloc info loa lambda-body)
                 (define fun-label (fresh-label))
                 (define this (fresh))
                 (define ys (info-ref info 'free))
                 (define let-closure-vars (for/list ([y ys]
                                                     [i (in-naturals)])
                                            `(,y (closure-ref ,this ,i))))

                 (values
                  `(,fun-label (lambda ,(cons this loa)
                                 (let ,let-closure-vars
                                   ,(convert-closures/value lambda-body))))
                  `(,name-aloc (make-closure ,fun-label ,(length loa) ,@ys))))
               fun-names
               infos
               alocs
               vs))

       `(letrec ,letrec-clauses
          (cletrec ,cletrec-clauses
                   ,(convert-closures/value value)))]
      [`(let ([,alocs ,vs] ...) ,body)
       `(let ,(for/list ([aloc alocs]
                         [v vs])
                `(,aloc ,(convert-closures/value v)))
          ,(convert-closures/value body))]
      [`(if ,operand ,conseq ,alt)
       `(if ,(convert-closures/value operand)
            ,(convert-closures/value conseq)
            ,(convert-closures/value alt))]
      [`(begin ,fx ... ,value)
       `(begin ,@(map convert-closures/effect fx)
               ,(convert-closures/value value))]
      [`(,primop ,args ...) #:when (unsafe-primop? primop) `(,primop ,@(map convert-closures/value args))]
      [triv triv]))

  ;; lam-free-lang-v9.effect -> closure-lang-v9.effect
  ;; converts closure in effect statements
  (define (convert-closures/effect effect)
    (match effect
      [`(,primop ,values ...) `(,primop ,@(map convert-closures/value values))]
      [`(begin ,fx ...) `(begin ,@(map convert-closures/effect fx))]))

  (match p
    [`(module ,value) `(module ,(convert-closures/value value))]))

