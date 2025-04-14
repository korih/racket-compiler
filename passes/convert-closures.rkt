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

  ;; lambda is `(lambda (,alocs ...) ,value)
  ;; interp. a procedure with parameters alocs and body value

  ;; closure is `(make-closure ,label ,values ...)
  ;; interp. creates a runtime closure

  ;; lam-free-lang-v9.value (closure-lang-v9.value -> closure-lang-v9.value) -> closure-lang-v9.value
  ;; interp. binds the closure to a label if needed and forms a closure-call
  (define (with-bound-fun value k)
    (if (aloc? value)
        (k value)
        (let ([l (fresh)])
          `(let ([,l ,(convert-closures/value value)])
             ,(k l)))))

  ;; aloc lam-free-lang-v9.info (List-of aloc) lam-free-lang-v9.value -> (list label lambda) (list aloc closure)
  ;; interp. given a function name, its info, its parameter list, and its body,
  ;; generates the closure-converted function definition and closure binding
  (define (convert-function-definition name-aloc info param-list body)
    (define fun-label (fresh-label))
    (define this (fresh))
    (define free-vars (info-ref info 'free))

    (define closure-extracts
      (for/list ([fv free-vars]
                 [i (in-naturals)])
        `(,fv (closure-ref ,this ,i))))

    (values
     `(,fun-label (lambda (,this ,@param-list)
                    (let ,closure-extracts
                      ,(convert-closures/value body))))
     `(,name-aloc (make-closure ,fun-label ,(length param-list) ,@free-vars))))

  ;; lam-free-lang-v9.value -> closure-lang-v9.value
  ;; interp. performs closure conversion on the given value expression
  (define (convert-closures/value value)
    (match value
      [`(unsafe-procedure-call ,fun ,args ...)
       (with-bound-fun fun
         (lambda (aloc)
           `(closure-call ,aloc ,aloc ,@(map convert-closures/value args))))]
      [`(letrec ([,fun-names (lambda ,infos (,alocs ...) ,vs)] ...) ,value)
       (define-values (letrec-clauses cletrec-clauses)
         (map2 convert-function-definition fun-names infos alocs vs))
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
      [`(,primop ,args ...)
       #:when (unsafe-primop? primop)
       `(,primop ,@(map convert-closures/value args))]
      [triv triv]))

  ;; lam-free-lang-v9.effect -> closure-lang-v9.effect
  ;; interp. performs closure conversion on the given effect expression
  (define (convert-closures/effect effect)
    (match effect
      [`(,primop ,values ...)
       `(,primop ,@(map convert-closures/value values))]
      [`(begin ,fx ...)
       `(begin ,@(map convert-closures/effect fx))]))

  (match p
    [`(module ,value)
     `(module ,(convert-closures/value value))]))
