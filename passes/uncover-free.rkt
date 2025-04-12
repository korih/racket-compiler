#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9)

(provide uncover-free)

;; lam-opticon-lang-v9 -> lam-free-lang-v9
;; compiles p to Lam-free-lang v9 by explicitly annotate procedures with their
;; free variable sets
(define/contract (uncover-free p)
  (-> lam-opticon-lang-v9? lam-free-lang-v9?)

  ;; bound is (Set-of aloc)
  ;; interp. the set of variables that are currently bound and should not be
  ;; considered free

  ;; lam-opticon-lang-v9.value (Set-of aloc) -> lam-free-lang-v9.value (Set-of aloc)
  ;; interp. annotates a value expression with its free variables, given a set
  ;; of currently bound variables
  ;; INVARIANT: result contains all variables referenced in the value that are
  ;; not bound in the current scope
  (define (uncover-free-value v bound)
    (match v
      [`(begin ,effects ... ,value)
       (define-values (effects^ free1)
         (for/foldr ([acc '()] [free-vars-acc '()])
           ([e effects])
           (define-values (e^ f) (uncover-free-effect e bound))
           (values (cons e^ acc) (set-union f free-vars-acc))))
       (define-values (value^ free2) (uncover-free-value value bound))
       (values `(begin ,@effects^ ,value^) (set-union free1 free2))]
      [`(if ,v1 ,v2 ,v3)
       (define-values (v1^ f1) (uncover-free-value v1 bound))
       (define-values (v2^ f2) (uncover-free-value v2 bound))
       (define-values (v3^ f3) (uncover-free-value v3 bound))
       (values `(if ,v1^ ,v2^ ,v3^) (set-union f1 f2 f3))]
      [`(let ([,alocs ,vs] ...) ,body)
       (define-values (bindings f1 _)
         (for/foldr ([binding-acc '()] [free-vars-acc '()] [bound-vars bound])
           ([aloc alocs] [val vs])
           (define-values (val^ f) (uncover-free-value val bound-vars))
           (values (cons `(,aloc ,val^) binding-acc)
                   (set-union f free-vars-acc)
                   (cons aloc bound-vars))))
       (define-values (body^ f2) (uncover-free-value body (append alocs bound)))
       (values `(let ,bindings ,body^) (set-union f1 f2))]
      [`(letrec ([,alocs ,vs] ...) ,body)
       (define-values (bindings f1)
         (for/foldr ([binding-acc '()] [free-vars-acc '()])
           ([aloc alocs] [val vs])
           (match val
             [`(lambda (,args ...) ,body)
              (define-values (body^ f) (uncover-free-value body args))
              (define info (info-set '() 'free f))
              (values (cons `(,aloc (lambda ,info ,args ,body^)) binding-acc)
                      (set-subtract (set-union f free-vars-acc) bound))])))
       (define-values (body^ f2) (uncover-free-value body bound))
       (values `(letrec ,bindings ,body^) (remove* alocs (set-union f1 f2)))]
      [`(unsafe-procedure-call ,vs ...)
       (define-values (vs^ free)
         (for/foldr ([acc '()] [free-vars-acc '()])
           ([v vs])
           (define-values (v^ f) (uncover-free-value v bound))
           (values (cons v^ acc) (set-union f free-vars-acc))))
       (values `(unsafe-procedure-call ,@vs^) free)]
      [`(,primops ,vs ...)
       (define-values (vs^ free)
         (for/foldr ([acc '()] [free-vars-acc '()])
           ([v vs])
           (define-values (v^ f) (uncover-free-value v bound))
           (values (cons v^ acc) (set-union f free-vars-acc))))
       (values `(,primops ,@vs^) free)]
      [triv
       (define free-vars
         (if (and (aloc? triv) (not (member triv bound)))
             (list triv)
             '()))
       (values triv free-vars)]))

  ;; lam-opticon-lang-v9.value (Set-of aloc) -> lam-free-lang-v9.value (Set-of aloc)
  ;; interp. annotates an effect expression with its free variables, given a set
  ;; of currently bound variables
  ;; INVARIANT: result contains all variables referenced in the effect that are
  ;; not bound in the current scope
  (define (uncover-free-effect e bound)
    (match e
      [`(,primops ,vs ...)
       (define-values (vs^ free)
         (for/foldr ([acc '()] [free-vars-acc '()])
           ([v vs])
           (define-values (v^ f) (uncover-free-value v bound))
           (values (cons v^ acc) (set-union f free-vars-acc))))
       (values `(,primops ,@vs^) free)]

      [`(begin ,effects ...)
       (define-values (effects^ free)
         (for/foldr ([acc '()] [free-vars-acc '()])
           ([e effects])
           (define-values (e^ f) (uncover-free-effect e bound))
           (values (cons e^ acc) (set-union f free-vars-acc))))
       (values `(begin ,@effects^) free)]))

  (match p
    [`(module ,value)
     (define-values (value^ _) (uncover-free-value value '()))
     `(module ,value^)]))
