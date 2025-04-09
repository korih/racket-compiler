#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9)

(provide uncover-free)

;; lam-opticon-lang-v9 -> lam-free-lang-v9
;; compiles p to Lam-free-lang v9 by explicitly annotate procedures with their
;; free variable sets.
(define/contract (uncover-free p)
  (-> lam-opticon-lang-v9? lam-free-lang-v9?)


  ;; (Listof lam-opticon-lang-v9.value) -> (Listof lam-free-lang-v9.value) (Listof alocs)
  ;; helper compiles the list of values in lam-opticon-lang-v9 to lam-free-lang-v9
  (define (traverse-values-list vs env)
    (define-values (reversed-vs^ all-free-vars)
      (for/foldr ([acc '()]
                  [free-vars-acc '()])
        ([v vs])
        (define-values (v^ val-free-vars) (uncover-free-value v env))
        (values (cons v^ acc)
                (set-union val-free-vars free-vars-acc))))
    (values reversed-vs^ all-free-vars))

  ;; (Listof lam-opticon-lang-v9.effect) -> (Listof lam-free-lang-v9.effect)
  ;; helper compiles the list of values in lam-opticon-lang-v9 to lam-free-lang-v9
  (define (traverse-effects-list effects env)
    (define-values (reversed-es^ all-free-vars)
      (for/foldr ([acc '()]
                  [free-vars-acc '()])
        ([e effects])
        (define-values (e^ effect-free-vars) (uncover-free-effect e env))
        (values (cons e^ acc)
                (set-union effect-free-vars free-vars-acc))))
    (values reversed-es^ all-free-vars))


  ;; (Listof alocs) (Listof lam-opticon-lang-v9.value) -> (Listof aloc lam-free-lang-v9.value)
  ;; helper compiles the letrec in lam-opticon-lang-v9 to lam-free-lang-v9
  ;; TODO: unbound var inside of value in aloc -> vs matching
  (define (traverse-letrec-bindings alocs vs env)
    (define-values (bindings free-variables)
      (for/foldr ([binding-acc '()]
                  [free-vars-acc '()])
        ([aloc alocs]
         [val vs])
        (match val
          [`(lambda (,args ...) ,body)
           (define env^ (extend-env* env args args))
           (define-values (body^ free-vars) (uncover-free-value body env^))
           (define info (info-set '() 'free free-vars))
           (values (cons `(,aloc (lambda ,info ,args ,body^)) binding-acc)
                   (set-union free-vars free-vars-acc))])))
    (values bindings free-variables))

  ;; (Listof alocs) (Listof lam-opticon-lang-v9.value) -> (Listof aloc lam-free-lang-v9.value)
  ;; helper compiles the list of values in lam-opticon-lang-v9 to lam-free-lang-v9
  ;; TODO: unbound var inside of value in aloc -> vs matching
  (define (traverse-bindings-list alocs vs env)
    (define-values (bindings free-variables)
      (for/foldr ([binding-acc '()]
                  [free-vars-acc '()])
        ([aloc alocs]
         [val vs])
        (define-values (val^ free-vars) (uncover-free-value val env))
        (values (cons `(,aloc ,val^) binding-acc)
                (set-union free-vars free-vars-acc))))
    (values bindings free-variables))

  ;; lam-opticon-lang-v9.value (Envof alocs)  -> lam-free-lang-v9.value (Listof alocs)
  ;; compiles the values in lam-opticon-lang-v9 to lam-free-lang-v9
  (define (uncover-free-value v env)
    (match v
      [`(begin ,effects ... ,value) (define-values (effects^ free-var-effects) (traverse-effects-list effects env))
                                    (define-values (value^ free-vars-values) (uncover-free-value value env))
                                    (values `(begin ,@effects^ ,value^) (set-union free-var-effects free-vars-values))]
      [`(if ,v1 ,v2 ,v3) (define-values (v1^ free-vars1) (uncover-free-value v1 env))
                         (define-values (v2^ free-vars2) (uncover-free-value v2 env))
                         (define-values (v3^ free-vars3) (uncover-free-value v3 env))
                         (values `(if ,v1^ ,v2^ ,v3^) (set-union free-vars1 free-vars2 free-vars3))]
      [`(let ([,alocs ,vs] ...) ,body) (define-values (bindings free-var-binding) (traverse-bindings-list alocs vs env))
                                       (define env^ (extend-env* env alocs vs))
                                       (define-values (body^ free-vars) (uncover-free-value body env^))
                                       (values `(let ,bindings ,body^) (set-union free-vars free-var-binding))]
      [`(letrec ([,alocs ,vs] ...) ,body) (define-values (bindings free-var-binding) (traverse-letrec-bindings alocs vs env))
                                          (define-values (body^ free-vars) (uncover-free-value body env))
                                          (values `(letrec ,bindings  ,body^)
                                                  (remove* alocs (set-union free-var-binding free-vars) ))]
      [`(unsafe-procedure-call ,vs ...) (define-values (vs^ free-vars) (traverse-values-list vs env))
                                        (values `(unsafe-procedure-call ,@vs^) free-vars)]
      [`(,primops ,vs ...) (define-values (vs^ free-vars) (traverse-values-list vs env))
                           (values `(,primops ,@vs^) free-vars)]
      [triv (define free-vars (cond
                                [(assoc v env) '()]
                                [(aloc? v) `(,triv)]
                                [else `()]))
            (values triv free-vars)]))


  ;; lam-opticon-lang-v9.value -> lam-free-lang-v9.value
  ;; compiles the values in lam-opticon-lang-v9 to lam-free-lang-v9
  (define (uncover-free-effect e env)
    (match e
      [`(,primops ,vs ...) (define-values (vs^ free-vars) (traverse-values-list vs env))
                           (values `(,primops ,@vs^) free-vars)]
      [`(begin ,effects ...) (define-values (effects^ free-vars) (traverse-effects-list effects env))
                             (values `(begin ,@effects^) free-vars)]))

  (match p
    [`(module ,value)
     (define-values (value^ _) (uncover-free-value value empty-env))
     `(module ,value^)]))

