#lang racket

(require
  cpsc411/langs/v9
  "common.rkt")

(provide optimize-known-calls)

;; Env is mapping of an aloc to label
;; interp. Holds the mapping from an aloc to label so that
;; when turning indirect calls to direct ones we can find the
;; label associated with the indirect call

;; closure-lang-v9 -> closure-lang-v9
;; optimizes p by optimizing calls to known closures
(define/contract (optimize-known-calls p)
  (-> closure-lang-v9? closure-lang-v9?)

  ;; (Listof closure-lang-v9.value) (Envof aloc to label) -> (Listof closure-lang-v9.value)
  ;; helper for traversing through lists of values and compiling them
  (define (traverse-values vs env)
    (reverse (for/fold ([vs^ '()])
                       ([v vs])
               (define-values (v^ _) (optimize-known-calls-value v env))
               (cons v^ vs^))))

  ;; (Listof closure-lang-v9.effects) (Envof aloc to label) -> (Listof closure-lang-v9.effects)
  ;; helper for traversing through lists of values and compiling them
  (define (traverse-effects vs env)
    (reverse (for/fold ([vs^ '()])
                       ([v vs])
               (define-values (v^ _) (optimize-known-calls-effect v env))
               (cons v^ vs^))))

  ;; (Listof aloc) (Listof closure-lang-v9.value) (Envof aloc to label)-> (Listof closure-lang-v9.binding)
  ;; helper for evaluating the values in binding position
  (define (traverse-bindings alocs vs env)
    (for/foldr ([binding '()])
      ([aloc alocs]
       [v vs])
      (define-values (v^ _) (optimize-known-calls-value v env))
      (cons `(,aloc ,v^) binding)))

  ;; (Listof closure-lang-v9.value) -> (Listof closure-lang-v9.lambda)
  ;; helper for evaluating the values in lambda body position
  (define (optimize-lambdas l env)
    (match l
      [`(lambda (,alocs ...) ,body)
       (define-values (body^ _) (optimize-known-calls-value body env))
       `(lambda ,alocs ,body^)]))

  ;; (Listof closure-lang-v9.value) (Envof aloc to label) -> (Listof closure-lang-v9.value) label
  ;; helper for evaluating the values in (make-closure ,label ,values ...)
  (define (optimize-make-closure mcs env)
    (match mcs
      [`(make-closure ,label ,vs ...)
       (define vs^ (for/foldr ([opt-vs '()])
                     ([v vs])
                     (define-values (v^ _) (optimize-known-calls-value v env))
                     (cons v^ opt-vs)))
       (values `(make-closure ,label ,@vs^) label)]))

  ;; closure-lang-v9.value (Envof aloc to label) -> closure-lang-v9.value (Envof aloc to label)
  ;; optimizes a value by optimizing calls to known closures
  (define (optimize-known-calls-value v env)
    (match v
      [`(letrec ([,labels ,lambdas] ...) ,body)
       (define-values (body^ env^) (optimize-known-calls-value body env))
       (define lambdas^ (for/foldr ([bindings '()])
                          ([label labels]
                           [lam lambdas])
                          (define lam^ (optimize-lambdas lam env^))
                          (cons `(,label ,lam^) bindings)))
       (values `(letrec ,lambdas^ ,body^) env)]
      [`(cletrec ([,alocs ,closures] ...) ,body)
       (define-values (bindings env^) (for/foldr ([acc '()]
                                                  [new-env env])
                                        ([aloc alocs]
                                         [closure closures])
                                        (define-values (closure^ label) (optimize-make-closure closure env))
                                        (values (cons `(,aloc ,closure^) acc)
                                                (extend-env new-env aloc label))))
       (define-values (body^ _) (optimize-known-calls-value body env^))
       (values `(cletrec ,bindings ,body^) env^)]
      [`(begin ,effects ... ,value) (define effects^ (traverse-effects effects env))
                                    (define-values (value^ _) (optimize-known-calls-value value env))
                                    (values `(begin ,effects^ ,value^) env)]
      [`(if ,v1 ,v2 ,v3) (define-values (v1^ _) (optimize-known-calls-value v1 env))
                         (define-values (v2^ __) (optimize-known-calls-value v2 env))
                         (define-values (v3^ ___) (optimize-known-calls-value v3 env))
                         (values `(if ,v1^ ,v2^ ,v3^) env)]
      [`(let ([,alocs ,vs] ...) ,body) (define bindings (traverse-bindings alocs vs env))
                                       (define-values (body^ _) (optimize-known-calls-value body env))
                                       (values `(let ,bindings ,body^) env)]
      [`(call ,vs ...) (define vs^ (traverse-values vs env))
                       (values `(call ,@vs^) env)]
      [`(closure-call ,id ,vs ...)
       (define vs^ (traverse-values vs env))
       (define new-call
         (cond
           [(assoc id env) `(call ,(lookup-env env id) ,@vs^)]
           [else `(closure-call ,id ,@vs^)]))
       (values new-call env)]
      [`(closure-ref ,v1 ,v2) (define-values (v1^ _) (optimize-known-calls-value v1 env))
                              (define-values (v2^ __) (optimize-known-calls-value v2 env))
                              (values `(closure-ref ,v1^ ,v2^) env)]
      [`(,primops ,vs ...) (define vs^ (traverse-values vs env))
                           (values `(,primops ,@vs^) env)]
      [triv (values triv env)]))

  ;; closure-lang-v9.effect (Envof aloc to label) -> closure-lang-v9.effect (Envof aloc to label)
  ;; compiles the effect in closure-lang-v9 to closure-lang-v9
  (define (optimize-known-calls-effect e env)
    (match e
      [`(,primops ,vs ...) (define vs^ (traverse-values vs env))
                           (values `(,primops ,@vs^) env)]
      [`(begin ,effects ...)  (define effects^ (traverse-effects effects env))
                              (values `(begin ,effects^) env)]))

  (match p
    [`(module ,value)
     (define-values (value^ _) (optimize-known-calls-value value empty-env))
     `(module ,value^)]))

