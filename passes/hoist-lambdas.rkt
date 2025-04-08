#lang racket

(require
  cpsc411/langs/v9)

(provide hoist-lambdas)

;; closure-lang-v9 -> hoisted-lang-v9
;; compiles p to Hoisted-lang v9 hoisting code to the top-level definitions
(define/contract (hoist-lambdas p)
  (-> closure-lang-v9? hoisted-lang-v9?)

  ;; func is (define label (lambda (alocs ...) value))
  ;; interp. This is a function definition

  ;; funcs is (Listof func)
  ;; interp. This is a list of function definitions that are being hosited up to the top
  (define funcs '())

  ;; (Listof closure-lang-v9.value) (Envof aloc to label) -> (Listof closure-lang-v9.value)
  ;; helper for traversing through lists of values and compiling them
  (define (traverse-values vs)
    (for/foldr ([vs^ '()])
      ([v vs])
      (define v^ (hoist-lambda-values v))
      (cons v^ vs^)))

  ;; (Listof closure-lang-v9.effects) (Envof aloc to label) -> (Listof closure-lang-v9.effects)
  ;; helper for traversing through lists of values and compiling them
  (define (traverse-effects vs)
    (for/foldr ([vs^ '()])
      ([v vs])
      (define v^ (hoist-lambdas-effect v))
      (cons v^ vs^)))

  ;; (Listof aloc) (Listof closure-lang-v9.value) (Envof aloc to label)-> (Listof closure-lang-v9.binding)
  ;; helper for evaluating the values in binding position
  (define (traverse-bindings alocs vs)
    (for/foldr ([binding '()])
      ([aloc alocs]
       [v vs])
      (define v^ (hoist-lambda-values v))
      (cons `(,aloc ,v^) binding)))

  ;; (Listof closure-lang-v9.value) -> (Listof closure-lang-v9.lambda)
  ;; helper for evaluating the values in lambda body position
  (define (optimize-lambdas l )
    (match l
      [`(lambda (,alocs ...) ,body)
       (define body^ (hoist-lambda-values body))
       `(lambda ,alocs ,body^)]))

  ;; (Listof closure-lang-v9.value) (Envof aloc to label) -> (Listof closure-lang-v9.value)
  ;; helper for evaluating the values in (make-closure ,label ,values ...)
  (define (optimize-make-closure mcs)
    (match mcs
      [`(make-closure ,label ,vs ...)
       (define vs^ (for/foldr ([opt-vs '()])
                     ([v vs])
                     (define v^ (hoist-lambda-values v))
                     (cons v^ opt-vs)))
       `(make-closure ,label ,@vs^)]))

  ;; closure-lang-v9.value -> hoisted-lang-v9.value
  ;; optimizes a value in closure-lang-v9 to hoisted-lang-v9
  (define (hoist-lambda-values v)
    (match v
      [`(letrec ([,labels ,lambdas] ...) ,body)
       (define body^ (hoist-lambda-values body))
       (define _ (for/fold ([bindings '()])
                           ([label labels]
                            [lam lambdas])
                   (define lam^ (optimize-lambdas lam))
                   (set! funcs (cons `(define ,label ,lam^) funcs))
                   (cons `(define ,label ,lam^) bindings)))
       body^]

      [`(cletrec ([,alocs ,closures] ...) ,body)
       (define bindings (for/foldr ([acc '()])
                          ([aloc alocs]
                           [closure closures])
                          (define closure^ (optimize-make-closure closure))
                          (cons `(,aloc ,closure^) acc)))
       (define body^ (hoist-lambda-values body))
       `(cletrec ,bindings ,body^)]
      [`(begin ,effects ... ,value) (define effects^ (traverse-effects effects))
                                    (define value^ (hoist-lambda-values value))
                                    `(begin ,effects^ ,value^)]
      [`(if ,v1 ,v2 ,v3) (define v1^ (hoist-lambda-values v1))
                         (define v2^ (hoist-lambda-values v2))
                         (define v3^ (hoist-lambda-values v3))
                         `(if ,v1^ ,v2^ ,v3^)]
      [`(let ([,alocs ,vs] ...) ,body) (define bindings (traverse-bindings alocs vs))
                                       (define body^ (hoist-lambda-values body))
                                       `(let ,bindings ,body^)]
      [`(call ,vs ...) (define vs^ (traverse-values vs))
                       `(call ,@vs^)]
      [`(closure-call ,vs ...)
       (define vs^ (traverse-values vs))
       `(closure-call ,@vs^)]
      [`(closure-ref ,v1 ,v2) (define v1^ (hoist-lambda-values v1))
                              (define v2^ (hoist-lambda-values v2))
                              `(closure-ref ,v1^ ,v2^)]
      [`(,primops ,vs ...) (define vs^ (traverse-values vs))
                           `(,primops ,@vs^)]
      [triv triv]))

  ;; closure-lang-v9.effect -> hoisted-lang-v9.effect
  ;; compiles the effect in closure-lang-v9 to hoisted-lang-v9
  (define (hoist-lambdas-effect e)
    (match e
      [`(,primops ,vs ...) (define vs^ (traverse-values vs))
                           `(,primops ,@vs^)]
      [`(begin ,effects ...)  (define effects^ (traverse-effects effects))
                              `(begin ,effects^)]))

  (match p
    [`(module ,value)
     (define value^ (hoist-lambda-values value))
     `(module ,@funcs ,value^)]))

