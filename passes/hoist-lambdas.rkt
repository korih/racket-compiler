#lang racket

(require
  cpsc411/langs/v9)

(provide hoist-lambdas)

;; closure-lang-v9 -> hoisted-lang-v9
;; compiles p to Hoisted-lang v9 by hoisting code to the top-level definitions
(define/contract (hoist-lambdas p)
  (-> closure-lang-v9? hoisted-lang-v9?)

  ;; func is (define label (lambda (alocs ...) value))
  ;; interp. a function definition

  ;; funcs is (List-of func)
  ;; interp. stores function definitions hoisted from letrec expressions
  (define funcs '())

  ;; closure-lang-v9.value -> hoisted-lang-v9.value
  ;; interp. recursively hoists lambdas in values to top-level definitions
  ;; EFFECTS: add hoisted lambdas to funcs
  (define (hoist-lambdas-value value)
    (match value
      [`(letrec ([,labels (lambda (,params ...) ,bodies)] ...) ,body)
       (for-each
        (lambda (label params body)
          (define body^ (hoist-lambdas-value body))
          (define lam `(lambda ,params ,body^))
          (set! funcs (cons `(define ,label ,lam) funcs)))
        labels params bodies)
       (hoist-lambdas-value body)]
      [`(cletrec ([,alocs (make-closure ,labels ,vs ...)] ...) ,body)
       (define bindings
         (for/list ([a alocs] [l labels] [vss vs])
           (define vs^ (map hoist-lambdas-value vss))
           `(,a (make-closure ,l ,@vs^))))
       `(cletrec ,bindings ,(hoist-lambdas-value body))]
      [`(begin ,effects ... ,value)
       `(begin ,@(map hoist-lambdas-effect effects) ,(hoist-lambdas-value value))]
      [`(if ,v1 ,v2 ,v3)
       `(if ,(hoist-lambdas-value v1)
            ,(hoist-lambdas-value v2)
            ,(hoist-lambdas-value v3))]
      [`(let ([,alocs ,vs] ...) ,body)
       (define bindings
         (map (lambda (aloc v)
                (list aloc (hoist-lambdas-value v)))
              alocs vs))
       `(let (,@bindings) ,(hoist-lambdas-value body))]
      [`(call ,vs ...)
       `(call ,@(map hoist-lambdas-value vs))]
      [`(closure-call ,vs ...)
       `(closure-call ,@(map hoist-lambdas-value vs))]
      [`(closure-ref ,v1 ,v2)
       (define v1^ (hoist-lambdas-value v1))
       (define v2^ (hoist-lambdas-value v2))
       `(closure-ref ,v1^ ,v2^)]
      [`(,primops ,vs ...)
       `(,primops ,@(map hoist-lambdas-value vs))]
      [triv triv]))

  ;; closure-lang-v9.effect -> hoisted-lang-v9.effect
  ;; interp. recursively hoists lambdas in an effect
  (define (hoist-lambdas-effect e)
    (match e
      [`(,primops ,vs ...)
       `(,primops ,@(map hoist-lambdas-value vs))]
      [`(begin ,effects ...)
       `(begin ,@(map hoist-lambdas-effect effects))]))

  (match p
    [`(module ,value)
     (define value^ (hoist-lambdas-value value))
     `(module ,@funcs ,value^)]))