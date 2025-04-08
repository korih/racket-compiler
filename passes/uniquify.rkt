#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9)

(provide uniquify)

;; exprs-lang-v9 -> exprs-unique-lang-v9
;; compiles p to Exprs-unique-lang v9 by resolving top-level lexical
;; identifiers into unique labels, and all other lexical identifiers into
;; unique abstract locations
(define/contract (uniquify p)
  (-> exprs-lang-v9? exprs-unique-lang-v9?)

  ;; func is `(define ,label (lambda (,alocs ...) ,value))
  ;; interp. a function definition

  ;; (List-of func) -> (Env-of exprs-unique-lang-v9.triv)
  ;; interp. creates an environment with all the unique function labels
  (define (initialize-env funcs)
    (for/fold ([env empty-env])
              ([fun funcs])
      (match fun
        [`(define ,funcName (lambda (,args ...) ,tail))
         (define unique-label (fresh funcName))
         (define env^ (extend-env env funcName unique-label))
         env^])))

  ;; (List-of func) (Env-of exprs-unique-lang-v9.triv) -> (values (List-of func) (Env-of exprs-unique-lang-v9.triv))
  ;; interp. processes each function definition by assigning lexical identifiers with unique labels and abstract locations
  (define (identify-function-labels funcs env)
    (for/fold ([updated-funcs '()]
               [updated-env env])
              ([func funcs])
      (define-values (updated-func new-env) (uniquify-func func updated-env))
      (values (cons updated-func updated-funcs) new-env)))

  ;; func (Env-of exprs-unique-lang-v9.triv) -> func (Env-of exprs-unique-lang-v9.triv)
  ;; interp. for a given function definition, go through its args and body and produce uniquified version
  (define (uniquify-func func env)
    (match func
      [`(define ,funcName (lambda (,args ...) ,value))
       (define unique-label (lookup-env env funcName))
       (define unique-args (map fresh args))
       (define new-env (extend-env* env args unique-args))
       (values `(define ,unique-label (lambda (,@unique-args) ,(uniquify-value value new-env)))
               env)]))

  ;; exprs-lang-v9.value (Env-of exprs-unique-lang-v9.triv) -> exprs-unique-lang-v9.value
  ;; interp. rom a given value and environment, produce the uniquified version of it
  (define (uniquify-value value env)
    (match value
      [`(let ([,xs ,vs] ...) ,v)
       (define unique-names (map fresh xs))
       (define new-env (extend-env* env xs unique-names))
       (define unique-binds
         (map (lambda (uname value)
                (list uname (uniquify-value value env)))
              unique-names vs))
       `(let (,@unique-binds) ,(uniquify-value v new-env))]
      [`(if ,p-value ,t-value ,f-value)
       `(if ,(uniquify-value p-value env)
            ,(uniquify-value t-value env)
            ,(uniquify-value f-value env))]
      [`(call ,vs ...)
       `(call ,@(map (lambda (v) (uniquify-value v env)) vs))]
      [triv (uniquify-triv triv env)]))

  ;; exprs-lang-v9.triv (Env-of exprs-unique-lang-v9.triv) -> exprs-unique-lang-v9.triv
  ;; interp. resolves triv as terminal case, or x
  (define (uniquify-triv triv env)
    (match triv
      [#t #t]
      [#f #f]
      ['empty 'empty]
      ['(void) '(void)]
      [`(error ,n) `(error ,n)]
      [asci #:when (ascii-char-literal? asci) asci]
      [fixnum #:when (fixnum? fixnum) fixnum]
      [`(lambda (,xs ...) ,value)
       (define unique-names (map fresh xs))
       (define new-env (extend-env* env xs unique-names))
       (define value^ (uniquify-value value new-env))
       `(lambda ,unique-names ,value^)]
      [x (uniquify-x x env)]))

  ;; exprs-unique-lang-v9.x (Env-of exprs-unique-lang-v9.triv) -> exprs-unique-lang-v9.triv
  ;; interp. resolves x primitive function or a unique variable definition
  (define (uniquify-x x env)
    (match x
      [prim-f
       #:when (prim-f? prim-f)
       (if (assoc prim-f env)
           (lookup-env env prim-f)
           prim-f)]
      [name #:when (name? name) (lookup-env env name)]))

  (match p
    [`(module ,funcs ... ,value)
     (define defined-funs (initialize-env funcs))
     (define-values (updated-funcs updated-env) (identify-function-labels funcs defined-funs))
     `(module ,@(reverse updated-funcs) ,(uniquify-value value updated-env))]))

