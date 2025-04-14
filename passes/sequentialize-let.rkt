#lang racket

(require
  cpsc411/langs/v8)

(provide sequentialize-let)

;; values-bits-lang-v8 -> imp-mf-lang-v8
;; compiles p to Imp-mf-lang v8 by picking a particular order to implement
;; let expressions using set!
(define/contract (sequentialize-let p)
  (-> values-bits-lang-v8? imp-mf-lang-v8?)

  ;; func is `(define ,label (lambda (,alocs ...) ,tail))
  ;; interp. a function definition

  ;; func -> func
  ;; interp. sequentializes all let bindings in the function’s tail expression
  (define (sequentialize-let-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,tail))
       `(define ,label (lambda (,@alocs) ,(sequentialize-let-tail tail)))]))

  ;; values-bits-lang-v8.tail -> imp-mf-lang-v8.tail
  ;; interp. sequentializes all let expressions for a given tail
  (define (sequentialize-let-tail tail)
    (match tail
      [`(let ([,xs ,vs] ...) ,tail)
       (define sequentialize-bindings
         (map (λ (x v) `(set! ,x ,(sequentialize-let-value v)))
              xs vs))
       `(begin ,@sequentialize-bindings ,(sequentialize-let-tail tail))]
      [`(if ,p ,t1 ,t2)
       `(if ,(sequentialize-let-pred p)
            ,(sequentialize-let-tail t1)
            ,(sequentialize-let-tail t2))]
      [`(call ,triv ,opand ...) tail]
      [`(begin ,es ... ,t)
       `(begin ,@(map sequentialize-let-effect es) ,(sequentialize-let-tail t))]
      [value (sequentialize-let-value value)]))

  ;; values-bits-lang-v8.value -> imp-mf-lang-v8.value
   ;; interp. sequentializes all let expressions for a given value 
  (define (sequentialize-let-value value)
    (match value
      [`(let ([,xs ,vs] ...) ,v)
       (define sequentialize-bindings
         (map (λ (x v) `(set! ,x ,(sequentialize-let-value v)))
              xs vs))
       `(begin ,@sequentialize-bindings ,(sequentialize-let-value v))]
      [`(if ,p ,v1 ,v2)
       `(if ,(sequentialize-let-pred p)
            ,(sequentialize-let-value v1)
            ,(sequentialize-let-value v2))]
      [`(begin ,es ... ,value)
       `(begin ,@(map sequentialize-let-effect es) ,(sequentialize-let-value value))]
      ;; Wildcard collapse case used because in all other cases, the
      ;; expression is already in imp-mf-lang-v8.value form
      [_ value]))

  ;; values-bits-lang-v8.pred -> imp-mf-lang-v8.pred
   ;; interp. sequentializes all let expressions for a given pred
  (define (sequentialize-let-pred pred)
    (match pred
      [`(let ([,xs ,vs] ...) ,pred)
       (define sequentialize-bindings
         (map (λ (x v) `(set! ,x ,(sequentialize-let-value v)))
              xs vs))
       `(begin ,@sequentialize-bindings ,(sequentialize-let-pred pred))]
      [`(,relop ,t1 ,t2) `(,relop ,t1 ,t2)]
      [`(true) '(true)]
      [`(false) '(false)]
      [`(not ,pred)
       `(not ,(sequentialize-let-pred pred))]
      [`(if ,p1 ,p2 ,p3)
       `(if ,(sequentialize-let-pred p1)
            ,(sequentialize-let-pred p2)
            ,(sequentialize-let-pred p3))]
      [`(begin ,es ... ,pred)
       `(begin ,@(map sequentialize-let-effect es) ,(sequentialize-let-pred pred))]))

  ;; values-bits-lang-v8.effect -> imp-mf-lang-v8.effect
   ;; interp. sequentializes all let expressions for a given effect
  (define (sequentialize-let-effect effect)
    (match effect
      [`(let ([,xs ,vs] ...) ,eff)
       (define sequentialize-bindings
         (map (λ (x v) `(set! ,x ,(sequentialize-let-value v)))
              xs vs))
       `(begin ,@sequentialize-bindings ,(sequentialize-let-effect eff))]
      [`(begin ,es ...)
       `(begin ,@(map sequentialize-let-effect es))]
      [`(mset! ,aloc ,opand ,value)
       `(mset! ,aloc ,opand ,(sequentialize-let-value value))]))

  (match p
    [`(module ,funcs ... ,tail)
     `(module ,@(map sequentialize-let-func funcs) ,(sequentialize-let-tail tail))]))

