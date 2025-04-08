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
  (define (sequentialize-let-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,tail))
       `(define ,label (lambda (,@alocs) ,(sequentialize-let-tail tail)))]))

  ;; values-bits-lang-v8.tail -> imp-mf-lang-v8.tail
  (define (sequentialize-let-tail tail)
    (match tail
      [`(let ([,xs ,vs] ...) ,tail)
       (define sequentialize-let-values (for/list ([x xs] [v vs])
                                          `(set! ,x ,(sequentialize-let-value v))))
       `(begin ,@sequentialize-let-values ,(sequentialize-let-tail tail))]
      [`(if ,p ,t1 ,t2)
       (define p^ (sequentialize-let-pred p))
       (define t1^ (sequentialize-let-tail t1))
       (define t2^ (sequentialize-let-tail t2))
       `(if ,p^ ,t1^ ,t2^)]
      [`(call ,triv ,opand ...) tail]
      [`(begin ,es ... ,t)
       `(begin ,@(map sequentialize-let-effect es) ,(sequentialize-let-tail t))]
      [value (sequentialize-let-value value)]))

  ;; values-bits-lang-v8.value -> imp-mf-lang-v8.value
  (define (sequentialize-let-value v)
    (match v
      [`(let ([,xs ,vs] ...) ,v)
       (define sequentialize-let-values (for/list ([x xs] [v vs])
                                          `(set! ,x ,(sequentialize-let-value v))))
       `(begin ,@sequentialize-let-values ,(sequentialize-let-value v))]
      [`(if ,p ,v1 ,v2)
       `(if ,(sequentialize-let-pred p)
            ,(sequentialize-let-value v1)
            ,(sequentialize-let-value v2))]
      [`(begin ,es ... ,value)
       `(begin ,@(map sequentialize-let-effect es) ,(sequentialize-let-value value))]
      ;; Using wildcard collapse case because in all other cases, the
      ;; expression is already in imp-mf-lang-v8.value form
      [_ v]))

  ;; values-bits-lang-v8.pred -> imp-mf-lang-v8.pred
  (define (sequentialize-let-pred p)
    (match p
      [`(let ([,xs ,vs] ...) ,pred)
       (define sequentialize-let-values (for/list ([x xs] [v vs])
                                          `(set! ,x ,(sequentialize-let-value v))))
       `(begin ,@sequentialize-let-values ,(sequentialize-let-pred pred))]
      [`(,relop ,t1 ,t2) `(,relop ,t1 ,t2)]
      [`(true) '(true)]
      [`(false) '(false)]
      [`(not ,pred)
       (define pred^ (sequentialize-let-pred pred))
       `(not ,pred^)]
      [`(if ,p1 ,p2 ,p3)
       (define p1^ (sequentialize-let-pred p1))
       (define p2^ (sequentialize-let-pred p2))
       (define p3^ (sequentialize-let-pred p3))
       `(if ,p1^ ,p2^ ,p3^)]
      [`(begin ,es ... ,pred)
       `(begin ,@(map sequentialize-let-effect es) ,(sequentialize-let-pred pred))]))

  ;; values-bits-lang-v8.effect -> imp-mf-lang-v8.effect
  (define (sequentialize-let-effect e)
    (match e
      [`(let ([,xs ,vs] ...) ,eff)
       (define sequentialize-let-values (for/list ([x xs] [v vs])
                                          `(set! ,x ,(sequentialize-let-value v))))
       `(begin ,@sequentialize-let-values ,(sequentialize-let-effect eff))]
      [`(begin ,es ...)
       `(begin ,@(map sequentialize-let-effect es))]
      [`(mset! ,aloc ,opand ,value)
       `(mset! ,aloc ,opand ,(sequentialize-let-value value))]))

  (match p
    [`(module ,funcs ... ,tail)
     `(module ,@(map sequentialize-let-func funcs) ,(sequentialize-let-tail tail))]))

