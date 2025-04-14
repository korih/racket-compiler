#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide normalize-bind)

;; imp-mf-lang-v8 -> proc-imp-cmf-lang-v8
;; compiles p to to to Proc-imp-cmf-lang v8 by pushing set! under begin so that
;; the right-hand-side of each set! is simple value-producing operation
(define/contract (normalize-bind p)
  (-> imp-mf-lang-v8? proc-imp-cmf-lang-v8?)

  ;; func is `(define ,label (lambda (,alocs ...) ,tail))
  ;; interp. a function definition

  ;; func -> func
  ;; interp. normalizes the tail of a function so all set! values are trivial
  (define (normalize-bind-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,tail))
       `(define ,label (lambda (,@alocs) ,(normalize-bind-tail tail)))]))

  ;; imp-mf-lang-v8.tail -> proc-imp-cmf-lang-v8.tail
  ;; interp. pushes non-trivial expressions out of tail position into let-bound
  ;; or set! form, ensuring all calls and set!s are over trivial operands
  (define (normalize-bind-tail tail)
    (match tail
      [`(begin ,e ... ,t)
       `(begin ,@(map normalize-bind-effect e) ,(normalize-bind-tail t))]
      [`(if ,p ,t1 ,t2)
       `(if ,(normalize-bind-pred p) ,(normalize-bind-tail t1) ,(normalize-bind-tail t2))]
      [`(call ,triv ,opand ...) tail]
      [value (normalize-bind-value value (lambda (v) v))]))

  ;; imp-mf-lang-v8.effect -> proc-imp-cmf-lang-v8.effect
  ;; interp. normalizes any set!/mset! values so they are trivial
  (define (normalize-bind-effect effect)
    (match effect
      [`(set! ,aloc ,value)
       (normalize-bind-value value (lambda (simple-v)
                                     `(set! ,aloc ,simple-v)))]
      [`(mset! ,aloc ,opand ,value)
       (normalize-bind-value value
                             (lambda (triv)
                               (if (or (symbol? triv) (number? triv))
                                   `(mset! ,aloc ,opand ,triv)
                                   (let ([tmp (fresh 'tmp)])
                                     `(begin
                                        (set! ,tmp ,triv)
                                        (mset! ,aloc ,opand ,tmp))))))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(normalize-bind-pred pred)
            ,(normalize-bind-effect e1)
            ,(normalize-bind-effect e2))]
      [`(begin ,effects ...)
       `(begin ,@(map normalize-bind-effect effects))]))

  ;; imp-mf-lang-v8.value (imp-mf-lang-v8.value -> proc-imp-cmf-lang-v8.effect) -> proc-imp-cmf-lang-v8.effect
  ;; interp. ensures the value is trivial, or lifts its computation into an effect
  (define (normalize-bind-value value cont)
    (match value
      [`(begin ,e ... ,v)
       `(begin ,@(map normalize-bind-effect e) ,(normalize-bind-value v cont))]
      [`(if ,p ,v1 ,v2)
       `(if ,(normalize-bind-pred p)
            ,(normalize-bind-value v1 cont)
            ,(normalize-bind-value v2 cont))]
      ;; Wildcard collapse case used because all remaining values are trivials
      ;; which do not require lifting, so we simply apply the continuation
      [_ (cont value)]))

  ;; imp-mf-lang-v8.pred -> proc-imp-cmf-lang-v8.pred
  ;; interp. normalizes predicate expressions by pushing effects out of conditions
  (define (normalize-bind-pred pred)
    (match pred
      ['(true) pred]
      ['(false) pred]
      [`(not ,p) `(not ,(normalize-bind-pred p))]
      [`(begin ,e ... ,p)
       `(begin ,@(map normalize-bind-effect e) ,(normalize-bind-pred p))]
      [`(if ,p1 ,p2 ,p3)
       `(if ,(normalize-bind-pred p1)
            ,(normalize-bind-pred p2)
            ,(normalize-bind-pred p3))]
      [`(,relop ,op1 ,op2) pred]))

  (match p
    [`(module ,funcs ... ,tail)
     `(module ,@(map normalize-bind-func funcs) ,(normalize-bind-tail tail))]))

