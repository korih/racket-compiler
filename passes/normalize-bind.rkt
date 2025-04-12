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
  (define (normalize-bind-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,tail))
       `(define ,label (lambda (,@alocs) ,(normalize-bind-tail tail)))]))

  ;; imp-mf-lang-v8.tail -> proc-imp-cmf-lang-v8.tail
  (define (normalize-bind-tail tail)
    (match tail
      [`(begin ,e ... ,t)
       `(begin ,@(map normalize-bind-effect e) ,(normalize-bind-tail t))]
      [`(if ,p ,t1 ,t2)
       `(if ,(normalize-bind-pred p) ,(normalize-bind-tail t1) ,(normalize-bind-tail t2))]
      [`(call ,triv ,opand ...) tail]
      [v (normalize-bind-value v (lambda (v) v))]))

  ;; imp-mf-lang-v8.effect -> proc-imp-cmf-lang-v8.effect
  (define (normalize-bind-effect effect)
    (match effect
      [`(set! ,aloc ,v)
       (normalize-bind-value v (lambda (simple-v)
                                 `(set! ,aloc ,simple-v)))]
      [`(mset! ,aloc ,opand ,v)
       (normalize-bind-value v
                             (lambda (triv)
                               (if (or (symbol? triv) (number? triv))
                                   `(mset! ,aloc ,opand ,triv)
                                   (let ([tmp (fresh 'tmp)])
                                     `(begin
                                        (set! ,tmp ,triv)
                                        (mset! ,aloc ,opand ,tmp))))))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(normalize-bind-pred pred) ,(normalize-bind-effect e1) ,(normalize-bind-effect e2))]
      [`(begin ,e ...)
       `(begin ,@(map normalize-bind-effect e))]))

  ;; imp-mf-lang-v8.value (imp-mf-lang-v8.value -> proc-imp-cmf-lang-v8.effect) -> proc-imp-cmf-lang-v8.value
  (define (normalize-bind-value value cont)
    (match value
      [`(begin ,e ... ,v)
       `(begin ,@(map normalize-bind-effect e) ,(normalize-bind-value v cont))]
      [`(if ,p ,v1 ,v2)
       `(if ,(normalize-bind-pred p)
            ,(normalize-bind-value v1 cont)
            ,(normalize-bind-value v2 cont))]
      ;; Using wildcard collapse case because in all other cases, the
      ;; expression is already in proc-imp-cmf-lang-v8.value form
      [_ (cont value)]))

  ;; imp-mf-lang-v8.pred -> proc-imp-cmf-lang-v8.pred
  (define (normalize-bind-pred pred)
    (match pred
      ['(true) pred]
      ['(false) pred]
      [`(not ,p) `(not ,(normalize-bind-pred p))]
      [`(begin ,e ... ,p)
       `(begin ,@(map normalize-bind-effect e) ,(normalize-bind-pred p))]
      [`(if ,p1 ,p2 ,p3)
       `(if ,(normalize-bind-pred p1) ,(normalize-bind-pred p2) ,(normalize-bind-pred p3))]
      [`(,relop ,op1 ,op2) pred]))

  (match p
    [`(module ,funcs ... ,tail)
     `(module ,@(map normalize-bind-func funcs) ,(normalize-bind-tail tail))]))

