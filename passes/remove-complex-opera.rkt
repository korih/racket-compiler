#lang racket


(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide remove-complex-opera*)

;; exprs-bits-lang-v8 -> values-bits-lang-v8
;; compiles p to Values-bits-lang v8 by performing the monadic form
;; transformation, unnesting all non-trivial operators and operands to binops,
;; calls, and relops, making data flow explicit and simple to implement
;; imperatively
(define/contract (remove-complex-opera* p)
  (-> exprs-bits-lang-v8? values-bits-lang-v8?)

  ;; func-value is `(define ,label (lambda (,aloc ...) ,value))
  ;; interp. a function definition with the body being a value

  ;; func-tail is `(define ,label (lambda (,aloc ...) ,tail))
  ;; interp. a function definition with the body being a tail

  ;; func-value -> func-tail
  ;; interp. transforms the body of a function from value form to tail form by
  ;; applying remove-complex-opera* to unnest all compound expressions
  (define (remove-complex-opera*-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,tail))
       `(define ,label (lambda (,@alocs) ,(remove-complex-opera*-tail tail (λ (v) v))))]))

  ;; exprs-bits-lang-v8.value (values-bits-lang-v8.value -> values-bits-lang-v8.value) -> values-bits-lang-v8.tail
  ;; interp. transforms a tail position expression using a CPS-style 
  ;; continuation k to manage sequencing of subcomputations
  (define (remove-complex-opera*-tail tail k)
    (match tail
      [`(let ([,alocs ,vs] ...) ,t)
       (define bindings
         (map (lambda (aloc val)
                (list aloc (remove-complex-opera*-value val k)))
              alocs vs))
       `(let (,@bindings) ,(remove-complex-opera*-tail t k))]
      [`(if ,pred ,t1 ,t2)
       `(if ,(remove-complex-opera*-pred pred k)
            ,(remove-complex-opera*-tail t1 k)
            ,(remove-complex-opera*-tail t2 k))]
      [`(call ,triv ,ops ...)
       (trivialize-value triv
                         (lambda (triv^)
                           (let loop ([remaining-ops ops]
                                      [accum '()]
                                      [k k])
                             (if (empty? remaining-ops)
                                 (k `(call ,triv^ ,@(reverse accum)))
                                 (trivialize-value (car remaining-ops)
                                                   (lambda (op^)
                                                     (loop (cdr remaining-ops) (cons op^ accum) k)))))))]
      [`(begin ,es ... ,t)
       `(begin ,@(map (lambda (e) (remove-complex-opera*-effect e k)) es) ,(remove-complex-opera*-tail t k))]
      [value (remove-complex-opera*-value value k)]))

  ;; exprs-bits-lang-v8.value (values-bits-lang-v8.value -> values-bits-lang-v8.value) -> values-bits-lang-v8.value
  ;; interp. transforms a value by unnesting compound expressions using
  ;; CPS-style continuation k
  (define (remove-complex-opera*-value value k)
    (match value
      [`(let ([,alocs ,vs] ...) ,v)
       (define bindings
         (map (lambda (aloc val)
                (list aloc (remove-complex-opera*-value val k)))
              alocs vs))
       `(let (,@bindings) ,(remove-complex-opera*-value v k))]
      [`(if ,pred ,v1 ,v2)
       `(if ,(remove-complex-opera*-pred pred k)
            ,(remove-complex-opera*-value v1 k)
            ,(remove-complex-opera*-value v2 k))]
      [`(call ,triv ,ops ...)
       (trivialize-value triv
                         (lambda (triv^)
                           (let loop ([remaining-ops ops]
                                      [accum '()]
                                      [k k])
                             (if (empty? remaining-ops)
                                 (k `(call ,triv^ ,@(reverse accum)))
                                 (trivialize-value (car remaining-ops)
                                                   (lambda (op^)
                                                     (loop (cdr remaining-ops) (cons op^ accum) k)))))))]
      [`(mref ,aloc ,opand)
       (trivialize-value aloc
                         (lambda (aloc^)
                           (trivialize-value opand
                                             (lambda (opand^)
                                               (k `(mref ,aloc^ ,opand^))))))]
      [`(alloc ,opand)
       (trivialize-value opand
                         (lambda (opand^)
                           (k `(alloc ,opand^))))]
      [`(begin ,es ... ,v)
       `(begin ,@(map (lambda (e) (remove-complex-opera*-effect e k)) es) ,(remove-complex-opera*-value v k))]
      [`(,binop ,op1 ,op2)
       (trivialize-value op1
                         (lambda (op1^)
                           (trivialize-value op2
                                             (lambda (op2^)
                                               (k `(,binop ,op1^ ,op2^))))))]
      [triv (trivialize-value triv k)]))

  ;; exprs-bits-lang-v8.pred (values-bits-lang-v8.value -> values-bits-lang-v8.value) -> values-bits-lang-v8.pred
  ;; interp. transforms a predicate by unnesting relops and nested values using
  ;; continuation k
  (define (remove-complex-opera*-pred pred k)
    (match pred
      ['(true) pred]
      ['(false) pred]
      [`(not ,p) `(not ,(remove-complex-opera*-pred p k))]
      [`(let ([,alocs ,vs] ...) ,p)
       (define bindings
         (map (lambda (aloc val)
                (list aloc (remove-complex-opera*-value val k)))
              alocs vs))
       `(let (,@bindings) ,(remove-complex-opera*-pred p k))]
      [`(if ,p1 ,p2 ,p3)
       `(if ,(remove-complex-opera*-pred p1 k)
            ,(remove-complex-opera*-pred p2 k)
            ,(remove-complex-opera*-pred p3 k))]
      [`(begin ,es ... ,p)
       `(begin ,@(map (lambda (e) (remove-complex-opera*-effect e k)) es) ,(remove-complex-opera*-pred p k))]
      [`(,relop ,op1 ,op2)
       (trivialize-value op1
                         (lambda (op1^)
                           (trivialize-value op2
                                             (lambda (op2^)
                                               (k `(,relop ,op1^ ,op2^))))))]))

  ;; exprs-bits-lang-v8.effect (values-bits-lang-v8.value -> values-bits-lang-v8.value) -> values-bits-lang-v8.effect
  ;; interp. transforms an effect expression by unnesting expressions used as
  ;; operands using continuation k
  (define (remove-complex-opera*-effect effect k)
    (match effect
      [`(mset! ,aloc ,opand ,value)
       (trivialize-value aloc
                         (lambda (aloc^)
                           (trivialize-value opand
                                             (lambda (opand^)
                                               (k `(mset! ,aloc^ ,opand^ ,(remove-complex-opera*-value value k)))))))]
      [`(let ([,alocs ,vs] ...) ,eff)
       (define bindings
         (map (lambda (aloc val)
                (list aloc (remove-complex-opera*-value val k)))
              alocs vs))
       `(let (,@bindings) ,(remove-complex-opera*-effect eff k))]
      [`(begin ,es ...)
       `(begin ,@(map (lambda (e) (remove-complex-opera*-effect e k)) es))]))

  ;; exprs-bits-lang-v8.value (values-bits-lang-v8.value -> values-bits-lang-v8.value) -> values-bits-lang-v8.opand
  ;; interp. ensures the value is a trivial operand, otherwise binds it to a
  ;; temporary variable and passes the temporary to k
  (define (trivialize-value value k)
    (match value
      [label #:when (label? label) (k label)]
      [int64 #:when (int64? int64) (k int64)]
      [aloc #:when (aloc? aloc) (k aloc)]
      ;; Wildcard collapse case used because all other values are guaranteed to 
      ;; be non-trivial and must be bound to a temporary to ensure the operand
      ;; position only contains trivials
      [_
       (define tmp (fresh))
       `(let ([,tmp ,(remove-complex-opera*-value value (λ (v) v))])
          ,(k tmp))]))

  (match p
    [`(module ,funcs ... ,tail)
     `(module ,@(map remove-complex-opera*-func funcs) ,(remove-complex-opera*-tail tail (λ (v) v)))]))

