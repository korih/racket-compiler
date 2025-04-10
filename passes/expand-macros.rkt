#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9
  cpsc411/langs/v11)

(provide expand-macros)

;; racketish-surface -> exprs-lang-v9
;; compiles p to Exprs-lang v9 by expanding all Racketish-Surface macros
(define/contract (expand-macros p)
  (-> racketish-surface? exprs-lang-v9?)

  ;; func is `(define ,x (lambda (,xs ...) ,value))
  ;; interp. a function definition

  ;; (List-of racketish-surface.value) -> exprs-lang-v9.value
  ;; interp. expands a list of expressions used in an and macro into nested
  ;; if expressions that short-circuit on false
  (define (expand-and vs)
    (match vs
      ['() #t]
      [(list e) (expand-macros-value e)]
      [(cons e es)
       `(if ,(expand-macros-value e) ,(expand-and es) #f)]))

  ;; (List-of racketish-surface.value) -> exprs-lang-v9.value
  ;; interp. expands a list of expressions used in an or macro into nested let
  ;; and if expressions that short-circuit on true
  (define (expand-or vs)
    (match vs
      ['() #f]
      [(list e) (expand-macros-value e)]
      [(cons e es)
       (define tmp (fresh 'tmp))
       `(let ([,tmp ,(expand-macros-value e)])
          (if ,tmp ,tmp ,(expand-or es)))]))

  ;; (List-of racketish-surface.value) -> exprs-lang-v9.value
  ;; interp. expands a list of expressions used in a vector macro into a
  ;; sequence of vector-set! calls followed by the resulting vector
  (define (expand-vector vs)
    (define len (length vs))
    (define tmp (fresh 'tmp))
    (define sets (for/list ([v vs]
                            [i (in-naturals)])
                   `(call vector-set! ,tmp ,i ,(expand-macros-value v))))
    `(let ([,tmp (call make-vector ,len)])
       ,(expand-begin (append sets (list tmp)))))

  ;; (List-of racketish-surface.value) -> exprs-lang-v9.value
  ;; interp. expands a begin sequence into nested let and if expressions that
  ;; propagate errors and return the final expression
  (define (expand-begin vs)
    (match vs
      ['() '(void)]
      [(list e) (expand-macros-value e)]
      [(cons e es)
       (define tmp (fresh 'tmp))
       `(let ([,tmp ,(expand-macros-value e)])
          (if (call error? ,tmp)
              ,tmp
              ,(expand-begin es)))]))

  ;; (List-of racketish-surface.value) -> exprs-lang-v9.value
  ;; interp. expands a quoted s-expression into a series of nested cons calls
  ;; ending in empty
  (define (expand-quote s-expr)
    (match s-expr
      [(? pair?)
       `(call cons ,(expand-quote (car s-expr)) ,(expand-quote (cdr s-expr)))]
      ['() 'empty]
      [_ s-expr]))

  ;; func -> func
  (define (expand-macros-func func)
    (match func
      [`(define ,x (lambda (,xs ...) ,value))
       `(define ,x (lambda (,@xs) ,(expand-macros-value value)))]))

  ;; racketish-surface.value -> exprs-lang-v9.value
  (define (expand-macros-value value)
    (match value
      [`(let ([,xs ,vs] ...) ,v)
       (define bindings
         (map (lambda (x^ v^)
                (list x^ (expand-macros-value v^)))
              xs vs))
       `(let (,@bindings) ,(expand-macros-value v))]
      [`(if ,v1 ,v2 ,v3)
       `(if ,(expand-macros-value v1)
            ,(expand-macros-value v2)
            ,(expand-macros-value v3))]
      [`(call ,vs ...)
       `(call ,@(map expand-macros-value vs))]
      [`(quote ,s-expr)
       (expand-quote s-expr)]
      [`(vector ,vs ...)
       (expand-vector vs)]
      [(vector vs ...)
       (expand-vector vs)]
      [`(and ,vs ...)
       (expand-and vs)]
      [`(or ,vs ...)
       (expand-or vs)]
      [`(begin ,vs ...)
       (expand-begin vs)]
      [`(lambda (,xs ...) ,v)
       `(lambda (,@xs) ,(expand-macros-value v))]
      [`(error ,uint8) `(error ,uint8)]
      [`(void) value]
      [`(,f ,args ...)
       `(call ,(expand-macros-value f) ,@(map expand-macros-value args))]
      [_ value]))

  (match p
    [`(module ,funcs ... ,value)
     `(module ,@(map expand-macros-func funcs) ,(expand-macros-value value))]))

(module+ test
  (require rackunit)
  (check-equal?
   (expand-macros '(module (error 5)))
   '(module (error 5))
   "Check if error works")
  (check-equal?
   (expand-macros '(module (call (void))))
   '(module (call (void)))
   "Check if error works")

  )
