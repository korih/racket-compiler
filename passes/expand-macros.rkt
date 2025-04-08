#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9
  cpsc411/langs/v11
  rackunit)

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
      [`(,f ,args ...)
       `(call ,(expand-macros-value f) ,@(map expand-macros-value args))]
      [_ value]))

  (match p
    [`(module ,funcs ... ,value)
     `(module ,@(map expand-macros-func funcs) ,(expand-macros-value value))]))

(module+ test
  (check-equal? (expand-macros '(module (#t #f)))
                '(module (call #t #f)))
  (check-equal? (expand-macros '(module '(#t #f)))
                '(module (call cons #t (call cons #f empty))))
  (check-equal? (expand-macros '(module '(1 2 #\a #\b (1 2))))
                '(module
                     (call
                      cons
                      1
                      (call
                       cons
                       2
                       (call
                        cons
                        #\a
                        (call cons #\b (call cons (call cons 1 (call cons 2 empty)) empty)))))))
  (check-equal? (expand-macros '(module (quote (#t #f))))
                '(module (call cons #t (call cons #f empty))))
  (check-equal? (expand-macros '(module (quote 1)))
                '(module 1))
  (check-equal? (expand-macros '(module (and 1)))
                '(module 1))
  (check-equal? (expand-macros '(module (and 1 2 3)))
                '(module (if 1 (if 2 3 #f) #f)))
  (check-equal? (expand-macros '(module (and 1 2 #f 3)))
                '(module (if 1 (if 2 (if #f 3 #f) #f) #f)))
  (check-equal? (expand-macros '(module (and)))
                '(module #t))
  (check-equal? (expand-macros '(module (or)))
                '(module #f))
  (check-equal? (expand-macros '(module (or 1)))
                '(module 1))
  (check-equal? (expand-macros '(module (or 1 2 #f #t)))
                '(module
                     (let ((tmp.1 1))
                       (if tmp.1
                           tmp.1
                           (let ((tmp.2 2))
                             (if tmp.2 tmp.2 (let ((tmp.3 #f)) (if tmp.3 tmp.3 #t))))))))
  (check-equal? (expand-macros '(module (vector 1 2 #t #f #\a)))
                '(module
                     (let ((tmp.4 (call make-vector 5)))
                       (let ((tmp.5 (call vector-set! tmp.4 0 1)))
                         (if (call error? tmp.5)
                             tmp.5
                             (let ((tmp.6 (call vector-set! tmp.4 1 2)))
                               (if (call error? tmp.6)
                                   tmp.6
                                   (let ((tmp.7 (call vector-set! tmp.4 2 #t)))
                                     (if (call error? tmp.7)
                                         tmp.7
                                         (let ((tmp.8 (call vector-set! tmp.4 3 #f)))
                                           (if (call error? tmp.8)
                                               tmp.8
                                               (let ((tmp.9 (call vector-set! tmp.4 4 #\a)))
                                                 (if (call error? tmp.9) tmp.9 tmp.4)))))))))))))
  (check-equal? (expand-macros '(module #(1 2 #t #f #\a)))
                '(module
                     (let ((tmp.10 (call make-vector 5)))
                       (let ((tmp.11 (call vector-set! tmp.10 0 1)))
                         (if (call error? tmp.11)
                             tmp.11
                             (let ((tmp.12 (call vector-set! tmp.10 1 2)))
                               (if (call error? tmp.12)
                                   tmp.12
                                   (let ((tmp.13 (call vector-set! tmp.10 2 #t)))
                                     (if (call error? tmp.13)
                                         tmp.13
                                         (let ((tmp.14 (call vector-set! tmp.10 3 #f)))
                                           (if (call error? tmp.14)
                                               tmp.14
                                               (let ((tmp.15 (call vector-set! tmp.10 4 #\a)))
                                                 (if (call error? tmp.15) tmp.15 tmp.10)))))))))))))
  (check-equal? (expand-macros '(module (begin)))
                '(module (void)))
  (check-equal? (expand-macros '(module (begin (eq? 1 2))))
                '(module (call eq? 1 2)))
  (check-equal? (expand-macros '(module (begin (eq? 1 2)
                                               (void? 1))))
                '(module
                     (let ((tmp.16 (call eq? 1 2)))
                       (if (call error? tmp.16) tmp.16 (call void? 1)))))
  (check-equal? (expand-macros '(module
                                    (begin
                                      (define x (lambda (y) (and y (or #f (begin (vector 1 2 #t #\a) #t)))))
                                      (define z (lambda (n m) (vector (and (fixnum? n) (>= n 0)) (or (boolean? m) #f))))
                                      (let ((a (call x #t))
                                            (b (call z 42 #f)))
                                        (begin
                                          (and (or (vector? a) (begin (void) #f)) (not (call error? b)))
                                          (call vector-ref b 0))))))
                '(module
                     (let ((tmp.17
                            (call
                             define
                             x
                             (lambda (y)
                               (if y
                                   (let ((tmp.18 #f))
                                     (if tmp.18
                                         tmp.18
                                         (let ((tmp.19
                                                (let ((tmp.20 (call make-vector 4)))
                                                  (let ((tmp.21 (call vector-set! tmp.20 0 1)))
                                                    (if (call error? tmp.21)
                                                        tmp.21
                                                        (let ((tmp.22 (call vector-set! tmp.20 1 2)))
                                                          (if (call error? tmp.22)
                                                              tmp.22
                                                              (let ((tmp.23
                                                                     (call vector-set! tmp.20 2 #t)))
                                                                (if (call error? tmp.23)
                                                                    tmp.23
                                                                    (let ((tmp.24
                                                                           (call
                                                                            vector-set!
                                                                            tmp.20
                                                                            3
                                                                            #\a)))
                                                                      (if (call error? tmp.24)
                                                                          tmp.24
                                                                          tmp.20)))))))))))
                                           (if (call error? tmp.19) tmp.19 #t))))
                                   #f)))))
                       (if (call error? tmp.17)
                           tmp.17
                           (let ((tmp.25
                                  (call
                                   define
                                   z
                                   (lambda (n m)
                                     (let ((tmp.26 (call make-vector 2)))
                                       (let ((tmp.28
                                              (call
                                               vector-set!
                                               tmp.26
                                               0
                                               (if (call fixnum? n) (call >= n 0) #f))))
                                         (if (call error? tmp.28)
                                             tmp.28
                                             (let ((tmp.29
                                                    (call
                                                     vector-set!
                                                     tmp.26
                                                     1
                                                     (let ((tmp.27 (call boolean? m)))
                                                       (if tmp.27 tmp.27 #f)))))
                                               (if (call error? tmp.29) tmp.29 tmp.26)))))))))
                             (if (call error? tmp.25)
                                 tmp.25
                                 (let ((a (call x #t)) (b (call z 42 #f)))
                                   (let ((tmp.30
                                          (if (let ((tmp.31 (call vector? a)))
                                                (if tmp.31
                                                    tmp.31
                                                    (let ((tmp.32 (call void)))
                                                      (if (call error? tmp.32) tmp.32 #f))))
                                              (call not (call error? b))
                                              #f)))
                                     (if (call error? tmp.30) tmp.30 (call vector-ref b 0)))))))))))