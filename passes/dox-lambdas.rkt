#lang racket

(require
  "common.rkt"
  cpsc411/compiler-lib
  cpsc411/langs/v9
  rackunit)

(provide dox-lambdas)

;; just-exprs-lang-v9 -> lam-opticon-lang-v9
;; compiles p to Lam-opticon-lang-v9 by explicitly binds all procedures to
;; abstract locations
(define/contract (dox-lambdas p)
  (-> just-exprs-lang-v9? lam-opticon-lang-v9?)

  ;; just-exprs-lang-v9.value -> lam-opticon-lang-v9.value
  ;; compiles the values expression, binding all procedures to abstract locations in value position
  (define (dox-lambdas/value v)
    (match v
      [`(unsafe-procedure-call ,procV ,argsV ...)
       `(unsafe-procedure-call ,(dox-lambdas/value procV) ,@(map dox-lambdas/value argsV))]
      [`(letrec ([,aloc1 (lambda (,lambdaArgs ...) ,lambdaVs)] ...) ,v)
       (define compiled-lambda-vs (map dox-lambdas/value lambdaVs))
       `(letrec ,(for/foldr ([acc '()])
                   ([compiled-lambda-body compiled-lambda-vs]
                    [args lambdaArgs]
                    [aloc aloc1])
                   (cons `(,aloc (lambda ,args ,compiled-lambda-body)) acc))
          ,(dox-lambdas/value v))]
      [`(let ([,aloc ,v] ...) ,bodyV)
       (define compiled-vs (map dox-lambdas/value v))
       `(let ,(for/foldr ([acc '()])
                ([compiled-v compiled-vs]
                 [a aloc])
                (cons `(,a ,compiled-v) acc))
          ,(dox-lambdas/value bodyV))]
      [`(if ,predV ,cV ,aV)
       `(if ,(dox-lambdas/value predV)
            ,(dox-lambdas/value cV)
            ,(dox-lambdas/value aV))]
      [`(begin ,e ... ,v)
       `(begin ,@(map dox-lambdas/effect e) ,(dox-lambdas/value v))]
      [`(,primop ,v ...) #:when (unsafe-primop? primop)
                         `(,primop ,@(map dox-lambdas/value v))]
      [triv (dox-lambdas/triv triv)]))

  ;; just-exprs-lang-v9.effect -> lam-opticon-lang-v9.effect
  ;; compiles the effect expression, binding all procedures to abstract locations in effect position
  (define (dox-lambdas/effect e)
    (match e
      [`(begin ,e ...) `(begin ,@(map dox-lambdas/effect e))]
      [`(,primop ,vs ...) `(,primop ,@(map dox-lambdas/value vs))]))

  ;; just-exprs-lang-v9.triv -> lam-opticon-lang-v9.value
  ;; compiles the triv expression, value expressions can replace triv expressions in src expression
  (define (dox-lambdas/triv t)
    (match t
      [#f #f]
      [#t #t]
      ['empty 'empty]
      ['(void) '(void)]
      [`(error ,i) `(error ,i)]
      [`(lambda (,alocs ...) ,v)
       (define fn (fresh 'lam))

       `(letrec ([,fn (lambda ,alocs ,(dox-lambdas/value v))])
          ,fn)]
      [i #:when (fixnum? i) i]
      [a #:when (aloc? a) a]
      [c c]))


  (match p
    [`(module ,value) `(module ,(dox-lambdas/value value))]))

(module+ test
  (check-equal? (dox-lambdas '(module 42)) '(module 42))
  (check-equal? (dox-lambdas '(module (unsafe-fx+ 3 4))) '(module (unsafe-fx+ 3 4)))
  (check-equal? (dox-lambdas '(module (if (unsafe-fx< 3 4) 1 0))) '(module (if (unsafe-fx< 3 4) 1 0)))
  (check-equal? (dox-lambdas '(module
                                  (let ([x.1 10]
                                        [y.2 20])
                                    (unsafe-fx+ x.1 y.2))))
                '(module
                     (let ([x.1 10]
                           [y.2 20])
                       (unsafe-fx+ x.1 y.2))))
  (check-equal? (dox-lambdas '(module
                                  (letrec ([fact.2 (lambda (n.1)
                                                     (if (unsafe-fx<= n.1 1)
                                                         1
                                                         (unsafe-fx* n.1
                                                                     (unsafe-procedure-call fact.2
                                                                                            (unsafe-fx- n.1 1)))))])
                                    (unsafe-procedure-call fact.2 5))))
                '(module
                     (letrec ([fact.2 (lambda (n.1)
                                        (if (unsafe-fx<= n.1 1)
                                            1
                                            (unsafe-fx* n.1
                                                        (unsafe-procedure-call fact.2
                                                                               (unsafe-fx- n.1 1)))))])
                       (unsafe-procedure-call fact.2 5))))
  (check-equal? (dox-lambdas '(module
                                  (begin
                                    (unsafe-vector-set! (unsafe-make-vector 1) 0 42)
                                    (unsafe-fx+ 1 2))))
                '(module
                     (begin
                       (unsafe-vector-set! (unsafe-make-vector 1) 0 42)
                       (unsafe-fx+ 1 2))))
  (check-equal? (dox-lambdas '(module
                                  (let ([f.3 (lambda (x.1 y.2) (unsafe-fx+ x.1 y.2))])
                                    (unsafe-procedure-call f.3 2 3))))
                '(module
                     (let ((f.3 (letrec ((lam.1 (lambda (x.1 y.2) (unsafe-fx+ x.1 y.2)))) lam.1)))
                       (unsafe-procedure-call f.3 2 3))))
  (check-equal? (dox-lambdas '(module
                                  (let ([x.1 #t]
                                        [y.2 #\a]
                                        [z.3 empty])
                                    (if (boolean? x.1)
                                        (if (ascii-char? y.2)
                                            1
                                            0)
                                        (error 1)))))
                '(module
                     (let ((x.1 #t) (y.2 #\a) (z.3 empty))
                       (if (boolean? x.1) (if (ascii-char? y.2) 1 0) (error 1)))))
  (check-equal? (interp-lam-opticon-lang-v9 (dox-lambdas '(module (letrec
                                                                      ((car.60 (lambda (tmp.61)
                                                                                 (if (pair? tmp.61) (unsafe-car tmp.61) (error 12))))
                                                                       (|+.64| (lambda (tmp.65 tmp.66)
                                                                                 (if (fixnum? tmp.65)
                                                                                     (if (fixnum? tmp.66)
                                                                                         (unsafe-fx+ tmp.65 tmp.66)
                                                                                         (error 2))
                                                                                     (error 2))))
                                                                       (eq?.54 (lambda (tmp.55 tmp.56)
                                                                                 (eq? tmp.55 tmp.56)))
                                                                       (cdr.62 (lambda (tmp.63)
                                                                                 (if (pair? tmp.63)
                                                                                     (unsafe-cdr tmp.63)
                                                                                     (error 13))))
                                                                       (cons.57 (lambda (tmp.58 tmp.59)
                                                                                  (cons tmp.58 tmp.59)))
                                                                       (map.25 (lambda (f.26 ls.27)
                                                                                 (if (unsafe-procedure-call eq?.54 empty ls.27)
                                                                                     empty
                                                                                     (unsafe-procedure-call cons.57
                                                                                                            (if (procedure? f.26)
                                                                                                                (if (eq? (unsafe-procedure-arity f.26) 1)
                                                                                                                    (unsafe-procedure-call f.26
                                                                                                                                           (unsafe-procedure-call car.60 ls.27))
                                                                                                                    (error 42))
                                                                                                                (error 43))
                                                                                                            (unsafe-procedure-call map.25
                                                                                                                                   f.26
                                                                                                                                   (unsafe-procedure-call cdr.62 ls.27)))))))
                                                                    (unsafe-procedure-call map.25
                                                                                           (lambda (x.28)
                                                                                             (unsafe-procedure-call |+.64| 1 x.28))
                                                                                           (unsafe-procedure-call cons.57
                                                                                                                  1
                                                                                                                  (unsafe-procedure-call cons.57
                                                                                                                                         2
                                                                                                                                         (unsafe-procedure-call cons.57
                                                                                                                                                                3
                                                                                                                                                                empty))))))))
                (interp-lam-opticon-lang-v9 '(module
                                                 (letrec ((car.60
                                                           (lambda (tmp.61)
                                                             (if (pair? tmp.61) (unsafe-car tmp.61) (error 12))))
                                                          (|+.64|
                                                           (lambda (tmp.65 tmp.66)
                                                             (if (fixnum? tmp.65)
                                                                 (if (fixnum? tmp.66) (unsafe-fx+ tmp.65 tmp.66) (error 2))
                                                                 (error 2))))
                                                          (eq?.54 (lambda (tmp.55 tmp.56) (eq? tmp.55 tmp.56)))
                                                          (cdr.62
                                                           (lambda (tmp.63)
                                                             (if (pair? tmp.63) (unsafe-cdr tmp.63) (error 13))))
                                                          (cons.57 (lambda (tmp.58 tmp.59) (cons tmp.58 tmp.59)))
                                                          (map.25
                                                           (lambda (f.26 ls.27)
                                                             (if (unsafe-procedure-call eq?.54 empty ls.27)
                                                                 empty
                                                                 (unsafe-procedure-call
                                                                  cons.57
                                                                  (if (procedure? f.26)
                                                                      (if (eq? (unsafe-procedure-arity f.26) 1)
                                                                          (unsafe-procedure-call
                                                                           f.26
                                                                           (unsafe-procedure-call car.60 ls.27))
                                                                          (error 42))
                                                                      (error 43))
                                                                  (unsafe-procedure-call
                                                                   map.25
                                                                   f.26
                                                                   (unsafe-procedure-call cdr.62 ls.27)))))))
                                                   (unsafe-procedure-call
                                                    map.25
                                                    (letrec ((lam.4 (lambda (x.28) (unsafe-procedure-call |+.64| 1 x.28))))
                                                      lam.4)
                                                    (unsafe-procedure-call
                                                     cons.57
                                                     1
                                                     (unsafe-procedure-call
                                                      cons.57
                                                      2
                                                      (unsafe-procedure-call cons.57 3 empty)))))))))
