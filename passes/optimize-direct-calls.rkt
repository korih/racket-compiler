#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9
  rackunit)

(provide optimize-direct-calls)

;; just-exprs-lang-v9 -> just-exprs-lang-v9
;; optimizes p by inlining all direct calls to first-class procedures
(define/contract (optimize-direct-calls p)
  (-> just-exprs-lang-v9? just-exprs-lang-v9?)

  (define (optimize-direct-calls-value value)
    (match value
      [`(unsafe-procedure-call ,func ,params ...)
       (define func^ (optimize-direct-calls-value func))
       (define params^ (map optimize-direct-calls-value params))
       (match func
         [`(lambda (,args ...) ,v)
          `(let ,(map list args params^) ,(optimize-direct-calls-value v))]
         [_ `(unsafe-procedure-call ,func^ ,@params^)])]
      [`(if ,v1 ,v2 ,v3)
       `(if ,(optimize-direct-calls-value v1)
            ,(optimize-direct-calls-value v2)
            ,(optimize-direct-calls-value v3))]
      [`(begin ,effs ... ,v)
       `(begin ,@(map optimize-direct-calls-effect effs) ,(optimize-direct-calls-value v))]
      [`(letrec ([,alocs (lambda (,args ...) ,bodies)] ...) ,v)
       (define bindings
         (map (lambda (aloc args body)
                (list aloc `(lambda ,args ,(optimize-direct-calls-value body))))
              alocs args bodies))
       `(letrec ,bindings ,(optimize-direct-calls-value v))]
      [`(let ([,alocs ,vs] ...) ,v)
       (define bindings
         (map (lambda (aloc val)
                (list aloc (optimize-direct-calls-value val)))
              alocs vs))
       `(let ,bindings ,(optimize-direct-calls-value v))]
      [`(,primop ,vs ...)
       `(,primop ,@(map optimize-direct-calls-value vs))]
      [triv (optimize-direct-calls-triv triv)]))

  (define (optimize-direct-calls-effect effect)
    (match effect
      [`(begin ,effs ...)
       `(begin ,@(map optimize-direct-calls-effect effs))]
      [`(,primop ,vs ...)
       `(,primop ,@(map optimize-direct-calls-value vs))]))

  (define (optimize-direct-calls-triv triv)
    (match triv
      [`(lambda (,args ...) ,v)
       `(lambda (,@args) ,(optimize-direct-calls-value v))]
      ;; Wildcard collapse cased used because 
      [_ triv]))

  (match p
    [`(module ,value)
     `(module ,(optimize-direct-calls-value value))]))

(module+ test
  (check-equal? (optimize-direct-calls '(module (unsafe-procedure-call (lambda (x.1) x.1) 1)))
                '(module (let ((x.1 1)) x.1)))
  (check-equal? (optimize-direct-calls '(module (unsafe-procedure-call (lambda () 1))))
                '(module (let () 1)))
  (check-equal? (optimize-direct-calls '(module (unsafe-procedure-call (lambda (x.1 x.2 x.3 x.4) x.1) 1 2 3 4)))
                '(module (let ((x.1 1) (x.2 2) (x.3 3) (x.4 4)) x.1)))
  (check-equal? (optimize-direct-calls '(module (unsafe-procedure-call (lambda (x.1) (let ([y.1 x.1]) y.1)) 1)))
                '(module (let ((x.1 1)) (let ((y.1 x.1)) y.1))))
  (check-equal? (optimize-direct-calls '(module (letrec ([x.1 (lambda (y.1) y.1)])
                                                  (unsafe-procedure-call x.1 10))))
                '(module (letrec ((x.1 (lambda (y.1) y.1))) (unsafe-procedure-call x.1 10))))
  (check-equal? (optimize-direct-calls '(module
                                            (letrec ([|+.1|
                                                      (lambda (tmp.22 tmp.23)
                                                        (if (fixnum? tmp.23)
                                                            (if (fixnum? tmp.22) (unsafe-fx+ tmp.22 tmp.23) (error 2))
                                                            (error 2)))]
                                                     [x.4 (lambda (y.5) (unsafe-procedure-call |+.1| y.5 1))])
                                              (unsafe-procedure-call x.4 10))))
                '(module
                     (letrec ((|+.1|
                               (lambda (tmp.22 tmp.23)
                                 (if (fixnum? tmp.23)
                                     (if (fixnum? tmp.22) (unsafe-fx+ tmp.22 tmp.23) (error 2))
                                     (error 2))))
                              (x.4 (lambda (y.5) (unsafe-procedure-call |+.1| y.5 1))))
                       (unsafe-procedure-call x.4 10))))
  (check-equal? (optimize-direct-calls '(module
                                            (letrec ([|+.1|
                                                      (lambda (tmp.22 tmp.23)
                                                        (if (fixnum? tmp.23)
                                                            (if (fixnum? tmp.22) (unsafe-fx+ tmp.22 tmp.23) (error 2))
                                                            (error 2)))])
                                              (unsafe-procedure-call (lambda (x.1) (unsafe-procedure-call |+.1| x.1 1)) 1))))
                '(module
                     (letrec ((|+.1|
                               (lambda (tmp.22 tmp.23)
                                 (if (fixnum? tmp.23)
                                     (if (fixnum? tmp.22) (unsafe-fx+ tmp.22 tmp.23) (error 2))
                                     (error 2)))))
                       (let ((x.1 1)) (unsafe-procedure-call |+.1| x.1 1))))))
