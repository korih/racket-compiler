#lang racket

(require
  rackunit
  cpsc411/langs/v9
  "../passes/dox-lambdas.rkt")

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
