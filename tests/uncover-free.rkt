#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9
  rackunit
  "../passes/uncover-free.rkt")

(module+ test
  (check-equal?
   (uncover-free
    `(module
         (letrec ([x.1 (lambda () (unsafe-procedure-call x.1))])
           x.1)))
   '(module
        (letrec ((x.1 (lambda ((free (x.1))) () (unsafe-procedure-call x.1)))) x.1))
   "Book test: basic unsafe-procedure-call")
  (check-equal?
   (uncover-free
    `(module
         (letrec ([f.1 (lambda ()
                         (letrec ([x.1 (lambda () (unsafe-procedure-call x.1))])
                           x.1))])
           f.1)))
   '(module
        (letrec ((f.1
                  (lambda ((free ()))
                    ()
                    (letrec ((x.1
                              (lambda ((free (x.1)))
                                ()
                                (unsafe-procedure-call x.1))))
                      x.1))))
          f.1))
   "Book test: unsafe call with free")

  (check-equal?
   (uncover-free
    `(module
         (letrec ([f.1 (lambda ()
                         (letrec ([x.1 (lambda () (unsafe-procedure-call x.1 tmp.1))])
                           x.1))])
           f.1)))
   '(module
        (letrec ((f.1
                  (lambda ((free (tmp.1)))
                    ()
                    (letrec ((x.1
                              (lambda ((free (tmp.1 x.1)))
                                ()
                                (unsafe-procedure-call x.1 tmp.1))))
                      x.1))))
          f.1))
   "Test with non-bounded variable in child letrec with no binding in letrec")

  (check-equal?
   (uncover-free '(module
                      (letrec ((|+.1|
                                (lambda (tmp.1 tmp.2)
                                  (if (fixnum? tmp.1)
                                      (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                                      (error 2))))
                               (|+.2|
                                (lambda (tmp.3 tmp.4) (unsafe-procedure-call |+.1| tmp.3 tmp.4))))
                        (unsafe-procedure-call |+.2| 1 2))))
   '(module
        (letrec ((|+.1|
                  (lambda ((free ()))
                    (tmp.1 tmp.2)
                    (if (fixnum? tmp.1)
                        (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                        (error 2))))
                 (|+.2|
                  (lambda ((free (|+.1|)))
                    (tmp.3 tmp.4)
                    (unsafe-procedure-call |+.1| tmp.3 tmp.4))))
          (unsafe-procedure-call |+.2| 1 2)))
   "Test with mutually dependent procedures")

  (check-equal?
   (uncover-free '(module
                      (letrec ((|+.1|
                                (lambda (tmp.1 tmp.2)
                                  (if (fixnum? tmp.1)
                                      (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2 tmp.3) (error 2))
                                      (error 2))))
                               (|+.2|
                                (lambda (tmp.3 tmp.4) (unsafe-procedure-call |+.1| tmp.3 tmp.4))))
                        (unsafe-procedure-call |+.2| 1 2))))
   '(module
        (letrec ((|+.1|
                  (lambda ((free (tmp.3)))
                    (tmp.1 tmp.2)
                    (if (fixnum? tmp.1)
                        (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2 tmp.3) (error 2))
                        (error 2))))
                 (|+.2|
                  (lambda ((free (|+.1|)))
                    (tmp.3 tmp.4)
                    (unsafe-procedure-call |+.1| tmp.3 tmp.4))))
          (unsafe-procedure-call |+.2| 1 2)))
   "Test with procedure call with free variables not in lambda arg")

  (check-equal?
   (uncover-free '(module
                      (letrec ((|+.1|
                                (lambda (tmp.1 tmp.2)
                                  (if (fixnum? tmp.1)
                                      (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2 tmp.3) (error 2))
                                      (error 2))))
                               (|+.2|
                                (lambda (tmp.3 tmp.4) (unsafe-procedure-call |+.1| tmp.3 tmp.4))))
                        (unsafe-procedure-call |+.2| 1 2 tmp.3))))
   '(module
        (letrec ((|+.1|
                  (lambda ((free (tmp.3)))
                    (tmp.1 tmp.2)
                    (if (fixnum? tmp.1)
                        (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2 tmp.3) (error 2))
                        (error 2))))
                 (|+.2|
                  (lambda ((free (|+.1|)))
                    (tmp.3 tmp.4)
                    (unsafe-procedure-call |+.1| tmp.3 tmp.4))))
          (unsafe-procedure-call |+.2| 1 2 tmp.3)))
   "Test with body cal with free variable")

  (check-equal?
   (uncover-free '(module
                      (letrec ([a.1
                                (lambda ()
                                  (letrec ([a.2 (lambda ()
                                                  (let ([a.3 (begin
                                                               (eq? 1 2)
                                                               (if (eq? 2 1)
                                                                   tmp.3
                                                                   tmp.4))])
                                                    a.3))])
                                    a.2))])
                        a.1)))
   '(module
        (letrec ((a.1
                  (lambda ((free (tmp.3 tmp.4)))
                    ()
                    (letrec ((a.2
                              (lambda ((free (tmp.3 tmp.4)))
                                ()
                                (let ((a.3
                                       (begin (eq? 1 2) (if (eq? 2 1) tmp.3 tmp.4))))
                                  a.3))))
                      a.2))))
          a.1))
   "Test with super nested letrec with let")

  (check-equal?
   (uncover-free '(module
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
                         (letrec ((lam.7 (lambda (x.28) (unsafe-procedure-call |+.64| 1 x.28))))
                           lam.7)
                         (unsafe-procedure-call
                          cons.57
                          1
                          (unsafe-procedure-call
                           cons.57
                           2
                           (unsafe-procedure-call cons.57 3 empty)))))))
   '(module
        (letrec ((car.60
                  (lambda ((free ()))
                    (tmp.61)
                    (if (pair? tmp.61) (unsafe-car tmp.61) (error 12))))
                 (|+.64|
                  (lambda ((free ()))
                    (tmp.65 tmp.66)
                    (if (fixnum? tmp.65)
                        (if (fixnum? tmp.66) (unsafe-fx+ tmp.65 tmp.66) (error 2))
                        (error 2))))
                 (eq?.54 (lambda ((free ())) (tmp.55 tmp.56) (eq? tmp.55 tmp.56)))
                 (cdr.62
                  (lambda ((free ()))
                    (tmp.63)
                    (if (pair? tmp.63) (unsafe-cdr tmp.63) (error 13))))
                 (cons.57 (lambda ((free ())) (tmp.58 tmp.59) (cons tmp.58 tmp.59)))
                 (map.25
                  (lambda ((free (eq?.54 car.60 cdr.62 map.25 cons.57)))
                    (f.26 ls.27)
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
           (letrec ((lam.7
                     (lambda ((free (|+.64|)))
                       (x.28)
                       (unsafe-procedure-call |+.64| 1 x.28))))
             lam.7)
           (unsafe-procedure-call
            cons.57
            1
            (unsafe-procedure-call
             cons.57
             2
             (unsafe-procedure-call cons.57 3 empty))))))
   "More complicated test from dox-lambdas")
  (check-equal?
   (uncover-free
    `(module
         (letrec ([x.1 (lambda () (let ([a.2 (unsafe-procedure-call identify.13 identify.13)]
                                        [a.3 (unsafe-procedure-call identify.13 identify.13)]
                                        [a.4 (unsafe-procedure-call identify.13 identify.13)])
                                    a.4))])
           x.1)))
   '(module
        (letrec ((x.1
                  (lambda ((free (identify.13)))
                    ()
                    (let ((a.2 (unsafe-procedure-call identify.13 identify.13))
                          (a.3 (unsafe-procedure-call identify.13 identify.13))
                          (a.4 (unsafe-procedure-call identify.13 identify.13)))
                      a.4))))
          x.1))
   "Test, let with duplicate bindings"))
