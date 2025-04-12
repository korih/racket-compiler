#lang racket

(require
  "../passes/define-letrec.rkt"
  rackunit)

(module+ test
  (check-equal?
   (define->letrec '(module
                        (define |+.2|
                          (lambda (tmp.3 tmp.4) (unsafe-procedure-call |+.1| tmp.3 tmp.4)))
                      (define |+.1|
                        (lambda (tmp.1 tmp.2)
                          (if (fixnum? tmp.1)
                              (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                              (error 2))))
                      (unsafe-procedure-call |+.2| 1 2)))
   '(module
        (letrec ((|+.2|
                  (lambda (tmp.3 tmp.4) (unsafe-procedure-call |+.1| tmp.3 tmp.4)))
                 (|+.1|
                  (lambda (tmp.1 tmp.2)
                    (if (fixnum? tmp.1)
                        (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                        (error 2)))))
          (unsafe-procedure-call |+.2| 1 2)))
   "check basic define->letrec transformation")

  (check-equal?
   (define->letrec '(module
                        (define |+.1|
                          (lambda (tmp.1 tmp.2)
                            (if (fixnum? tmp.1)
                                (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                                (error 2))))
                      (define |+.2|
                        (lambda (tmp.3 tmp.4) (unsafe-procedure-call |+.1| tmp.3 tmp.4)))
                      (unsafe-procedure-call |+.2| 1 2)))
   '(module
        (letrec ((|+.1|
                  (lambda (tmp.1 tmp.2)
                    (if (fixnum? tmp.1)
                        (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                        (error 2))))
                 (|+.2|
                  (lambda (tmp.3 tmp.4) (unsafe-procedure-call |+.1| tmp.3 tmp.4))))
          (unsafe-procedure-call |+.2| 1 2)))
   "check functions that depend on each other")

  (check-equal?
   (define->letrec '(module
                        (define |+.1|
                          (lambda (tmp.1 tmp.2)
                            (if (fixnum? tmp.1)
                                (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                                (error 2))))
                      (error 42)))
   '(module
        (letrec ((|+.1|
                  (lambda (tmp.1 tmp.2)
                    (if (fixnum? tmp.1)
                        (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                        (error 2)))))
          (error 42)))
   "check wrong-lambda arity")
  (check-equal?
   (define->letrec '(module (error 42)))
   '(module (error 42))
   "Check no functions"))
