#lang racket

(require
  rackunit
  "../passes/hoist-lambdas.rkt")

(module+ test
  (check-equal?
   (hoist-lambdas '(module
                       (letrec ((L.+.1.7
                                 (lambda (c.4 tmp.1 tmp.2)
                                   (let ((tmp.3 (closure-ref c.4 0)))
                                     (if (fixnum? tmp.1)
                                         (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2 tmp.3) (error 2))
                                         (error 2)))))
                                (L.+.2.8
                                 (lambda (c.5 tmp.3 tmp.4)
                                   (let ((|+.1| (closure-ref c.5 0)))
                                     (call L.+.1.7 |+.1| tmp.3 tmp.4)))))
                         (cletrec
                          ((|+.1| (make-closure L.+.1.7 2 tmp.3))
                           (|+.2| (make-closure L.+.2.8 2 |+.1|)))
                          (call L.+.2.8 |+.2| 1 2 tmp.3)))))
   '(module
        (define L.+.2.8
          (lambda (c.5 tmp.3 tmp.4)
            (let ((|+.1| (closure-ref c.5 0))) (call L.+.1.7 |+.1| tmp.3 tmp.4))))
      (define L.+.1.7
        (lambda (c.4 tmp.1 tmp.2)
          (let ((tmp.3 (closure-ref c.4 0)))
            (if (fixnum? tmp.1)
                (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2 tmp.3) (error 2))
                (error 2)))))
      (cletrec
       ((|+.1| (make-closure L.+.1.7 2 tmp.3))
        (|+.2| (make-closure L.+.2.8 2 |+.1|)))
       (call L.+.2.8 |+.2| 1 2 tmp.3)))
   "Basic test for hoisting two definitions")
  (check-equal?
   (hoist-lambdas
    '(module
         (letrec ((L.x.1.7
                   (lambda (c.4)
                     (let ((x.1 (closure-ref c.4 0))) (call L.x.1.7 x.1)))))
           (cletrec ((x.1 (make-closure L.x.1.7 0 x.1))) x.1))))
   '(module
        (define L.x.1.7
          (lambda (c.4) (let ((x.1 (closure-ref c.4 0))) (call L.x.1.7 x.1))))
      (cletrec ((x.1 (make-closure L.x.1.7 0 x.1))) x.1))
   "No call in body")
  (check-equal?
   (hoist-lambdas
    '(module
         (letrec ((L.x.1.7
                   (lambda (c.4)
                     (let ((x.1 (closure-ref c.4 0))) (call L.x.1.7 x.1)))))
           (cletrec ((x.1 (make-closure L.x.1.7 0 x.1))) (call L.x.1.7 x.1 1 2)))))
   '(module
        (define L.x.1.7
          (lambda (c.4) (let ((x.1 (closure-ref c.4 0))) (call L.x.1.7 x.1))))
      (cletrec ((x.1 (make-closure L.x.1.7 0 x.1))) (call L.x.1.7 x.1 1 2)))
   "Incorrect arg count test"))
