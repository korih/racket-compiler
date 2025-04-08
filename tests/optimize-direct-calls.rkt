#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9
  "../passes/optimize-direct-calls.rkt"
  rackunit)

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
