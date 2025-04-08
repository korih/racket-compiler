#lang racket

(require
  rackunit
  "../passes/implement-closures.rkt"
  cpsc411/langs/v9)

(module+ test
  (check-equal?
   (implement-closures '(module
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
                           (call L.+.2.8 |+.2| 1 2 tmp.3))))
   '(module
        (define L.+.2.8
          (lambda (c.5 tmp.3 tmp.4)
            (let ((|+.1| (unsafe-procedure-ref c.5 0)))
              (call L.+.1.7 |+.1| tmp.3 tmp.4))))
      (define L.+.1.7
        (lambda (c.4 tmp.1 tmp.2)
          (let ((tmp.3 (unsafe-procedure-ref c.4 0)))
            (if (fixnum? tmp.1)
                (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2 tmp.3) (error 2))
                (error 2)))))
      (let ((|+.1| (make-procedure L.+.1.7 2 1))
            (|+.2| (make-procedure L.+.2.8 2 1)))
        (begin
          (unsafe-procedure-set! |+.1| 0 tmp.3)
          (unsafe-procedure-set! |+.2| 0 |+.1|)
          (call L.+.2.8 |+.2| 1 2 tmp.3))))
   "check basic closure implementation")
  (check-equal?
   (implement-closures
    '(module
         (define L.x.1.7
           (lambda (c.4) (let ((x.1 (closure-ref c.4 0))) (call L.x.1.7 x.1))))
       (cletrec ((x.1 (make-closure L.x.1.7 0 x.1))) x.1)))
   '(module
        (define L.x.1.7
          (lambda (c.4)
            (let ((x.1 (unsafe-procedure-ref c.4 0))) (call L.x.1.7 x.1))))
      (let ((x.1 (make-procedure L.x.1.7 0 1)))
        (begin (unsafe-procedure-set! x.1 0 x.1) x.1)))
   "basic closure with no func call")
  (check-equal?
   (implement-closures
    '(module
         (define L.x.1.7
           (lambda (c.4) (let ((x.1 (closure-ref c.4 0))) (call L.x.1.7 x.1))))
       (cletrec ((x.1 (make-closure L.x.1.7 0 x.1 x.1))
                 (x.2 (make-closure L.x.17 0 x.1 x.1)))
                (closure-call L.x.1.7 x.1 x.1))))
   '(module
        (define L.x.1.7
          (lambda (c.4)
            (let ((x.1 (unsafe-procedure-ref c.4 0))) (call L.x.1.7 x.1))))
      (let ((x.1 (make-procedure L.x.1.7 0 2)) (x.2 (make-procedure L.x.17 0 2)))
        (begin
          (unsafe-procedure-set! x.1 0 x.1)
          (unsafe-procedure-set! x.1 1 x.1)
          (unsafe-procedure-set! x.2 0 x.1)
          (unsafe-procedure-set! x.2 1 x.1)
          (call (unsafe-procedure-label L.x.1.7) x.1 x.1))))
   "basic closure-call test"))
