#lang racket

(require
  rackunit
  "../passes/optimize-known-calls.rkt")


(module+ test
  (check-equal?
   (optimize-known-calls '(module
                              (letrec ((L.+.1.7
                                        (lambda (c.4 tmp.1 tmp.2)
                                          (let ((tmp.3 (closure-ref c.4 0)))
                                            (if (fixnum? tmp.1)
                                                (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2 tmp.3) (error 2))
                                                (error 2)))))
                                       (L.+.2.8
                                        (lambda (c.5 tmp.3 tmp.4)
                                          (let ((|+.1| (closure-ref c.5 0)))
                                            (closure-call |+.1| |+.1| tmp.3 tmp.4)))))
                                (cletrec
                                 ((|+.1| (make-closure L.+.1.7 2 tmp.3))
                                  (|+.2| (make-closure L.+.2.8 2 |+.1|)))
                                 (closure-call |+.2| |+.2| 1 2 tmp.3)))))
   '(module
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
           (call L.+.2.8 |+.2| 1 2 tmp.3))))
   "Convert closure call to call")
  (check-equal?
   (optimize-known-calls '(module
                              (letrec ((L.x.1.7
                                        (lambda (c.4)
                                          (let ((x.1 (closure-ref c.4 0))) (closure-call x.1 x.1)))))
                                (cletrec ((x.1 (make-closure L.x.1.7 0 x.1))) x.1))))
   '(module
        (letrec ((L.x.1.7
                  (lambda (c.4)
                    (let ((x.1 (closure-ref c.4 0))) (call L.x.1.7 x.1)))))
          (cletrec ((x.1 (make-closure L.x.1.7 0 x.1))) x.1)))
   "Convert closure call with no call")
  (check-equal?
   (optimize-known-calls '(module
                              (letrec ((L.x.1.7
                                        (lambda (c.4)
                                          (let ((x.1 (closure-ref c.4 0))) (closure-call x.1 x.1)))))
                                (cletrec ((x.1 (make-closure L.x.1.7 0 x.1)))
                                         (closure-call x.1 x.1 1 2)))))
   '(module
        (letrec ((L.x.1.7
                  (lambda (c.4)
                    (let ((x.1 (closure-ref c.4 0))) (call L.x.1.7 x.1)))))
          (cletrec ((x.1 (make-closure L.x.1.7 0 x.1))) (call L.x.1.7 x.1 1 2))))
   "Convert closure with call that has extra args")

  )
