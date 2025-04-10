#lang racket

(require
  rackunit
  cpsc411/langs/v9
  "../passes/implement-closures.rkt")

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
   "basic closure-call test")

  (check-equal?
   (interp-proc-exposed-lang-v9 (implement-closures
                                  '(module
                                       (define L.tmp.63
                                         (lambda (tmp.344 tmp.93)
                                           (let ((make-init-vector.91 (closure-ref tmp.344 0)))
                                             (if (fixnum? tmp.93)
                                                 (closure-call make-init-vector.91 make-init-vector.91 tmp.93)
                                                 (error 8)))))
                                     (define L.tmp.62
                                       (lambda (tmp.343 len.96 i.97 vec.98)
                                         (let ((vector-init-loop.92 (closure-ref tmp.343 0)))
                                           (if (eq? len.96 i.97)
                                               vec.98
                                               (begin
                                                 (unsafe-vector-set! vec.98 i.97 0)
                                                 (closure-call
                                                  vector-init-loop.92
                                                  vector-init-loop.92
                                                  len.96
                                                  (unsafe-fx+ i.97 1)
                                                  vec.98))))))
                                     (define L.tmp.61
                                       (lambda (tmp.342 tmp.94)
                                         (let ((vector-init-loop.92 (closure-ref tmp.342 0)))
                                           (if (unsafe-fx>= tmp.94 0)
                                               (let ((tmp.95 (unsafe-make-vector tmp.94)))
                                                 (closure-call
                                                  vector-init-loop.92
                                                  vector-init-loop.92
                                                  tmp.94
                                                  0
                                                  tmp.95))
                                               (error 12)))))
                                     (cletrec
                                      ((make-init-vector.91 (make-closure L.tmp.61 1 vector-init-loop.92))
                                       (vector-init-loop.92 (make-closure L.tmp.62 3 vector-init-loop.92))
                                       (make-vector.90 (make-closure L.tmp.63 1 make-init-vector.91)))
                                      (closure-call make-vector.90 make-vector.90 0)))))
   (interp-proc-exposed-lang-v9
    '(module
         (define L.tmp.63
           (lambda (tmp.344 tmp.93)
             (let ((make-init-vector.91 (unsafe-procedure-ref tmp.344 0)))
               (if (fixnum? tmp.93)
                   (call
                    (unsafe-procedure-label make-init-vector.91)
                    make-init-vector.91
                    tmp.93)
                   (error 8)))))
       (define L.tmp.62
         (lambda (tmp.343 len.96 i.97 vec.98)
           (let ((vector-init-loop.92 (unsafe-procedure-ref tmp.343 0)))
             (if (eq? len.96 i.97)
                 vec.98
                 (begin
                   (unsafe-vector-set! vec.98 i.97 0)
                   (call
                    (unsafe-procedure-label vector-init-loop.92)
                    vector-init-loop.92
                    len.96
                    (unsafe-fx+ i.97 1)
                    vec.98))))))
       (define L.tmp.61
         (lambda (tmp.342 tmp.94)
           (let ((vector-init-loop.92 (unsafe-procedure-ref tmp.342 0)))
             (if (unsafe-fx>= tmp.94 0)
                 (let ((tmp.95 (unsafe-make-vector tmp.94)))
                   (call
                    (unsafe-procedure-label vector-init-loop.92)
                    vector-init-loop.92
                    tmp.94
                    0
                    tmp.95))
                 (error 12)))))
       (let ((make-init-vector.91 (make-procedure L.tmp.61 1 1))
             (vector-init-loop.92 (make-procedure L.tmp.62 3 1))
             (make-vector.90 (make-procedure L.tmp.63 1 1)))
         (begin
           (unsafe-procedure-set! make-init-vector.91 0 vector-init-loop.92)
           (unsafe-procedure-set! vector-init-loop.92 0 vector-init-loop.92)
           (unsafe-procedure-set! make-vector.90 0 make-init-vector.91)
           (call (unsafe-procedure-label make-vector.90) make-vector.90 0)))))))
