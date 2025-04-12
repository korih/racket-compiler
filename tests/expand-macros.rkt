#lang racket

(require rackunit
         "../passes/expand-macros.rkt")

(module+ test
  (check-equal? (expand-macros '(module (error 5)))
                '(module (error 5)))
  (check-equal? (expand-macros '(module (#t #f)))
                '(module (call #t #f)))
  (check-equal? (expand-macros '(module '(#t #f)))
                '(module (call cons #t (call cons #f empty))))
  (check-equal? (expand-macros '(module '(1 2 #\a #\b (1 2))))
                '(module
                     (call
                      cons
                      1
                      (call
                       cons
                       2
                       (call
                        cons
                        #\a
                        (call cons #\b (call cons (call cons 1 (call cons 2 empty)) empty)))))))
  (check-equal? (expand-macros '(module (quote (#t #f))))
                '(module (call cons #t (call cons #f empty))))
  (check-equal? (expand-macros '(module (quote 1)))
                '(module 1))
  (check-equal? (expand-macros '(module (and 1)))
                '(module 1))
  (check-equal? (expand-macros '(module (and 1 2 3)))
                '(module (if 1 (if 2 3 #f) #f)))
  (check-equal? (expand-macros '(module (and 1 2 #f 3)))
                '(module (if 1 (if 2 (if #f 3 #f) #f) #f)))
  (check-equal? (expand-macros '(module (and)))
                '(module #t))
  (check-equal? (expand-macros '(module (or)))
                '(module #f))
  (check-equal? (expand-macros '(module (or 1)))
                '(module 1))
  (check-equal? (expand-macros '(module (or 1 2 #f #t)))
                '(module
                     (let ((tmp.1 1))
                       (if tmp.1
                           tmp.1
                           (let ((tmp.2 2))
                             (if tmp.2 tmp.2 (let ((tmp.3 #f)) (if tmp.3 tmp.3 #t))))))))
  (check-equal? (expand-macros '(module (vector 1 2 #t #f #\a)))
                '(module
                     (let ((tmp.4 (call make-vector 5)))
                       (let ((tmp.5 (call vector-set! tmp.4 0 1)))
                         (if (call error? tmp.5)
                             tmp.5
                             (let ((tmp.6 (call vector-set! tmp.4 1 2)))
                               (if (call error? tmp.6)
                                   tmp.6
                                   (let ((tmp.7 (call vector-set! tmp.4 2 #t)))
                                     (if (call error? tmp.7)
                                         tmp.7
                                         (let ((tmp.8 (call vector-set! tmp.4 3 #f)))
                                           (if (call error? tmp.8)
                                               tmp.8
                                               (let ((tmp.9 (call vector-set! tmp.4 4 #\a)))
                                                 (if (call error? tmp.9) tmp.9 tmp.4)))))))))))))
  (check-equal? (expand-macros '(module #(1 2 #t #f #\a)))
                '(module
                     (let ((tmp.10 (call make-vector 5)))
                       (let ((tmp.11 (call vector-set! tmp.10 0 1)))
                         (if (call error? tmp.11)
                             tmp.11
                             (let ((tmp.12 (call vector-set! tmp.10 1 2)))
                               (if (call error? tmp.12)
                                   tmp.12
                                   (let ((tmp.13 (call vector-set! tmp.10 2 #t)))
                                     (if (call error? tmp.13)
                                         tmp.13
                                         (let ((tmp.14 (call vector-set! tmp.10 3 #f)))
                                           (if (call error? tmp.14)
                                               tmp.14
                                               (let ((tmp.15 (call vector-set! tmp.10 4 #\a)))
                                                 (if (call error? tmp.15) tmp.15 tmp.10)))))))))))))
  (check-equal? (expand-macros '(module (begin)))
                '(module (void)))
  (check-equal? (expand-macros '(module (begin (eq? 1 2))))
                '(module (call eq? 1 2)))
  (check-equal? (expand-macros '(module (begin (eq? 1 2)
                                               (void? 1))))
                '(module
                     (let ((tmp.16 (call eq? 1 2)))
                       (if (call error? tmp.16) tmp.16 (call void? 1)))))
  (check-equal? (expand-macros '(module
                                    (begin
                                      (define x (lambda (y) (and y (or #f (begin (vector 1 2 #t #\a) #t)))))
                                      (define z (lambda (n m) (vector (and (fixnum? n) (>= n 0)) (or (boolean? m) #f))))
                                      (let ((a (call x #t))
                                            (b (call z 42 #f)))
                                        (begin
                                          (and (or (vector? a) (begin (void) #f)) (not (call error? b)))
                                          (call vector-ref b 0))))))
                '(module
                     (let ((tmp.17
                            (call
                             define
                             x
                             (lambda (y)
                               (if y
                                   (let ((tmp.18 #f))
                                     (if tmp.18
                                         tmp.18
                                         (let ((tmp.19
                                                (let ((tmp.20 (call make-vector 4)))
                                                  (let ((tmp.21 (call vector-set! tmp.20 0 1)))
                                                    (if (call error? tmp.21)
                                                        tmp.21
                                                        (let ((tmp.22 (call vector-set! tmp.20 1 2)))
                                                          (if (call error? tmp.22)
                                                              tmp.22
                                                              (let ((tmp.23
                                                                     (call vector-set! tmp.20 2 #t)))
                                                                (if (call error? tmp.23)
                                                                    tmp.23
                                                                    (let ((tmp.24
                                                                           (call
                                                                            vector-set!
                                                                            tmp.20
                                                                            3
                                                                            #\a)))
                                                                      (if (call error? tmp.24)
                                                                          tmp.24
                                                                          tmp.20)))))))))))
                                           (if (call error? tmp.19) tmp.19 #t))))
                                   #f)))))
                       (if (call error? tmp.17)
                           tmp.17
                           (let ((tmp.25
                                  (call
                                   define
                                   z
                                   (lambda (n m)
                                     (let ((tmp.26 (call make-vector 2)))
                                       (let ((tmp.28
                                              (call
                                               vector-set!
                                               tmp.26
                                               0
                                               (if (call fixnum? n) (call >= n 0) #f))))
                                         (if (call error? tmp.28)
                                             tmp.28
                                             (let ((tmp.29
                                                    (call
                                                     vector-set!
                                                     tmp.26
                                                     1
                                                     (let ((tmp.27 (call boolean? m)))
                                                       (if tmp.27 tmp.27 #f)))))
                                               (if (call error? tmp.29) tmp.29 tmp.26)))))))))
                             (if (call error? tmp.25)
                                 tmp.25
                                 (let ((a (call x #t)) (b (call z 42 #f)))
                                   (let ((tmp.30
                                          (if (let ((tmp.31 (call vector? a)))
                                                (if tmp.31
                                                    tmp.31
                                                    (let ((tmp.32 (call void)))
                                                      (if (call error? tmp.32) tmp.32 #f))))
                                              (call not (call error? b))
                                              #f)))
                                     (if (call error? tmp.30) tmp.30 (call vector-ref b 0))))))))))
  (check-equal?
   (expand-macros '(module (error 5)))
   '(module (error 5))
   "Check if error works")
  (check-equal?
   (expand-macros '(module (call (void))))
   '(module (call (void)))
   "Check if error works"))
