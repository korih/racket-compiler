#lang racket

(require
  rackunit
  "../passes/implement-safe-call.rkt")

(module+ test
  (require rackunit)
  (check-equal? (implement-safe-call '(module
                                          (define |+.1|
                                            (lambda (tmp.1 tmp.2)
                                              (if (fixnum? tmp.1)
                                                  (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                                                  (error 2))))
                                        (call |+.1| 1 2)))
                '(module
                     (define |+.1|
                       (lambda (tmp.1 tmp.2)
                         (if (fixnum? tmp.1)
                             (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                             (error 2))))
                   (unsafe-procedure-call |+.1| 1 2))
                "Checking if normal call with addition works")
  (check-equal? (implement-safe-call '(module
                                          (define |+.1|
                                            (lambda (tmp.1 tmp.2)
                                              (if (fixnum? tmp.1)
                                                  (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                                                  (error 2))))
                                        (call (lambda (tmp.10 tmp.11) (call |+.1| tmp.10 tmp.12)) 1 2)))
                '(module
                     (define |+.1|
                       (lambda (tmp.1 tmp.2)
                         (if (fixnum? tmp.1)
                             (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                             (error 2))))
                   (unsafe-procedure-call
                    (lambda (tmp.10 tmp.11) (unsafe-procedure-call |+.1| tmp.10 tmp.12))
                    1
                    2))
                "Checking if lambda with nested call works")

  ;; NOTE: Does tmp.1 get shadowed? they should be unique here, just going to let it pass for now
  (check-equal? (implement-safe-call '(module
                                          (define |+.1|
                                            (lambda (tmp.1 tmp.2)
                                              (if (fixnum? tmp.1)
                                                  (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                                                  (error 2))))
                                        (call 1)))
                '(module
                     (define |+.1|
                       (lambda (tmp.1 tmp.2)
                         (if (fixnum? tmp.1)
                             (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                             (error 2))))
                   (let ((call-tmp.1 1))
                     (if (procedure? call-tmp.1)
                         (if (eq? (unsafe-procedure-arity call-tmp.1) 0)
                             (unsafe-procedure-call call-tmp.1)
                             (error 42))
                         (error 43))))
                "Checking if it works with no args and non-procedure")

  (check-equal? (implement-safe-call '(module
                                          (define |+.1|
                                            (lambda (tmp.1 tmp.2)
                                              (if (fixnum? tmp.1)
                                                  (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                                                  (error 2))))
                                        (call |+.1| 1 2)))
                '(module
                     (define |+.1|
                       (lambda (tmp.1 tmp.2)
                         (if (fixnum? tmp.1)
                             (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                             (error 2))))
                   (unsafe-procedure-call |+.1| 1 2))
                "Checking if it works with defined function call")
  (check-equal?
   (implement-safe-call '(module
                             (define |+.1|
                               (lambda (tmp.1 tmp.2)
                                 (if (fixnum? tmp.1)
                                     (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                                     (error 2))))
                           (call a.1 1 2)))
   '(module
        (define |+.1|
          (lambda (tmp.1 tmp.2)
            (if (fixnum? tmp.1)
                (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                (error 2))))
      (if (procedure? a.1)
          (if (eq? (unsafe-procedure-arity a.1) 2)
              (unsafe-procedure-call a.1 1 2)
              (error 42))
          (error 43)))
   "Checking if there is it works with aloc as procedure call")
  (check-equal?
   (implement-safe-call '(module
                             (define |+.1|
                               (lambda (tmp.1 tmp.2)
                                 (if (fixnum? tmp.1)
                                     (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                                     (error 2))))
                           (call |+.1|)))
   '(module
        (define |+.1|
          (lambda (tmp.1 tmp.2)
            (if (fixnum? tmp.1)
                (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                (error 2))))
      (error 42))
   "checking if it works with no args and defined function")
  (check-equal?
   (implement-safe-call '(module
                             (define |+.1|
                               (lambda (tmp.1 tmp.2)
                                 (if (fixnum? tmp.1)
                                     (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                                     (error 2))))
                           (call |+.1| 1 2 3 )))
   '(module
        (define |+.1|
          (lambda (tmp.1 tmp.2)
            (if (fixnum? tmp.1)
                (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                (error 2))))
      (error 42))
   "checking if it works with too many args and defined function")
  (check-equal?
   (implement-safe-call '(module
                             (define |+.1|
                               (lambda (tmp.1 tmp.2)
                                 (if (fixnum? tmp.1)
                                     (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                                     (error 2))))
                           (call |+.1| (void) (void))))
   '(module
        (define |+.1|
          (lambda (tmp.1 tmp.2)
            (if (fixnum? tmp.1)
                (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                (error 2))))
      (unsafe-procedure-call |+.1| (void) (void)))
   "wrong types for addition")

  (check-equal?
   (implement-safe-call '(module
                             (define |+.1|
                               (lambda (tmp.1 tmp.2)
                                 (if (fixnum? tmp.1)
                                     (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                                     (error 2))))
                           (call (lambda (tmp.1 tmp.2) (call |+.1| tmp.1 tmp.2)) (void) (void))))
   '(module
        (define |+.1|
          (lambda (tmp.1 tmp.2)
            (if (fixnum? tmp.1)
                (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                (error 2))))
      (unsafe-procedure-call
       (lambda (tmp.1 tmp.2) (unsafe-procedure-call |+.1| tmp.1 tmp.2))
       (void)
       (void)))
   "checking lambda with wrong arg types")

  (check-equal?
   (implement-safe-call '(module
                             (define |+.1|
                               (lambda (tmp.1 tmp.2)
                                 (if (fixnum? tmp.1)
                                     (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                                     (error 2))))
                           (call (lambda (tmp.1 tmp.2) (call |+.1| tmp.1 tmp.2)) 1 (void) (void))))
   '(module
        (define |+.1|
          (lambda (tmp.1 tmp.2)
            (if (fixnum? tmp.1)
                (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                (error 2))))
      (error 42))
   "checking lambda with wrong arg arity")

  (check-equal?
   (implement-safe-call '(module
                             (define |+.1|
                               (lambda (tmp.1 tmp.2)
                                 (if (fixnum? tmp.1)
                                     (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                                     (error 2))))
                           (call (lambda (tmp.1 tmp.2) (call |+.1| tmp.1 tmp.2)) 1 )))
   '(module
        (define |+.1|
          (lambda (tmp.1 tmp.2)
            (if (fixnum? tmp.1)
                (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                (error 2))))
      (error 42))
   "checking lambda with wrong arity")
  (check-equal?
   (implement-safe-call '(module
                             (define |+.1|
                               (lambda (tmp.1 tmp.2)
                                 (if (fixnum? tmp.1)
                                     (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                                     (error 2))))
                           (define |+.2|
                             (lambda (tmp.3 tmp.4) (call |+.1| tmp.3 tmp.4)))
                           (call |+.2| 1 2)))
   '(module
        (define |+.1|
          (lambda (tmp.1 tmp.2)
            (if (fixnum? tmp.1)
                (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                (error 2))))
      (define |+.2|
        (lambda (tmp.3 tmp.4) (unsafe-procedure-call |+.1| tmp.3 tmp.4)))
      (unsafe-procedure-call |+.2| 1 2))
   "Check functions that depend on each other")
  (check-equal?
   (implement-safe-call '(module
                             (define |+.2|
                               (lambda (tmp.3 tmp.4) (call |+.1| tmp.3 tmp.4)))
                           (define |+.1|
                             (lambda (tmp.1 tmp.2)
                               (if (fixnum? tmp.1)
                                   (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                                   (error 2))))

                           (call |+.2| 1 2)))
   '(module
        (define |+.2|
          (lambda (tmp.3 tmp.4) (unsafe-procedure-call |+.1| tmp.3 tmp.4)))
      (define |+.1|
        (lambda (tmp.1 tmp.2)
          (if (fixnum? tmp.1)
              (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
              (error 2))))
      (unsafe-procedure-call |+.2| 1 2))
   "Check out of order function dependecy"))
