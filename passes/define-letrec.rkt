#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9
  rackunit)

(provide define->letrec)

;; exprs-unsafe-lang-v9 -> just-exprs-lang-v9
;; compiles p to Just-exprs-lang v9 by  transforming all top-level bindings into
;; local bindings
(define/contract (define->letrec p)
  (-> exprs-unsafe-lang-v9? just-exprs-lang-v9?)

  ;; func is `(define ,label (lambda (,alocs ...) ,value))
  ;; interp. a function definition
  ;;
  ;; exprs-unsafe-lang-v9 -> just-exprs-lang-v9
  ;; destructure the funciton definition to for letrec bindings
  (define (transform-funcs->let f)
    (match f
      [`(define ,label (lambda (,alocs ...) ,value))
       `(,label (lambda ,alocs ,value))]))

  ;; exprs-unsafe-lang-v9 -> just-exprs-lang-v9
  ;; transform the function definitions into letrec bindings in value
  ;; if there are no funciton definitions don't do a letrec
  (define (transform-funcs->letrec funcs v)
    (cond
      [(empty? funcs) v]
      [else
       `(letrec ,funcs ,v)]))

  (match p
    [`(module ,funcs ... ,value)
     (define funcs^ (for/fold ([acc '()])
                              ([f funcs])
                      (cons (transform-funcs->let f) acc)))
     `(module ,(transform-funcs->letrec (reverse funcs^) value))]))

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
   "Check no functions")

  )
