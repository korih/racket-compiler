#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9)

(provide implement-safe-call)

;; exprs-unsafe-data-lang-v9 -> exprs-unsafe-lang-v9
;; interp. Implement call as an unsafe procedure call with dynamic checks.
(define/contract (implement-safe-call p)
  (-> exprs-unsafe-data-lang-v9? exprs-unsafe-lang-v9?)

  ;; func is `(define ,label (lambda (,alocs ...) ,value))
  ;; interp. a function definition

  (define bad-arrity-error `(error 42))
  (define bad-proc-error `(error 43))

  ;; map of Label -> arity
  (define func-map (make-hash))

  ;; exprs.safe-data-lang-v9.func -> exprs-unsafe-data-lang-v9.func
  ;; produce exprs-unsafe-data-lang-v9 of function definitions
  (define (implement-safe-primops-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,value))
       (hash-set! func-map label (length alocs))
       `(define ,label (lambda (,@alocs) ,(implement-safe-primops-value value)))]))

  ;; exprs.safe-data-lang-v9.func -> MAP
  ;; produce exprs-unsafe-data-lang-v9 of function definitions
  (define (implement-safe-primops-func-scan func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,value))
       (hash-set! func-map label (length alocs))]))

  ;; exprs-unsafe-data-lang-v9.value -> exprs-unsafe-data-lang-v9.value
  ;; produce unsafe values from exprs-unsafe-data-lang-v9
  (define (implement-safe-primops-value value)
    (match value
      [`(call ,p ,args ...)
       (define args^ (map implement-safe-primops-value args))
       (define p^ (implement-safe-primops-value p))
       (check-call-triv p^ args^)]

      [`(let ([,alocs ,vs] ...) ,v)
       (define bindings
         (map (lambda (aloc val)
                (list aloc (implement-safe-primops-value val)))
              alocs vs))
       `(let ,bindings ,(implement-safe-primops-value v))]
      [`(if ,v1 ,v2 ,v3)
       `(if ,(implement-safe-primops-value v1)
            ,(implement-safe-primops-value v2)
            ,(implement-safe-primops-value v3))]
      [`(begin ,effects ... ,value)
       (define effects^ (map implement-safe-primops-effect effects))
       (define value^ (implement-safe-primops-value value))
       `(begin ,@effects^ ,value^)]
      [triv (implement-safe-primops-triv triv)]))

  ;; exprs-unsafe-data-lang-v9.effect -> exprs-unsafe-data-lang-v9.effect
  ;; interp. produces unsafe effects from exprs-unsafe-data-lang-v9
  (define (implement-safe-primops-effect e)
    (match e
      [`(begin ,effects ... ,effect) (define effects^ (map implement-safe-primops-effect effects))
                                     (define effect^ (implement-safe-primops-effect effect))
                                     `(begin ,@effects^ ,effect^)]
      [`(,primop ,vs ...) (define vs^ (map implement-safe-primops-value vs))
                          `(,primop ,@vs^)]))

  ;; exprs-unsafe-data-lang-v9.triv -> exprs-unsafe-data-lang-v9.triv
  ;; GLOBAL VARIABLE: new-funcs maps prim-f expressions to (Listof Label Safety-Check-Funtion)
  ;; interp. produce unsafe triv from exprs-unsafe-data-lang-v9.triv
  (define (implement-safe-primops-triv triv)
    (match triv
      [`(lambda (,alocs ...) ,value)
       `(lambda ,alocs ,(implement-safe-primops-value value))]
      ;; Wildcard collapse case used because they are terminal cases with no transformation
      [_ triv]))

  ;; exprs-unsafe-data-lang-v9.triv -> exprs-unsafe-lang-v9.triv
  (define (check-call-triv t args)
    (match t
      [`(lambda (,alocs ...) ,value) (define value^ (implement-safe-primops-value value))
                                     (define lambda^ `(lambda ,alocs ,value^))
                                     ;; if the length is not equal we know we have a bad arity
                                     (if (equal? (length alocs) (length args))
                                         `(unsafe-procedure-call ,lambda^ ,@args)
                                         bad-arrity-error)]

      [label #:when (hash-has-key? func-map label) (define arity (hash-ref func-map label))
             (if (equal? (length args) arity)
                 `(unsafe-procedure-call ,label ,@args)
                 bad-arrity-error)]

      ;; WILDCARD: collapse case because they all result in the same transformation
      [_ (define (procedure-check tmp) `(if (procedure? ,tmp)
                                            (if (eq? (unsafe-procedure-arity ,tmp) ,(length args))
                                                (unsafe-procedure-call ,tmp ,@args)
                                                ,bad-arrity-error)
                                            ,bad-proc-error))
         (define fresh-var (fresh 'call-tmp))
         (if (aloc? t)
             (procedure-check t)
             `(let ((,fresh-var ,t))
                ,(procedure-check fresh-var)))]))

  (match p
    [`(module ,funcs ... ,value)
     ;; top level pass to get function names
     (map implement-safe-primops-func-scan funcs)
     (define funcs^ (map implement-safe-primops-func funcs))
     (define value^ (implement-safe-primops-value value))
     `(module ,@funcs^ ,value^)]))

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
