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

  ;; TODO: make osme table for this
  (define bad-arrity-error `(error 42))
  (define bad-proc-error `(error 43))

  ;; map of Label -> arity
  (define func-map (make-hash))

  ;; exprs.safe-data-lang-v8.func -> exprs-unsafe-data-lang-v8.func
  ;; produce exprs-unsafe-data-lang-v8 of function definitions
  (define (implement-safe-primops-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,value))
       ;; TODO: figure out how to let the funcs call each other
       (hash-set! func-map label (length alocs))
       `(define ,label (lambda (,@alocs) ,(implement-safe-primops-value value)))]))

  ;; exprs-unsafe-data-lang-v9.value -> exprs-unsafe-data-lang-v8.value
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
       `(let (,@bindings) ,(implement-safe-primops-value v))]
      [`(if ,v1 ,v2 ,v3)
       `(if ,(implement-safe-primops-value v1)
            ,(implement-safe-primops-value v2)
            ,(implement-safe-primops-value v3))]
      [`(begin ,effects ... ,value)
       (define effects^ (map implement-safe-primops-effect effects))
       (define value^ (implement-safe-primops-value value))
       `(begin ,@effects^ ,value^)]
      [triv (implement-safe-primops-triv triv)]))

  ;; exprs-unsafe-data-lang-v9.effect -> exprs-unsafe-data-lang-v8.effect
  ;; interp. produces unsafe effects from exprs-unsafe-data-lang-v9
  (define (implement-safe-primops-effect e)
    (match e
      [`(begin ,effects ... ,effect) (define effects^ (map implement-safe-primops-effect effects))
                                     (define effect^ (implement-safe-primops-effect effect))
                                     `(begin ,@effects^ ,effect^)]
      [`(,primop ,vs ...) (define vs^ (map implement-safe-primops-value vs))
                          `(,primop ,@vs^)]))

  ;; exprs-unsafe-data-lang-v9.triv -> exprs-unsafe-data-lang-v8.triv
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
                                     (if (equal? (length alocs) (length args))
                                         `(unsafe-procedure-call ,lambda^ ,@alocs)
                                         bad-arrity-error)]

      [label #:when (hash-has-key? func-map label) (define arity (hash-ref func-map label))
             (if (equal? (length args) arity)
                 `(unsafe-procedure-call ,label ,@args)
                 bad-arrity-error)]
      ;; WILDCARD: collapse case because they all result in the same transformation
      [_ (define tmp (fresh))
         `(let ((,tmp ,(first args)))
            (if (procedure? ,tmp)
                (if (eq? (unsafe-procedure-arity ,tmp) ,(length args))
                    (unsafe-procedure-call ,tmp)
                    ,bad-arrity-error)
                ,bad-proc-error))]))

  (match p
    [`(module ,funcs ... ,value)
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
                   (unsafe-procedure-call |+.1| 1 2)))
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
                    2)))

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
                   (let ((tmp.4 1))
                     (if (procedure? tmp.4)
                         (if (eq? (unsafe-procedure-arity tmp.4) 0)
                             (unsafe-procedure-call tmp.4)
                             (error 42))
                         (error 43)))))
  )
