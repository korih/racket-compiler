#lang racket

(require
  "common.rkt"
  cpsc411/compiler-lib
  cpsc411/langs/v9
  rackunit)

(provide uncover-free)

;; lam-opticon-lang-v9 -> lam-free-lang-v9
;; compiles p to Lam-free-lang v9 by explicitly annotate procedures with their
;; free variable sets.
(define/contract (uncover-free p)
  (-> lam-opticon-lang-v9? lam-free-lang-v9?)


  ;; (Listof lam-opticon-lang-v9.value) -> (Listof lam-free-lang-v9.value) (Listof alocs)
  ;; helper compiles the list of values in lam-opticon-lang-v9 to lam-free-lang-v9
  (define (traverse-values-list vs env)
    (define-values (reversed-vs^ all-free-vars)
      (for/foldr ([acc '()]
                  [free-vars-acc '()])
        ([v vs])
        (define-values (v^ val-free-vars) (uncover-free-value v env))
        (values (cons v^ acc)
                (set-union val-free-vars free-vars-acc))))
    (values reversed-vs^ all-free-vars))

  ;; (Listof lam-opticon-lang-v9.effect) -> (Listof lam-free-lang-v9.effect)
  ;; helper compiles the list of values in lam-opticon-lang-v9 to lam-free-lang-v9
  (define (traverse-effects-list effects env)
    (define-values (reversed-es^ all-free-vars)
      (for/foldr ([acc '()]
                  [free-vars-acc '()])
        ([e effects])
        (define-values (e^ effect-free-vars) (uncover-free-effect e env))
        (values (cons e^ acc)
                (set-union effect-free-vars free-vars-acc))))
    (values reversed-es^ all-free-vars))


  ;; (Listof alocs) (Listof lam-opticon-lang-v9.value) -> (Listof aloc lam-free-lang-v9.value)
  ;; helper compiles the letrec in lam-opticon-lang-v9 to lam-free-lang-v9
  ;; TODO: unbound var inside of value in aloc -> vs matching
  (define (traverse-letrec-bindings alocs vs env)
    (define-values (bindings free-variables)
      (for/foldr ([binding-acc '()]
                  [free-vars-acc '()])
        ([aloc alocs]
         [val vs])
        (match val
          [`(lambda (,args ...) ,body)
           (define env^ (extend-env* env args args))
           (define-values (body^ free-vars) (uncover-free-value body env^))
           (define info (info-set '() 'free free-vars))
           (values (cons `(,aloc (lambda ,info ,args ,body^)) binding-acc)
                   (append free-vars free-vars-acc))])))
    (values bindings free-variables))

  ;; (Listof alocs) (Listof lam-opticon-lang-v9.value) -> (Listof aloc lam-free-lang-v9.value)
  ;; helper compiles the list of values in lam-opticon-lang-v9 to lam-free-lang-v9
  ;; TODO: unbound var inside of value in aloc -> vs matching
  (define (traverse-bindings-list alocs vs env)
    (define-values (bindings free-variables)
      (for/foldr ([binding-acc '()]
                  [free-vars-acc '()])
        ([aloc alocs]
         [val vs])
        (define-values (val^ free-vars) (uncover-free-value val env))
        (values (cons `(,aloc ,val^) binding-acc)
                (set-union free-vars free-vars-acc))))
    (values bindings free-variables))

  ;; lam-opticon-lang-v9.value (Envof alocs)  -> lam-free-lang-v9.value (Listof alocs)
  ;; compiles the values in lam-opticon-lang-v9 to lam-free-lang-v9
  (define (uncover-free-value v env)
    (match v
      [`(begin ,effects ... ,value) (define-values (effects^ free-var-effects) (traverse-effects-list effects env))
                                    (define-values (value^ free-vars-values) (uncover-free-value value env))
                                    (values `(begin ,@effects^ ,value^) (append free-var-effects free-vars-values))]
      [`(if ,v1 ,v2 ,v3) (define-values (v1^ free-vars1) (uncover-free-value v1 env))
                         (define-values (v2^ free-vars2) (uncover-free-value v2 env))
                         (define-values (v3^ free-vars3) (uncover-free-value v3 env))
                         (values `(if ,v1^ ,v2^ ,v3^) (append free-vars1 free-vars2 free-vars3))]
      [`(let ([,alocs ,vs] ...) ,body) (define-values (bindings free-var-binding) (traverse-bindings-list alocs vs env))
                                       (define env^ (extend-env* env alocs vs))
                                       (define-values (body^ free-vars) (uncover-free-value body env^))
                                       (values `(let ,bindings ,body^) (append free-vars free-var-binding))]
      [`(letrec ([,alocs ,vs] ...) ,body) (define-values (bindings free-var-binding) (traverse-letrec-bindings alocs vs env))
                                          (define-values (body^ free-vars) (uncover-free-value body env))
                                          (values `(letrec ,bindings  ,body^)
                                                  (remove* alocs (append free-var-binding free-vars) ))]
      [`(unsafe-procedure-call ,vs ...) (define-values (vs^ free-vars) (traverse-values-list vs env))
                                        (values `(unsafe-procedure-call ,@vs^) free-vars)]
      [`(,primops ,vs ...) (define-values (vs^ free-vars) (traverse-values-list vs env))
                           (values `(,primops ,@vs^) free-vars)]
      [triv (define free-vars (cond
                                [(assoc v env) '()]
                                [(aloc? v) `(,triv)]
                                [else `()]))
            (values triv free-vars)]))


  ;; lam-opticon-lang-v9.value -> lam-free-lang-v9.value
  ;; compiles the values in lam-opticon-lang-v9 to lam-free-lang-v9
  (define (uncover-free-effect e env)
    (match e
      [`(,primops ,vs ...) (define-values (vs^ free-vars) (traverse-values-list vs env))
                           (values `(,primops ,@vs^) free-vars)]
      [`(begin ,effects ...) (define-values (effects^ free-vars) (traverse-effects-list effects env))
                             (values `(begin ,@effects^) free-vars)]))

  (match p
    [`(module ,value)
     (define-values (value^ _) (uncover-free-value value empty-env))
     `(module ,value^)]))

(module+ test
  (check-equal?
   (uncover-free
    `(module
         (letrec ([x.1 (lambda () (unsafe-procedure-call x.1))])
           x.1)))
   '(module
        (letrec ((x.1 (lambda ((free (x.1))) () (unsafe-procedure-call x.1)))) x.1))
   "Book test: basic unsafe-procedure-call")
  (check-equal?
   (uncover-free
    `(module
         (letrec ([f.1 (lambda ()
                         (letrec ([x.1 (lambda () (unsafe-procedure-call x.1))])
                           x.1))])
           f.1)))
   '(module
        (letrec ((f.1
                  (lambda ((free ()))
                    ()
                    (letrec ((x.1
                              (lambda ((free (x.1)))
                                ()
                                (unsafe-procedure-call x.1))))
                      x.1))))
          f.1))
   "Book test: unsafe call with free")

  (check-equal?
   (uncover-free
    `(module
         (letrec ([f.1 (lambda ()
                         (letrec ([x.1 (lambda () (unsafe-procedure-call x.1 tmp.1))])
                           x.1))])
           f.1)))
   '(module
        (letrec ((f.1
                  (lambda ((free (tmp.1)))
                    ()
                    (letrec ((x.1
                              (lambda ((free (tmp.1 x.1)))
                                ()
                                (unsafe-procedure-call x.1 tmp.1))))
                      x.1))))
          f.1))
   "Test with non-bounded variable in child letrec with no binding in letrec")

  (check-equal?
   (uncover-free '(module
                      (letrec ((|+.1|
                                (lambda (tmp.1 tmp.2)
                                  (if (fixnum? tmp.1)
                                      (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                                      (error 2))))
                               (|+.2|
                                (lambda (tmp.3 tmp.4) (unsafe-procedure-call |+.1| tmp.3 tmp.4))))
                        (unsafe-procedure-call |+.2| 1 2))))
   '(module
        (letrec ((|+.1|
                  (lambda ((free ()))
                    (tmp.1 tmp.2)
                    (if (fixnum? tmp.1)
                        (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                        (error 2))))
                 (|+.2|
                  (lambda ((free (|+.1|)))
                    (tmp.3 tmp.4)
                    (unsafe-procedure-call |+.1| tmp.3 tmp.4))))
          (unsafe-procedure-call |+.2| 1 2)))
   "Test with mutually dependent procedures")

  (check-equal?
   (uncover-free '(module
                      (letrec ((|+.1|
                                (lambda (tmp.1 tmp.2)
                                  (if (fixnum? tmp.1)
                                      (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2 tmp.3) (error 2))
                                      (error 2))))
                               (|+.2|
                                (lambda (tmp.3 tmp.4) (unsafe-procedure-call |+.1| tmp.3 tmp.4))))
                        (unsafe-procedure-call |+.2| 1 2))))
   '(module
        (letrec ((|+.1|
                  (lambda ((free (tmp.3)))
                    (tmp.1 tmp.2)
                    (if (fixnum? tmp.1)
                        (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2 tmp.3) (error 2))
                        (error 2))))
                 (|+.2|
                  (lambda ((free (|+.1|)))
                    (tmp.3 tmp.4)
                    (unsafe-procedure-call |+.1| tmp.3 tmp.4))))
          (unsafe-procedure-call |+.2| 1 2)))
   "Test with procedure call with free variables not in lambda arg")

  (check-equal?
   (uncover-free '(module
                      (letrec ((|+.1|
                                (lambda (tmp.1 tmp.2)
                                  (if (fixnum? tmp.1)
                                      (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2 tmp.3) (error 2))
                                      (error 2))))
                               (|+.2|
                                (lambda (tmp.3 tmp.4) (unsafe-procedure-call |+.1| tmp.3 tmp.4))))
                        (unsafe-procedure-call |+.2| 1 2 tmp.3))))
   '(module
        (letrec ((|+.1|
                  (lambda ((free (tmp.3)))
                    (tmp.1 tmp.2)
                    (if (fixnum? tmp.1)
                        (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2 tmp.3) (error 2))
                        (error 2))))
                 (|+.2|
                  (lambda ((free (|+.1|)))
                    (tmp.3 tmp.4)
                    (unsafe-procedure-call |+.1| tmp.3 tmp.4))))
          (unsafe-procedure-call |+.2| 1 2 tmp.3)))
   "Test with body cal with free variable")

  (check-equal?
   (uncover-free '(module
                      (letrec ([a.1
                                (lambda ()
                                  (letrec ([a.2 (lambda ()
                                                  (let ([a.3 (begin
                                                               (eq? 1 2)
                                                               (if (eq? 2 1)
                                                                   tmp.3
                                                                   tmp.4))])
                                                    a.3))])
                                    a.2))])
                        a.1)))
   '(module
        (letrec ((a.1
                  (lambda ((free (tmp.3 tmp.4)))
                    ()
                    (letrec ((a.2
                              (lambda ((free (tmp.3 tmp.4)))
                                ()
                                (let ((a.3
                                       (begin (eq? 1 2) (if (eq? 2 1) tmp.3 tmp.4))))
                                  a.3))))
                      a.2))))
          a.1))
   "Test with super nested letrec with let")

  (check-equal?
   (uncover-free '(module
                      (letrec ((car.60
                                (lambda (tmp.61)
                                  (if (pair? tmp.61) (unsafe-car tmp.61) (error 12))))
                               (|+.64|
                                (lambda (tmp.65 tmp.66)
                                  (if (fixnum? tmp.65)
                                      (if (fixnum? tmp.66) (unsafe-fx+ tmp.65 tmp.66) (error 2))
                                      (error 2))))
                               (eq?.54 (lambda (tmp.55 tmp.56) (eq? tmp.55 tmp.56)))
                               (cdr.62
                                (lambda (tmp.63)
                                  (if (pair? tmp.63) (unsafe-cdr tmp.63) (error 13))))
                               (cons.57 (lambda (tmp.58 tmp.59) (cons tmp.58 tmp.59)))
                               (map.25
                                (lambda (f.26 ls.27)
                                  (if (unsafe-procedure-call eq?.54 empty ls.27)
                                      empty
                                      (unsafe-procedure-call
                                       cons.57
                                       (if (procedure? f.26)
                                           (if (eq? (unsafe-procedure-arity f.26) 1)
                                               (unsafe-procedure-call
                                                f.26
                                                (unsafe-procedure-call car.60 ls.27))
                                               (error 42))
                                           (error 43))
                                       (unsafe-procedure-call
                                        map.25
                                        f.26
                                        (unsafe-procedure-call cdr.62 ls.27)))))))
                        (unsafe-procedure-call
                         map.25
                         (letrec ((lam.7 (lambda (x.28) (unsafe-procedure-call |+.64| 1 x.28))))
                           lam.7)
                         (unsafe-procedure-call
                          cons.57
                          1
                          (unsafe-procedure-call
                           cons.57
                           2
                           (unsafe-procedure-call cons.57 3 empty)))))))
   '(module
        (letrec ((car.60
                  (lambda ((free ()))
                    (tmp.61)
                    (if (pair? tmp.61) (unsafe-car tmp.61) (error 12))))
                 (|+.64|
                  (lambda ((free ()))
                    (tmp.65 tmp.66)
                    (if (fixnum? tmp.65)
                        (if (fixnum? tmp.66) (unsafe-fx+ tmp.65 tmp.66) (error 2))
                        (error 2))))
                 (eq?.54 (lambda ((free ())) (tmp.55 tmp.56) (eq? tmp.55 tmp.56)))
                 (cdr.62
                  (lambda ((free ()))
                    (tmp.63)
                    (if (pair? tmp.63) (unsafe-cdr tmp.63) (error 13))))
                 (cons.57 (lambda ((free ())) (tmp.58 tmp.59) (cons tmp.58 tmp.59)))
                 (map.25
                  (lambda ((free (eq?.54 car.60 cdr.62 map.25 cons.57)))
                    (f.26 ls.27)
                    (if (unsafe-procedure-call eq?.54 empty ls.27)
                        empty
                        (unsafe-procedure-call
                         cons.57
                         (if (procedure? f.26)
                             (if (eq? (unsafe-procedure-arity f.26) 1)
                                 (unsafe-procedure-call
                                  f.26
                                  (unsafe-procedure-call car.60 ls.27))
                                 (error 42))
                             (error 43))
                         (unsafe-procedure-call
                          map.25
                          f.26
                          (unsafe-procedure-call cdr.62 ls.27)))))))
          (unsafe-procedure-call
           map.25
           (letrec ((lam.7
                     (lambda ((free (|+.64|)))
                       (x.28)
                       (unsafe-procedure-call |+.64| 1 x.28))))
             lam.7)
           (unsafe-procedure-call
            cons.57
            1
            (unsafe-procedure-call
             cons.57
             2
             (unsafe-procedure-call cons.57 3 empty))))))
   "More complicated test from dox-lambdas")
  (check-equal?
   (uncover-free
    `(module
         (letrec ([x.1 (lambda () (let ([a.2 (unsafe-procedure-call identify.13 identify.13)]
                                        [a.3 (unsafe-procedure-call identify.13 identify.13)]
                                        [a.4 (unsafe-procedure-call identify.13 identify.13)])
                                    a.4))])
           x.1)))
   '(module
        (letrec ((x.1
                  (lambda ((free (identify.13)))
                    ()
                    (let ((a.2 (unsafe-procedure-call identify.13 identify.13))
                          (a.3 (unsafe-procedure-call identify.13 identify.13))
                          (a.4 (unsafe-procedure-call identify.13 identify.13)))
                      a.4))))
          x.1))
   "Test, let with duplicate bindings"))
