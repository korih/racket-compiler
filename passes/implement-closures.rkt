#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9
  rackunit)

(provide implement-closures)

;; hoisted-lang-v9 -> proc-exposed-lang-v9
;; compiles p to Proc-exposed-lang v9 by implementing closures in terms of the
;; procedure data structure
(define/contract (implement-closures p)
  (-> hoisted-lang-v9? any #;proc-exposed-lang-v9?)


  ;; (Listof hoisted-lang-v9) (Envof aloc to label) -> (Listof proc-exposed-lang-v9)
  ;; helper for traversing through lists of values and compiling them
  (define (traverse-values vs)
    (for/foldr ([vs^ '()])
      ([v vs])
      (define v^ (implement-closure-value v))
      (cons v^ vs^)))

  ;; (Listof hoisted-lang-v9) (Envof aloc to label) -> (Listof proc-exposed-lang-v9)
  ;; helper for traversing through lists of values and compiling them
  (define (traverse-effects vs)
    (for/foldr ([vs^ '()])
      ([v vs])
      (define v^ (implement-closures-effect v))
      (cons v^ vs^)))

  ;; (Listof aloc) (Listof hoisted-lang-v9) (Envof aloc to label)-> (Listof proc-exposed-lang-v9)
  ;; helper for evaluating the values in binding position
  (define (traverse-bindings alocs vs)
    (for/foldr ([binding '()])
      ([aloc alocs]
       [v vs])
      (define v^ (implement-closure-value v))
      (cons `(,aloc ,v^) binding)))

  ;; (Listof hoisted-lang-v9) -> (Listof proc-exposed-lang-v9)
  ;; helper for evaluating the values in (make-closure ,label ,values ...)
  (define (compile-make-closure aloc mcs)
    (match mcs
      [`(make-closure ,label ,arity ,vs ...)
       (define vs^ (for/foldr ([opt-vs '()])
                     ([v vs])
                     (define v^ (implement-closure-value v))
                     (cons v^ opt-vs)))
       (define unsafe-procs (for/fold ([procs '()])
                                      ([index (in-range (length vs^) 0 -1)])
                              (define proc
                                `(unsafe-procedure-set!
                                  ,aloc
                                  ,(sub1 index)
                                  ,(list-ref vs^ (sub1 index))))
                              (cons proc procs)))
       (values `(make-procedure ,label ,arity ,(length vs^))
               unsafe-procs)]))

  ;; func is (define label (lambda (alocs ...) value))
  ;; interp. This is a function definition

  ;; hoisted-lang-v9.func -> proc-exposed-lang-v9.func
  ;; compile a hoisted-lang-b9 func to proc-exposed-lang func
  (define (implement-closure-func f)
    (match f
      [`(define ,label (lambda (,alocs ...) ,body)) (define body^ (implement-closure-value body))
                                                    `(define ,label (lambda ,alocs ,body^))]))

  ;; hoisted-lang-v9.value -> proc-exposed-lang-v9.value
  ;; optimizes a value in hoisted-lang-v9 to proc-exposed-lang-v9
  (define (implement-closure-value v)
    (match v
      [`(cletrec ([,alocs ,closures] ...) ,body)
       (define-values (bindings procs) (for/foldr ([acc '()]
                                                   [proc-acc '()])
                                         ([aloc alocs]
                                          [closure closures])
                                         (define-values (closure^ unsafe-procs) (compile-make-closure aloc closure))
                                         (values (cons `(,aloc ,closure^) acc)
                                                 (append unsafe-procs proc-acc))))
       (define body^ (implement-closure-value body))
       `(let ,bindings (begin ,@procs ,body^))]
      [`(begin ,effects ... ,value) (define effects^ (traverse-effects effects))
                                    (define value^ (implement-closure-value value))
                                    `(begin ,effects^ ,value^)]
      [`(if ,v1 ,v2 ,v3) (define v1^ (implement-closure-value v1))
                         (define v2^ (implement-closure-value v2))
                         (define v3^ (implement-closure-value v3))
                         `(if ,v1^ ,v2^ ,v3^)]
      [`(let ([,alocs ,vs] ...) ,body) (define bindings (traverse-bindings alocs vs))
                                       (define body^ (implement-closure-value body))
                                       `(let ,bindings ,body^)]
      [`(call ,vs ...) (define vs^ (traverse-values vs))
                       `(call ,@vs^)]
      [`(closure-call ,c ,vs ...)
       (define c^ (implement-closure-value c))
       (define vs^ (traverse-values vs))
       `(call (unsafe-procedure-label ,c^) ,@vs^)]
      [`(closure-ref ,v1 ,v2) (define v1^ (implement-closure-value v1))
                              (define v2^ (implement-closure-value v2))
                              `(unsafe-procedure-ref ,v1^ ,v2^)]
      [`(,primops ,vs ...) (define vs^ (traverse-values vs))
                           `(,primops ,@vs^)]
      [triv triv]))

  ;; hoisted-lang-v9.effect -> proc-exposed-lang-v9.effect
  ;; compiles the effect in hoisted-lang-v9 to proc-exposed-lang-v9
  (define (implement-closures-effect e)
    (match e
      [`(,primops ,vs ...) (define vs^ (traverse-values vs))
                           `(,primops ,@vs^)]
      [`(begin ,effects ...)  (define effects^ (traverse-effects effects))
                              `(begin ,effects^)]))
  (match p
    [`(module ,funcs ... ,v) (define funcs^ (for/foldr ([closed-funcs '()])
                                              ([func funcs])
                                              (cons (implement-closure-func func) closed-funcs)))
                             (define v^ (implement-closure-value v))
                             `(module ,@funcs^ ,v^)]))

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
