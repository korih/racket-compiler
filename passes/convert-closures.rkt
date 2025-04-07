#lang racket

(require
  "common.rkt"
  cpsc411/compiler-lib
  cpsc411/langs/v9
  rackunit)

(provide convert-closures)

;; lam-free-lang-v9 -> closure-lang-v9
;; compiles p to Closure-lang v9 by performing closure conversion, converting
;; all procedures into explicit closures
(define/contract (convert-closures p)
  (-> lam-free-lang-v9? closure-lang-v9?)

  ;; lam-free-lang-v9.value -> closure-lang-v9.value
  ;; performs closure conversion on the value
  (define (convert-closures/value value)
    (match value
      [`(unsafe-procedure-call ,fun ,args ...)
       `(closure-call ,fun ,fun ,@(map convert-closures/value args))]
      [`(letrec ([,fun-names (lambda ,infos (,alocs ...) ,vs)] ...) ,value)
       (define-values (letrec-clauses cletrec-clauses)
         (map2 (lambda (name-aloc info loa lambda-body)
                 (define fun-label (fresh-label))
                 (define this (fresh))
                 (define ys (info-ref info 'free))
                 (define let-closure-vars (for/list ([y ys]
                                                     [i (in-naturals)])
                                            `(,y (closure-ref ,this ,i))))

                 (values
                  `(,fun-label (lambda ,(cons this loa)
                                 (let ,let-closure-vars
                                   ,(convert-closures/value lambda-body))))
                  `(,name-aloc (make-closure ,fun-label ,(length loa) ,@ys))))
               fun-names
               infos
               alocs
               vs))

       `(letrec ,letrec-clauses
          (cletrec ,cletrec-clauses
                   ,(convert-closures/value value)))]
      [`(let ([,alocs ,vs] ...) ,body)
       `(let ,(for/list ([aloc alocs]
                         [v vs])
                `(,aloc ,(convert-closures/value v)))
          ,(convert-closures/value body))]
      [`(if ,operand ,conseq ,alt)
       `(if ,(convert-closures/value operand)
            ,(convert-closures/value conseq)
            ,(convert-closures/value alt))]
      [`(begin ,fx ... ,value)
       `(begin ,@(map convert-closures/effect fx)
          ,(convert-closures/value value))]
      [`(,primop ,args ...) #:when (unsafe-primop? primop) `(,primop ,@(map convert-closures/value args))]
      [triv triv]))

    ;; lam-free-lang-v9.effect -> closure-lang-v9.effect
    ;; converts closure in effect statements
    (define (convert-closures/effect effect)
      (match effect
        [`(,primop ,values ...) `(,primop ,@(map convert-closures/value values))]
        [`(begin ,fx ...) `(begin ,@(map convert-closures/effect fx))]))

  (match p
    [`(module ,value) `(module ,(convert-closures/value value))]))

(module+ test
  (check-equal? (interp-closure-lang-v9 (convert-closures '(module
                                                               (letrec ([f.3 (lambda ((free ())) (x.1 y.2) (unsafe-fx+ x.1 y.2))])
                                                                 (unsafe-procedure-call f.3 2 3)))))
                (interp-closure-lang-v9 '(module
                                             (letrec ((L.f.3.7 (lambda (c.4 x.1 y.2) (let () (unsafe-fx+ x.1 y.2)))))
                                               (cletrec ((f.3 (make-closure L.f.3.7 2))) (closure-call f.3 f.3 2 3))))))
  (check-equal? (interp-closure-lang-v9 (convert-closures '(module
                                                               (letrec ([g.4 (lambda ((free ())) (n.5)
                                                                               (if (unsafe-fx< n.5 0) #f #t))])
                                                                 (unsafe-procedure-call g.4 5)))))
                (interp-closure-lang-v9 '(module
                                             (letrec ((L.g.4.7 (lambda (c.4 n.5) (let () (if (unsafe-fx< n.5 0) #f #t)))))
                                               (cletrec ((g.4 (make-closure L.g.4.7 1))) (closure-call g.4 g.4 5))))))
  (let ([compiled-program (convert-closures '(module
                                                 (letrec ([add.1 (lambda ((free ())) (x.1 y.2) (unsafe-fx+ x.1 y.2))]
                                                          [mul.2 (lambda ((free ())) (x.3 y.4) (unsafe-fx* x.3 y.4))])
                                                   (unsafe-procedure-call mul.2 (unsafe-procedure-call add.1 1 2) 3))))])
    (displayln (format "compiled program: ~a" compiled-program))
    (check-equal? (interp-closure-lang-v9 compiled-program)
                  (interp-closure-lang-v9 '(module
                                               (letrec ((L.add.1.7 (lambda (c.4 x.1 y.2) (let () (unsafe-fx+ x.1 y.2))))
                                                        (L.mul.2.8 (lambda (c.5 x.3 y.4) (let () (unsafe-fx* x.3 y.4)))))
                                                 (cletrec
                                                  ((add.1 (make-closure L.add.1.7 2)) (mul.2 (make-closure L.mul.2.8 2)))
                                                  (closure-call mul.2 mul.2 (closure-call add.1 add.1 1 2) 3)))))))
  (check-equal? (interp-closure-lang-v9 (convert-closures '(module
                                                               (let ([z.1 10])
                                                                 (letrec ([add-z.2 (lambda ((free (z.1))) (x.3) (unsafe-fx+ x.3 z.1))])
                                                                   (unsafe-procedure-call add-z.2 5))))))
                (interp-closure-lang-v9 '(module
                                             (let ((z.1 10))
                                               (letrec ((L.add-z.2.7
                                                         (lambda (c.4 x.3)
                                                           (let ((z.1 (closure-ref c.4 0))) (unsafe-fx+ x.3 z.1)))))
                                                 (cletrec
                                                  ((add-z.2 (make-closure L.add-z.2.7 1 z.1)))
                                                  (closure-call add-z.2 add-z.2 5)))))))
  (check-equal? (interp-closure-lang-v9 (convert-closures '(module
                                                               (letrec ([f.1 (lambda ((free ())) (x.2)
                                                                               (begin
                                                                                 (unsafe-vector-set! (unsafe-make-vector 1) 0 x.2)
                                                                                 (unsafe-fx+ x.2 1)))])
                                                                 (unsafe-procedure-call f.1 10)))))
                (interp-closure-lang-v9 '(module
                                             (letrec ((L.f.1.7
                                                       (lambda (c.4 x.2)
                                                         (let ()
                                                           (begin
                                                             (unsafe-vector-set! (unsafe-make-vector 1) 0 x.2)
                                                             (unsafe-fx+ x.2 1))))))
                                               (cletrec ((f.1 (make-closure L.f.1.7 1))) (closure-call f.1 f.1 10)))))))
