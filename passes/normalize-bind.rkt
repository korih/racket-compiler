#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8
  rackunit)

(provide normalize-bind)

;; imp-mf-lang-v8 -> proc-imp-cmf-lang-v8
;; compiles p to to to Proc-imp-cmf-lang v8 by pushing set! under begin so that
;; the right-hand-side of each set! is simple value-producing operation
(define/contract (normalize-bind p)
  (-> imp-mf-lang-v8? proc-imp-cmf-lang-v8?)

  ;; func is `(define ,label (lambda (,alocs ...) ,tail))
  ;; interp. a function definition

  ;; func -> func
  (define (normalize-bind-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,tail))
       `(define ,label (lambda (,@alocs) ,(normalize-bind-tail tail)))]))

  ;; imp-mf-lang-v8.tail -> proc-imp-cmf-lang-v8.tail
  (define (normalize-bind-tail tail)
    (match tail
      [`(begin ,e ... ,t)
       `(begin ,@(map normalize-bind-effect e) ,(normalize-bind-tail t))]
      [`(if ,p ,t1 ,t2)
       `(if ,(normalize-bind-pred p) ,(normalize-bind-tail t1) ,(normalize-bind-tail t2))]
      [`(call ,triv ,opand ...) tail]
      [v (normalize-bind-value v (lambda (v) v))]))

  ;; imp-mf-lang-v8.effect -> proc-imp-cmf-lang-v8.effect
  (define (normalize-bind-effect effect)
    (match effect
      [`(set! ,aloc ,v)
       (normalize-bind-value v (lambda (simple-v)
                                 `(set! ,aloc ,simple-v)))]
      [`(mset! ,aloc ,opand ,v)
       (normalize-bind-value v
                             (lambda (triv)
                               (if (or (symbol? triv) (number? triv))
                                   `(mset! ,aloc ,opand ,triv)
                                   (let ([tmp (fresh 'tmp)])
                                     `(begin
                                        (set! ,tmp ,triv)
                                        (mset! ,aloc ,opand ,tmp))))))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(normalize-bind-pred pred) ,(normalize-bind-effect e1) ,(normalize-bind-effect e2))]
      [`(begin ,e ...)
       `(begin ,@(map normalize-bind-effect e))]))

  ;; imp-mf-lang-v8.value (imp-mf-lang-v8.value -> proc-imp-cmf-lang-v8.effect) -> proc-imp-cmf-lang-v8.value
  (define (normalize-bind-value value cont)
    (match value
      [`(begin ,e ... ,v)
       `(begin ,@(map normalize-bind-effect e) ,(normalize-bind-value v cont))]
      [`(if ,p ,v1 ,v2)
       `(if ,(normalize-bind-pred p)
            ,(normalize-bind-value v1 cont)
            ,(normalize-bind-value v2 cont))]
      ;; Using wildcard collapse case because in all other cases, the
      ;; expression is already in proc-imp-cmf-lang-v8.value form
      [_ (cont value)]))

  ;; imp-mf-lang-v8.pred -> proc-imp-cmf-lang-v8.pred
  (define (normalize-bind-pred pred)
    (match pred
      ['(true) pred]
      ['(false) pred]
      [`(not ,p) `(not ,(normalize-bind-pred p))]
      [`(begin ,e ... ,p)
       `(begin ,@(map normalize-bind-effect e) ,(normalize-bind-pred p))]
      [`(if ,p1 ,p2 ,p3)
       `(if ,(normalize-bind-pred p1) ,(normalize-bind-pred p2) ,(normalize-bind-pred p3))]
      [`(,relop ,op1 ,op2) pred]))

  (match p
    [`(module ,funcs ... ,tail)
     `(module ,@(map normalize-bind-func funcs) ,(normalize-bind-tail tail))]))

(module+ test
  (check-equal? (normalize-bind '(module
                                     (define L.f.1
                                       (lambda (x.1 x.2)
                                         (begin
                                           (begin
                                             (set! tmp.38 (begin (set! tmp.39 (+ 10 6)) (alloc tmp.39)))
                                             (begin
                                               (set! tmp.40 (call L.g.1))
                                               (mset! tmp.38 tmp.40 (if (true) x.1 x.2))))
                                           (begin
                                             (set! tmp.41 (begin (set! tmp.42 (+ 10 6)) (alloc tmp.42)))
                                             (begin (set! tmp.43 (bitwise-and 8 8)) (mref tmp.41 tmp.43))))))
                                   (define L.g.1 (lambda () 8))
                                   (call L.f.1 1 2)))
                '(module
                     (define L.f.1
                       (lambda (x.1 x.2)
                         (begin
                           (begin
                             (begin (set! tmp.39 (+ 10 6)) (set! tmp.38 (alloc tmp.39)))
                             (begin
                               (set! tmp.40 (call L.g.1))
                               (if (true) (mset! tmp.38 tmp.40 x.1) (mset! tmp.38 tmp.40 x.2))))
                           (begin
                             (begin (set! tmp.42 (+ 10 6)) (set! tmp.41 (alloc tmp.42)))
                             (begin (set! tmp.43 (bitwise-and 8 8)) (mref tmp.41 tmp.43))))))
                   (define L.g.1 (lambda () 8))
                   (call L.f.1 1 2)))
  (check-equal? (normalize-bind '(module
                                     (define L.g.1 (lambda (x.1) x.1))
                                   (define L.f.1 (lambda (x.1) (begin
                                                                 (set! x.2 (begin
                                                                             (set! x.1 (+ x.1 10))
                                                                             (set! x.3 (call L.g.1 x.1))
                                                                             (* x.1 x.3)))
                                                                 (set! x.3 (if (begin
                                                                                 (set! x.4 (begin
                                                                                             (set! x.5 (call L.g.1 1))
                                                                                             (+ x.5 100)))
                                                                                 (> x.4 x.2))
                                                                               (+ x.2 100)
                                                                               (- x.2 100)))
                                                                 (+ x.2 x.3))))
                                   (begin
                                     (set! a.1 (begin
                                                 (set! b.1 (call L.f.1 1))
                                                 (- b.1 b.1)))
                                     a.1)))
                '(module
                     (define L.g.1 (lambda (x.1) x.1))
                   (define L.f.1
                     (lambda (x.1)
                       (begin
                         (begin
                           (set! x.1 (+ x.1 10))
                           (set! x.3 (call L.g.1 x.1))
                           (set! x.2 (* x.1 x.3)))
                         (if (begin
                               (begin (set! x.5 (call L.g.1 1)) (set! x.4 (+ x.5 100)))
                               (> x.4 x.2))
                             (set! x.3 (+ x.2 100))
                             (set! x.3 (- x.2 100)))
                         (+ x.2 x.3))))
                   (begin (begin (set! b.1 (call L.f.1 1)) (set! a.1 (- b.1 b.1))) a.1)))
  (check-equal? (normalize-bind '(module
                                     (begin
                                       (set! x.1 (if (true)
                                                     (begin
                                                       (set! x.2 10)
                                                       (set! x.3 100)
                                                       (* x.2 x.3))
                                                     (begin
                                                       (set! x.4 10)
                                                       (+ x.4 1))))
                                       x.1)))
                '(module
                     (begin
                       (if (true)
                           (begin (set! x.2 10) (set! x.3 100) (set! x.1 (* x.2 x.3)))
                           (begin (set! x.4 10) (set! x.1 (+ x.4 1))))
                       x.1)))
  (check-equal? (normalize-bind '(module
                                     (define L.f.1 (lambda (x.1) (begin
                                                                   (set! x.2 (begin
                                                                               (set! x.1 (+ x.1 10))
                                                                               (* x.1 100)))
                                                                   (set! x.3 (if (begin
                                                                                   (set! x.4 (begin
                                                                                               (set! x.5 x.1)
                                                                                               (+ x.5 100)))
                                                                                   (> x.4 x.2))
                                                                                 (+ x.2 100)
                                                                                 (* x.2 100)))
                                                                   (+ x.2 x.3))))
                                   (call L.f.1 10)))
                '(module
                     (define L.f.1
                       (lambda (x.1)
                         (begin
                           (begin (set! x.1 (+ x.1 10)) (set! x.2 (* x.1 100)))
                           (if (begin (begin (set! x.5 x.1) (set! x.4 (+ x.5 100))) (> x.4 x.2))
                               (set! x.3 (+ x.2 100))
                               (set! x.3 (* x.2 100)))
                           (+ x.2 x.3))))
                   (call L.f.1 10)))
  (check-equal? (normalize-bind '(module
                                     (begin
                                       (set! x.1 (if (begin
                                                       (set! x.2 (if (begin
                                                                       (set! x.3 (begin
                                                                                   (set! x.4 10)
                                                                                   (* x.4 100)))
                                                                       (set! x.5 (begin
                                                                                   (set! x.6 100)
                                                                                   (+ x.6 100)))
                                                                       (> x.3 x.5))
                                                                     (begin
                                                                       (set! x.7 (if (not (true))
                                                                                     10
                                                                                     1000))
                                                                       (+ x.7 10))
                                                                     (begin
                                                                       (set! x.8 (begin
                                                                                   (set! x.9 100)
                                                                                   (+ x.9 100)))
                                                                       (* x.8 100))))
                                                       (> x.2 100))
                                                     100
                                                     10))
                                       (+ x.1 100))))
                '(module
                     (begin
                       (if (begin
                             (if (begin
                                   (begin (set! x.4 10) (set! x.3 (* x.4 100)))
                                   (begin (set! x.6 100) (set! x.5 (+ x.6 100)))
                                   (> x.3 x.5))
                                 (begin
                                   (if (not (true)) (set! x.7 10) (set! x.7 1000))
                                   (set! x.2 (+ x.7 10)))
                                 (begin
                                   (begin (set! x.9 100) (set! x.8 (+ x.9 100)))
                                   (set! x.2 (* x.8 100))))
                             (> x.2 100))
                           (set! x.1 100)
                           (set! x.1 10))
                       (+ x.1 100))))
  (check-equal? (normalize-bind '(module (begin (set! x.6 (+ 2 3)) (set! x.7 (+ x.6 x.6)) (begin (set! y.2 5) x.6))))
                '(module
                     (begin (set! x.6 (+ 2 3)) (set! x.7 (+ x.6 x.6)) (begin (set! y.2 5) x.6))))
  (check-equal? (normalize-bind '(module (begin (set! x.1 (if (true) 1 2)) x.1)))
                '(module (begin (if (true) (set! x.1 1) (set! x.1 2)) x.1)))
  (check-equal? (normalize-bind '(module (if (begin
                                               (set! x.1 (if (not (= 1 2))
                                                             x.2
                                                             5))
                                               (set! x.2 (begin
                                                           (if (> x.1 2)
                                                               (set! x.1 1)
                                                               (set! x.1 2))
                                                           5))
                                               (false))
                                             x.1
                                             x.2)))
                '(module
                     (if (begin
                           (if (not (= 1 2)) (set! x.1 x.2) (set! x.1 5))
                           (begin (if (> x.1 2) (set! x.1 1) (set! x.1 2)) (set! x.2 5))
                           (false))
                         x.1
                         x.2)))
  (check-equal? (normalize-bind '(module x.1)) '(module x.1))
  (check-equal? (normalize-bind '(module (begin (set! x.1 1) x.1))) '(module (begin (set! x.1 1) x.1)))
  (check-equal? (normalize-bind '(module (begin (set! x.2 2) (begin (set! x.3 4) x.3))))
                '(module (begin (set! x.2 2) (begin (set! x.3 4) x.3))))
  (check-equal? (normalize-bind '(module (begin (set! x.4 1) (set! x.5 1) (+ x.4 x.5))))
                '(module (begin (set! x.4 1) (set! x.5 1) (+ x.4 x.5))))
  (check-equal? (normalize-bind '(module (begin (set! x.5 (begin (set! x.6 5) -2)) (+ x.5 1))))
                '(module (begin (begin (set! x.6 5) (set! x.5 -2)) (+ x.5 1))))
  (check-equal? (normalize-bind '(module (begin (set! x.0 0) (set! x.1 (begin (set! x.2 2) (+ x.2 1))) x.0)))
                '(module (begin (set! x.0 0) (begin (set! x.2 2) (set! x.1 (+ x.2 1))) x.0)))
  (check-equal? (normalize-bind '(module (begin 0))) '(module (begin 0)))
  (check-equal? (normalize-bind '(module (begin (set! x.1 1)
                                                (begin (set! x.2 -2)
                                                       (set! x.3 3)
                                                       (set! x.4 (+ x.2 x.3))
                                                       (begin (set! x.5 0)
                                                              (begin (set! x.6 7)
                                                                     x.6))))))
                '(module (begin (set! x.1 1)
                                (begin (set! x.2 -2)
                                       (set! x.3 3)
                                       (set! x.4 (+ x.2 x.3))
                                       (begin (set! x.5 0)
                                              (begin (set! x.6 7)
                                                     x.6))))))
  (check-equal? (normalize-bind '(module (begin (set! x.0 -3)
                                                (set! x.1 (begin (set! x.2 2)
                                                                 (set! x.3 (begin (set! x.4 4) x.4)) (+ x.2 x.3))) x.0)))
                '(module (begin (set! x.0 -3)
                                (begin (set! x.2 2)
                                       (begin (set! x.4 4)
                                              (set! x.3 x.4))
                                       (set! x.1 (+ x.2 x.3))) x.0)))
  (check-equal? (normalize-bind '(module
                                     (define L.f.1
                                       (lambda (x.1)
                                         (begin
                                           (set! y.1 1)
                                           (set! z.1 2)
                                           (begin
                                             (set! a.1 (bitwise-and y.1 x.1))
                                             (set! b.1 (bitwise-ior z.1 x.1))
                                             (begin
                                               (set! a.1 (bitwise-xor a.1 b.1))
                                               (arithmetic-shift-right a.1 3))))))
                                   (begin
                                     (set! x.2 10)
                                     (if (begin (set! x.3 100) (not (!= x.2 x.3)))
                                         (call L.f.1 x.2)
                                         (call L.f.2 1000)))))
                '(module
                     (define L.f.1
                       (lambda (x.1)
                         (begin
                           (set! y.1 1)
                           (set! z.1 2)
                           (begin
                             (set! a.1 (bitwise-and y.1 x.1))
                             (set! b.1 (bitwise-ior z.1 x.1))
                             (begin
                               (set! a.1 (bitwise-xor a.1 b.1))
                               (arithmetic-shift-right a.1 3))))))
                   (begin
                     (set! x.2 10)
                     (if (begin (set! x.3 100) (not (!= x.2 x.3)))
                         (call L.f.1 x.2)
                         (call L.f.2 1000)))))
  (check-equal? (normalize-bind '(module
                                     (define L.*.2
                                       (lambda (tmp.1 tmp.2)
                                         (if (begin
                                               (set! tmp.23
                                                     (if (begin (set! tmp.24 (bitwise-and tmp.2 7)) (= tmp.24 0))
                                                         14
                                                         6))
                                               (!= tmp.23 6))
                                             (if (begin
                                                   (set! tmp.25
                                                         (if (begin (set! tmp.26 (bitwise-and tmp.1 7)) (= tmp.26 0))
                                                             14
                                                             6))
                                                   (!= tmp.25 6))
                                                 (begin
                                                   (set! tmp.27 (arithmetic-shift-right tmp.2 3))
                                                   (* tmp.1 tmp.27))
                                                 318)
                                             318)))
                                   (define L.+.1
                                     (lambda (tmp.3 tmp.4)
                                       (if (begin
                                             (set! tmp.28
                                                   (if (begin (set! tmp.29 (bitwise-and tmp.4 7)) (= tmp.29 0))
                                                       14
                                                       6))
                                             (!= tmp.28 6))
                                           (if (begin
                                                 (set! tmp.30
                                                       (if (begin (set! tmp.31 (bitwise-and tmp.3 7)) (= tmp.31 0))
                                                           14
                                                           6))
                                                 (!= tmp.30 6))
                                               (+ tmp.3 tmp.4)
                                               574)
                                           574)))
                                   (define L.add.10
                                     (lambda (a.61 b.62 c.63 d.64 e.65 f.66 g.67 h.68)
                                       (begin
                                         (set! tmp.32
                                               (begin
                                                 (set! tmp.33
                                                       (begin
                                                         (set! tmp.34
                                                               (begin
                                                                 (set! tmp.35
                                                                       (begin
                                                                         (set! tmp.36
                                                                               (begin
                                                                                 (set! tmp.37 (call L.+.1 g.67 h.68))
                                                                                 (call L.+.1 f.66 tmp.37)))
                                                                         (call L.+.1 e.65 tmp.36)))
                                                                 (call L.+.1 d.64 tmp.35)))
                                                         (call L.+.1 c.63 tmp.34)))
                                                 (call L.+.1 b.62 tmp.33)))
                                         (call L.+.1 a.61 tmp.32))))
                                   (define L.add-and-multiply.11
                                     (lambda (a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76 i.77)
                                       (begin
                                         (set! sum.78 (call L.add.10 a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76))
                                         (call L.*.2 sum.78 i.77))))
                                   (call L.add-and-multiply.11 8 16 24 32 40 48 56 64 16)))
                '(module
                     (define L.*.2
                       (lambda (tmp.1 tmp.2)
                         (if (begin
                               (if (begin (set! tmp.24 (bitwise-and tmp.2 7)) (= tmp.24 0))
                                   (set! tmp.23 14)
                                   (set! tmp.23 6))
                               (!= tmp.23 6))
                             (if (begin
                                   (if (begin (set! tmp.26 (bitwise-and tmp.1 7)) (= tmp.26 0))
                                       (set! tmp.25 14)
                                       (set! tmp.25 6))
                                   (!= tmp.25 6))
                                 (begin
                                   (set! tmp.27 (arithmetic-shift-right tmp.2 3))
                                   (* tmp.1 tmp.27))
                                 318)
                             318)))
                   (define L.+.1
                     (lambda (tmp.3 tmp.4)
                       (if (begin
                             (if (begin (set! tmp.29 (bitwise-and tmp.4 7)) (= tmp.29 0))
                                 (set! tmp.28 14)
                                 (set! tmp.28 6))
                             (!= tmp.28 6))
                           (if (begin
                                 (if (begin (set! tmp.31 (bitwise-and tmp.3 7)) (= tmp.31 0))
                                     (set! tmp.30 14)
                                     (set! tmp.30 6))
                                 (!= tmp.30 6))
                               (+ tmp.3 tmp.4)
                               574)
                           574)))
                   (define L.add.10
                     (lambda (a.61 b.62 c.63 d.64 e.65 f.66 g.67 h.68)
                       (begin
                         (begin
                           (begin
                             (begin
                               (begin
                                 (begin
                                   (set! tmp.37 (call L.+.1 g.67 h.68))
                                   (set! tmp.36 (call L.+.1 f.66 tmp.37)))
                                 (set! tmp.35 (call L.+.1 e.65 tmp.36)))
                               (set! tmp.34 (call L.+.1 d.64 tmp.35)))
                             (set! tmp.33 (call L.+.1 c.63 tmp.34)))
                           (set! tmp.32 (call L.+.1 b.62 tmp.33)))
                         (call L.+.1 a.61 tmp.32))))
                   (define L.add-and-multiply.11
                     (lambda (a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76 i.77)
                       (begin
                         (set! sum.78 (call L.add.10 a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76))
                         (call L.*.2 sum.78 i.77))))
                   (call L.add-and-multiply.11 8 16 24 32 40 48 56 64 16)))
  (check-equal? (normalize-bind '(module (define L.addup.1 (lambda () (begin (set! y.1 (alloc 16)) (mset! y.1 8 (begin (set! x.2 8) (set! x.3 16) (+ x.2 x.3))) (mref y.1 8)))) (call L.addup.1)))
                '(module
                     (define L.addup.1
                       (lambda ()
                         (begin
                           (set! y.1 (alloc 16))
                           (begin
                             (set! x.2 8)
                             (set! x.3 16)
                             (begin (set! tmp.1 (+ x.2 x.3)) (mset! y.1 8 tmp.1)))
                           (mref y.1 8))))
                   (call L.addup.1)))
  (check-equal? (normalize-bind '(module (if (begin (set! x.1 (alloc 8)) (set! y.1 (alloc 16)) (set! z.1 0) (begin (mset! x.1 0 (begin (set! tmp.164 (begin (set! t.1 32) (begin (set! tmp.165 (+ t.1 8)) (+ t.1 tmp.165)))) (alloc tmp.164))) (mset! y.1 z.1 18) (begin (set! tmp.166 (+ z.1 8)) (mset! y.1 tmp.166 40)) (begin (set! tmp.167 (mref y.1 z.1)) (begin (set! tmp.168 (begin (set! tmp.169 (+ z.1 8)) (mref y.1 tmp.169))) (= tmp.167 tmp.168))))) 8 16)))
                '(module
                     (if (begin
                           (set! x.1 (alloc 8))
                           (set! y.1 (alloc 16))
                           (set! z.1 0)
                           (begin
                             (begin
                               (begin
                                 (set! t.1 32)
                                 (begin (set! tmp.165 (+ t.1 8)) (set! tmp.164 (+ t.1 tmp.165))))
                               (begin (set! tmp.2 (alloc tmp.164)) (mset! x.1 0 tmp.2)))
                             (mset! y.1 z.1 18)
                             (begin (set! tmp.166 (+ z.1 8)) (mset! y.1 tmp.166 40))
                             (begin
                               (set! tmp.167 (mref y.1 z.1))
                               (begin
                                 (begin
                                   (set! tmp.169 (+ z.1 8))
                                   (set! tmp.168 (mref y.1 tmp.169)))
                                 (= tmp.167 tmp.168)))))
                         8
                         16))))
