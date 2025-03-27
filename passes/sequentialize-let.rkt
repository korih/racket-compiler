#lang racket

(require
  cpsc411/langs/v8
  rackunit)

(provide sequentialize-let)

;; values-bits-lang-v8 -> imp-mf-lang-v8
;; compiles p to Imp-mf-lang v8 by picking a particular order to implement
;; let expressions using set!
(define/contract (sequentialize-let p)
  (-> values-bits-lang-v8? imp-mf-lang-v8?)

  ;; func is `(define ,label (lambda (,alocs ...) ,tail))
  ;; interp. a function definition

  ;; func -> func
  (define (sequentialize-let-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,tail))
       `(define ,label (lambda (,@alocs) ,(sequentialize-let-tail tail)))]))

  ;; values-bits-lang-v8.tail -> imp-mf-lang-v8.tail
  (define (sequentialize-let-tail tail)
    (match tail
      [`(let ([,xs ,vs] ...) ,tail)
       (define sequentialize-let-values (for/list ([x xs] [v vs])
                                          `(set! ,x ,(sequentialize-let-value v))))
       `(begin ,@sequentialize-let-values ,(sequentialize-let-tail tail))]
      [`(if ,p ,t1 ,t2)
       (define p^ (sequentialize-let-pred p))
       (define t1^ (sequentialize-let-tail t1))
       (define t2^ (sequentialize-let-tail t2))
       `(if ,p^ ,t1^ ,t2^)]
      [`(call ,triv ,opand ...) tail]
      [`(begin ,es ... ,t)
       `(begin ,@(map sequentialize-let-effect es) ,(sequentialize-let-tail t))]
      [value (sequentialize-let-value value)]))

  ;; values-bits-lang-v8.value -> imp-mf-lang-v8.value
  (define (sequentialize-let-value v)
    (match v
      [`(let ([,xs ,vs] ...) ,v)
       (define sequentialize-let-values (for/list ([x xs] [v vs])
                                          `(set! ,x ,(sequentialize-let-value v))))
       `(begin ,@sequentialize-let-values ,(sequentialize-let-value v))]
      [`(if ,p ,v1 ,v2)
       `(if ,(sequentialize-let-pred p)
            ,(sequentialize-let-value v1)
            ,(sequentialize-let-value v2))]
      [`(begin ,es ... ,value)
       `(begin ,@(map sequentialize-let-effect es) ,(sequentialize-let-value value))]
      ;; Using wildcard collapse case because in all other cases, the
      ;; expression is already in imp-mf-lang-v8.value form
      [_ v]))

  ;; values-bits-lang-v8.pred -> imp-mf-lang-v8.pred
  (define (sequentialize-let-pred p)
    (match p
      [`(let ([,xs ,vs] ...) ,pred)
       (define sequentialize-let-values (for/list ([x xs] [v vs])
                                          `(set! ,x ,(sequentialize-let-value v))))
       `(begin ,@sequentialize-let-values ,(sequentialize-let-pred pred))]
      [`(,relop ,t1 ,t2) `(,relop ,t1 ,t2)]
      [`(true) '(true)]
      [`(false) '(false)]
      [`(not ,pred)
       (define pred^ (sequentialize-let-pred pred))
       `(not ,pred^)]
      [`(if ,p1 ,p2 ,p3)
       (define p1^ (sequentialize-let-pred p1))
       (define p2^ (sequentialize-let-pred p2))
       (define p3^ (sequentialize-let-pred p3))
       `(if ,p1^ ,p2^ ,p3^)]
      [`(begin ,es ... ,pred)
       `(begin ,@(map sequentialize-let-effect es) ,(sequentialize-let-pred pred))]))

  ;; values-bits-lang-v8.effect -> imp-mf-lang-v8.effect
  (define (sequentialize-let-effect e)
    (match e
      [`(let ([,xs ,vs] ...) ,eff)
       (define sequentialize-let-values (for/list ([x xs] [v vs])
                                          `(set! ,x ,(sequentialize-let-value v))))
       `(begin ,@sequentialize-let-values ,(sequentialize-let-effect eff))]
      [`(begin ,es ...)
       `(begin ,@(map sequentialize-let-effect es))]
      [`(mset! ,aloc ,opand ,value)
       `(mset! ,aloc ,opand ,(sequentialize-let-value value))]))

  (match p
    [`(module ,funcs ... ,tail)
     `(module ,@(map sequentialize-let-func funcs) ,(sequentialize-let-tail tail))]))

(module+ test
  (check-equal? (sequentialize-let '(module
                                        (define L.f.1
                                          (lambda (x.1 x.2)
                                            (begin
                                              (let ((tmp.38 (let ((tmp.39 (+ 10 6))) (alloc tmp.39))))
                                                (let ((tmp.40 (call L.g.1)))
                                                  (mset! tmp.38 tmp.40 (if (true) x.1 x.2))))
                                              (let ((tmp.41 (let ((tmp.42 (+ 10 6))) (alloc tmp.42))))
                                                (let ((tmp.43 (bitwise-and 8 8))) (mref tmp.41 tmp.43))))))
                                      (define L.g.1 (lambda () 8))
                                      (call L.f.1 1 2)))
                '(module
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
  (check-equal? (sequentialize-let '(module
                                        (define L.f.1 (lambda (x.1 y.1)
                                                        (let ([a.1 2] [b.1 (call L.g.1 x.1 y.1)])
                                                          (let ([c.1 (call L.g.1 a.1 b.1)])
                                                            (- a.1 b.1)))))
                                      (define L.g.1 (lambda (x.2 y.2)
                                                      (let ([z.1 (+ x.2 y.2)])
                                                        (let ([a.2 (- y.2 x.2)])
                                                          (* z.1 a.2)))))
                                      (let ([x.2 (call L.f.1 1 2)])
                                        (if (let ([x.3 (call L.g.1 1 2)])
                                              (not (!= x.2 x.3)))
                                            (call L.f.1 10 20)
                                            (call L.f.1 x.2 1)))))
                '(module
                     (define L.f.1
                       (lambda (x.1 y.1)
                         (begin
                           (set! a.1 2)
                           (set! b.1 (call L.g.1 x.1 y.1))
                           (begin (set! c.1 (call L.g.1 a.1 b.1)) (- a.1 b.1)))))
                   (define L.g.1
                     (lambda (x.2 y.2)
                       (begin
                         (set! z.1 (+ x.2 y.2))
                         (begin (set! a.2 (- y.2 x.2)) (* z.1 a.2)))))
                   (begin
                     (set! x.2 (call L.f.1 1 2))
                     (if (begin (set! x.3 (call L.g.1 1 2)) (not (!= x.2 x.3)))
                         (call L.f.1 10 20)
                         (call L.f.1 x.2 1)))))
  (check-equal? (sequentialize-let '(module
                                        (define L.f.1 (lambda (x.1)
                                                        (let ([y.1 1] [z.1 2])
                                                          (let ([a.1 (* y.1 x.1)]
                                                                [b.1 (* z.1 x.1)])
                                                            (+ a.1 b.1)))))
                                      (let ([x.2 10])
                                        (if (let ([x.3 100])
                                              (not (!= x.2 x.3)))
                                            (call L.f.1 x.2)
                                            (call L.f.2 1000)))))
                '(module
                     (define L.f.1
                       (lambda (x.1)
                         (begin
                           (set! y.1 1)
                           (set! z.1 2)
                           (begin
                             (set! a.1 (* y.1 x.1))
                             (set! b.1 (* z.1 x.1))
                             (+ a.1 b.1)))))
                   (begin
                     (set! x.2 10)
                     (if (begin
                           (set! x.3 100)
                           (not (!= x.2 x.3)))
                         (call L.f.1 x.2)
                         (call L.f.2 1000)))))
  (check-equal? (sequentialize-let '(module (if (let ([x.1 1] [y.2 2])
                                                  (if (not (< x.1 y.2)) (> x.1 y.2) (false)))
                                                (let ([x.2 0]
                                                      [x.3 1])
                                                  (+ x.2 x.3))
                                                (let ([x.4 2]
                                                      [x.5 3])
                                                  (+ x.4 x.5)))))
                '(module
                     (if (begin
                           (set! x.1 1)
                           (set! y.2 2)
                           (if (not (< x.1 y.2)) (> x.1 y.2) (false)))
                         (begin (set! x.2 0) (set! x.3 1) (+ x.2 x.3))
                         (begin (set! x.4 2) (set! x.5 3) (+ x.4 x.5)))))
  (check-equal? (sequentialize-let '(module (if (let ([x.1 1] [y.2 2]) (> x.1 y.2))
                                                (let ([x.2 0]
                                                      [x.3 1])
                                                  (+ x.2 x.3))
                                                (let ([x.4 2]
                                                      [x.5 3])
                                                  (+ x.4 x.5)))))
                '(module
                     (if (begin (set! x.1 1) (set! y.2 2) (> x.1 y.2))
                         (begin (set! x.2 0) (set! x.3 1) (+ x.2 x.3))
                         (begin (set! x.4 2) (set! x.5 3) (+ x.4 x.5)))))
  (check-equal? (sequentialize-let '(module (let ([x.1 3]) x.1))) '(module (begin (set! x.1 3) x.1)))
  (check-equal? (sequentialize-let '(module (let ([x.1 0]
                                                  [x.2 1])
                                              (+ x.1 x.2))))
                '(module (begin (set! x.1 0) (set! x.2 1) (+ x.1 x.2))))
  (check-equal? (sequentialize-let '(module (let ([x.1 (let ([x.7 5]) (* 5 x.7))]) (+ x.1 -1))))
                '(module (begin (set! x.1 (begin (set! x.7 5) (* 5 x.7))) (+ x.1 -1))))
  (check-equal? (sequentialize-let '(module
                                        (define L.f.1 (lambda (x.1)
                                                        (let ([y.1 1] [z.1 2])
                                                          (let ([a.1 (bitwise-and y.1 x.1)]
                                                                [b.1 (bitwise-ior z.1 x.1)])
                                                            (let ([a.1 (bitwise-xor a.1 b.1)])
                                                              (arithmetic-shift-right a.1 3))))))
                                      (let ([x.2 10])
                                        (if (let ([x.3 100])
                                              (not (!= x.2 x.3)))
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
  (check-equal? (sequentialize-let   '(module
                                          (define L.*.2
                                            (lambda (tmp.1 tmp.2)
                                              (if (let ((tmp.23
                                                         (if (let ((tmp.24 (bitwise-and tmp.2 7))) (= tmp.24 0))
                                                             14
                                                             6)))
                                                    (!= tmp.23 6))
                                                  (if (let ((tmp.25
                                                             (if (let ((tmp.26 (bitwise-and tmp.1 7))) (= tmp.26 0))
                                                                 14
                                                                 6)))
                                                        (!= tmp.25 6))
                                                      (let ((tmp.27 (arithmetic-shift-right tmp.2 3))) (* tmp.1 tmp.27))
                                                      318)
                                                  318)))
                                        (define L.+.1
                                          (lambda (tmp.3 tmp.4)
                                            (if (let ((tmp.28
                                                       (if (let ((tmp.29 (bitwise-and tmp.4 7))) (= tmp.29 0))
                                                           14
                                                           6)))
                                                  (!= tmp.28 6))
                                                (if (let ((tmp.30
                                                           (if (let ((tmp.31 (bitwise-and tmp.3 7))) (= tmp.31 0))
                                                               14
                                                               6)))
                                                      (!= tmp.30 6))
                                                    (+ tmp.3 tmp.4)
                                                    574)
                                                574)))
                                        (define L.add.10
                                          (lambda (a.61 b.62 c.63 d.64 e.65 f.66 g.67 h.68)
                                            (let ((tmp.32
                                                   (let ((tmp.33
                                                          (let ((tmp.34
                                                                 (let ((tmp.35
                                                                        (let ((tmp.36
                                                                               (let ((tmp.37
                                                                                      (call L.+.1 g.67 h.68)))
                                                                                 (call L.+.1 f.66 tmp.37))))
                                                                          (call L.+.1 e.65 tmp.36))))
                                                                   (call L.+.1 d.64 tmp.35))))
                                                            (call L.+.1 c.63 tmp.34))))
                                                     (call L.+.1 b.62 tmp.33))))
                                              (call L.+.1 a.61 tmp.32))))
                                        (define L.add-and-multiply.11
                                          (lambda (a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76 i.77)
                                            (let ((sum.78 (call L.add.10 a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76)))
                                              (call L.*.2 sum.78 i.77))))
                                        (call L.add-and-multiply.11 8 16 24 32 40 48 56 64 16)))
                '(module
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
  (check-equal? (sequentialize-let '(module (if (let ((x.1 (alloc 8)) (y.1 (alloc 16)) (z.1 0)) (begin (mset! x.1 0 (let ((tmp.164 (let ((t.1 32)) (let ((tmp.165 (+ t.1 8))) (+ t.1 tmp.165))))) (alloc tmp.164))) (mset! y.1 z.1 18) (let ((tmp.166 (+ z.1 8))) (mset! y.1 tmp.166 40)) (let ((tmp.167 (mref y.1 z.1))) (let ((tmp.168 (let ((tmp.169 (+ z.1 8))) (mref y.1 tmp.169)))) (= tmp.167 tmp.168))))) 8 16)))
                '(module
                     (if (begin
                           (set! x.1 (alloc 8))
                           (set! y.1 (alloc 16))
                           (set! z.1 0)
                           (begin
                             (mset!
                              x.1
                              0
                              (begin
                                (set! tmp.164
                                      (begin
                                        (set! t.1 32)
                                        (begin (set! tmp.165 (+ t.1 8)) (+ t.1 tmp.165))))
                                (alloc tmp.164)))
                             (mset! y.1 z.1 18)
                             (begin (set! tmp.166 (+ z.1 8)) (mset! y.1 tmp.166 40))
                             (begin
                               (set! tmp.167 (mref y.1 z.1))
                               (begin
                                 (set! tmp.168
                                       (begin (set! tmp.169 (+ z.1 8)) (mref y.1 tmp.169)))
                                 (= tmp.167 tmp.168)))))
                         8
                         16))))
