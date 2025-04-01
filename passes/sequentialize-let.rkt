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
                         16)))

  (check-equal? (interp-imp-mf-lang-v8 (sequentialize-let
                                        '(module
                                             (define L.+.12
                                               (lambda (tmp.19 tmp.20)
                                                 (if (let ((tmp.54
                                                            (if (let ((tmp.55 (bitwise-and tmp.20 7))) (= tmp.55 0))
                                                                14
                                                                6)))
                                                       (!= tmp.54 6))
                                                     (if (let ((tmp.56
                                                                (if (let ((tmp.57 (bitwise-and tmp.19 7))) (= tmp.57 0))
                                                                    14
                                                                    6)))
                                                           (!= tmp.56 6))
                                                         (+ tmp.19 tmp.20)
                                                         574)
                                                     574)))
                                           (define L.void?.11
                                             (lambda (tmp.43)
                                               (if (let ((tmp.58 (bitwise-and tmp.43 255))) (= tmp.58 30)) 14 6)))
                                           (define L.unsafe-vector-ref.3
                                             (lambda (tmp.14 tmp.15)
                                               (if (let ((tmp.59
                                                          (if (let ((tmp.60 (mref tmp.14 -3))) (< tmp.15 tmp.60))
                                                              14
                                                              6)))
                                                     (!= tmp.59 6))
                                                   (if (let ((tmp.61 (if (>= tmp.15 0) 14 6))) (!= tmp.61 6))
                                                       (let ((tmp.62
                                                              (let ((tmp.63
                                                                     (let ((tmp.64 (arithmetic-shift-right tmp.15 3)))
                                                                       (* tmp.64 8))))
                                                                (+ tmp.63 5))))
                                                         (mref tmp.14 tmp.62))
                                                       2878)
                                                   2878)))
                                           (define L.vector-ref.10
                                             (lambda (tmp.36 tmp.37)
                                               (if (let ((tmp.65
                                                          (if (let ((tmp.66 (bitwise-and tmp.37 7))) (= tmp.66 0))
                                                              14
                                                              6)))
                                                     (!= tmp.65 6))
                                                   (if (let ((tmp.67
                                                              (if (let ((tmp.68 (bitwise-and tmp.36 7))) (= tmp.68 3))
                                                                  14
                                                                  6)))
                                                         (!= tmp.67 6))
                                                       (call L.unsafe-vector-ref.3 tmp.36 tmp.37)
                                                       2878)
                                                   2878)))
                                           (define L.unsafe-vector-set!.2
                                             (lambda (tmp.9 tmp.10 tmp.11)
                                               (if (let ((tmp.69
                                                          (if (let ((tmp.70 (mref tmp.9 -3))) (< tmp.10 tmp.70)) 14 6)))
                                                     (!= tmp.69 6))
                                                   (if (let ((tmp.71 (if (>= tmp.10 0) 14 6))) (!= tmp.71 6))
                                                       (begin
                                                         (let ((tmp.72
                                                                (let ((tmp.73
                                                                       (let ((tmp.74 (arithmetic-shift-right tmp.10 3)))
                                                                         (* tmp.74 8))))
                                                                  (+ tmp.73 5))))
                                                           (mset! tmp.9 tmp.72 tmp.11))
                                                         30)
                                                       2622)
                                                   2622)))
                                           (define L.vector-set!.9
                                             (lambda (tmp.33 tmp.34 tmp.35)
                                               (if (let ((tmp.75
                                                          (if (let ((tmp.76 (bitwise-and tmp.34 7))) (= tmp.76 0))
                                                              14
                                                              6)))
                                                     (!= tmp.75 6))
                                                   (if (let ((tmp.77
                                                              (if (let ((tmp.78 (bitwise-and tmp.33 7))) (= tmp.78 3))
                                                                  14
                                                                  6)))
                                                         (!= tmp.77 6))
                                                       (call L.unsafe-vector-set!.2 tmp.33 tmp.34 tmp.35)
                                                       2622)
                                                   2622)))
                                           (define L.vector-init-loop.7
                                             (lambda (len.6 i.8 vec.7)
                                               (if (let ((tmp.79 (if (= len.6 i.8) 14 6))) (!= tmp.79 6))
                                                   vec.7
                                                   (begin
                                                     (let ((tmp.80
                                                            (let ((tmp.81
                                                                   (let ((tmp.82 (arithmetic-shift-right i.8 3)))
                                                                     (* tmp.82 8))))
                                                              (+ tmp.81 5))))
                                                       (mset! vec.7 tmp.80 0))
                                                     (let ((tmp.83 (+ i.8 8)))
                                                       (call L.vector-init-loop.7 len.6 tmp.83 vec.7))))))
                                           (define L.make-init-vector.1
                                             (lambda (tmp.4)
                                               (if (let ((tmp.84 (if (>= tmp.4 0) 14 6))) (!= tmp.84 6))
                                                   (let ((tmp.5
                                                          (let ((tmp.53
                                                                 (let ((tmp.85
                                                                        (let ((tmp.86
                                                                               (let ((tmp.87
                                                                                      (let ((tmp.88
                                                                                             (arithmetic-shift-right
                                                                                              tmp.4
                                                                                              3)))
                                                                                        (+ 1 tmp.88))))
                                                                                 (* tmp.87 8))))
                                                                          (alloc tmp.86))))
                                                                   (+ tmp.85 3))))
                                                            (begin (mset! tmp.53 -3 tmp.4) tmp.53))))
                                                     (call L.vector-init-loop.7 tmp.4 0 tmp.5))
                                                   3134)))
                                           (define L.make-vector.8
                                             (lambda (tmp.31)
                                               (if (let ((tmp.89
                                                          (if (let ((tmp.90 (bitwise-and tmp.31 7))) (= tmp.90 0))
                                                              14
                                                              6)))
                                                     (!= tmp.89 6))
                                                   (call L.make-init-vector.1 tmp.31)
                                                   2110)))
                                           (define L.v.4 (lambda () (call L.make-vector.8 24)))
                                           (define L.set-first.5 (lambda (vec.1) (call L.vector-set!.9 vec.1 0 336)))
                                           (define L.get-first.6 (lambda (vec.2) (call L.vector-ref.10 vec.2 0)))
                                           (let ((vec.3 (call L.v.4)))
                                             (let ((tmp.91
                                                    (if (let ((tmp.92
                                                               (let ((tmp.93 (call L.set-first.5 vec.3)))
                                                                 (call L.void?.11 tmp.93))))
                                                          (!= tmp.92 6))
                                                        0
                                                        318)))
                                               (let ((tmp.94 (call L.get-first.6 vec.3)))
                                                 (call L.+.12 tmp.91 tmp.94)))))))
                (interp-imp-mf-lang-v8 '(module
                                            (define L.+.12
                                              (lambda (tmp.19 tmp.20)
                                                (if (begin
                                                      (set! tmp.54
                                                            (if (begin (set! tmp.55 (bitwise-and tmp.20 7)) (= tmp.55 0))
                                                                14
                                                                6))
                                                      (!= tmp.54 6))
                                                    (if (begin
                                                          (set! tmp.56
                                                                (if (begin (set! tmp.57 (bitwise-and tmp.19 7)) (= tmp.57 0))
                                                                    14
                                                                    6))
                                                          (!= tmp.56 6))
                                                        (+ tmp.19 tmp.20)
                                                        574)
                                                    574)))
                                          (define L.void?.11
                                            (lambda (tmp.43)
                                              (if (begin (set! tmp.58 (bitwise-and tmp.43 255)) (= tmp.58 30)) 14 6)))
                                          (define L.unsafe-vector-ref.3
                                            (lambda (tmp.14 tmp.15)
                                              (if (begin
                                                    (set! tmp.59
                                                          (if (begin (set! tmp.60 (mref tmp.14 -3)) (< tmp.15 tmp.60))
                                                              14
                                                              6))
                                                    (!= tmp.59 6))
                                                  (if (begin (set! tmp.61 (if (>= tmp.15 0) 14 6)) (!= tmp.61 6))
                                                      (begin
                                                        (set! tmp.62
                                                              (begin
                                                                (set! tmp.63
                                                                      (begin
                                                                        (set! tmp.64 (arithmetic-shift-right tmp.15 3))
                                                                        (* tmp.64 8)))
                                                                (+ tmp.63 5)))
                                                        (mref tmp.14 tmp.62))
                                                      2878)
                                                  2878)))
                                          (define L.vector-ref.10
                                            (lambda (tmp.36 tmp.37)
                                              (if (begin
                                                    (set! tmp.65
                                                          (if (begin (set! tmp.66 (bitwise-and tmp.37 7)) (= tmp.66 0))
                                                              14
                                                              6))
                                                    (!= tmp.65 6))
                                                  (if (begin
                                                        (set! tmp.67
                                                              (if (begin (set! tmp.68 (bitwise-and tmp.36 7)) (= tmp.68 3))
                                                                  14
                                                                  6))
                                                        (!= tmp.67 6))
                                                      (call L.unsafe-vector-ref.3 tmp.36 tmp.37)
                                                      2878)
                                                  2878)))
                                          (define L.unsafe-vector-set!.2
                                            (lambda (tmp.9 tmp.10 tmp.11)
                                              (if (begin
                                                    (set! tmp.69
                                                          (if (begin (set! tmp.70 (mref tmp.9 -3)) (< tmp.10 tmp.70))
                                                              14
                                                              6))
                                                    (!= tmp.69 6))
                                                  (if (begin (set! tmp.71 (if (>= tmp.10 0) 14 6)) (!= tmp.71 6))
                                                      (begin
                                                        (begin
                                                          (set! tmp.72
                                                                (begin
                                                                  (set! tmp.73
                                                                        (begin
                                                                          (set! tmp.74 (arithmetic-shift-right tmp.10 3))
                                                                          (* tmp.74 8)))
                                                                  (+ tmp.73 5)))
                                                          (mset! tmp.9 tmp.72 tmp.11))
                                                        30)
                                                      2622)
                                                  2622)))
                                          (define L.vector-set!.9
                                            (lambda (tmp.33 tmp.34 tmp.35)
                                              (if (begin
                                                    (set! tmp.75
                                                          (if (begin (set! tmp.76 (bitwise-and tmp.34 7)) (= tmp.76 0))
                                                              14
                                                              6))
                                                    (!= tmp.75 6))
                                                  (if (begin
                                                        (set! tmp.77
                                                              (if (begin (set! tmp.78 (bitwise-and tmp.33 7)) (= tmp.78 3))
                                                                  14
                                                                  6))
                                                        (!= tmp.77 6))
                                                      (call L.unsafe-vector-set!.2 tmp.33 tmp.34 tmp.35)
                                                      2622)
                                                  2622)))
                                          (define L.vector-init-loop.7
                                            (lambda (len.6 i.8 vec.7)
                                              (if (begin (set! tmp.79 (if (= len.6 i.8) 14 6)) (!= tmp.79 6))
                                                  vec.7
                                                  (begin
                                                    (begin
                                                      (set! tmp.80
                                                            (begin
                                                              (set! tmp.81
                                                                    (begin
                                                                      (set! tmp.82 (arithmetic-shift-right i.8 3))
                                                                      (* tmp.82 8)))
                                                              (+ tmp.81 5)))
                                                      (mset! vec.7 tmp.80 0))
                                                    (begin
                                                      (set! tmp.83 (+ i.8 8))
                                                      (call L.vector-init-loop.7 len.6 tmp.83 vec.7))))))
                                          (define L.make-init-vector.1
                                            (lambda (tmp.4)
                                              (if (begin (set! tmp.84 (if (>= tmp.4 0) 14 6)) (!= tmp.84 6))
                                                  (begin
                                                    (set! tmp.5
                                                          (begin
                                                            (set! tmp.53
                                                                  (begin
                                                                    (set! tmp.85
                                                                          (begin
                                                                            (set! tmp.86
                                                                                  (begin
                                                                                    (set! tmp.87
                                                                                          (begin
                                                                                            (set! tmp.88 (arithmetic-shift-right tmp.4 3))
                                                                                            (+ 1 tmp.88)))
                                                                                    (* tmp.87 8)))
                                                                            (alloc tmp.86)))
                                                                    (+ tmp.85 3)))
                                                            (begin (mset! tmp.53 -3 tmp.4) tmp.53)))
                                                    (call L.vector-init-loop.7 tmp.4 0 tmp.5))
                                                  3134)))
                                          (define L.make-vector.8
                                            (lambda (tmp.31)
                                              (if (begin
                                                    (set! tmp.89
                                                          (if (begin (set! tmp.90 (bitwise-and tmp.31 7)) (= tmp.90 0))
                                                              14
                                                              6))
                                                    (!= tmp.89 6))
                                                  (call L.make-init-vector.1 tmp.31)
                                                  2110)))
                                          (define L.v.4 (lambda () (call L.make-vector.8 24)))
                                          (define L.set-first.5 (lambda (vec.1) (call L.vector-set!.9 vec.1 0 336)))
                                          (define L.get-first.6 (lambda (vec.2) (call L.vector-ref.10 vec.2 0)))
                                          (begin
                                            (set! vec.3 (call L.v.4))
                                            (begin
                                              (set! tmp.91
                                                    (if (begin
                                                          (set! tmp.92
                                                                (begin
                                                                  (set! tmp.93 (call L.set-first.5 vec.3))
                                                                  (call L.void?.11 tmp.93)))
                                                          (!= tmp.92 6))
                                                        0
                                                        318))
                                              (begin
                                                (set! tmp.94 (call L.get-first.6 vec.3))
                                                (call L.+.12 tmp.91 tmp.94))))))))
