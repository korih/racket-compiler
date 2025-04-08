#lang racket

(require
  cpsc411/langs/v8
  rackunit
  "../passes/remove-complex-opera.rkt")

(module+ test
  (check-equal? (remove-complex-opera* '(module
                                            (define L.+.1
                                              (lambda (tmp.1 tmp.2)
                                                (if (!= (if (= (bitwise-and tmp.2 7) 0) 14 6) 6)
                                                    (if (!= (if (= (bitwise-and tmp.1 7) 0) 14 6) 6) (+ tmp.1 tmp.2) 574)
                                                    574)))
                                          (call L.+.1 8 16)))
                '(module
                     (define L.+.1
                       (lambda (tmp.1 tmp.2)
                         (if (let ((tmp.1
                                    (if (let ((tmp.2 (bitwise-and tmp.2 7))) (= tmp.2 0)) 14 6)))
                               (!= tmp.1 6))
                             (if (let ((tmp.3
                                        (if (let ((tmp.4 (bitwise-and tmp.1 7))) (= tmp.4 0))
                                            14
                                            6)))
                                   (!= tmp.3 6))
                                 (+ tmp.1 tmp.2)
                                 574)
                             574)))
                   (call L.+.1 8 16)))
  (check-equal? (remove-complex-opera* '(module
                                            (define L.ascii-char?.6
                                              (lambda (tmp.21) (if (= (bitwise-and tmp.21 255) 46) 14 6)))
                                          (define L.+.5
                                            (lambda (tmp.3 tmp.4)
                                              (if (!= (if (= (bitwise-and tmp.4 7) 0) 14 6) 6)
                                                  (if (!= (if (= (bitwise-and tmp.3 7) 0) 14 6) 6) (+ tmp.3 tmp.4) 574)
                                                  574)))
                                          (define L.empty?.4
                                            (lambda (tmp.19) (if (= (bitwise-and tmp.19 255) 22) 14 6)))
                                          (define L.error?.3
                                            (lambda (tmp.22) (if (= (bitwise-and tmp.22 255) 62) 14 6)))
                                          (define L.not.2 (lambda (tmp.23) (if (!= tmp.23 6) 6 14)))
                                          (define L.boolean?.1
                                            (lambda (tmp.18) (if (= (bitwise-and tmp.18 247) 6) 14 6)))
                                          (let ((x.1 14) (x.2 6) (x.3 22) (x.4 30) (x.5 65342) (x.6 30766))
                                            (if (!= (call L.not.2 (call L.boolean?.1 x.1)) 6)
                                                (call L.+.5 (call L.error?.3 x.5) (call L.empty?.4 x.3))
                                                (call L.ascii-char?.6 x.6)))))
                '(module
                     (define L.ascii-char?.6
                       (lambda (tmp.21)
                         (if (let ((tmp.5 (bitwise-and tmp.21 255))) (= tmp.5 46)) 14 6)))
                   (define L.+.5
                     (lambda (tmp.3 tmp.4)
                       (if (let ((tmp.6
                                  (if (let ((tmp.7 (bitwise-and tmp.4 7))) (= tmp.7 0)) 14 6)))
                             (!= tmp.6 6))
                           (if (let ((tmp.8
                                      (if (let ((tmp.9 (bitwise-and tmp.3 7))) (= tmp.9 0))
                                          14
                                          6)))
                                 (!= tmp.8 6))
                               (+ tmp.3 tmp.4)
                               574)
                           574)))
                   (define L.empty?.4
                     (lambda (tmp.19)
                       (if (let ((tmp.10 (bitwise-and tmp.19 255))) (= tmp.10 22)) 14 6)))
                   (define L.error?.3
                     (lambda (tmp.22)
                       (if (let ((tmp.11 (bitwise-and tmp.22 255))) (= tmp.11 62)) 14 6)))
                   (define L.not.2 (lambda (tmp.23) (if (!= tmp.23 6) 6 14)))
                   (define L.boolean?.1
                     (lambda (tmp.18)
                       (if (let ((tmp.12 (bitwise-and tmp.18 247))) (= tmp.12 6)) 14 6)))
                   (let ((x.1 14) (x.2 6) (x.3 22) (x.4 30) (x.5 65342) (x.6 30766))
                     (if (let ((tmp.13
                                (let ((tmp.14 (call L.boolean?.1 x.1)))
                                  (call L.not.2 tmp.14))))
                           (!= tmp.13 6))
                         (let ((tmp.15 (call L.error?.3 x.5)))
                           (let ((tmp.16 (call L.empty?.4 x.3))) (call L.+.5 tmp.15 tmp.16)))
                         (call L.ascii-char?.6 x.6)))))
  (check-equal? (remove-complex-opera* '(module
                                            (define L.+.11
                                              (lambda (tmp.20 tmp.21)
                                                (if (!= (if (= (bitwise-and tmp.21 7) 0) 14 6) 6)
                                                    (if (!= (if (= (bitwise-and tmp.20 7) 0) 14 6) 6)
                                                        (+ tmp.20 tmp.21)
                                                        574)
                                                    574)))
                                          (define L.eq?.10 (lambda (tmp.18 tmp.19) (if (= tmp.18 tmp.19) 14 6)))
                                          (define L.odd?.4
                                            (lambda (x.45)
                                              (if (!= (call L.eq?.10 x.45 0) 6)
                                                  0
                                                  (let ((y.46 (call L.+.11 x.45 -8))) (call L.even?.5 y.46)))))
                                          (define L.even?.5
                                            (lambda (x.47)
                                              (if (!= (call L.eq?.10 x.47 0) 6)
                                                  8
                                                  (let ((y.48 (call L.+.11 x.47 -8))) (call L.odd?.4 y.48)))))
                                          (call L.even?.5 40)))
                '(module
                     (define L.+.11
                       (lambda (tmp.20 tmp.21)
                         (if (let ((tmp.17
                                    (if (let ((tmp.18 (bitwise-and tmp.21 7))) (= tmp.18 0))
                                        14
                                        6)))
                               (!= tmp.17 6))
                             (if (let ((tmp.19
                                        (if (let ((tmp.20 (bitwise-and tmp.20 7))) (= tmp.20 0))
                                            14
                                            6)))
                                   (!= tmp.19 6))
                                 (+ tmp.20 tmp.21)
                                 574)
                             574)))
                   (define L.eq?.10 (lambda (tmp.18 tmp.19) (if (= tmp.18 tmp.19) 14 6)))
                   (define L.odd?.4
                     (lambda (x.45)
                       (if (let ((tmp.21 (call L.eq?.10 x.45 0))) (!= tmp.21 6))
                           0
                           (let ((y.46 (call L.+.11 x.45 -8))) (call L.even?.5 y.46)))))
                   (define L.even?.5
                     (lambda (x.47)
                       (if (let ((tmp.22 (call L.eq?.10 x.47 0))) (!= tmp.22 6))
                           8
                           (let ((y.48 (call L.+.11 x.47 -8))) (call L.odd?.4 y.48)))))
                   (call L.even?.5 40)))
  (check-equal? (remove-complex-opera* '(module
                                            (define L.*.2
                                              (lambda (tmp.1 tmp.2)
                                                (if (!= (if (= (bitwise-and tmp.2 7) 0) 14 6) 6)
                                                    (if (!= (if (= (bitwise-and tmp.1 7) 0) 14 6) 6)
                                                        (* tmp.1 (arithmetic-shift-right tmp.2 3))
                                                        318)
                                                    318)))
                                          (define L.+.1
                                            (lambda (tmp.3 tmp.4)
                                              (if (!= (if (= (bitwise-and tmp.4 7) 0) 14 6) 6)
                                                  (if (!= (if (= (bitwise-and tmp.3 7) 0) 14 6) 6) (+ tmp.3 tmp.4) 574)
                                                  574)))
                                          (define L.add.10
                                            (lambda (a.61 b.62 c.63 d.64 e.65 f.66 g.67 h.68)
                                              (call
                                               L.+.1
                                               a.61
                                               (call
                                                L.+.1
                                                b.62
                                                (call
                                                 L.+.1
                                                 c.63
                                                 (call
                                                  L.+.1
                                                  d.64
                                                  (call L.+.1 e.65 (call L.+.1 f.66 (call L.+.1 g.67 h.68)))))))))
                                          (define L.add-and-multiply.11
                                            (lambda (a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76 i.77)
                                              (let ((sum.78 (call L.add.10 a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76)))
                                                (call L.*.2 sum.78 i.77))))
                                          (call L.add-and-multiply.11 8 16 24 32 40 48 56 64 16)))
                '(module
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
  (check-equal? (remove-complex-opera* '(module (define L.f.1 (lambda (x.1 x.2)
                                                                (begin
                                                                  (mset! (alloc (+ 10 6)) (call L.g.1) (if (true) x.1 x.2))
                                                                  (mref (alloc (+ 10 6)) (bitwise-and 8 8)))))
                                          (define L.g.1 (lambda () 8))
                                          (call L.f.1 1 2)))
                '(module
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
  (check-equal? (interp-values-bits-lang-v8 (remove-complex-opera*
                                             '(module
                                                  (define L.+.12
                                                    (lambda (tmp.19 tmp.20)
                                                      (if (!= (if (= (bitwise-and tmp.20 7) 0) 14 6) 6)
                                                          (if (!= (if (= (bitwise-and tmp.19 7) 0) 14 6) 6)
                                                              (+ tmp.19 tmp.20)
                                                              574)
                                                          574)))
                                                (define L.void?.11
                                                  (lambda (tmp.43) (if (= (bitwise-and tmp.43 255) 30) 14 6)))
                                                (define L.unsafe-vector-ref.3
                                                  (lambda (tmp.14 tmp.15)
                                                    (if (!= (if (< tmp.15 (mref tmp.14 -3)) 14 6) 6)
                                                        (if (!= (if (>= tmp.15 0) 14 6) 6)
                                                            (mref tmp.14 (+ (* (arithmetic-shift-right tmp.15 3) 8) 5))
                                                            2878)
                                                        2878)))
                                                (define L.vector-ref.10
                                                  (lambda (tmp.36 tmp.37)
                                                    (if (!= (if (= (bitwise-and tmp.37 7) 0) 14 6) 6)
                                                        (if (!= (if (= (bitwise-and tmp.36 7) 3) 14 6) 6)
                                                            (call L.unsafe-vector-ref.3 tmp.36 tmp.37)
                                                            2878)
                                                        2878)))
                                                (define L.unsafe-vector-set!.2
                                                  (lambda (tmp.9 tmp.10 tmp.11)
                                                    (if (!= (if (< tmp.10 (mref tmp.9 -3)) 14 6) 6)
                                                        (if (!= (if (>= tmp.10 0) 14 6) 6)
                                                            (begin
                                                              (mset! tmp.9 (+ (* (arithmetic-shift-right tmp.10 3) 8) 5) tmp.11)
                                                              30)
                                                            2622)
                                                        2622)))
                                                (define L.vector-set!.9
                                                  (lambda (tmp.33 tmp.34 tmp.35)
                                                    (if (!= (if (= (bitwise-and tmp.34 7) 0) 14 6) 6)
                                                        (if (!= (if (= (bitwise-and tmp.33 7) 3) 14 6) 6)
                                                            (call L.unsafe-vector-set!.2 tmp.33 tmp.34 tmp.35)
                                                            2622)
                                                        2622)))
                                                (define L.vector-init-loop.7
                                                  (lambda (len.6 i.8 vec.7)
                                                    (if (!= (if (= len.6 i.8) 14 6) 6)
                                                        vec.7
                                                        (begin
                                                          (mset! vec.7 (+ (* (arithmetic-shift-right i.8 3) 8) 5) 0)
                                                          (call L.vector-init-loop.7 len.6 (+ i.8 8) vec.7)))))
                                                (define L.make-init-vector.1
                                                  (lambda (tmp.4)
                                                    (if (!= (if (>= tmp.4 0) 14 6) 6)
                                                        (let ((tmp.5
                                                               (let ((tmp.53
                                                                      (+
                                                                       (alloc (* (+ 1 (arithmetic-shift-right tmp.4 3)) 8))
                                                                       3)))
                                                                 (begin (mset! tmp.53 -3 tmp.4) tmp.53))))
                                                          (call L.vector-init-loop.7 tmp.4 0 tmp.5))
                                                        3134)))
                                                (define L.make-vector.8
                                                  (lambda (tmp.31)
                                                    (if (!= (if (= (bitwise-and tmp.31 7) 0) 14 6) 6)
                                                        (call L.make-init-vector.1 tmp.31)
                                                        2110)))
                                                (define L.v.4 (lambda () (call L.make-vector.8 24)))
                                                (define L.set-first.5 (lambda (vec.1) (call L.vector-set!.9 vec.1 0 336)))
                                                (define L.get-first.6 (lambda (vec.2) (call L.vector-ref.10 vec.2 0)))
                                                (let ((vec.3 (call L.v.4)))
                                                  (call
                                                   L.+.12
                                                   (if (!= (call L.void?.11 (call L.set-first.5 vec.3)) 6) 0 318)
                                                   (call L.get-first.6 vec.3))))))
                (interp-values-bits-lang-v8 '(module
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
                                                     (call L.+.12 tmp.91 tmp.94))))))))
