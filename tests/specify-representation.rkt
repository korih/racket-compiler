#lang racket

(require
  cpsc411/langs/v8
  rackunit
  "../passes/specify-representation.rkt")

(module+ test
  (check-equal? (specify-representation '(module (unsafe-fx+ 100 20)))
                '(module (+ 800 160)))
  (check-equal? (specify-representation '(module (unsafe-fx* 100 20)))
                '(module (* 100 160)))
  (check-equal? (specify-representation '(module (unsafe-fx< 100 20)))
                '(module (if (< 800 160) 14 6)))
  (check-equal? (specify-representation '(module
                                             (define L.+.1
                                               (lambda (tmp.1 tmp.2)
                                                 (if (fixnum? tmp.2)
                                                     (if (fixnum? tmp.1) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                                                     (error 2))))
                                           (call L.+.1 1 2)))
                '(module
                     (define L.+.1
                       (lambda (tmp.1 tmp.2)
                         (if (!= (if (= (bitwise-and tmp.2 7) 0) 14 6) 6)
                             (if (!= (if (= (bitwise-and tmp.1 7) 0) 14 6) 6) (+ tmp.1 tmp.2) 574)
                             574)))
                   (call L.+.1 8 16)))
  (check-equal? (specify-representation '(module
                                             (define L.*.2
                                               (lambda (tmp.3 tmp.4)
                                                 (if (fixnum? tmp.4)
                                                     (if (fixnum? tmp.3) (unsafe-fx* tmp.3 tmp.4) (error 1))
                                                     (error 1))))
                                           (call L.*.2 1 2)))
                '(module
                     (define L.*.2
                       (lambda (tmp.3 tmp.4)
                         (if (!= (if (= (bitwise-and tmp.4 7) 0) 14 6) 6)
                             (if (!= (if (= (bitwise-and tmp.3 7) 0) 14 6) 6)
                                 (* tmp.3 (arithmetic-shift-right tmp.4 3))
                                 318)
                             318)))
                   (call L.*.2 8 16)))
  (check-equal? (specify-representation '(module
                                             (define L.+.11
                                               (lambda (tmp.20 tmp.21)
                                                 (if (fixnum? tmp.21)
                                                     (if (fixnum? tmp.20) (unsafe-fx+ tmp.20 tmp.21) (error 2))
                                                     (error 2))))
                                           (define L.eq?.10 (lambda (tmp.18 tmp.19) (eq? tmp.18 tmp.19)))
                                           (define L.odd?.4
                                             (lambda (x.45)
                                               (if (call L.eq?.10 x.45 0)
                                                   0
                                                   (let ((y.46 (call L.+.11 x.45 -1))) (call L.even?.5 y.46)))))
                                           (define L.even?.5
                                             (lambda (x.47)
                                               (if (call L.eq?.10 x.47 0)
                                                   1
                                                   (let ((y.48 (call L.+.11 x.47 -1))) (call L.odd?.4 y.48)))))
                                           (call L.even?.5 5)))
                '(module
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
  (check-equal? (specify-representation '(module
                                             (define L.ascii-char?.6 (lambda (tmp.21) (ascii-char? tmp.21)))
                                           (define L.+.5
                                             (lambda (tmp.3 tmp.4)
                                               (if (fixnum? tmp.4)
                                                   (if (fixnum? tmp.3) (unsafe-fx+ tmp.3 tmp.4) (error 2))
                                                   (error 2))))
                                           (define L.empty?.4 (lambda (tmp.19) (empty? tmp.19)))
                                           (define L.error?.3 (lambda (tmp.22) (error? tmp.22)))
                                           (define L.not.2 (lambda (tmp.23) (not tmp.23)))
                                           (define L.boolean?.1 (lambda (tmp.18) (boolean? tmp.18)))
                                           (let ((x.1 #t) (x.2 #f) (x.3 empty) (x.4 (void)) (x.5 (error 255)) (x.6 #\x))
                                             (if (call L.not.2 (call L.boolean?.1 x.1))
                                                 (call L.+.5 (call L.error?.3 x.5) (call L.empty?.4 x.3))
                                                 (call L.ascii-char?.6 x.6)))))
                '(module
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
  (check-equal? (specify-representation '(module
                                             (define L.*.2
                                               (lambda (tmp.1 tmp.2)
                                                 (if (fixnum? tmp.2)
                                                     (if (fixnum? tmp.1) (unsafe-fx* tmp.1 tmp.2) (error 1))
                                                     (error 1))))
                                           (define L.+.1
                                             (lambda (tmp.3 tmp.4)
                                               (if (fixnum? tmp.4)
                                                   (if (fixnum? tmp.3) (unsafe-fx+ tmp.3 tmp.4) (error 2))
                                                   (error 2))))
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
                                           (call L.add-and-multiply.11 1 2 3 4 5 6 7 8 2)))
                '(module
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
  (check-equal? (specify-representation '(module (pair? (cons 5 6))))
                '(module
                     (if (=
                          (bitwise-and
                           (let ((tmp.1 (+ (alloc 16) 1)))
                             (begin (mset! tmp.1 -1 40) (mset! tmp.1 7 48) tmp.1))
                           7)
                          1)
                         14
                         6)))
  (check-equal? (specify-representation '(module (unsafe-cdr (cons 5 6))))
                '(module
                     (mref
                      (let ((tmp.2 (+ (alloc 16) 1)))
                        (begin (mset! tmp.2 -1 40) (mset! tmp.2 7 48) tmp.2))
                      7)))
  (check-equal? (specify-representation '(module (vector? (unsafe-make-vector 3))))
                '(module
                     (if (=
                          (bitwise-and
                           (let ((tmp.3 (+ (alloc 32) 3))) (begin (mset! tmp.3 -3 24) tmp.3))
                           7)
                          3)
                         14
                         6)))
  (check-equal? (specify-representation '(module (unsafe-vector-length (unsafe-make-vector #t))))
                '(module
                     (mref
                      (let ((tmp.4 (+ (alloc 16) 3))) (begin (mset! tmp.4 -3 14) tmp.4))
                      -3)))
  (check-equal? (specify-representation '(module
                                             (let ([v.1 (unsafe-make-vector 2)])
                                               (begin
                                                 (unsafe-vector-set! v.1 1 (error 100))
                                                 (unsafe-vector-ref v.1 1)))))
                '(module
                     (let ((v.1
                            (let ((tmp.5 (+ (alloc 24) 3))) (begin (mset! tmp.5 -3 16) tmp.5))))
                       (begin (mset! v.1 13 25662) (mref v.1 13)))))
  (check-equal? (specify-representation '(module (cons 5 6)))
                '(module
                     (let ((tmp.6 (+ (alloc 16) 1)))
                       (begin (mset! tmp.6 -1 40) (mset! tmp.6 7 48) tmp.6))))
  (check-equal? (specify-representation '(module (unsafe-car (cons 5 6))))
                '(module
                     (mref
                      (let ((tmp.7 (+ (alloc 16) 1)))
                        (begin (mset! tmp.7 -1 40) (mset! tmp.7 7 48) tmp.7))
                      -1)))
  (check-equal? (specify-representation '(module (unsafe-vector-ref (unsafe-make-vector 3) 6)))
                '(module
                     (mref
                      (let ((tmp.8 (+ (alloc 32) 3))) (begin (mset! tmp.8 -3 24) tmp.8))
                      53)))
  (check-equal? (specify-representation '(module (unsafe-make-vector 5)))
                '(module (let ((tmp.9 (+ (alloc 48) 3))) (begin (mset! tmp.9 -3 40) tmp.9))))
  (check-equal? (specify-representation '(module (unsafe-vector-ref (unsafe-make-vector 3) 0)))
                '(module
                     (mref
                      (let ((tmp.10 (+ (alloc 32) 3))) (begin (mset! tmp.10 -3 24) tmp.10))
                      5)))
  (check-equal? (specify-representation '(module (define L.vector-ref.32 (lambda (tmp.70 tmp.71) (if (fixnum? tmp.71) (if (vector? tmp.70) (call L.unsafe-vector-ref.33 tmp.70 tmp.71) (error 11)) (error 11)))) (define L.make-vector.34 (lambda (tmp.74) (if (fixnum? tmp.74) (call L.make-init-vector.35 tmp.74) (error 8)))) (define L.vector-init-loop.36 (lambda (len.77 i.78 vec.79) (if (eq? len.77 i.78) vec.79 (begin (unsafe-vector-set! vec.79 i.78 0) (call L.vector-init-loop.36 len.77 (unsafe-fx+ i.78 1) vec.79))))) (define L.unsafe-vector-ref.33 (lambda (tmp.72 tmp.73) (if (unsafe-fx< tmp.73 (unsafe-vector-length tmp.72)) (if (unsafe-fx>= tmp.73 0) (unsafe-vector-ref tmp.72 tmp.73) (error 11)) (error 11)))) (define L.make-init-vector.35 (lambda (tmp.75) (if (unsafe-fx>= tmp.75 0) (let ((tmp.76 (unsafe-make-vector tmp.75))) (call L.vector-init-loop.36 tmp.75 0 tmp.76)) (error 12)))) (call L.vector-ref.32 (call L.make-vector.34 2) 0)))
                '(module
                     (define L.vector-ref.32
                       (lambda (tmp.70 tmp.71)
                         (if (!= (if (= (bitwise-and tmp.71 7) 0) 14 6) 6)
                             (if (!= (if (= (bitwise-and tmp.70 7) 3) 14 6) 6)
                                 (call L.unsafe-vector-ref.33 tmp.70 tmp.71)
                                 2878)
                             2878)))
                   (define L.make-vector.34
                     (lambda (tmp.74)
                       (if (!= (if (= (bitwise-and tmp.74 7) 0) 14 6) 6)
                           (call L.make-init-vector.35 tmp.74)
                           2110)))
                   (define L.vector-init-loop.36
                     (lambda (len.77 i.78 vec.79)
                       (if (!= (if (= len.77 i.78) 14 6) 6)
                           vec.79
                           (begin
                             (mset! vec.79 (+ (* (arithmetic-shift-right i.78 3) 8) 5) 0)
                             (call L.vector-init-loop.36 len.77 (+ i.78 8) vec.79)))))
                   (define L.unsafe-vector-ref.33
                     (lambda (tmp.72 tmp.73)
                       (if (!= (if (< tmp.73 (mref tmp.72 -3)) 14 6) 6)
                           (if (!= (if (>= tmp.73 0) 14 6) 6)
                               (mref tmp.72 (+ (* (arithmetic-shift-right tmp.73 3) 8) 5))
                               2878)
                           2878)))
                   (define L.make-init-vector.35
                     (lambda (tmp.75)
                       (if (!= (if (>= tmp.75 0) 14 6) 6)
                           (let ((tmp.76
                                  (let ((tmp.11
                                         (+
                                          (alloc (* (+ 1 (arithmetic-shift-right tmp.75 3)) 8))
                                          3)))
                                    (begin (mset! tmp.11 -3 tmp.75) tmp.11))))
                             (call L.vector-init-loop.36 tmp.75 0 tmp.76))
                           3134)))
                   (call L.vector-ref.32 (call L.make-vector.34 16) 0)))
  (check-equal? (specify-representation '(module (define L.vector-init-loop.20 (lambda (len.45 i.46 vec.47) (if (eq? len.45 i.46) vec.47 (begin (unsafe-vector-set! vec.47 i.46 0) (call L.vector-init-loop.20 len.45 (unsafe-fx+ i.46 1) vec.47))))) (define L.make-vector.18 (lambda (tmp.42) (if (fixnum? tmp.42) (call L.make-init-vector.19 tmp.42) (error 8)))) (define L.make-init-vector.19 (lambda (tmp.43) (if (unsafe-fx>= tmp.43 0) (let ((tmp.44 (unsafe-make-vector tmp.43))) (call L.vector-init-loop.20 tmp.43 0 tmp.44)) (error 12)))) (call L.make-vector.18 0)))
                '(module
                     (define L.vector-init-loop.20
                       (lambda (len.45 i.46 vec.47)
                         (if (!= (if (= len.45 i.46) 14 6) 6)
                             vec.47
                             (begin
                               (mset! vec.47 (+ (* (arithmetic-shift-right i.46 3) 8) 5) 0)
                               (call L.vector-init-loop.20 len.45 (+ i.46 8) vec.47)))))
                   (define L.make-vector.18
                     (lambda (tmp.42)
                       (if (!= (if (= (bitwise-and tmp.42 7) 0) 14 6) 6)
                           (call L.make-init-vector.19 tmp.42)
                           2110)))
                   (define L.make-init-vector.19
                     (lambda (tmp.43)
                       (if (!= (if (>= tmp.43 0) 14 6) 6)
                           (let ((tmp.44
                                  (let ((tmp.12
                                         (+
                                          (alloc (* (+ 1 (arithmetic-shift-right tmp.43 3)) 8))
                                          3)))
                                    (begin (mset! tmp.12 -3 tmp.43) tmp.12))))
                             (call L.vector-init-loop.20 tmp.43 0 tmp.44))
                           3134)))
                   (call L.make-vector.18 0)))
  (check-equal? (specify-representation '(module (define L.make-vector.28 (lambda (tmp.62) (if (fixnum? tmp.62) (call L.make-init-vector.29 tmp.62) (error 8)))) (define L.make-init-vector.29 (lambda (tmp.63) (if (unsafe-fx>= tmp.63 0) (let ((tmp.64 (unsafe-make-vector tmp.63))) (call L.vector-init-loop.30 tmp.63 0 tmp.64)) (error 12)))) (define L.vector-init-loop.30 (lambda (len.65 i.66 vec.67) (if (eq? len.65 i.66) vec.67 (begin (unsafe-vector-set! vec.67 i.66 0) (call L.vector-init-loop.30 len.65 (unsafe-fx+ i.66 1) vec.67))))) (call L.make-vector.28 2)))
                '(module
                     (define L.make-vector.28
                       (lambda (tmp.62)
                         (if (!= (if (= (bitwise-and tmp.62 7) 0) 14 6) 6)
                             (call L.make-init-vector.29 tmp.62)
                             2110)))
                   (define L.make-init-vector.29
                     (lambda (tmp.63)
                       (if (!= (if (>= tmp.63 0) 14 6) 6)
                           (let ((tmp.64
                                  (let ((tmp.13
                                         (+
                                          (alloc (* (+ 1 (arithmetic-shift-right tmp.63 3)) 8))
                                          3)))
                                    (begin (mset! tmp.13 -3 tmp.63) tmp.13))))
                             (call L.vector-init-loop.30 tmp.63 0 tmp.64))
                           3134)))
                   (define L.vector-init-loop.30
                     (lambda (len.65 i.66 vec.67)
                       (if (!= (if (= len.65 i.66) 14 6) 6)
                           vec.67
                           (begin
                             (mset! vec.67 (+ (* (arithmetic-shift-right i.66 3) 8) 5) 0)
                             (call L.vector-init-loop.30 len.65 (+ i.66 8) vec.67)))))
                   (call L.make-vector.28 16)))
  (check-equal? (interp-exprs-bits-lang-v8 (specify-representation
                                            '(module (define L.vector-init-loop.9 (lambda (len.10 i.11 vec.12) (if (eq? len.10 i.11) vec.12 (begin (unsafe-vector-set! vec.12 i.11 0) (call L.vector-init-loop.9 len.10 (unsafe-fx+ i.11 1) vec.12))))) (define L.vector-set!.10 (lambda (tmp.13 tmp.14 tmp.15) (if (fixnum? tmp.14) (if (vector? tmp.13) (call L.unsafe-vector-set!.11 tmp.13 tmp.14 tmp.15) (error 10)) (error 10)))) (define L.void?.15 (lambda (tmp.25) (void? tmp.25))) (define L.make-init-vector.8 (lambda (tmp.8) (if (unsafe-fx>= tmp.8 0) (let ((tmp.9 (unsafe-make-vector tmp.8))) (call L.vector-init-loop.9 tmp.8 0 tmp.9)) (error 12)))) (define L.+.14 (lambda (tmp.23 tmp.24) (if (fixnum? tmp.23) (if (fixnum? tmp.24) (unsafe-fx+ tmp.23 tmp.24) (error 2)) (error 2)))) (define L.unsafe-vector-set!.11 (lambda (tmp.16 tmp.17 tmp.18) (if (unsafe-fx< tmp.17 (unsafe-vector-length tmp.16)) (if (unsafe-fx>= tmp.17 0) (begin (unsafe-vector-set! tmp.16 tmp.17 tmp.18) (void)) (error 10)) (error 10)))) (define L.vector-ref.12 (lambda (tmp.19 tmp.20) (if (fixnum? tmp.20) (if (vector? tmp.19) (call L.unsafe-vector-ref.13 tmp.19 tmp.20) (error 11)) (error 11)))) (define L.make-vector.7 (lambda (tmp.7) (if (fixnum? tmp.7) (call L.make-init-vector.8 tmp.7) (error 8)))) (define L.unsafe-vector-ref.13 (lambda (tmp.21 tmp.22) (if (unsafe-fx< tmp.22 (unsafe-vector-length tmp.21)) (if (unsafe-fx>= tmp.22 0) (unsafe-vector-ref tmp.21 tmp.22) (error 11)) (error 11)))) (define L.v.4 (lambda () (call L.make-vector.7 3))) (define L.set-first.5 (lambda (vec.4) (call L.vector-set!.10 vec.4 0 42))) (define L.get-first.6 (lambda (vec.5) (call L.vector-ref.12 vec.5 0))) (let ((vec.6 (call L.v.4))) (call L.+.14 (if (call L.void?.15 (call L.set-first.5 vec.6)) 0 (error 1)) (call L.get-first.6 vec.6))))))
                (interp-exprs-bits-lang-v8
                 '(module
                      (define L.vector-init-loop.9
                        (lambda (len.10 i.11 vec.12)
                          (if (!= (if (= len.10 i.11) 14 6) 6)
                              vec.12
                              (begin
                                (mset! vec.12 (+ (* (arithmetic-shift-right i.11 3) 8) 5) 0)
                                (call L.vector-init-loop.9 len.10 (+ i.11 8) vec.12)))))
                    (define L.vector-set!.10
                      (lambda (tmp.13 tmp.14 tmp.15)
                        (if (!= (if (= (bitwise-and tmp.14 7) 0) 14 6) 6)
                            (if (!= (if (= (bitwise-and tmp.13 7) 3) 14 6) 6)
                                (call L.unsafe-vector-set!.11 tmp.13 tmp.14 tmp.15)
                                2622)
                            2622)))
                    (define L.void?.15
                      (lambda (tmp.25) (if (= (bitwise-and tmp.25 255) 30) 14 6)))
                    (define L.make-init-vector.8
                      (lambda (tmp.8)
                        (if (!= (if (>= tmp.8 0) 14 6) 6)
                            (let ((tmp.9
                                   (let ((tmp.1
                                          (+
                                           (alloc (* (+ 1 (arithmetic-shift-right tmp.8 3)) 8))
                                           3)))
                                     (begin (mset! tmp.1 -3 tmp.8) tmp.1))))
                              (call L.vector-init-loop.9 tmp.8 0 tmp.9))
                            3134)))
                    (define L.+.14
                      (lambda (tmp.23 tmp.24)
                        (if (!= (if (= (bitwise-and tmp.23 7) 0) 14 6) 6)
                            (if (!= (if (= (bitwise-and tmp.24 7) 0) 14 6) 6)
                                (+ tmp.23 tmp.24)
                                574)
                            574)))
                    (define L.unsafe-vector-set!.11
                      (lambda (tmp.16 tmp.17 tmp.18)
                        (if (!= (if (< tmp.17 (mref tmp.16 -3)) 14 6) 6)
                            (if (!= (if (>= tmp.17 0) 14 6) 6)
                                (begin
                                  (mset! tmp.16 (+ (* (arithmetic-shift-right tmp.17 3) 8) 5) tmp.18)
                                  30)
                                2622)
                            2622)))
                    (define L.vector-ref.12
                      (lambda (tmp.19 tmp.20)
                        (if (!= (if (= (bitwise-and tmp.20 7) 0) 14 6) 6)
                            (if (!= (if (= (bitwise-and tmp.19 7) 3) 14 6) 6)
                                (call L.unsafe-vector-ref.13 tmp.19 tmp.20)
                                2878)
                            2878)))
                    (define L.make-vector.7
                      (lambda (tmp.7)
                        (if (!= (if (= (bitwise-and tmp.7 7) 0) 14 6) 6)
                            (call L.make-init-vector.8 tmp.7)
                            2110)))
                    (define L.unsafe-vector-ref.13
                      (lambda (tmp.21 tmp.22)
                        (if (!= (if (< tmp.22 (mref tmp.21 -3)) 14 6) 6)
                            (if (!= (if (>= tmp.22 0) 14 6) 6)
                                (mref tmp.21 (+ (* (arithmetic-shift-right tmp.22 3) 8) 5))
                                2878)
                            2878)))
                    (define L.v.4 (lambda () (call L.make-vector.7 24)))
                    (define L.set-first.5 (lambda (vec.4) (call L.vector-set!.10 vec.4 0 336)))
                    (define L.get-first.6 (lambda (vec.5) (call L.vector-ref.12 vec.5 0)))
                    (let ((vec.6 (call L.v.4)))
                      (call
                       L.+.14
                       (if (!= (call L.void?.15 (call L.set-first.5 vec.6)) 6) 0 318)
                       (call L.get-first.6 vec.6))))))
  (check-equal? (interp-exprs-bits-lang-v8 (specify-representation
                                            '(module
                                                 (define L.+.12
                                                   (lambda (tmp.19 tmp.20)
                                                     (if (fixnum? tmp.20)
                                                         (if (fixnum? tmp.19) (unsafe-fx+ tmp.19 tmp.20) (error 2))
                                                         (error 2))))
                                               (define L.void?.11 (lambda (tmp.43) (void? tmp.43)))
                                               (define L.unsafe-vector-ref.3
                                                 (lambda (tmp.14 tmp.15)
                                                   (if (unsafe-fx< tmp.15 (unsafe-vector-length tmp.14))
                                                       (if (unsafe-fx>= tmp.15 0)
                                                           (unsafe-vector-ref tmp.14 tmp.15)
                                                           (error 11))
                                                       (error 11))))
                                               (define L.vector-ref.10
                                                 (lambda (tmp.36 tmp.37)
                                                   (if (fixnum? tmp.37)
                                                       (if (vector? tmp.36)
                                                           (call L.unsafe-vector-ref.3 tmp.36 tmp.37)
                                                           (error 11))
                                                       (error 11))))
                                               (define L.unsafe-vector-set!.2
                                                 (lambda (tmp.9 tmp.10 tmp.11)
                                                   (if (unsafe-fx< tmp.10 (unsafe-vector-length tmp.9))
                                                       (if (unsafe-fx>= tmp.10 0)
                                                           (begin (unsafe-vector-set! tmp.9 tmp.10 tmp.11) (void))
                                                           (error 10))
                                                       (error 10))))
                                               (define L.vector-set!.9
                                                 (lambda (tmp.33 tmp.34 tmp.35)
                                                   (if (fixnum? tmp.34)
                                                       (if (vector? tmp.33)
                                                           (call L.unsafe-vector-set!.2 tmp.33 tmp.34 tmp.35)
                                                           (error 10))
                                                       (error 10))))
                                               (define L.vector-init-loop.7
                                                 (lambda (len.6 i.8 vec.7)
                                                   (if (eq? len.6 i.8)
                                                       vec.7
                                                       (begin
                                                         (unsafe-vector-set! vec.7 i.8 0)
                                                         (call L.vector-init-loop.7 len.6 (unsafe-fx+ i.8 1) vec.7)))))
                                               (define L.make-init-vector.1
                                                 (lambda (tmp.4)
                                                   (if (unsafe-fx>= tmp.4 0)
                                                       (let ((tmp.5 (unsafe-make-vector tmp.4)))
                                                         (call L.vector-init-loop.7 tmp.4 0 tmp.5))
                                                       (error 12))))
                                               (define L.make-vector.8
                                                 (lambda (tmp.31)
                                                   (if (fixnum? tmp.31) (call L.make-init-vector.1 tmp.31) (error 8))))
                                               (define L.v.4 (lambda () (call L.make-vector.8 3)))
                                               (define L.set-first.5 (lambda (vec.1) (call L.vector-set!.9 vec.1 0 42)))
                                               (define L.get-first.6 (lambda (vec.2) (call L.vector-ref.10 vec.2 0)))
                                               (let ((vec.3 (call L.v.4)))
                                                 (call
                                                  L.+.12
                                                  (if (call L.void?.11 (call L.set-first.5 vec.3)) 0 (error 1))
                                                  (call L.get-first.6 vec.3))))))
                (interp-exprs-bits-lang-v8
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
  (check-equal?
   (specify-representation '(module
                                (define L.+.1
                                  (lambda (tmp.1 tmp.2)
                                    (if (fixnum? tmp.2)
                                        (if (fixnum? tmp.1) (make-procedure L.tmp.1 1 1) (error 2))
                                        (error 2))))
                              (call L.+.1 1 2)))
   '(module
        (define L.+.1
          (lambda (tmp.1 tmp.2)
            (if (!= (if (= (bitwise-and tmp.2 7) 0) 14 6) 6)
                (if (!= (if (= (bitwise-and tmp.1 7) 0) 14 6) 6)
                    (let ((tmp.16 (+ (alloc 24) 2)))
                      (begin (mset! tmp.16 -2 L.tmp.1) (mset! tmp.16 6 8) tmp.16))
                    574)
                574)))
      (call L.+.1 8 16))
   "incorrect program, just to test make-procedure")
  (check-equal?
   (specify-representation '(module
                                (define L.+.1
                                  (lambda (tmp.1 tmp.2)
                                    (if (fixnum? tmp.2)
                                        (if (fixnum? tmp.1) (unsafe-procedure-arity L.tmp.1) (error 2))
                                        (error 2))))
                              (call L.+.1 1 2)))
   '(module
        (define L.+.1
          (lambda (tmp.1 tmp.2)
            (if (!= (if (= (bitwise-and tmp.2 7) 0) 14 6) 6)
                (if (!= (if (= (bitwise-and tmp.1 7) 0) 14 6) 6) (mref L.tmp.1 6) 574)
                574)))
      (call L.+.1 8 16))
   "unsafe-procedure-arity call test basic")
  (check-equal?
   (specify-representation '(module
                                (define L.+.1
                                  (lambda (tmp.1 tmp.2)
                                    (if (fixnum? tmp.2)
                                        (if (fixnum? tmp.1) (unsafe-procedure-label L.tmp.1) (error 2))
                                        (error 2))))
                              (call L.+.1 1 2)))
   '(module
        (define L.+.1
          (lambda (tmp.1 tmp.2)
            (if (!= (if (= (bitwise-and tmp.2 7) 0) 14 6) 6)
                (if (!= (if (= (bitwise-and tmp.1 7) 0) 14 6) 6) (mref L.tmp.1 -2) 574)
                574)))
      (call L.+.1 8 16))
   "unsafe-procedure-label call test basic")
  (check-equal?
   (specify-representation '(module
                                (define L.+.1
                                  (lambda (tmp.1 tmp.2)
                                    (if (fixnum? tmp.2)
                                        (if (fixnum? tmp.1) (unsafe-procedure-ref L.tmp.1 2) (error 2))
                                        (error 2))))
                              (call L.+.1 1 2)))
   '(module
        (define L.+.1
          (lambda (tmp.1 tmp.2)
            (if (!= (if (= (bitwise-and tmp.2 7) 0) 14 6) 6)
                (if (!= (if (= (bitwise-and tmp.1 7) 0) 14 6) 6) (mref L.tmp.1 30) 574)
                574)))
      (call L.+.1 8 16))
   "unsafe-procedure-ref call test basic")
  (check-equal?
   (specify-representation '(module
                                (define L.x.1.7
                                  (lambda (c.4)
                                    (let ((x.1 (unsafe-procedure-ref c.4 0))) (call L.x.1.7 x.1))))
                              (let ((x.1 (make-procedure L.x.1.7 0 0)))
                                (begin (unsafe-procedure-set! x.1 0 x.1) x.1))))
   '(module
        (define L.x.1.7
          (lambda (c.4) (let ((x.1 (mref c.4 14))) (call L.x.1.7 x.1))))
      (let ((x.1
             (let ((tmp.17 (+ (alloc 16) 2)))
               (begin (mset! tmp.17 -2 L.x.1.7) (mset! tmp.17 6 0) tmp.17))))
        (begin (mset! x.1 14 x.1) x.1)))
   "make-procedure 0 n")
  (check-equal?
   (specify-representation '(module
                                (define L.x.1.7
                                  (lambda (c.4)
                                    (let ((x.1 (unsafe-procedure-ref c.4 0))) (call L.x.1.7 x.1))))
                              (let ((x.1 (make-procedure L.x.1.7 0 2)))
                                (begin (unsafe-procedure-set! x.1 0 x.1) x.1))))
   '(module
        (define L.x.1.7
          (lambda (c.4) (let ((x.1 (mref c.4 14))) (call L.x.1.7 x.1))))
      (let ((x.1
             (let ((tmp.18 (+ (alloc 32) 2)))
               (begin (mset! tmp.18 -2 L.x.1.7) (mset! tmp.18 6 0) tmp.18))))
        (begin (mset! x.1 14 x.1) x.1)))
   "unsafe-procedure-set! call test")
  (check-equal?
   (specify-representation '(module
                                (define L.x.1.7
                                  (lambda (c.4)
                                    (let ((x.1 (unsafe-procedure-ref c.4 x.4))) (call L.x.1.7 x.1))))
                              (let ((x.1 (make-procedure L.x.1.7 0 0)))
                                (begin (unsafe-procedure-set! x.1 0 x.1) x.1))))
   '(module
        (define L.x.1.7
          (lambda (c.4)
            (let ((x.1 (mref c.4 (+ (* (arithmetic-shift-right x.4 3) 8) 14))))
              (call L.x.1.7 x.1))))
      (let ((x.1
             (let ((tmp.19 (+ (alloc 16) 2)))
               (begin (mset! tmp.19 -2 L.x.1.7) (mset! tmp.19 6 0) tmp.19))))
        (begin (mset! x.1 14 x.1) x.1)))
   "unsafe-procedure-ref with aloc in i")
  (check-equal? (specify-representation '(module (define L.tmp.36 (lambda (tmp.316 x.30) (let ((*.219 (unsafe-procedure-ref tmp.316 0)) (fact.29 (unsafe-procedure-ref tmp.316 1)) (|+.216| (unsafe-procedure-ref tmp.316 2)) (eq?.213 (unsafe-procedure-ref tmp.316 3))) (if (call (unsafe-procedure-label eq?.213) eq?.213 x.30 0) 1 (let ((z.31 (call (unsafe-procedure-label |+.216|) |+.216| x.30 -1))) (let ((y.32 (call (unsafe-procedure-label fact.29) fact.29 z.31))) (call (unsafe-procedure-label *.219) *.219 x.30 y.32))))))) (define L.tmp.35 (lambda (tmp.315 tmp.214 tmp.215) (let () (eq? tmp.214 tmp.215)))) (define L.tmp.34 (lambda (tmp.314 tmp.217 tmp.218) (let () (if (fixnum? tmp.217) (if (fixnum? tmp.218) (unsafe-fx+ tmp.217 tmp.218) (error 2)) (error 2))))) (define L.tmp.33 (lambda (tmp.313 tmp.220 tmp.221) (let () (if (fixnum? tmp.220) (if (fixnum? tmp.221) (unsafe-fx* tmp.220 tmp.221) (error 1)) (error 1))))) (let ((*.219 (make-procedure L.tmp.33 2 0)) (|+.216| (make-procedure L.tmp.34 2 0)) (eq?.213 (make-procedure L.tmp.35 2 0)) (fact.29 (make-procedure L.tmp.36 1 4))) (begin (unsafe-procedure-set! fact.29 0 *.219) (unsafe-procedure-set! fact.29 1 fact.29) (unsafe-procedure-set! fact.29 2 |+.216|) (unsafe-procedure-set! fact.29 3 eq?.213) (call (unsafe-procedure-label fact.29) fact.29 5)))))
                '(module
                     (define L.tmp.36
                       (lambda (tmp.316 x.30)
                         (let ((*.219 (mref tmp.316 14))
                               (fact.29 (mref tmp.316 22))
                               (|+.216| (mref tmp.316 30))
                               (eq?.213 (mref tmp.316 38)))
                           (if (!= (call (mref eq?.213 -2) eq?.213 x.30 0) 6)
                               8
                               (let ((z.31 (call (mref |+.216| -2) |+.216| x.30 -8)))
                                 (let ((y.32 (call (mref fact.29 -2) fact.29 z.31)))
                                   (call (mref *.219 -2) *.219 x.30 y.32)))))))
                   (define L.tmp.35
                     (lambda (tmp.315 tmp.214 tmp.215) (let () (if (= tmp.214 tmp.215) 14 6))))
                   (define L.tmp.34
                     (lambda (tmp.314 tmp.217 tmp.218)
                       (let ()
                         (if (!= (if (= (bitwise-and tmp.217 7) 0) 14 6) 6)
                             (if (!= (if (= (bitwise-and tmp.218 7) 0) 14 6) 6)
                                 (+ tmp.217 tmp.218)
                                 574)
                             574))))
                   (define L.tmp.33
                     (lambda (tmp.313 tmp.220 tmp.221)
                       (let ()
                         (if (!= (if (= (bitwise-and tmp.220 7) 0) 14 6) 6)
                             (if (!= (if (= (bitwise-and tmp.221 7) 0) 14 6) 6)
                                 (* tmp.220 (arithmetic-shift-right tmp.221 3))
                                 318)
                             318))))
                   (let ((*.219
                          (let ((tmp.20 (+ (alloc 16) 2)))
                            (begin (mset! tmp.20 -2 L.tmp.33) (mset! tmp.20 6 16) tmp.20)))
                         (|+.216|
                          (let ((tmp.21 (+ (alloc 16) 2)))
                            (begin (mset! tmp.21 -2 L.tmp.34) (mset! tmp.21 6 16) tmp.21)))
                         (eq?.213
                          (let ((tmp.22 (+ (alloc 16) 2)))
                            (begin (mset! tmp.22 -2 L.tmp.35) (mset! tmp.22 6 16) tmp.22)))
                         (fact.29
                          (let ((tmp.23 (+ (alloc 48) 2)))
                            (begin (mset! tmp.23 -2 L.tmp.36) (mset! tmp.23 6 8) tmp.23))))
                     (begin
                       (mset! fact.29 14 *.219)
                       (mset! fact.29 22 fact.29)
                       (mset! fact.29 30 |+.216|)
                       (mset! fact.29 38 eq?.213)
                       (call (mref fact.29 -2) fact.29 40)))))
  (check-equal? (specify-representation '(module (define L.tmp.30 (lambda (tmp.310 x.28) (let ((|+.166| (unsafe-procedure-ref tmp.310 0))) (call (unsafe-procedure-label |+.166|) |+.166| 1 x.28)))) (define L.tmp.29 (lambda (tmp.309 f.26 ls.27) (let ((cons.159 (unsafe-procedure-ref tmp.309 0)) (map.25 (unsafe-procedure-ref tmp.309 1)) (cdr.164 (unsafe-procedure-ref tmp.309 2)) (car.162 (unsafe-procedure-ref tmp.309 3)) (eq?.156 (unsafe-procedure-ref tmp.309 4))) (if (call (unsafe-procedure-label eq?.156) eq?.156 empty ls.27) empty (call (unsafe-procedure-label cons.159) cons.159 (if (procedure? f.26) (if (eq? (unsafe-procedure-arity f.26) 1) (call (unsafe-procedure-label f.26) f.26 (call (unsafe-procedure-label car.162) car.162 ls.27)) (error 42)) (error 43)) (call (unsafe-procedure-label map.25) map.25 f.26 (call (unsafe-procedure-label cdr.164) cdr.164 ls.27))))))) (define L.tmp.28 (lambda (tmp.308 tmp.160 tmp.161) (let () (cons tmp.160 tmp.161)))) (define L.tmp.27 (lambda (tmp.307 tmp.165) (let () (if (pair? tmp.165) (unsafe-cdr tmp.165) (error 13))))) (define L.tmp.26 (lambda (tmp.306 tmp.157 tmp.158) (let () (eq? tmp.157 tmp.158)))) (define L.tmp.25 (lambda (tmp.305 tmp.167 tmp.168) (let () (if (fixnum? tmp.167) (if (fixnum? tmp.168) (unsafe-fx+ tmp.167 tmp.168) (error 2)) (error 2))))) (define L.tmp.24 (lambda (tmp.304 tmp.163) (let () (if (pair? tmp.163) (unsafe-car tmp.163) (error 12))))) (let ((car.162 (make-procedure L.tmp.24 1 0)) (|+.166| (make-procedure L.tmp.25 2 0)) (eq?.156 (make-procedure L.tmp.26 2 0)) (cdr.164 (make-procedure L.tmp.27 1 0)) (cons.159 (make-procedure L.tmp.28 2 0)) (map.25 (make-procedure L.tmp.29 2 5))) (begin (unsafe-procedure-set! map.25 0 cons.159) (unsafe-procedure-set! map.25 1 map.25) (unsafe-procedure-set! map.25 2 cdr.164) (unsafe-procedure-set! map.25 3 car.162) (unsafe-procedure-set! map.25 4 eq?.156) (call (unsafe-procedure-label map.25) map.25 (let ((lam.278 (make-procedure L.tmp.30 1 1))) (begin (unsafe-procedure-set! lam.278 0 |+.166|) lam.278)) (call (unsafe-procedure-label cons.159) cons.159 1 empty))))))
                '(module
                     (define L.tmp.30
                       (lambda (tmp.310 x.28)
                         (let ((|+.166| (mref tmp.310 14)))
                           (call (mref |+.166| -2) |+.166| 8 x.28))))
                   (define L.tmp.29
                     (lambda (tmp.309 f.26 ls.27)
                       (let ((cons.159 (mref tmp.309 14))
                             (map.25 (mref tmp.309 22))
                             (cdr.164 (mref tmp.309 30))
                             (car.162 (mref tmp.309 38))
                             (eq?.156 (mref tmp.309 46)))
                         (if (!= (call (mref eq?.156 -2) eq?.156 22 ls.27) 6)
                             22
                             (call
                              (mref cons.159 -2)
                              cons.159
                              (if (!= (if (= (bitwise-and f.26 7) 2) 14 6) 6)
                                  (if (!= (if (= (mref f.26 6) 8) 14 6) 6)
                                      (call
                                       (mref f.26 -2)
                                       f.26
                                       (call (mref car.162 -2) car.162 ls.27))
                                      10814)
                                  11070)
                              (call
                               (mref map.25 -2)
                               map.25
                               f.26
                               (call (mref cdr.164 -2) cdr.164 ls.27)))))))
                   (define L.tmp.28
                     (lambda (tmp.308 tmp.160 tmp.161)
                       (let ()
                         (let ((tmp.24 (+ (alloc 16) 1)))
                           (begin
                             (mset! tmp.24 -1 tmp.160)
                             (mset! tmp.24 7 tmp.161)
                             tmp.24)))))
                   (define L.tmp.27
                     (lambda (tmp.307 tmp.165)
                       (let ()
                         (if (!= (if (= (bitwise-and tmp.165 7) 1) 14 6) 6)
                             (mref tmp.165 7)
                             3390))))
                   (define L.tmp.26
                     (lambda (tmp.306 tmp.157 tmp.158) (let () (if (= tmp.157 tmp.158) 14 6))))
                   (define L.tmp.25
                     (lambda (tmp.305 tmp.167 tmp.168)
                       (let ()
                         (if (!= (if (= (bitwise-and tmp.167 7) 0) 14 6) 6)
                             (if (!= (if (= (bitwise-and tmp.168 7) 0) 14 6) 6)
                                 (+ tmp.167 tmp.168)
                                 574)
                             574))))
                   (define L.tmp.24
                     (lambda (tmp.304 tmp.163)
                       (let ()
                         (if (!= (if (= (bitwise-and tmp.163 7) 1) 14 6) 6)
                             (mref tmp.163 -1)
                             3134))))
                   (let ((car.162
                          (let ((tmp.25 (+ (alloc 16) 2)))
                            (begin (mset! tmp.25 -2 L.tmp.24) (mset! tmp.25 6 8) tmp.25)))
                         (|+.166|
                          (let ((tmp.26 (+ (alloc 16) 2)))
                            (begin (mset! tmp.26 -2 L.tmp.25) (mset! tmp.26 6 16) tmp.26)))
                         (eq?.156
                          (let ((tmp.27 (+ (alloc 16) 2)))
                            (begin (mset! tmp.27 -2 L.tmp.26) (mset! tmp.27 6 16) tmp.27)))
                         (cdr.164
                          (let ((tmp.28 (+ (alloc 16) 2)))
                            (begin (mset! tmp.28 -2 L.tmp.27) (mset! tmp.28 6 8) tmp.28)))
                         (cons.159
                          (let ((tmp.29 (+ (alloc 16) 2)))
                            (begin (mset! tmp.29 -2 L.tmp.28) (mset! tmp.29 6 16) tmp.29)))
                         (map.25
                          (let ((tmp.30 (+ (alloc 56) 2)))
                            (begin (mset! tmp.30 -2 L.tmp.29) (mset! tmp.30 6 16) tmp.30))))
                     (begin
                       (mset! map.25 14 cons.159)
                       (mset! map.25 22 map.25)
                       (mset! map.25 30 cdr.164)
                       (mset! map.25 38 car.162)
                       (mset! map.25 46 eq?.156)
                       (call
                        (mref map.25 -2)
                        map.25
                        (let ((lam.278
                               (let ((tmp.31 (+ (alloc 24) 2)))
                                 (begin
                                   (mset! tmp.31 -2 L.tmp.30)
                                   (mset! tmp.31 6 8)
                                   tmp.31))))
                          (begin (mset! lam.278 14 |+.166|) lam.278))
                        (call (mref cons.159 -2) cons.159 8 22))))))
  (check-equal? (specify-representation '(module (define L.tmp.42 (lambda (tmp.322 x.42) (let ((*.178 (unsafe-procedure-ref tmp.322 0)) (fact.38 (unsafe-procedure-ref tmp.322 1)) (|-.172| (unsafe-procedure-ref tmp.322 2)) (identity.37 (unsafe-procedure-ref tmp.322 3)) (eq?.169 (unsafe-procedure-ref tmp.322 4))) (let ((x.43 (call (unsafe-procedure-label identity.37) identity.37 x.42)) (y.44 (call (unsafe-procedure-label identity.37) identity.37 0))) (if (call (unsafe-procedure-label eq?.169) eq?.169 x.43 y.44) (let ((z.45 (call (unsafe-procedure-label identity.37) identity.37 1))) z.45) (let ((n.46 (call (unsafe-procedure-label identity.37) identity.37 1))) (let ((z.47 (call (unsafe-procedure-label |-.172|) |-.172| x.43 n.46))) (let ((y.48 (call (unsafe-procedure-label fact.38) fact.38 z.47))) (call (unsafe-procedure-label *.178) *.178 x.43 y.48))))))))) (define L.tmp.41 (lambda (tmp.321 x.39) (let ((|+.175| (unsafe-procedure-ref tmp.321 0)) (identity.37 (unsafe-procedure-ref tmp.321 1)) (|-.172| (unsafe-procedure-ref tmp.321 2)) (eq?.169 (unsafe-procedure-ref tmp.321 3))) (if (call (unsafe-procedure-label eq?.169) eq?.169 x.39 0) 0 (let ((y.40 (call (unsafe-procedure-label |-.172|) |-.172| x.39 1))) (let ((x.41 (call (unsafe-procedure-label identity.37) identity.37 y.40))) (call (unsafe-procedure-label |+.175|) |+.175| 1 x.41))))))) (define L.tmp.40 (lambda (tmp.320 tmp.170 tmp.171) (let () (eq? tmp.170 tmp.171)))) (define L.tmp.39 (lambda (tmp.319 tmp.173 tmp.174) (let () (if (fixnum? tmp.173) (if (fixnum? tmp.174) (unsafe-fx- tmp.173 tmp.174) (error 3)) (error 3))))) (define L.tmp.38 (lambda (tmp.318 tmp.176 tmp.177) (let () (if (fixnum? tmp.176) (if (fixnum? tmp.177) (unsafe-fx+ tmp.176 tmp.177) (error 2)) (error 2))))) (define L.tmp.37 (lambda (tmp.317 tmp.179 tmp.180) (let () (if (fixnum? tmp.179) (if (fixnum? tmp.180) (unsafe-fx* tmp.179 tmp.180) (error 1)) (error 1))))) (let ((*.178 (make-procedure L.tmp.37 2 0)) (|+.175| (make-procedure L.tmp.38 2 0)) (|-.172| (make-procedure L.tmp.39 2 0)) (eq?.169 (make-procedure L.tmp.40 2 0)) (identity.37 (make-procedure L.tmp.41 1 4)) (fact.38 (make-procedure L.tmp.42 1 5))) (begin (unsafe-procedure-set! identity.37 0 |+.175|) (unsafe-procedure-set! identity.37 1 identity.37) (unsafe-procedure-set! identity.37 2 |-.172|) (unsafe-procedure-set! identity.37 3 eq?.169) (unsafe-procedure-set! fact.38 0 *.178) (unsafe-procedure-set! fact.38 1 fact.38) (unsafe-procedure-set! fact.38 2 |-.172|) (unsafe-procedure-set! fact.38 3 identity.37) (unsafe-procedure-set! fact.38 4 eq?.169) (call (unsafe-procedure-label fact.38) fact.38 5)))))
                '(module
                     (define L.tmp.42
                       (lambda (tmp.322 x.42)
                         (let ((*.178 (mref tmp.322 14))
                               (fact.38 (mref tmp.322 22))
                               (|-.172| (mref tmp.322 30))
                               (identity.37 (mref tmp.322 38))
                               (eq?.169 (mref tmp.322 46)))
                           (let ((x.43 (call (mref identity.37 -2) identity.37 x.42))
                                 (y.44 (call (mref identity.37 -2) identity.37 0)))
                             (if (!= (call (mref eq?.169 -2) eq?.169 x.43 y.44) 6)
                                 (let ((z.45 (call (mref identity.37 -2) identity.37 8))) z.45)
                                 (let ((n.46 (call (mref identity.37 -2) identity.37 8)))
                                   (let ((z.47 (call (mref |-.172| -2) |-.172| x.43 n.46)))
                                     (let ((y.48 (call (mref fact.38 -2) fact.38 z.47)))
                                       (call (mref *.178 -2) *.178 x.43 y.48)))))))))
                   (define L.tmp.41
                     (lambda (tmp.321 x.39)
                       (let ((|+.175| (mref tmp.321 14))
                             (identity.37 (mref tmp.321 22))
                             (|-.172| (mref tmp.321 30))
                             (eq?.169 (mref tmp.321 38)))
                         (if (!= (call (mref eq?.169 -2) eq?.169 x.39 0) 6)
                             0
                             (let ((y.40 (call (mref |-.172| -2) |-.172| x.39 8)))
                               (let ((x.41 (call (mref identity.37 -2) identity.37 y.40)))
                                 (call (mref |+.175| -2) |+.175| 8 x.41)))))))
                   (define L.tmp.40
                     (lambda (tmp.320 tmp.170 tmp.171) (let () (if (= tmp.170 tmp.171) 14 6))))
                   (define L.tmp.39
                     (lambda (tmp.319 tmp.173 tmp.174)
                       (let ()
                         (if (!= (if (= (bitwise-and tmp.173 7) 0) 14 6) 6)
                             (if (!= (if (= (bitwise-and tmp.174 7) 0) 14 6) 6)
                                 (- tmp.173 tmp.174)
                                 830)
                             830))))
                   (define L.tmp.38
                     (lambda (tmp.318 tmp.176 tmp.177)
                       (let ()
                         (if (!= (if (= (bitwise-and tmp.176 7) 0) 14 6) 6)
                             (if (!= (if (= (bitwise-and tmp.177 7) 0) 14 6) 6)
                                 (+ tmp.176 tmp.177)
                                 574)
                             574))))
                   (define L.tmp.37
                     (lambda (tmp.317 tmp.179 tmp.180)
                       (let ()
                         (if (!= (if (= (bitwise-and tmp.179 7) 0) 14 6) 6)
                             (if (!= (if (= (bitwise-and tmp.180 7) 0) 14 6) 6)
                                 (* tmp.179 (arithmetic-shift-right tmp.180 3))
                                 318)
                             318))))
                   (let ((*.178
                          (let ((tmp.32 (+ (alloc 16) 2)))
                            (begin (mset! tmp.32 -2 L.tmp.37) (mset! tmp.32 6 16) tmp.32)))
                         (|+.175|
                          (let ((tmp.33 (+ (alloc 16) 2)))
                            (begin (mset! tmp.33 -2 L.tmp.38) (mset! tmp.33 6 16) tmp.33)))
                         (|-.172|
                          (let ((tmp.34 (+ (alloc 16) 2)))
                            (begin (mset! tmp.34 -2 L.tmp.39) (mset! tmp.34 6 16) tmp.34)))
                         (eq?.169
                          (let ((tmp.35 (+ (alloc 16) 2)))
                            (begin (mset! tmp.35 -2 L.tmp.40) (mset! tmp.35 6 16) tmp.35)))
                         (identity.37
                          (let ((tmp.36 (+ (alloc 48) 2)))
                            (begin (mset! tmp.36 -2 L.tmp.41) (mset! tmp.36 6 8) tmp.36)))
                         (fact.38
                          (let ((tmp.37 (+ (alloc 56) 2)))
                            (begin (mset! tmp.37 -2 L.tmp.42) (mset! tmp.37 6 8) tmp.37))))
                     (begin
                       (mset! identity.37 14 |+.175|)
                       (mset! identity.37 22 identity.37)
                       (mset! identity.37 30 |-.172|)
                       (mset! identity.37 38 eq?.169)
                       (mset! fact.38 14 *.178)
                       (mset! fact.38 22 fact.38)
                       (mset! fact.38 30 |-.172|)
                       (mset! fact.38 38 identity.37)
                       (mset! fact.38 46 eq?.169)
                       (call (mref fact.38 -2) fact.38 40)))))
  (check-equal? (specify-representation '(module (define L.tmp.21 (lambda (tmp.301 x.52) (let ((|+.191| (unsafe-procedure-ref tmp.301 0))) (call (unsafe-procedure-label |+.191|) |+.191| 1 x.52)))) (define L.tmp.20 (lambda (tmp.300 f.50 ls.51) (let ((cons.184 (unsafe-procedure-ref tmp.300 0)) (map.49 (unsafe-procedure-ref tmp.300 1)) (cdr.189 (unsafe-procedure-ref tmp.300 2)) (car.187 (unsafe-procedure-ref tmp.300 3)) (eq?.181 (unsafe-procedure-ref tmp.300 4))) (if (call (unsafe-procedure-label eq?.181) eq?.181 empty ls.51) empty (call (unsafe-procedure-label cons.184) cons.184 (if (procedure? f.50) (if (eq? (unsafe-procedure-arity f.50) 1) (call (unsafe-procedure-label f.50) f.50 (call (unsafe-procedure-label car.187) car.187 ls.51)) (error 42)) (error 43)) (call (unsafe-procedure-label map.49) map.49 f.50 (call (unsafe-procedure-label cdr.189) cdr.189 ls.51))))))) (define L.tmp.19 (lambda (tmp.299 tmp.185 tmp.186) (let () (cons tmp.185 tmp.186)))) (define L.tmp.18 (lambda (tmp.298 tmp.190) (let () (if (pair? tmp.190) (unsafe-cdr tmp.190) (error 13))))) (define L.tmp.17 (lambda (tmp.297 tmp.182 tmp.183) (let () (eq? tmp.182 tmp.183)))) (define L.tmp.16 (lambda (tmp.296 tmp.192 tmp.193) (let () (if (fixnum? tmp.192) (if (fixnum? tmp.193) (unsafe-fx+ tmp.192 tmp.193) (error 2)) (error 2))))) (define L.tmp.15 (lambda (tmp.295 tmp.188) (let () (if (pair? tmp.188) (unsafe-car tmp.188) (error 12))))) (let ((car.187 (make-procedure L.tmp.15 1 0)) (|+.191| (make-procedure L.tmp.16 2 0)) (eq?.181 (make-procedure L.tmp.17 2 0)) (cdr.189 (make-procedure L.tmp.18 1 0)) (cons.184 (make-procedure L.tmp.19 2 0)) (map.49 (make-procedure L.tmp.20 2 5))) (begin (unsafe-procedure-set! map.49 0 cons.184) (unsafe-procedure-set! map.49 1 map.49) (unsafe-procedure-set! map.49 2 cdr.189) (unsafe-procedure-set! map.49 3 car.187) (unsafe-procedure-set! map.49 4 eq?.181) (call (unsafe-procedure-label map.49) map.49 (let ((lam.279 (make-procedure L.tmp.21 1 1))) (begin (unsafe-procedure-set! lam.279 0 |+.191|) lam.279)) (call (unsafe-procedure-label cons.184) cons.184 1 (call (unsafe-procedure-label cons.184) cons.184 2 (call (unsafe-procedure-label cons.184) cons.184 3 empty))))))))
                '(module
                     (define L.tmp.21
                       (lambda (tmp.301 x.52)
                         (let ((|+.191| (mref tmp.301 14)))
                           (call (mref |+.191| -2) |+.191| 8 x.52))))
                   (define L.tmp.20
                     (lambda (tmp.300 f.50 ls.51)
                       (let ((cons.184 (mref tmp.300 14))
                             (map.49 (mref tmp.300 22))
                             (cdr.189 (mref tmp.300 30))
                             (car.187 (mref tmp.300 38))
                             (eq?.181 (mref tmp.300 46)))
                         (if (!= (call (mref eq?.181 -2) eq?.181 22 ls.51) 6)
                             22
                             (call
                              (mref cons.184 -2)
                              cons.184
                              (if (!= (if (= (bitwise-and f.50 7) 2) 14 6) 6)
                                  (if (!= (if (= (mref f.50 6) 8) 14 6) 6)
                                      (call
                                       (mref f.50 -2)
                                       f.50
                                       (call (mref car.187 -2) car.187 ls.51))
                                      10814)
                                  11070)
                              (call
                               (mref map.49 -2)
                               map.49
                               f.50
                               (call (mref cdr.189 -2) cdr.189 ls.51)))))))
                   (define L.tmp.19
                     (lambda (tmp.299 tmp.185 tmp.186)
                       (let ()
                         (let ((tmp.38 (+ (alloc 16) 1)))
                           (begin
                             (mset! tmp.38 -1 tmp.185)
                             (mset! tmp.38 7 tmp.186)
                             tmp.38)))))
                   (define L.tmp.18
                     (lambda (tmp.298 tmp.190)
                       (let ()
                         (if (!= (if (= (bitwise-and tmp.190 7) 1) 14 6) 6)
                             (mref tmp.190 7)
                             3390))))
                   (define L.tmp.17
                     (lambda (tmp.297 tmp.182 tmp.183) (let () (if (= tmp.182 tmp.183) 14 6))))
                   (define L.tmp.16
                     (lambda (tmp.296 tmp.192 tmp.193)
                       (let ()
                         (if (!= (if (= (bitwise-and tmp.192 7) 0) 14 6) 6)
                             (if (!= (if (= (bitwise-and tmp.193 7) 0) 14 6) 6)
                                 (+ tmp.192 tmp.193)
                                 574)
                             574))))
                   (define L.tmp.15
                     (lambda (tmp.295 tmp.188)
                       (let ()
                         (if (!= (if (= (bitwise-and tmp.188 7) 1) 14 6) 6)
                             (mref tmp.188 -1)
                             3134))))
                   (let ((car.187
                          (let ((tmp.39 (+ (alloc 16) 2)))
                            (begin (mset! tmp.39 -2 L.tmp.15) (mset! tmp.39 6 8) tmp.39)))
                         (|+.191|
                          (let ((tmp.40 (+ (alloc 16) 2)))
                            (begin (mset! tmp.40 -2 L.tmp.16) (mset! tmp.40 6 16) tmp.40)))
                         (eq?.181
                          (let ((tmp.41 (+ (alloc 16) 2)))
                            (begin (mset! tmp.41 -2 L.tmp.17) (mset! tmp.41 6 16) tmp.41)))
                         (cdr.189
                          (let ((tmp.42 (+ (alloc 16) 2)))
                            (begin (mset! tmp.42 -2 L.tmp.18) (mset! tmp.42 6 8) tmp.42)))
                         (cons.184
                          (let ((tmp.43 (+ (alloc 16) 2)))
                            (begin (mset! tmp.43 -2 L.tmp.19) (mset! tmp.43 6 16) tmp.43)))
                         (map.49
                          (let ((tmp.44 (+ (alloc 56) 2)))
                            (begin (mset! tmp.44 -2 L.tmp.20) (mset! tmp.44 6 16) tmp.44))))
                     (begin
                       (mset! map.49 14 cons.184)
                       (mset! map.49 22 map.49)
                       (mset! map.49 30 cdr.189)
                       (mset! map.49 38 car.187)
                       (mset! map.49 46 eq?.181)
                       (call
                        (mref map.49 -2)
                        map.49
                        (let ((lam.279
                               (let ((tmp.45 (+ (alloc 24) 2)))
                                 (begin
                                   (mset! tmp.45 -2 L.tmp.21)
                                   (mset! tmp.45 6 8)
                                   tmp.45))))
                          (begin (mset! lam.279 14 |+.191|) lam.279))
                        (call
                         (mref cons.184 -2)
                         cons.184
                         8
                         (call
                          (mref cons.184 -2)
                          cons.184
                          16
                          (call (mref cons.184 -2) cons.184 24 22))))))))
  (check-equal? (specify-representation '(module (define L.tmp.48 (lambda (tmp.329 x.57 y.58) (let ((swap.56 (unsafe-procedure-ref tmp.329 0)) (<.108 (unsafe-procedure-ref tmp.329 1))) (if (call (unsafe-procedure-label <.108) <.108 y.58 x.57) x.57 (let ((z.59 (call (unsafe-procedure-label swap.56) swap.56 y.58 x.57))) z.59))))) (define L.tmp.47 (lambda (tmp.328 tmp.109 tmp.110) (let () (if (fixnum? tmp.109) (if (fixnum? tmp.110) (unsafe-fx< tmp.109 tmp.110) (error 4)) (error 4))))) (let ((<.108 (make-procedure L.tmp.47 2 0)) (swap.56 (make-procedure L.tmp.48 2 2))) (begin (unsafe-procedure-set! swap.56 0 swap.56) (unsafe-procedure-set! swap.56 1 <.108) (call (unsafe-procedure-label swap.56) swap.56 1 2)))))
                '(module
                     (define L.tmp.48
                       (lambda (tmp.329 x.57 y.58)
                         (let ((swap.56 (mref tmp.329 14)) (<.108 (mref tmp.329 22)))
                           (if (!= (call (mref <.108 -2) <.108 y.58 x.57) 6)
                               x.57
                               (let ((z.59 (call (mref swap.56 -2) swap.56 y.58 x.57))) z.59)))))
                   (define L.tmp.47
                     (lambda (tmp.328 tmp.109 tmp.110)
                       (let ()
                         (if (!= (if (= (bitwise-and tmp.109 7) 0) 14 6) 6)
                             (if (!= (if (= (bitwise-and tmp.110 7) 0) 14 6) 6)
                                 (if (< tmp.109 tmp.110) 14 6)
                                 1086)
                             1086))))
                   (let ((<.108
                          (let ((tmp.46 (+ (alloc 16) 2)))
                            (begin (mset! tmp.46 -2 L.tmp.47) (mset! tmp.46 6 16) tmp.46)))
                         (swap.56
                          (let ((tmp.47 (+ (alloc 32) 2)))
                            (begin (mset! tmp.47 -2 L.tmp.48) (mset! tmp.47 6 16) tmp.47))))
                     (begin
                       (mset! swap.56 14 swap.56)
                       (mset! swap.56 22 <.108)
                       (call (mref swap.56 -2) swap.56 8 16)))))
  (check-equal? (specify-representation '(module (define L.tmp.4 (lambda (tmp.284 x.34) (let ((*.105 (unsafe-procedure-ref tmp.284 0)) (fact.33 (unsafe-procedure-ref tmp.284 1)) (|+.102| (unsafe-procedure-ref tmp.284 2)) (eq?.99 (unsafe-procedure-ref tmp.284 3))) (if (call (unsafe-procedure-label eq?.99) eq?.99 x.34 0) 1 (let ((z.35 (call (unsafe-procedure-label |+.102|) |+.102| x.34 -1))) (let ((y.36 (call (unsafe-procedure-label fact.33) fact.33 z.35))) (call (unsafe-procedure-label *.105) *.105 x.34 y.36))))))) (define L.tmp.3 (lambda (tmp.283 tmp.100 tmp.101) (let () (eq? tmp.100 tmp.101)))) (define L.tmp.2 (lambda (tmp.282 tmp.103 tmp.104) (let () (if (fixnum? tmp.103) (if (fixnum? tmp.104) (unsafe-fx+ tmp.103 tmp.104) (error 2)) (error 2))))) (define L.tmp.1 (lambda (tmp.281 tmp.106 tmp.107) (let () (if (fixnum? tmp.106) (if (fixnum? tmp.107) (unsafe-fx* tmp.106 tmp.107) (error 1)) (error 1))))) (let ((*.105 (make-procedure L.tmp.1 2 0)) (|+.102| (make-procedure L.tmp.2 2 0)) (eq?.99 (make-procedure L.tmp.3 2 0)) (fact.33 (make-procedure L.tmp.4 1 4))) (begin (unsafe-procedure-set! fact.33 0 *.105) (unsafe-procedure-set! fact.33 1 fact.33) (unsafe-procedure-set! fact.33 2 |+.102|) (unsafe-procedure-set! fact.33 3 eq?.99) (call (unsafe-procedure-label fact.33) fact.33 10)))))
                '(module
                     (define L.tmp.4
                       (lambda (tmp.284 x.34)
                         (let ((*.105 (mref tmp.284 14))
                               (fact.33 (mref tmp.284 22))
                               (|+.102| (mref tmp.284 30))
                               (eq?.99 (mref tmp.284 38)))
                           (if (!= (call (mref eq?.99 -2) eq?.99 x.34 0) 6)
                               8
                               (let ((z.35 (call (mref |+.102| -2) |+.102| x.34 -8)))
                                 (let ((y.36 (call (mref fact.33 -2) fact.33 z.35)))
                                   (call (mref *.105 -2) *.105 x.34 y.36)))))))
                   (define L.tmp.3
                     (lambda (tmp.283 tmp.100 tmp.101) (let () (if (= tmp.100 tmp.101) 14 6))))
                   (define L.tmp.2
                     (lambda (tmp.282 tmp.103 tmp.104)
                       (let ()
                         (if (!= (if (= (bitwise-and tmp.103 7) 0) 14 6) 6)
                             (if (!= (if (= (bitwise-and tmp.104 7) 0) 14 6) 6)
                                 (+ tmp.103 tmp.104)
                                 574)
                             574))))
                   (define L.tmp.1
                     (lambda (tmp.281 tmp.106 tmp.107)
                       (let ()
                         (if (!= (if (= (bitwise-and tmp.106 7) 0) 14 6) 6)
                             (if (!= (if (= (bitwise-and tmp.107 7) 0) 14 6) 6)
                                 (* tmp.106 (arithmetic-shift-right tmp.107 3))
                                 318)
                             318))))
                   (let ((*.105
                          (let ((tmp.48 (+ (alloc 16) 2)))
                            (begin (mset! tmp.48 -2 L.tmp.1) (mset! tmp.48 6 16) tmp.48)))
                         (|+.102|
                          (let ((tmp.49 (+ (alloc 16) 2)))
                            (begin (mset! tmp.49 -2 L.tmp.2) (mset! tmp.49 6 16) tmp.49)))
                         (eq?.99
                          (let ((tmp.50 (+ (alloc 16) 2)))
                            (begin (mset! tmp.50 -2 L.tmp.3) (mset! tmp.50 6 16) tmp.50)))
                         (fact.33
                          (let ((tmp.51 (+ (alloc 48) 2)))
                            (begin (mset! tmp.51 -2 L.tmp.4) (mset! tmp.51 6 8) tmp.51))))
                     (begin
                       (mset! fact.33 14 *.105)
                       (mset! fact.33 22 fact.33)
                       (mset! fact.33 30 |+.102|)
                       (mset! fact.33 38 eq?.99)
                       (call (mref fact.33 -2) fact.33 80)))))
  (check-equal? (specify-representation '(module (procedure? 10)))
                '(module (if (= (bitwise-and 80 7) 2) 14 6)))
  (check-equal? (specify-representation '(module (procedure? L.f.1)))
                '(module (if (= (bitwise-and L.f.1 7) 2) 14 6)))
  (check-equal? (specify-representation '(module (make-procedure L.f.1 2 1)))
                '(module
                     (let ((tmp.52 (+ (alloc 24) 2)))
                       (begin (mset! tmp.52 -2 L.f.1) (mset! tmp.52 6 16) tmp.52))))
  (check-equal? (specify-representation '(module (unsafe-procedure-ref L.f.1 0)))
                '(module (mref L.f.1 14)))
  (check-equal? (specify-representation '(module (let ((x.1 (make-procedure L.x.1.7 0 0)))
                                                   (begin (unsafe-procedure-set! x.1 0 x.1) x.1))))
                '(module
                     (let ((x.1
                            (let ((tmp.53 (+ (alloc 16) 2)))
                              (begin (mset! tmp.53 -2 L.x.1.7) (mset! tmp.53 6 0) tmp.53))))
                       (begin (mset! x.1 14 x.1) x.1))))
  (check-equal? (specify-representation '(module (let ((x.1 (make-procedure L.x.1.7 0 0)))
                                                   (begin (unsafe-procedure-set! x.1 2 x.1) x.1))))
                '(module
                     (let ((x.1
                            (let ((tmp.54 (+ (alloc 16) 2)))
                              (begin (mset! tmp.54 -2 L.x.1.7) (mset! tmp.54 6 0) tmp.54))))
                       (begin (mset! x.1 30 x.1) x.1))))
  (check-equal? (specify-representation '(module (unsafe-procedure-label L.tmp.1)))
                '(module (mref L.tmp.1 -2)))
  (check-equal? (specify-representation '(module (unsafe-procedure-arity L.f.1)))
                '(module (mref L.f.1 6)))
  (check-equal? (specify-representation '(module
                                             (let ([v.1 (unsafe-make-vector 5)])
                                               (begin
                                                 (unsafe-vector-set! v.1 4 (error 100))
                                                 (unsafe-vector-ref v.1 4)))))
                '(module
                     (let ((v.1
                            (let ((tmp.55 (+ (alloc 48) 3)))
                              (begin (mset! tmp.55 -3 40) tmp.55))))
                       (begin (mset! v.1 37 25662) (mref v.1 37)))))
  (check-equal? (specify-representation '(module
                                             (let ([x.1 2]
                                                   [v.1 (unsafe-make-vector 3)])
                                               (begin
                                                 (unsafe-vector-set! v.1 x.1 10)
                                                 (unsafe-vector-ref v.1 x.1)))))
                '(module
                     (let ((x.1 16)
                           (v.1
                            (let ((tmp.56 (+ (alloc 32) 3)))
                              (begin (mset! tmp.56 -3 24) tmp.56))))
                       (begin
                         (mset! v.1 (+ (* (arithmetic-shift-right x.1 3) 8) 5) 80)
                         (mref v.1 (+ (* (arithmetic-shift-right x.1 3) 8) 5)))))))
