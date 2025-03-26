#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8
  rackunit)

(provide remove-complex-opera*)

;; exprs-bits-lang-v8 -> values-bits-lang-v8
;; compiles p to Values-bits-lang v8 by performing the monadic form
;; transformation, unnesting all non-trivial operators and operands to binops,
;; calls, and relops, making data flow explicit and simple to implement
;; imperatively
(define/contract (remove-complex-opera* p)
  (-> exprs-bits-lang-v8? values-bits-lang-v8?)

  ;; func-value is `(define ,label (lambda (,aloc ...) ,value))
  ;; interp. a function definition with the body being a value

  ;; func-tail is `(define ,label (lambda (,aloc ...) ,tail))
  ;; interp. a function definition with the body being a tail

  ;; func-value -> func-tail
  (define (remove-complex-opera*-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,tail))
       `(define ,label (lambda (,@alocs) ,(remove-complex-opera*-tail tail (λ (v) v))))]))

  ;; exprs-bits-lang-v8.value (values-bits-lang-v8.value -> values-bits-lang-v8.value) -> values-bits-lang-v8.tail
  (define (remove-complex-opera*-tail tail k)
    (match tail
      [`(let ([,alocs ,vs] ...) ,t)
       (define bindings
         (map (lambda (aloc val)
                (list aloc (remove-complex-opera*-value val k)))
              alocs vs))
       `(let (,@bindings) ,(remove-complex-opera*-tail t k))]
      [`(if ,pred ,t1 ,t2)
       `(if ,(remove-complex-opera*-pred pred k)
            ,(remove-complex-opera*-tail t1 k)
            ,(remove-complex-opera*-tail t2 k))]
      [`(call ,triv ,ops ...)
       (trivialize-value triv
                         (lambda (triv^)
                           (let loop ([remaining-ops ops]
                                      [accum '()]
                                      [k k])
                             (if (empty? remaining-ops)
                                 (k `(call ,triv^ ,@(reverse accum)))
                                 (trivialize-value (car remaining-ops)
                                                   (lambda (op^)
                                                     (loop (cdr remaining-ops) (cons op^ accum) k)))))))]
      [`(begin ,es ... ,t)
       `(begin ,@(map (lambda (e) (remove-complex-opera*-effect e k)) es) ,(remove-complex-opera*-tail t k))]
      [value (remove-complex-opera*-value value k)]))

  ;; exprs-bits-lang-v8.value (values-bits-lang-v8.value -> values-bits-lang-v8.value) -> values-bits-lang-v8.value
  (define (remove-complex-opera*-value value k)
    (match value
      [`(let ([,alocs ,vs] ...) ,v)
       (define bindings
         (map (lambda (aloc val)
                (list aloc (remove-complex-opera*-value val k)))
              alocs vs))
       `(let (,@bindings) ,(remove-complex-opera*-value v k))]
      [`(if ,pred ,v1 ,v2)
       `(if ,(remove-complex-opera*-pred pred k)
            ,(remove-complex-opera*-value v1 k)
            ,(remove-complex-opera*-value v2 k))]
      [`(call ,triv ,ops ...)
       (trivialize-value triv
                         (lambda (triv^)
                           (let loop ([remaining-ops ops]
                                      [accum '()]
                                      [k k])
                             (if (empty? remaining-ops)
                                 (k `(call ,triv^ ,@(reverse accum)))
                                 (trivialize-value (car remaining-ops)
                                                   (lambda (op^)
                                                     (loop (cdr remaining-ops) (cons op^ accum) k)))))))]
      [`(mref ,aloc ,opand)
       (trivialize-value aloc
                         (lambda (aloc^)
                           (trivialize-value opand
                                             (lambda (opand^)
                                               (k `(mref ,aloc^ ,opand^))))))]
      [`(alloc ,opand)
       (trivialize-value opand
                         (lambda (opand^)
                           (k `(alloc ,opand^))))]
      [`(begin ,es ... ,v)
       `(begin ,@(map (lambda (e) (remove-complex-opera*-effect e k)) es) ,(remove-complex-opera*-value v k))]
      [`(,binop ,op1 ,op2)
       (trivialize-value op1
                         (lambda (op1^)
                           (trivialize-value op2
                                             (lambda (op2^)
                                               (k `(,binop ,op1^ ,op2^))))))]
      [triv (trivialize-value triv k)]))

  ;; exprs-bits-lang-v8.pred (values-bits-lang-v8.value -> values-bits-lang-v8.value) -> values-bits-lang-v8.pred
  (define (remove-complex-opera*-pred pred k)
    (match pred
      ['(true) pred]
      ['(false) pred]
      [`(not ,p) `(not ,(remove-complex-opera*-pred p k))]
      [`(let ([,alocs ,vs] ...) ,p)
       (define bindings
         (map (lambda (aloc val)
                (list aloc (remove-complex-opera*-value val k)))
              alocs vs))
       `(let (,@bindings) ,(remove-complex-opera*-pred p k))]
      [`(if ,p1 ,p2 ,p3)
       `(if ,(remove-complex-opera*-pred p1 k)
            ,(remove-complex-opera*-pred p2 k)
            ,(remove-complex-opera*-pred p3 k))]
      [`(begin ,es ... ,p)
       `(begin ,@(map (lambda (e) (remove-complex-opera*-effect e k)) es) ,(remove-complex-opera*-pred p k))]
      [`(,relop ,op1 ,op2)
       (trivialize-value op1
                         (lambda (op1^)
                           (trivialize-value op2
                                             (lambda (op2^)
                                               (k `(,relop ,op1^ ,op2^))))))]))

  ;; exprs-bits-lang-v8.effect (values-bits-lang-v8.value -> values-bits-lang-v8.value) -> values-bits-lang-v8.effect
  (define (remove-complex-opera*-effect effect k)
    (match effect
      [`(mset! ,aloc ,opand ,value)
       (trivialize-value aloc
                         (lambda (aloc^)
                           (trivialize-value opand
                                             (lambda (opand^)
                                               (k `(mset! ,aloc^ ,opand^ ,(remove-complex-opera*-value value k)))))))]
      [`(let ([,alocs ,vs] ...) ,eff)
       (define bindings
         (map (lambda (aloc val)
                (list aloc (remove-complex-opera*-value val k)))
              alocs vs))
       `(let (,@bindings) ,(remove-complex-opera*-effect eff k))]
      [`(begin ,es ...)
       `(begin ,@(map (lambda (e) (remove-complex-opera*-effect e k)) es))]))

  ;; exprs-bits-lang-v8.value (values-bits-lang-v8.value -> values-bits-lang-v8.value) -> values-bits-lang-v8.opand
  (define (trivialize-value value k)
    (match value
      [label #:when (label? label) (k label)]
      [int64 #:when (int64? int64) (k int64)]
      [aloc #:when (aloc? aloc) (k aloc)]
      [else
       (define tmp (fresh))
       `(let ([,tmp ,(remove-complex-opera*-value value (λ (v) v))])
          ,(k tmp))]))

  (match p
    [`(module ,funcs ... ,tail)
     `(module ,@(map remove-complex-opera*-func funcs) ,(remove-complex-opera*-tail tail (λ (v) v)))]))

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
                   (call L.f.1 1 2))))
