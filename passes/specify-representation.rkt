#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8
  rackunit)

(provide specify-representation)

;; exprs-unsafe-data-lang-v8 -> exprs-bits-lang-v8
;; compiles p to Exprs-bit-lang v8 by compiling immediate data and primitive
;; operations into their implementations as ptrs and primitive bitwise
;; operations on ptrs
(define/contract (specify-representation p)
  (-> exprs-unsafe-data-lang-v8? exprs-bits-lang-v8?)

  ;; func is `(define ,label (lambda (,alocs ...) ,value))
  ;; interp. a function definition

  ;; func -> func
  (define (specify-representation-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,value))
       `(define ,label (lambda (,@alocs) ,(specify-representation-value value)))]))

  ;; exprs-unsafe-data-lang-v8.value -> exprs-bits-lang-v8.value
  (define (specify-representation-value value)
    (match value
      [`(let ([,alocs ,vs] ...) ,v)
       (define bindings
         (map (lambda (aloc val)
                (list aloc (specify-representation-value val)))
              alocs vs))
       `(let (,@bindings) ,(specify-representation-value v))]
      [`(call ,vs ...)
       `(call ,@(map specify-representation-value vs))]
      [`(if ,v1 ,v2 ,v3)
       `(if (!= ,(specify-representation-value v1) ,(current-false-ptr))
            ,(specify-representation-value v2)
            ,(specify-representation-value v3))]
      [`(begin ,es ... ,v)
       `(begin ,@(map specify-representation-effect es) ,(specify-representation-value v))]
      [`(,primop ,vs ...)
       #:when (unsafe-primop? primop)
       ((specify-representation-primop primop) (map specify-representation-value vs))]
      [triv (specify-representation-triv triv)]))

  ;; exprs-unsafe-data-lang-v8.effect -> exprs-bits-lang-v8.effect
  (define (specify-representation-effect effect)
    (match effect
      [`(,primop ,vs ...)
       #:when (unsafe-primop? primop)
       ((specify-representation-primop primop) (map specify-representation-value vs))]
      [`(begin ,es ...)
       `(begin ,@(map specify-representation-effect es))]))

  ;; exprs-unsafe-data-lang-v8.triv -> exprs-bits-lang-v8.triv
  (define (specify-representation-triv triv)
    (match triv
      ['#t (current-true-ptr)]
      ['#f (current-false-ptr)]
      ['empty (current-empty-ptr)]
      ['(void) (current-void-ptr)]
      [`(error ,uint8)
       (bitwise-ior (arithmetic-shift uint8 (current-error-shift))
                    (current-error-tag))]
      [label #:when (label? label) label]
      [aloc #:when (aloc? aloc) aloc]
      [fixnum #:when (int61? fixnum)
              (arithmetic-shift fixnum (current-fixnum-shift))]
      [char #:when (ascii-char-literal? char)
            (bitwise-ior (arithmetic-shift (char->integer char) (current-ascii-char-shift))
                         (current-ascii-char-tag))]))

  ;; exprs-unsafe-data-lang-v8.primop -> ((List-of exprs-bits-lang-v8.value) -> exprs-bits-lang-v8.value)
  (define (specify-representation-primop primop)
    (match primop
      ['unsafe-fx*
       (lambda (values)
         (if (int61? (second values))
             `(* ,(arithmetic-shift (first values) (* -1 (current-fixnum-shift))) ,(second values))
             `(* ,(first values) (arithmetic-shift-right ,(second values) ,(current-fixnum-shift)))))]
      ['unsafe-fx+
       (lambda (values) `(+ ,(first values) ,(second values)))]
      ['unsafe-fx-
       (lambda (values) `(- ,(first values) ,(second values)))]
      ['eq?
       (lambda (values) `(if (= ,(first values) ,(second values)) ,(current-true-ptr) ,(current-false-ptr)))]
      ['unsafe-fx<
       (lambda (values) `(if (< ,(first values) ,(second values)) ,(current-true-ptr) ,(current-false-ptr)))]
      ['unsafe-fx<=
       (lambda (values) `(if (<= ,(first values) ,(second values)) ,(current-true-ptr) ,(current-false-ptr)))]
      ['unsafe-fx>
       (lambda (values) `(if (> ,(first values) ,(second values)) ,(current-true-ptr) ,(current-false-ptr)))]
      ['unsafe-fx>=
       (lambda (values) `(if (>= ,(first values) ,(second values)) ,(current-true-ptr) ,(current-false-ptr)))]
      ['fixnum?
       (lambda (values) `(if (= (bitwise-and ,(first values) ,(current-fixnum-mask)) ,(current-fixnum-tag))
                             ,(current-true-ptr)
                             ,(current-false-ptr)))]
      ['boolean?
       (lambda (values) `(if (= (bitwise-and ,(first values) ,(current-boolean-mask)) ,(current-boolean-tag))
                             ,(current-true-ptr)
                             ,(current-false-ptr)))]
      ['empty?
       (lambda (values) `(if (= (bitwise-and ,(first values) ,(current-empty-mask)) ,(current-empty-tag))
                             ,(current-true-ptr)
                             ,(current-false-ptr)))]
      ['void?
       (lambda (values) `(if (= (bitwise-and ,(first values) ,(current-void-mask)) ,(current-void-tag))
                             ,(current-true-ptr)
                             ,(current-false-ptr)))]
      ['ascii-char?
       (lambda (values) `(if (= (bitwise-and ,(first values) ,(current-ascii-char-mask)) ,(current-ascii-char-tag))
                             ,(current-true-ptr)
                             ,(current-false-ptr)))]
      ['error?
       (lambda (values) `(if (= (bitwise-and ,(first values) ,(current-error-mask)) ,(current-error-tag))
                             ,(current-true-ptr)
                             ,(current-false-ptr)))]
      ['not
       (lambda (values) `(if (!= ,(first values) ,(current-false-ptr))
                             ,(current-false-ptr)
                             ,(current-true-ptr)))]
      ['pair?
       (lambda (values) `(if (= (bitwise-and ,(first values) ,(current-pair-mask)) ,(current-pair-tag))
                             ,(current-true-ptr)
                             ,(current-false-ptr)))]
      ['vector?
       (lambda (values) `(if (= (bitwise-and ,(first values) ,(current-vector-mask)) ,(current-vector-tag))
                             ,(current-true-ptr)
                             ,(current-false-ptr)))]
      ['cons
       (define tmp (fresh 'tmp))
       (lambda (values) `(let ([,tmp (+ (alloc ,(current-pair-size)) ,(current-pair-tag))])
                           (begin
                             (mset! ,tmp ,(car-offset) ,(first values))
                             (mset! ,tmp ,(cdr-offset) ,(second values))
                             ,tmp)))]
      ['unsafe-car
       (lambda (values) `(mref ,(first values) ,(car-offset)))]
      ['unsafe-cdr
       (lambda (values) `(mref ,(first values) ,(cdr-offset)))]
      ['unsafe-make-vector
       (define tmp (fresh 'tmp))
       (lambda (values)
         (define alloc-size
           (if (int64? (first values))
               (+ (first values) (current-word-size-bytes))
               `(* (+ 1 (arithmetic-shift-right ,(first values) ,(current-vector-shift))) ,(current-word-size-bytes))))
         `(let ([,tmp (+ (alloc ,alloc-size) ,(current-vector-tag))])
            (begin (mset! ,tmp ,(- (current-vector-tag)) ,(first values)) ,tmp)))]
      ['unsafe-vector-length
       (lambda (values) `(mref ,(first values) ,(- (current-vector-tag))))]
      ['unsafe-vector-set!
       (lambda (values)
         (define index
           (if (int64? (second values))
               (second values)
               `(+ (* (arithmetic-shift-right ,(second values) ,(current-vector-shift)) ,(current-word-size-bytes)) ,(- (current-word-size-bytes) (current-vector-tag)))))
         `(mset! ,(first values) ,index ,(third values)))]
      ['unsafe-vector-ref
       (lambda (values)
         (define index
           (if (int64? (second values))
               (+ (second values) (- (current-word-size-bytes) (current-vector-tag)))
               `(+ (* (arithmetic-shift-right ,(second values) ,(current-vector-shift)) ,(current-word-size-bytes)) ,(- (current-word-size-bytes) (current-vector-tag)))))
         `(mref ,(first values) ,index))]))

  (match p
    [`(module ,funcs ... ,value)
     `(module ,@(map specify-representation-func funcs) ,(specify-representation-value value))]))

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
                      (let ((tmp.4 (+ (alloc 22) 3))) (begin (mset! tmp.4 -3 14) tmp.4))
                      -3)))
  (check-equal? (specify-representation '(module
                                             (let ([v.1 (unsafe-make-vector 2)])
                                               (begin
                                                 (unsafe-vector-set! v.1 1 (error 100))
                                                 (unsafe-vector-ref v.1 1)))))
                '(module
                     (let ((v.1
                            (let ((tmp.5 (+ (alloc 24) 3))) (begin (mset! tmp.5 -3 16) tmp.5))))
                       (begin (mset! v.1 8 25662) (mref v.1 13)))))
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
                       (call L.get-first.6 vec.3)))))))
