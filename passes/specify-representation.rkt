#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v7
  rackunit)

(provide specify-representation)

;; exprs-unsafe-data-lang-v7 -> exprs-bits-lang-v7
;; compiles p to Exprs-bit-lang v7 by compiling immediate data and primitive
;; operations into their implementations as ptrs and primitive bitwise
;; operations on ptrs
(define/contract (specify-representation p)
  (-> exprs-unsafe-data-lang-v7? exprs-bits-lang-v7?)

  ;; func is `(define ,label (lambda (,alocs ...) ,value))
  ;; interp. a function definition

  ;; func -> func
  (define (specify-representation-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,value))
       `(define ,label (lambda (,@alocs) ,(specify-representation-value value)))]))

  ;; exprs-unsafe-data-lang-v7.value -> exprs-bits-lang-v7.value
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
      [`(,primop ,vs ...)
       #:when (or (unsafe-binop? primop) (unop? primop))
       ((specify-representation-primop primop) (map specify-representation-value vs))]
      [triv (specify-representation-triv triv)]))

  ;; exprs-unsafe-data-lang-v7.triv -> exprs-bits-lang-v7.triv
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
              (bitwise-ior (arithmetic-shift fixnum (current-fixnum-shift))
                           (current-fixnum-tag))]
      [char #:when (ascii-char-literal? char)
            (bitwise-ior (arithmetic-shift (char->integer char) (current-ascii-char-shift))
                         (current-ascii-char-tag))]))

  ;; exprs-unsafe-data-lang-v7.primop -> ((List-of exprs-bits-lang-v7.value) -> exprs-bits-lang-v7.value)
  (define (specify-representation-primop primop)
    (match primop
      [binop #:when (unsafe-binop? binop) (specify-representation-binop binop)]
      [unop #:when (unop? unop) (specify-representation-unop unop)]))

  ;; exprs-unsafe-data-lang-v7.binop -> ((List-of exprs-bits-lang-v7.value) -> exprs-bits-lang-v7.value)
  (define (specify-representation-binop binop)
    (match binop
      ['unsafe-fx*
       (lambda (values) `(* ,(first values) (arithmetic-shift-right ,(second values) ,(current-fixnum-shift))))]
      ['unsafe-fx+
       (lambda (values) `(+ ,(first values) ,(second values)))]
      ['unsafe-fx-
       (lambda (values) `(- ,(first values) ,(second values)))]
      ['eq?
       (lambda (values) `(if (= ,(first values) ,(second values)) ,(current-true-ptr) ,(current-false-ptr)))]
      ['unsafe-fx<
       (lambda (values) `(< ,(first values) ,(second values)))]
      ['unsafe-fx<=
       (lambda (values) `(<= ,(first values) ,(second values)))]
      ['unsafe-fx>
       (lambda (values) `(> ,(first values) ,(second values)))]
      ['unsafe-fx>=
       (lambda (values) `(>= ,(first values) ,(second values)))]))

  ;; exprs-unsafe-data-lang-v7.unop -> ((List-of exprs-bits-lang-v7.value) -> exprs-bits-lang-v7.value)
  (define (specify-representation-unop unop)
    (match unop
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
                             ,(current-true-ptr)))]))

  (match p
    [`(module ,funcs ... ,value)
     `(module ,@(map specify-representation-func funcs) ,(specify-representation-value value))]))

(module+ test
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
                         (call L.ascii-char?.6 x.6))))))