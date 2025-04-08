#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8
  cpsc411/langs/v9)

(provide specify-representation)

;; proc-exposed-lang-v9 -> exprs-bits-lang-v8
;; compiles p to Exprs-bit-lang v8 by compiling immediate data and primitive
;; operations into their implementations as ptrs and primitive bitwise
;; operations on ptrs
(define/contract (specify-representation p)
  (-> proc-exposed-lang-v9? exprs-bits-lang-v8?)

  ;; func is `(define ,label (lambda (,alocs ...) ,value))
  ;; interp. a function definition

  ;; func -> func
  (define (specify-representation-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,value))
       `(define ,label (lambda (,@alocs) ,(specify-representation-value value)))]))

  ;; proc-exposed-lang-v9 -> exprs-bits-lang-v8.value
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

  ;; proc-exposed-lang-v9 -> exprs-bits-lang-v8.effect
  (define (specify-representation-effect effect)
    (match effect
      [`(,primop ,vs ...)
       #:when (unsafe-primop? primop)
       ((specify-representation-primop primop) (map specify-representation-value vs))]
      [`(begin ,es ...)
       `(begin ,@(map specify-representation-effect es))]))

  ;; proc-exposed-lang-v9 -> exprs-bits-lang-v8.triv
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

  ;; proc-exposed-lang-v9 -> ((List-of exprs-bits-lang-v8.value) -> exprs-bits-lang-v8.value)
  (define (specify-representation-primop primop)
    (match primop
      ['procedure? (lambda (values)
                     `(if (= (bitwise-and ,(first values) ,(current-procedure-mask)) ,(current-procedure-tag))
                          ,(current-true-ptr) ,(current-false-ptr)))]
      ['make-procedure
       (define tmp (fresh 'tmp))
       (lambda (values)
         `(let ([,tmp (+ (alloc 24) ,(current-procedure-tag))])
            (begin
              (mset! ,tmp ,(- (current-procedure-tag)) ,(first values))
              (mset! ,tmp 6 ,(second values))
              ,tmp)))]

      ['unsafe-procedure-arity
       (lambda (values)
         `(mref ,(first values) 6))]

      ['unsafe-procedure-label
       (lambda (values)
         `(mref ,(first values) ,(- (current-procedure-tag))))]

      ['unsafe-procedure-ref
       (lambda (values)
         (define offset
           (if (int64? (second values))
               (+ (second values) 8)
               `(+ 14 (* (arithmetic-shift-right ,(second values) ,(current-fixnum-shift)) 8))))
         `(mref ,(first values) ,offset))]

      ['unsafe-procedure-set!
       (lambda (values)
         (define offset
           (if (int64? (second values))
               (+ 14 (* (second values) 8))
               `(+ 14 (* (arithmetic-shift-right ,(second values) ,(current-fixnum-shift)) 8))))
         `(mset! ,(first values) ,offset ,(third values)))]
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

