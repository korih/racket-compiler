#lang racket

(provide optimize-predicates)

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4)

;; Exercise 11
;; nested-asm-lang-v4 -> nested-asm-lang-v4
;; simplifies the predicates in the program
(define/contract (optimize-predicates p)
  (-> nested-asm-lang-v4? nested-asm-lang-v4?)
  ;; (Envof nested-asm-lang-v4-loc RangeValue)
  ;; invariant: env contains a mapping of the locations to their currently known value ranges
  (define env empty-env)

  ;; RangeValue is one-of:
  ;; - (Pairof int64 int64) representing the range [a, b]

  ;; int64 int64 -> RangeValue
  ;; interp. create a RangeValue enforcing ordering and the clamped range
  (define (make-range-value a b)
    (define lower (min a b))
    (define upper (max a b))
    (cons (max lower (min-int 64)) (min upper (max-int 64))))

  ;; RangeValue RangeValue -> boolean
  ;; interp. true if the two ranges intersect each other
  (define (range-value-intersects? rv1 rv2)
    (not (or (< (cdr rv1) (car rv2))
             (> (car rv1) (cdr rv2)))))

  ;; nested-asm-lang-v4-tail -> nested-asm-lang-v4-tail
  ;; interp. optimize predicates in the program tail
  (define (optimize-predicates/tail t)
    (match t
      [`(halt ,triv) `(halt ,triv)]
      [`(begin ,fx ... ,tail) `(begin ,@(map optimize-predicates/effect fx) ,(optimize-predicates/tail tail))]
      [`(if ,pred ,t-tail ,f-tail) (optimize-conditional pred
                                                         (optimize-predicates/tail t-tail)
                                                         (optimize-predicates/tail f-tail))]))

  ;; nested-asm-lang-v4-pred nested-asm-lang-v4-tail nested-asm-lang-v4-tail -> nested-asm-lang-v4-tail
  ;; OR
  ;; nested-asm-lang-v4-pred nested-asm-lang-v4-effect nested-asm-lang-v4-effect -> nested-asm-lang-v4-effect
  ;; interp. optimize the predicate if possible and return the corresponding tail
  (define (optimize-conditional pred k-t k-f)
    (match pred
      [`(,relop ,loc ,triv) (interp-relop-conditional relop loc triv k-t k-f)]
      ['(true) k-t]
      ['(false) k-f]
      [`(not ,pred) (optimize-conditional pred k-f k-t)]
      [`(begin ,fx ... ,pred)
       `(begin ,@(map optimize-predicates/effect fx) ,(optimize-conditional pred k-t k-f))]
      [`(if ,pred ,t-pred ,f-pred)
       (optimize-conditional pred
                             (optimize-conditional t-pred k-t k-f)
                             (optimize-conditional f-pred k-t k-f))]))

  ;; nested-asm-lang-v4-effect -> nested-asm-lang-v4-effect
  ;; interp. optimize predicates in the program effect
  (define (optimize-predicates/effect e)
    (match e
      [`(set! ,loc ,triv) (set! env (extend-env env loc (interp-triv triv)))
                          `(set! ,loc ,triv)]
      [`(set! ,loc (,binop ,loc ,triv))
       (define triv-val (interp-triv triv))
       (set! env (extend-env env loc (interp-binop binop (interp-triv loc) triv-val)))
       `(set! ,loc ,triv-val)]
      [`(begin ,fx ...) `(begin ,@(map optimize-predicates/effect fx))]
      [`(if ,pred ,t-e ,f-e)
       (optimize-conditional pred
                             (optimize-predicates/effect t-e)
                             (optimize-predicates/effect f-e))]))

  ;; nested-asm-lang-v4-binop RangeValue RangeValue -> RangeValue
  ;; interp. the known abstract value resulting from the binary operation
  (define (interp-binop binop val1 val2)
    (match binop
      ['* (make-range-value (x64-mul (car val1) (car val2)) (x64-mul (cdr val1) (cdr val2)))]
      ['+ (make-range-value (x64-add (car val1) (car val2)) (x64-add (cdr val1) (cdr val2)))]))

  ;; nested-asm-lang-v4-triv -> RangeValue
  ;; interp. the known value or range of the triv
  (define (interp-triv triv)
    (match triv
      [x #:when (int64? x) (make-range-value x x)]
      [loc (with-handlers ([exn:fail? (lambda (_) (cons (min-int 64) (max-int 64)))])
             (lookup-env env loc))]))

  ;; nested-asm-lang-v4-relop nested-asm-lang-v4-loc nested-asm-lang-v4-triv nested-asm-lang-v4-tail
  ;; nested-asm-lang-v4-tail -> nested-asm-lang-v4-tail
  ;; OR
  ;; nested-asm-lang-v4-relop nested-asm-lang-v4-loc nested-asm-lang-v4-triv nested-asm-lang-v4-effect
  ;; nested-asm-lang-v4-effect -> nested-asm-lang-v4-effect
  ;; interp. optimize the relop if possible and return the corresponding expression
  (define (interp-relop-conditional relop loc triv k-t k-f)
    (define op1 (interp-triv loc))
    (define op2 (interp-triv triv))
    (cond
      [(interp-relop-optimize-true? relop op1 op2) k-t]
      [(interp-relop-optimize-false? relop op1 op2) k-f]
      [else `(if (,relop ,loc ,triv) ,k-t ,k-f)]))


  ;; nested-asm-lang-v4-relop nested-asm-lang-v4-triv nested-asm-lang-v4-triv -> boolean
  ;; interp. true if the relop can be optimized to true
  (define (interp-relop-optimize-true? relop op1 op2)
    (match relop
      ['< (< (cdr op1) (car op2))]
      ['<= (<= (cdr op1) (car op2))]
      ['= (and (= (car op1) (car op2))
               (= (cdr op1) (cdr op2)))] ['>= (>= (car op1) (cdr op2))]
      ['> (> (car op1) (cdr op2))]
      ['>= (>= (car op1) (cdr op2))]
      ['!= (not (range-value-intersects? op1 op2))]))

  ;; nested-asm-lang-v4-relop nested-asm-lang-v4-triv nested-asm-lang-v4-triv -> boolean
  ;; interp. true if the relop can be optimized to false (the relop is guaranteed to be false)
  (define (interp-relop-optimize-false? relop op1 op2)
    (match relop
      ['< (>= (car op2) (cdr op1))]
      ['<= (> (car op2) (cdr op1))]
      ['= (not (range-value-intersects? op1 op2))]
      ['> (<= (cdr op2) (car op1))]
      ['>= (< (cdr op2) (car op1))]
      ['!= (and (= (car op1) (car op2))
                (= (cdr op1) (cdr op2)))]))

  (match p
    [`(module ,tail) `(module ,(optimize-predicates/tail tail))]))

(module+ test
  (require rackunit)

  (check-equal? (optimize-predicates '(module (if (true) (halt 0) (halt 1)))) '(module (halt 0)))
  (check-equal? (optimize-predicates '(module (if (false) (begin (set! rax 1) (halt rax)) (halt 0))))
                '(module (halt 0)))
  (check-equal? (optimize-predicates '(module (if (not (true)) (halt 0) (begin (set! rax 1) (halt rax)))))
                '(module (begin (set! rax 1) (halt rax))))
  (check-equal? (optimize-predicates '(module (if (if (true) (false) (true)) (halt 0) (halt 1))))
                '(module (halt 1)))
  (check-equal? (optimize-predicates '(module (begin (set! rax 1) (if (> rax 0) (halt 0) (halt 1)))))
                '(module (begin (set! rax 1) (halt 0))))
  (check-equal? (optimize-predicates '(module (begin (set! rbx 1) (if (< rbx 1) (halt 0) (halt 3)))))
                '(module (begin (set! rbx 1) (halt 3))))
  (check-equal? (optimize-predicates `(module (begin (set! rbx rax) (if (<= rbx ,(max-int 64)) (halt 12) (halt 133)))))
                '(module (begin (set! rbx rax) (halt 12))))
  (check-equal? (optimize-predicates '(module (begin (set! rbx rax)
                                                     (if (= rbx rax)
                                                         (begin (set! rax rcx) (halt rax))
                                                         (halt rax)))))
                '(module (begin (set! rbx rax) (begin (set! rax rcx) (halt rax)))))
  (check-equal? (optimize-predicates '(module (begin (set! rbx 33) (if (= rbx 44) (halt -1) (halt 0)))))
                '(module (begin (set! rbx 33) (halt 0))))
  (check-equal? (optimize-predicates '(module (begin (set! rbx rcx) (if (= rbx 5) (halt 0) (halt 1)))))
                '(module (begin (set! rbx rcx) (if (= rbx 5) (halt 0) (halt 1)))))
  (check-equal? (optimize-predicates '(module (begin (set! rbx 33) (if (= rbx 44) (halt -1) (halt 3)))))
                '(module (begin (set! rbx 33) (halt 3)))))
