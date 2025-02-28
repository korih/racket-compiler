#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5
  rackunit)

(provide impose-calling-conventions)

;; proc-imp-cmf-lang-v5 -> imp-cmf-lang-v5
;; compiles p to Imp-cmf-lang v5 by imposing calling conventions on all calls
;; and procedure definitions
(define/contract (impose-calling-conventions p)
  (-> proc-imp-cmf-lang-v5? imp-cmf-lang-v5?)

  ;; (List-of opand) (List-of register) Natural -> (list (List-of imp-cmf-lang-v5.effect) (List-of imp-cmf-lang-v5.rloc))
  (define (transform-tail-call ops regs fvidx)
    (cond
      [(empty? ops) (values '() '())]
      [else
       (if (not (empty? regs))
           (let ([reg (first regs)])
             (call-with-values
              (lambda () (transform-tail-call (rest ops) (rest regs) fvidx))
              (lambda (rest-list used-list)
                (values (cons `(set! ,reg ,(first ops)) rest-list) (cons reg used-list)))))
           (let ([fvar (make-fvar fvidx)])
             (call-with-values
              (lambda () (transform-tail-call (rest ops) regs (+ fvidx 1)))
              (lambda (rest-list used-list)
                (values (cons `(set! ,fvar ,(first ops)) rest-list) (cons fvar used-list))))))]))

  ;; (List-of aloc) (List-of register) Natural -> (List-of imp-cmf-lang-v5.effect)
  (define (transform-procedure alocs regs fvidx)
    (cond
      [(empty? alocs) '()]
      [else
       (if (not (empty? regs))
           (cons `(set! ,(first alocs) ,(first regs))
                 (transform-procedure (rest alocs) (rest regs) fvidx))
           (cons `(set! ,(first alocs) ,(make-fvar fvidx))
                 (transform-procedure (rest alocs) regs (+ fvidx 1))))]))

  (define (impose-calling-conventions-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,tail))
       (define calling-convention (transform-procedure alocs (current-parameter-registers) 0))
       `(define ,label (begin ,@calling-convention ,(impose-calling-conventions-tail tail)))]))

  ;; proc-imp-cmf-lang-v5.tail -> imp-cmf-lang-v5.tail
  (define (impose-calling-conventions-tail tail)
    (match tail
      [`(begin ,es ... ,t)
       `(begin ,@(map impose-calling-conventions-effect es) ,(impose-calling-conventions-tail t))]
      [`(if ,pred ,t1 ,t2)
       `(if ,(impose-calling-conventions-pred pred)
            ,(impose-calling-conventions-tail t1)
            ,(impose-calling-conventions-tail t2))]
      [`(call ,triv ,ops ...)
       (define-values (effects used-rlocs) (transform-tail-call ops (current-parameter-registers) 0))
       `(begin ,@(reverse effects) (jump ,triv ,(current-frame-base-pointer-register) ,@used-rlocs))]
      [value (impose-calling-conventions-value value)]))

  ;; proc-imp-cmf-lang-v5.effect -> imp-cmf-lang-v5.effect
  (define (impose-calling-conventions-effect effect)
    (match effect
      [`(set! ,aloc ,value)
       `(set! ,aloc ,(impose-calling-conventions-value value))]
      [`(begin ,es ...)
       `(begin ,@(map impose-calling-conventions-effect es))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(impose-calling-conventions-pred pred)
            ,(impose-calling-conventions-effect e1)
            ,(impose-calling-conventions-effect e2))]))

  ;; proc-imp-cmf-lang-v5.pred -> imp-cmf-lang-v5.pred
  (define (impose-calling-conventions-pred pred)
    (match pred
      ['(true) pred]
      ['(false) pred]
      [`(not ,p)
       `(not ,(impose-calling-conventions-pred p))]
      [`(begin ,es ... ,p)
       `(begin ,@(map impose-calling-conventions-effect es) ,(impose-calling-conventions-pred p))]
      [`(if ,p1 ,p2 ,p3)
       `(if ,(impose-calling-conventions-pred p1)
            ,(impose-calling-conventions-pred p2)
            ,(impose-calling-conventions-pred p3))]
      [`(,relop ,op1 ,op2) pred]))

  ;; proc-imp-cmf-lang-v5.value -> imp-cmf-lang-v5.value
  (define (impose-calling-conventions-value value)
    (match value
      [`(,binop ,op1 ,op2) value]
      [triv triv]))

  (match p
    [`(module ,funcs ... ,tail)
     `(module ,@(map impose-calling-conventions-func funcs) ,(impose-calling-conventions-tail tail))]))

(module+ test
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda () (* 1 2))) (call L.f.1)))
                '(module (define L.f.1 (begin (* 1 2))) (begin (jump L.f.1 rbp))))
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda (x.1 y.1 z.1) (begin
                                                                                           (set! a.1 (+ x.1 y.1))
                                                                                           (* z.1 a.1))))
                                               (call L.f.1 1 2 3)))
                '(module
                     (define L.f.1
                       (begin
                         (set! x.1 rdi)
                         (set! y.1 rsi)
                         (set! z.1 rdx)
                         (begin (set! a.1 (+ x.1 y.1)) (* z.1 a.1))))
                   (begin (set! rdx 3) (set! rsi 2) (set! rdi 1) (jump L.f.1 rbp rdi rsi rdx))))
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda (x.1) (begin
                                                                                   (set! y.1 100)
                                                                                   (* x.1 y.1))))
                                               (define L.g.1 (lambda (x.1 y.1) (if (not (> x.1 y.1))
                                                                                   (call L.f.1 x.1)
                                                                                   (call L.f.1 y.1))))
                                               (begin
                                                 (set! x.1 1)
                                                 (set! x.2 2)
                                                 (if (<= x.1 x.2)
                                                     (call L.f.1 x.1)
                                                     (call L.g.1 x.2 x.1)))))
                '(module
                     (define L.f.1 (begin (set! x.1 rdi) (begin (set! y.1 100) (* x.1 y.1))))
                   (define L.g.1
                     (begin
                       (set! x.1 rdi)
                       (set! y.1 rsi)
                       (if (not (> x.1 y.1))
                           (begin (set! rdi x.1) (jump L.f.1 rbp rdi))
                           (begin (set! rdi y.1) (jump L.f.1 rbp rdi)))))
                   (begin
                     (set! x.1 1)
                     (set! x.2 2)
                     (if (<= x.1 x.2)
                         (begin (set! rdi x.1) (jump L.f.1 rbp rdi))
                         (begin (set! rsi x.1) (set! rdi x.2) (jump L.g.1 rbp rdi rsi)))))))

