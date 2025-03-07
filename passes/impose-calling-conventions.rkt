#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v6
  rackunit)

(provide impose-calling-conventions)

;; proc-imp-cmf-lang-v6 -> imp-cmf-lang-v6
;; compiles p to Imp-cmf-lang v6 by imposing calling conventions on all calls
;; and procedure definitions
(define/contract (impose-calling-conventions p)
  (-> proc-imp-cmf-lang-v6? imp-cmf-lang-v6?)

  ;; func-lambda is `(define ,label (lambda (,alocs ...) ,entry))
  ;; interp. a function definition that uses lambdas

  ;; func is `(define ,label ,info ,tail)
  ;; interp. a function definition that uses calling conventions for arguments

  ;; (List-of opand) (List-of register) Natural aloc -> (List-of imp-cmf-lang-v6.effect) (List-of imp-cmf-lang-v6.rloc)
  ;; interp. converts a list of arguments from a tail call into a sequence of register assignments, following calling conventions
  (define (transform-tail-call ops regs fvidx return-aloc)
    (cond
      [(empty? ops) (values (list `(set! ,(current-return-address-register) ,return-aloc)) '())]
      [else
       (if (not (empty? regs))
           (let ([reg (first regs)])
             (call-with-values
              (lambda () (transform-tail-call (rest ops) (rest regs) fvidx return-aloc))
              (lambda (rest-list used-list)
                (values (cons `(set! ,reg ,(first ops)) rest-list) (cons reg used-list)))))
           (let ([fvar (make-fvar fvidx)])
             (call-with-values
              (lambda () (transform-tail-call (rest ops) regs (+ fvidx 1) return-aloc))
              (lambda (rest-list used-list)
                (values (cons `(set! ,fvar ,(first ops)) rest-list) (cons fvar used-list))))))]))

  ;; (List-of opand) (List-of register) aloc -> (List-of imp-cmf-lang-v6.effect) (List-of imp-cmf-lang-v6.rloc) (List-of aloc)
  ;; interp. converts a list of arguments from a non-tail call into a sequence of register assignments, following calling conventions
  (define (transform-non-tail-call ops regs return-label)
    (cond
      [(empty? ops) (values (list `(set! ,(current-return-address-register) ,return-label)) '() '())]
      [else
       (if (not (empty? regs))
           (let ([reg (first regs)])
             (call-with-values
              (lambda () (transform-non-tail-call (rest ops) (rest regs) return-label))
              (lambda (rest-list used-list nfvar-list)
                (values (cons `(set! ,reg ,(first ops)) rest-list) (cons reg used-list) nfvar-list))))
           (let ([nfvar (fresh 'nfv)])
             (call-with-values
              (lambda () (transform-non-tail-call (rest ops) regs return-label))
              (lambda (rest-list used-list nfvar-list)
                (values (cons `(set! ,nfvar ,(first ops)) rest-list) (cons nfvar used-list) (cons nfvar nfvar-list))))))]))

  ;; (List-of aloc) (List-of register) Natural -> (List-of imp-cmf-lang-v6.effect)
  ;; interp. transforms procedure parameter allocations by assigning them to registers based on calling conventions
  (define (transform-procedure alocs regs fvidx)
    (cond
      [(empty? alocs) '()]
      [else
       (if (not (empty? regs))
           (cons `(set! ,(first alocs) ,(first regs))
                 (transform-procedure (rest alocs) (rest regs) fvidx))
           (cons `(set! ,(first alocs) ,(make-fvar fvidx))
                 (transform-procedure (rest alocs) regs (+ fvidx 1))))]))

  ;; info is info
  ;; interp. keeps track of new frames for a given entry
  (define info (info-set '() 'new-frames '()))

  ;; info (List-of aloc) -> info
  ;; EFFECTS: adds the frame to the end of the new-frames's info
  (define (add-frame! frame)
    (set! info (info-set '() 'new-frames (append (info-ref info 'new-frames) (list frame)))))

  ;; ->
  ;; EFFECTS: resets the new-frames of info to an empty list
  (define (reset-info!)
    (set! info (info-set '() 'new-frames '())))
  
  ;; func-lambda -> func
  (define (impose-calling-conventions-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,entry))
       (reset-info!)
       `(define ,label ,info ,(impose-calling-conventions-entry entry alocs))]))

  ;; proc-imp-cmf-lang-v6.entry (List-of aloc) -> imp-cmf-lang-v6.tail
  (define (impose-calling-conventions-entry entry alocs)
    (match entry
      [tail
       (define return-aloc (fresh 'tmp-ra))
       (if (empty? alocs)
           `(begin
              (set! ,return-aloc ,(current-return-address-register))
              ,(impose-calling-conventions-tail tail return-aloc))
           (let ([calling-convention (transform-procedure alocs (current-parameter-registers) 0)])
             `(begin
                (set! ,return-aloc ,(current-return-address-register))
                (begin
                  ,@calling-convention
                  ,(impose-calling-conventions-tail tail return-aloc)))))]))

  ;; proc-imp-cmf-lang-v6.tail aloc -> imp-cmf-lang-v6.tail
  (define (impose-calling-conventions-tail tail return-aloc)
    (match tail
      [`(begin ,es ... ,t)
       `(begin ,@(map impose-calling-conventions-effect es) ,(impose-calling-conventions-tail t return-aloc))]
      [`(if ,pred ,t1 ,t2)
       `(if ,(impose-calling-conventions-pred pred)
            ,(impose-calling-conventions-tail t1 return-aloc)
            ,(impose-calling-conventions-tail t2 return-aloc))]
      [`(call ,triv ,ops ...)
       (define-values (effects used-rlocs) (transform-tail-call ops (current-parameter-registers) 0 return-aloc))
       `(begin
          ,@effects
          (jump ,triv ,(current-frame-base-pointer-register) ,(current-return-address-register) ,@used-rlocs))]
      [value
       (define-values (effect^ value^) (impose-calling-conventions-value value))
       (if (empty? effect^)
           `(begin
              (set! ,(current-return-value-register) ,value^)
              (jump ,return-aloc ,(current-frame-base-pointer-register) ,(current-return-value-register)))
           effect^)]))

  ;; proc-imp-cmf-lang-v6.effect -> imp-cmf-lang-v6.effect
  (define (impose-calling-conventions-effect effect)
    (match effect
      [`(set! ,aloc ,value)
       (define-values (effect^ value^) (impose-calling-conventions-value value))
       (if (empty? effect^)
           `(set! ,aloc ,value^)
           `(begin
              (return-point ,value^ ,effect^)
              (set! ,aloc ,(current-return-value-register))))]
      [`(begin ,es ...)
       `(begin ,@(map impose-calling-conventions-effect es))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(impose-calling-conventions-pred pred)
            ,(impose-calling-conventions-effect e1)
            ,(impose-calling-conventions-effect e2))]))

  ;; proc-imp-cmf-lang-v6.pred -> imp-cmf-lang-v6.pred
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

  ;; proc-imp-cmf-lang-v6.value -> imp-cmf-lang-v6.effect imp-cmf-lang-v6.value
  (define (impose-calling-conventions-value value)
    (match value
      [`(call ,triv ,ops ...)
       (define return-label (fresh-label 'rp))
       (define-values (effects used-rlocs nfvar-list) (transform-non-tail-call ops (current-parameter-registers) return-label))
       (add-frame! nfvar-list)
       (define effect^ `(begin
                          ,@effects
                          (jump ,triv ,(current-frame-base-pointer-register) ,(current-return-address-register) ,@used-rlocs)))
       (values effect^ return-label)]
      [`(,binop ,op1 ,op2) (values '() value)]
      [triv (values '() triv)]))

  (match p
    [`(module ,funcs ... ,entry)
     (define funcs^ (map impose-calling-conventions-func funcs))
     (reset-info!)
     (define entry^ (impose-calling-conventions-entry entry '()))
     `(module ,info ,@funcs^ ,entry^)]))

(module+ test
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda (x.1) x.1))
                                               (begin
                                                 (set! x.1 (call L.f.1 1))
                                                 x.1)))
                '(module
                     ((new-frames (())))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.1 r15)
                       (begin (set! x.1 rdi) (begin (set! rax x.1) (jump tmp-ra.1 rbp rax)))))
                   (begin
                     (set! tmp-ra.2 r15)
                     (begin
                       (begin
                         (return-point L.rp.1
                                       (begin (set! rdi 1) (set! r15 L.rp.1) (jump L.f.1 rbp r15 rdi)))
                         (set! x.1 rax))
                       (begin (set! rax x.1) (jump tmp-ra.2 rbp rax))))))
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda (a.1 b.1 c.1 d.1 e.1 f.1 g.1) x.1))
                                               (begin
                                                 (set! x.1 (call L.f.1 1 2 3 4 5 6 7))
                                                 x.1)))
                '(module
                     ((new-frames ((nfv.5))))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.3 r15)
                       (begin
                         (set! a.1 rdi)
                         (set! b.1 rsi)
                         (set! c.1 rdx)
                         (set! d.1 rcx)
                         (set! e.1 r8)
                         (set! f.1 r9)
                         (set! g.1 fv0)
                         (begin (set! rax x.1) (jump tmp-ra.3 rbp rax)))))
                   (begin
                     (set! tmp-ra.4 r15)
                     (begin
                       (begin
                         (return-point L.rp.2
                                       (begin
                                         (set! rdi 1)
                                         (set! rsi 2)
                                         (set! rdx 3)
                                         (set! rcx 4)
                                         (set! r8 5)
                                         (set! r9 6)
                                         (set! nfv.5 7)
                                         (set! r15 L.rp.2)
                                         (jump L.f.1 rbp r15 rdi rsi rdx rcx r8 r9 nfv.5)))
                         (set! x.1 rax))
                       (begin (set! rax x.1) (jump tmp-ra.4 rbp rax))))))
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda (x.1) x.1))
                                               (begin
                                                 (set! x.1 1)
                                                 (set! x.2 10)
                                                 (set! x.3 (+ x.1 x.2))
                                                 (set! x.4 (call L.f.4 x.3))
                                                 (* x.4 x.3))))
                '(module
                     ((new-frames (())))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.6 r15)
                       (begin (set! x.1 rdi) (begin (set! rax x.1) (jump tmp-ra.6 rbp rax)))))
                   (begin
                     (set! tmp-ra.7 r15)
                     (begin
                       (set! x.1 1)
                       (set! x.2 10)
                       (set! x.3 (+ x.1 x.2))
                       (begin
                         (return-point L.rp.3
                                       (begin (set! rdi x.3) (set! r15 L.rp.3) (jump L.f.4 rbp r15 rdi)))
                         (set! x.4 rax))
                       (begin (set! rax (* x.4 x.3)) (jump tmp-ra.7 rbp rax))))))
  (check-equal? (impose-calling-conventions '(module
                                                 (define L.f.2
                                                   (lambda (x.1 x.2)
                                                     (+ x.1 x.2)))
                                               (define L.f.3
                                                 (lambda (y.1)
                                                   (call L.f.2 y.1 10)))
                                               (begin
                                                 (set! x.1 5)
                                                 (set! x.2 10)
                                                 (set! x.3 (call L.f.2 x.1 x.2))
                                                 (set! x.4 (call L.f.3 x.3))
                                                 (set! x.5 (- x.3 x.4))
                                                 (call L.f.3 x.5))))
                '(module
                     ((new-frames (() ())))
                   (define L.f.2
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.8 r15)
                       (begin
                         (set! x.1 rdi)
                         (set! x.2 rsi)
                         (begin (set! rax (+ x.1 x.2)) (jump tmp-ra.8 rbp rax)))))
                   (define L.f.3
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.9 r15)
                       (begin
                         (set! y.1 rdi)
                         (begin
                           (set! rdi y.1)
                           (set! rsi 10)
                           (set! r15 tmp-ra.9)
                           (jump L.f.2 rbp r15 rdi rsi)))))
                   (begin
                     (set! tmp-ra.10 r15)
                     (begin
                       (set! x.1 5)
                       (set! x.2 10)
                       (begin
                         (return-point L.rp.4
                                       (begin
                                         (set! rdi x.1)
                                         (set! rsi x.2)
                                         (set! r15 L.rp.4)
                                         (jump L.f.2 rbp r15 rdi rsi)))
                         (set! x.3 rax))
                       (begin
                         (return-point L.rp.5
                                       (begin (set! rdi x.3) (set! r15 L.rp.5) (jump L.f.3 rbp r15 rdi)))
                         (set! x.4 rax))
                       (set! x.5 (- x.3 x.4))
                       (begin (set! rdi x.5) (set! r15 tmp-ra.10) (jump L.f.3 rbp r15 rdi))))))
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda () (* 1 2))) (call L.f.1)))
                '(module
                     ((new-frames ()))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.11 r15)
                       (begin (set! rax (* 1 2)) (jump tmp-ra.11 rbp rax))))
                   (begin
                     (set! tmp-ra.12 r15)
                     (begin (set! r15 tmp-ra.12) (jump L.f.1 rbp r15)))))
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda (a.1 b.1 c.1 d.1 e.1 f.1 g.1 h.1 i.1 j.1 k.1) 10))
                                               (begin
                                                 (set! a.1 1)
                                                 (set! b.1 2)
                                                 (set! c.1 3)
                                                 (set! d.1 4)
                                                 (set! e.1 5)
                                                 (set! f.1 6)
                                                 (set! g.1 7)
                                                 (set! h.1 8)
                                                 (set! i.1 9)
                                                 (set! j.1 10)
                                                 (set! k.1 11)
                                                 (call L.f.1 a.1 b.1 c.1 d.1 e.1 f.1 g.1 h.1 i.1 j.1 k.1))))
                '(module
                     ((new-frames ()))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.13 r15)
                       (begin
                         (set! a.1 rdi)
                         (set! b.1 rsi)
                         (set! c.1 rdx)
                         (set! d.1 rcx)
                         (set! e.1 r8)
                         (set! f.1 r9)
                         (set! g.1 fv0)
                         (set! h.1 fv1)
                         (set! i.1 fv2)
                         (set! j.1 fv3)
                         (set! k.1 fv4)
                         (begin (set! rax 10) (jump tmp-ra.13 rbp rax)))))
                   (begin
                     (set! tmp-ra.14 r15)
                     (begin
                       (set! a.1 1)
                       (set! b.1 2)
                       (set! c.1 3)
                       (set! d.1 4)
                       (set! e.1 5)
                       (set! f.1 6)
                       (set! g.1 7)
                       (set! h.1 8)
                       (set! i.1 9)
                       (set! j.1 10)
                       (set! k.1 11)
                       (begin
                         (set! rdi a.1)
                         (set! rsi b.1)
                         (set! rdx c.1)
                         (set! rcx d.1)
                         (set! r8 e.1)
                         (set! r9 f.1)
                         (set! fv0 g.1)
                         (set! fv1 h.1)
                         (set! fv2 i.1)
                         (set! fv3 j.1)
                         (set! fv4 k.1)
                         (set! r15 tmp-ra.14)
                         (jump L.f.1 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4))))))
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda (a.1 b.1 c.1 d.1 e.1 f.1) (begin
                                                                                                       (set! a.1 (+ a.1 b.1))
                                                                                                       (set! a.1 (+ a.1 c.1))
                                                                                                       (set! a.1 (+ a.1 d.1))
                                                                                                       (set! a.1 (+ a.1 e.1))
                                                                                                       (set! a.1 (+ a.1 f.1))
                                                                                                       a.1)))
                                               (call L.f.1 1 2 3 4 5 6)))
                '(module
                     ((new-frames ()))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.15 r15)
                       (begin
                         (set! a.1 rdi)
                         (set! b.1 rsi)
                         (set! c.1 rdx)
                         (set! d.1 rcx)
                         (set! e.1 r8)
                         (set! f.1 r9)
                         (begin
                           (set! a.1 (+ a.1 b.1))
                           (set! a.1 (+ a.1 c.1))
                           (set! a.1 (+ a.1 d.1))
                           (set! a.1 (+ a.1 e.1))
                           (set! a.1 (+ a.1 f.1))
                           (begin (set! rax a.1) (jump tmp-ra.15 rbp rax))))))
                   (begin
                     (set! tmp-ra.16 r15)
                     (begin
                       (set! rdi 1)
                       (set! rsi 2)
                       (set! rdx 3)
                       (set! rcx 4)
                       (set! r8 5)
                       (set! r9 6)
                       (set! r15 tmp-ra.16)
                       (jump L.f.1 rbp r15 rdi rsi rdx rcx r8 r9)))))
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda (a.1 b.1 c.1 d.1 e.1 f.1 g.1 h.1 i.1 j.1 k.1) (begin
                                                                                                                           (set! a.1 (+ a.1 b.1))
                                                                                                                           (set! a.1 (+ a.1 c.1))
                                                                                                                           (set! a.1 (+ a.1 d.1))
                                                                                                                           (set! a.1 (+ a.1 e.1))
                                                                                                                           (set! a.1 (+ a.1 f.1))
                                                                                                                           (set! a.1 (+ a.1 g.1))
                                                                                                                           (set! a.1 (+ a.1 h.1))
                                                                                                                           (set! a.1 (+ a.1 i.1))
                                                                                                                           (set! a.1 (+ a.1 j.1))
                                                                                                                           (set! a.1 (+ a.1 k.1))
                                                                                                                           a.1)))
                                               (call L.f.1 1 2 3 4 5 6 7 8 9 10 11)))
                '(module
                     ((new-frames ()))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.17 r15)
                       (begin
                         (set! a.1 rdi)
                         (set! b.1 rsi)
                         (set! c.1 rdx)
                         (set! d.1 rcx)
                         (set! e.1 r8)
                         (set! f.1 r9)
                         (set! g.1 fv0)
                         (set! h.1 fv1)
                         (set! i.1 fv2)
                         (set! j.1 fv3)
                         (set! k.1 fv4)
                         (begin
                           (set! a.1 (+ a.1 b.1))
                           (set! a.1 (+ a.1 c.1))
                           (set! a.1 (+ a.1 d.1))
                           (set! a.1 (+ a.1 e.1))
                           (set! a.1 (+ a.1 f.1))
                           (set! a.1 (+ a.1 g.1))
                           (set! a.1 (+ a.1 h.1))
                           (set! a.1 (+ a.1 i.1))
                           (set! a.1 (+ a.1 j.1))
                           (set! a.1 (+ a.1 k.1))
                           (begin (set! rax a.1) (jump tmp-ra.17 rbp rax))))))
                   (begin
                     (set! tmp-ra.18 r15)
                     (begin
                       (set! rdi 1)
                       (set! rsi 2)
                       (set! rdx 3)
                       (set! rcx 4)
                       (set! r8 5)
                       (set! r9 6)
                       (set! fv0 7)
                       (set! fv1 8)
                       (set! fv2 9)
                       (set! fv3 10)
                       (set! fv4 11)
                       (set! r15 tmp-ra.18)
                       (jump L.f.1 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4)))))
  (check-equal? (impose-calling-conventions '(module (define L.f.1 (lambda (x.1 y.1 z.1) (begin
                                                                                           (set! a.1 (+ x.1 y.1))
                                                                                           (* z.1 a.1))))
                                               (call L.f.1 1 2 3)))
                '(module
                     ((new-frames ()))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.19 r15)
                       (begin
                         (set! x.1 rdi)
                         (set! y.1 rsi)
                         (set! z.1 rdx)
                         (begin
                           (set! a.1 (+ x.1 y.1))
                           (begin (set! rax (* z.1 a.1)) (jump tmp-ra.19 rbp rax))))))
                   (begin
                     (set! tmp-ra.20 r15)
                     (begin
                       (set! rdi 1)
                       (set! rsi 2)
                       (set! rdx 3)
                       (set! r15 tmp-ra.20)
                       (jump L.f.1 rbp r15 rdi rsi rdx)))))
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
                     ((new-frames ()))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.21 r15)
                       (begin
                         (set! x.1 rdi)
                         (begin
                           (set! y.1 100)
                           (begin (set! rax (* x.1 y.1)) (jump tmp-ra.21 rbp rax))))))
                   (define L.g.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.22 r15)
                       (begin
                         (set! x.1 rdi)
                         (set! y.1 rsi)
                         (if (not (> x.1 y.1))
                             (begin (set! rdi x.1) (set! r15 tmp-ra.22) (jump L.f.1 rbp r15 rdi))
                             (begin
                               (set! rdi y.1)
                               (set! r15 tmp-ra.22)
                               (jump L.f.1 rbp r15 rdi))))))
                   (begin
                     (set! tmp-ra.23 r15)
                     (begin
                       (set! x.1 1)
                       (set! x.2 2)
                       (if (<= x.1 x.2)
                           (begin (set! rdi x.1) (set! r15 tmp-ra.23) (jump L.f.1 rbp r15 rdi))
                           (begin
                             (set! rdi x.2)
                             (set! rsi x.1)
                             (set! r15 tmp-ra.23)
                             (jump L.g.1 rbp r15 rdi rsi))))))))

