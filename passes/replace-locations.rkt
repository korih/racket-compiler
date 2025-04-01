#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8
  rackunit)

(provide replace-locations)

;; asm-lang-v8/assignments -> nested-asm-lang-fvars-v8
;; compiles p to Nested-asm-lang v8 by replacing all abstract location with
;; physical locations using the assignment described in the assignment info
;; field, and dropping any register-allocation-related metadata from the program
(define (replace-locations p)
  (-> asm-pred-lang-v8/assignments? nested-asm-lang-fvars-v8?)

  ;; func is `(define ,label ,tail)
  ;; interp. a function definition that does not have metadata

  ;; asm-lang-v8/assignments -> (Dict-of aloc rloc)
  ;; interp. creates a dictionary of assignments
  (define (make-assignments-dict assignments)
    (for/fold ([acc (hash)])
              ([pair assignments])
      (dict-set acc (first pair) (second pair))))

  ;; asm-pred-lang-v8/assignments.label asm-pred-lang-v8/assignments.info asm-pred-lang-v8/assignments.tail -> func
  (define (replace-locations-func label info tail)
    (define assignments (make-assignments-dict (info-ref info 'assignment)))
    `(define ,label ,(replace-locations-tail tail assignments)))

  ;; asm-lang-v8/assignments.tail (Dict-of aloc rloc) -> nested-asm-lang-v8.tail
  (define (replace-locations-tail t assignments)
    (match t
      [`(begin ,fx ... ,tail)
       (define compiled-fx (for/list ([e fx])
                             (replace-locations-effect e assignments)))
       (define compiled-tail (replace-locations-tail tail assignments))
       `(begin ,@compiled-fx ,compiled-tail)]
      [`(if ,pred ,t1 ,t2)
       (define pred^ (replace-locations-pred pred assignments))
       (define t1^ (replace-locations-tail t1 assignments))
       (define t2^ (replace-locations-tail t2 assignments))
       `(if ,pred^ ,t1^ ,t2^)]
      [`(jump ,trg ,locs ...)
       `(jump ,(replace-locations-trg trg assignments))]))

  ;; asm-lang-v8/assignments.effect (Dict-of aloc rloc) -> nested-asm-lang-v8.effect
  (define (replace-locations-effect e assignments)
    (match e
      [`(set! ,loc1 (mref ,loc2 ,index))
       (define loc1^ (replace-locations-loc loc1 assignments))
       (define loc2^ (replace-locations-loc loc2 assignments))
       (define index^ (replace-locations-opand index assignments))
       `(set! ,loc1^ (mref ,loc2^ ,index^))]
      [`(set! ,loc (,binop ,loc ,op))
       (define loc^ (replace-locations-loc loc assignments))
       (define op^ (replace-locations-opand op assignments))
       `(set! ,loc^ (,binop ,loc^ ,op^))]
      [`(set! ,loc ,triv)
       (define loc^ (replace-locations-loc loc assignments))
       (define triv^ (replace-locations-triv triv assignments))
       `(set! ,loc^ ,triv^)]
      [`(mset! ,loc ,index ,triv)
       (define loc^ (replace-locations-loc loc assignments))
       (define index^ (replace-locations-opand index assignments))
       (define triv^ (replace-locations-triv triv assignments))
       `(mset! ,loc^ ,index^ ,triv^)]
      [`(begin ,fx ... ,e)
       (define compiled-fx (for/list ([e fx]) (replace-locations-effect e assignments)))
       (define compiled-e (replace-locations-effect e assignments))
       `(begin ,@compiled-fx ,compiled-e)]
      [`(if ,pred ,e1 ,e2)
       (define pred^ (replace-locations-pred pred assignments))
       (define e1^ (replace-locations-effect e1 assignments))
       (define e2^ (replace-locations-effect e2 assignments))
       `(if ,pred^ ,e1^ ,e2^)]
      [`(return-point ,label ,tail) `(return-point ,label ,(replace-locations-tail tail assignments))]))

  ;; asm-lang-v8/assignments.pred (Dict-of aloc rloc) -> nested-asm-lang-v8.pred
  (define (replace-locations-pred p assignments)
    (match p
      ['(true) p]
      ['(false) p]
      [`(begin ,effects ... ,pred)
       (define effects^
         (for/list ([effect effects])
           (replace-locations-effect effect assignments)))
       (define pred^ (replace-locations-pred pred assignments))
       `(begin ,@effects^ ,pred^)]
      [`(not ,pred) `(not ,(replace-locations-pred pred assignments))]
      [`(,relop ,loc ,op)
       (define loc^ (replace-locations-loc loc assignments))
       (define op^ (replace-locations-opand op assignments))
       `(,relop ,loc^ ,op^)]
      [`(if ,p1 ,p2 ,p3)
       (define p1^ (replace-locations-pred p1 assignments))
       (define p2^ (replace-locations-pred p2 assignments))
       (define p3^ (replace-locations-pred p3 assignments))
       `(if ,p1^ ,p2^ ,p3^)]))

  ;; asm-lang-v8/assignments.triv (Dict-of aloc rloc) -> nested-asm-lang-v8.triv
  (define (replace-locations-triv t assignments)
    (match t
      [label #:when (label? label) label]
      [op (replace-locations-opand op assignments)]))

  ;; asm-lang-v8/assignments.opand (Dict-of aloc rloc) -> nested-asm-lang-v8.opand
  (define (replace-locations-opand op assignments)
    (match op
      [int64 #:when (int64? int64) int64]
      [loc (replace-locations-loc loc assignments)]))

  ;; asm-lang-v8/assignments.loc (Dict-of aloc rloc) -> nested-asm-lang-v8.loc
  (define (replace-locations-loc loc assignments)
    (match loc
      [aloc #:when (aloc? aloc) (dict-ref assignments aloc)]
      [rloc #:when (rloc? rloc) rloc]))

  ;; asm-lang-v8/assignments.trg (Dict-of aloc rloc) -> nested-asm-lang-v8.trg
  (define (replace-locations-trg trg assignments)
    (match trg
      [label #:when (label? label) label]
      [loc (replace-locations-loc loc assignments)]))

  (match p
    [`(module ,info (define ,labels ,infos ,tails) ... ,tail)
     `(module ,@(for/list ([label labels] [info infos] [tail tails])
                  (replace-locations-func label info tail))
        ,(replace-locations-tail tail (make-assignments-dict (info-ref info 'assignment))))]))

(module+ test
  (check-equal? (replace-locations '(module ((locals ()) (assignment ())) (define L.id.13 ((locals (x.2)) (assignment ((x.2 rsp)))) (begin (set! x.2 rdi) (jump rax x.2))) (begin (set! rdi 5) (jump L.id.13 rbp rdi))))
                '(module
                     (define L.id.13 (begin (set! rsp rdi) (jump rax)))
                   (begin (set! rdi 5) (jump L.id.13))))
  (check-equal? (replace-locations '(module ((locals (x.1 y.2)) (assignment ((y.2 rsp) (x.1 rsp)))) (begin (set! x.1 3) (set! y.2 x.1) (if (> y.2 x.1) (jump rax x.1) (jump rax y.2)))))
                '(module
                     (begin (set! rsp 3) (set! rsp rsp) (if (> rsp rsp) (jump rax) (jump rax)))))
  (check-equal? (replace-locations '(module ((locals (tmp.18 y.6)) (assignment ((y.6 rbx) (tmp.18 rsp)))) (begin (set! y.6 200) (if (begin (set! tmp.18 3) (< tmp.18 y.6)) (jump rax) (jump rax)))))
                '(module
                     (begin
                       (set! rbx 200)
                       (if (begin (set! rsp 3) (< rsp rbx)) (jump rax) (jump rax)))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (begin (set! x.1 1) (if (> x.1 0) (set! x.1 2) (set! x.1 3)) (jump rax x.1))))
                '(module
                     (begin (set! rax 1) (if (> rax 0) (set! rax 2) (set! rax 3)) (jump rax))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (if (true) (true) (false)) (jump rax 1) (jump rax 0))))
                '(module (if (if (true) (true) (false)) (jump rax) (jump rax))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (begin (set! x.1 1) (begin (set! x.1 2) (set! x.1 3)) (> x.1 0)) (jump rax) (jump rax))))
                '(module
                     (if (begin (set! rax 1) (begin (set! rax 2) (set! rax 3)) (> rax 0))
                         (jump rax)
                         (jump rax))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (begin (set! x.1 1) (set! x.1 1) (> x.1 0)) (jump rax) (jump rax))))
                '(module (if (begin (set! rax 1) (set! rax 1) (> rax 0)) (jump rax) (jump rax))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (begin (set! x.1 1) (> x.1 0)) (jump rax) (jump rax))))
                '(module (if (begin (set! rax 1) (> rax 0)) (jump rax) (jump rax))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (not (> x.1 1)) (jump rax) (jump rax))))
                '(module (if (not (> rax 1)) (jump rax) (jump rax))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (true) (jump rax) (jump rax))))
                '(module (if (true) (jump rax) (jump rax))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (> x.1 1) (jump rax) (jump rax))))
                '(module (if (> rax 1) (jump rax) (jump rax))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (begin (if (true)
                                                 (begin
                                                   (set! x.1 (+ x.1 1))
                                                   (if (> x.1 0) (set! x.1 1) (set! x.1 0))
                                                   (jump rax x.1))
                                                 (begin
                                                   (set! x.1 (+ x.1 2))
                                                   (if (if (true) (false) (false))
                                                       (set! x.1 2)
                                                       (set! x.1 3))
                                                   (jump rax x.1))))))
                '(module
                     (begin
                       (if (true)
                           (begin
                             (set! rax (+ rax 1))
                             (if (> rax 0) (set! rax 1) (set! rax 0))
                             (jump rax))
                           (begin
                             (set! rax (+ rax 2))
                             (if (if (true) (false) (false)) (set! rax 2) (set! rax 3))
                             (jump rax))))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (begin (set! x.1 (+ x.1 1)) (jump rax x.1))))
                '(module (begin (set! rax (+ rax 1)) (jump rax))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax)))) (begin (set! x.1 0) (jump rax x.1))))
                '(module (begin (set! rax 0) (jump rax))))
  (check-equal? (replace-locations '(module ((locals (x.1 y.1 w.1)) (assignment ((x.1 rax) (y.1 rbx) (w.1 r9))))
                                      (begin (set! x.1 0)
                                             (set! y.1 x.1)
                                             (set! w.1 1)
                                             (set! w.1 (+ w.1 y.1))
                                             (jump rax w.1))))
                ' (module
                      (begin
                        (set! rax 0)
                        (set! rbx rax)
                        (set! r9 1)
                        (set! r9 (+ r9 rbx))
                        (jump rax))))

  (define label2 (fresh-label))
  (check-equal? (replace-locations `(module ((locals (x.1)) (assignment ((x.1 r8))))
                                      (define ,label2
                                        ((locals (x.3)) (assignment ((x.3 r9))))
                                        (begin (set! x.3 (+ x.3 1)) (jump rax x.3)))
                                      (begin (set! x.1 0) (jump ,label2 x.1))))
                `(module
                     (define ,label2 (begin (set! r9 (+ r9 1)) (jump rax)))
                   (begin (set! r8 0) (jump ,label2))))

  (define label1 (fresh-label))
  (define label3 (fresh-label))
  (check-equal? (replace-locations `(module ((locals (x.2 x.3)) (assignment ((x.2 rbx) (x.3 r8))))
                                      (define ,label1
                                        ((locals (x.2)) (assignment ((x.2 rbx))))
                                        (begin (set! x.2 0)
                                               (set! x.2 (* x.2 2))
                                               (jump rax x.2)))
                                      (define ,label3
                                        ((locals (x.3)) (assignment ((x.3 r8))))
                                        (begin
                                          (set! x.3 -1)
                                          (set! x.3 (+ x.3 2))
                                          (jump rax x.3)))
                                      (begin (set! x.2 1) (jump ,label1 x.2))))
                `(module
                     (define ,label1 (begin (set! rbx 0) (set! rbx (* rbx 2)) (jump rax)))
                   (define ,label3 (begin (set! r8 -1) (set! r8 (+ r8 2)) (jump rax)))
                   (begin (set! rbx 1) (jump ,label1))))

  (define label4 (fresh-label))
  (define label5 (fresh-label))
  (check-equal? (replace-locations `(module ((locals (x.1 x.2 x.3)) (assignment ((x.1 rax) (x.2 rbx) (x.3 rcx))))
                                      (define ,label4
                                        ((locals (x.4 x.5)) (assignment ((x.4 r13) (x.5 r12))))
                                        (begin (set! x.4 (+ x.4 x.5)) (jump ,label5 x.4 x.5)))
                                      (define ,label5
                                        ((locals (x.5 x.7)) (assignment ((x.5 r12) (x.7 fv0))))
                                        (begin (set! x.5 (* x.5 x.7)) (jump rax x.5)))
                                      (begin (set! x.1 1)
                                             (set! x.2 x.1)
                                             (set! x.2 (+ x.2 x.1))
                                             (set! x.3 x.2)
                                             (jump ,label4 x.3))))
                `(module
                     (define ,label4 (begin (set! r13 (+ r13 r12)) (jump ,label5)))
                   (define ,label5 (begin (set! r12 (* r12 fv0)) (jump rax)))
                   (begin
                     (set! rax 1)
                     (set! rbx rax)
                     (set! rbx (+ rbx rax))
                     (set! rcx rbx)
                     (jump ,label4))))

  (define label6 (fresh-label))
  (define label7 (fresh-label))
  (check-equal? (replace-locations `(module ((locals (x.4 x.5 x.6)) (assignment ((x.4 fv0) (x.5 fv1) (x.6 r15))))
                                      (define ,label6 ((locals (x.4)) (assignment ((x.4 r13))))
                                        (jump ,label7 x.4))
                                      (define ,label7 ((locals (x.5)) (assignment ((x.5 r12))))
                                        (jump ,label6 x.5))
                                      (begin (set! x.4 0)
                                             (set! x.5 3)
                                             (set! x.5 (+ x.5 x.4))
                                             (set! x.6 x.5)
                                             (set! x.6 (* x.6 x.5))
                                             (jump ,label6 x.6))))
                `(module
                     (define ,label6 (jump ,label7))
                   (define ,label7 (jump ,label6))
                   (begin
                     (set! fv0 0)
                     (set! fv1 3)
                     (set! fv1 (+ fv1 fv0))
                     (set! r15 fv1)
                     (set! r15 (* r15 fv1))
                     (jump ,label6))))

  (check-equal? (replace-locations '(module
                                        ((locals ())
                                         (conflicts
                                          ((tmp-ra.4 (fv0 fv1 rbp))
                                           (rbp (r15 fv0 fv1 tmp-ra.4))
                                           (fv1 (r15 fv0 rbp tmp-ra.4))
                                           (fv0 (r15 rbp fv1 tmp-ra.4))
                                           (r15 (rbp fv0 fv1))))
                                         (assignment ((tmp-ra.4 r15))))
                                      (define L.swap.1
                                        ((locals ())
                                         (conflicts
                                          ((y.2 (rbp tmp-ra.1 x.1 nfv.3))
                                           (x.1 (y.2 rbp tmp-ra.1 fv1))
                                           (tmp-ra.1 (y.2 x.1 rbp fv1 fv0 rax z.3))
                                           (z.3 (rbp tmp-ra.1))
                                           (nfv.3 (r15 nfv.2 rbp y.2))
                                           (nfv.2 (r15 rbp nfv.3))
                                           (rbp (y.2 x.1 tmp-ra.1 rax z.3 r15 nfv.2 nfv.3))
                                           (r15 (rbp nfv.2 nfv.3))
                                           (rax (rbp tmp-ra.1))
                                           (fv0 (tmp-ra.1))
                                           (fv1 (x.1 tmp-ra.1))))
                                         (assignment
                                          ((tmp-ra.1 fv2) (nfv.2 fv3) (nfv.3 fv4) (y.2 r15) (x.1 r14) (z.3 r15))))
                                        (begin
                                          (set! tmp-ra.1 r15)
                                          (set! x.1 fv0)
                                          (set! y.2 fv1)
                                          (if (< y.2 x.1)
                                              (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
                                              (begin
                                                (begin
                                                  (set! rbp (- rbp 24))
                                                  (return-point L.rp.1
                                                                (begin
                                                                  (set! nfv.3 x.1)
                                                                  (set! nfv.2 y.2)
                                                                  (set! r15 L.rp.1)
                                                                  (jump L.swap.1 rbp r15 nfv.2 nfv.3)))
                                                  (set! rbp (+ rbp 24)))
                                                (set! z.3 rax)
                                                (set! rax z.3)
                                                (jump tmp-ra.1 rbp rax)))))
                                      (begin
                                        (set! tmp-ra.4 r15)
                                        (set! fv1 2)
                                        (set! fv0 1)
                                        (set! r15 tmp-ra.4)
                                        (jump L.swap.1 rbp r15 fv0 fv1))))
                '(module
                     (define L.swap.1
                       (begin
                         (set! fv2 r15)
                         (set! r14 fv0)
                         (set! r15 fv1)
                         (if (< r15 r14)
                             (begin (set! rax r14) (jump fv2))
                             (begin
                               (begin
                                 (set! rbp (- rbp 24))
                                 (return-point L.rp.1
                                               (begin
                                                 (set! fv4 r14)
                                                 (set! fv3 r15)
                                                 (set! r15 L.rp.1)
                                                 (jump L.swap.1)))
                                 (set! rbp (+ rbp 24)))
                               (set! r15 rax)
                               (set! rax r15)
                               (jump fv2)))))
                   (begin
                     (set! r15 r15)
                     (set! fv1 2)
                     (set! fv0 1)
                     (set! r15 r15)
                     (jump L.swap.1))))

  (check-equal? (replace-locations `(module
                                        ((locals ()) (assignment ()))
                                      (define ,label1
                                        ((locals (x.1)) (assignment ((x.1 rbx))))
                                        (begin
                                          (set! x.1 ,label2)
                                          (set! rcx 0)
                                          (jump x.1 rcx)))
                                      (define ,label2
                                        ((locals ()) (assignment ()))
                                        (jump rax rcx))
                                      (jump ,label1)))
                `(module
                     (define ,label1 (begin (set! rbx ,label2) (set! rcx 0) (jump rbx)))
                   (define ,label2 (jump rax))
                   (jump ,label1)))

  (check-equal? (replace-locations `(module
                                        ((locals ()) (assignment ()))
                                      (define ,label1
                                        ((locals (x.1 x.2)) (assignment ((x.1 rbx) (x.2 rcx))))
                                        (begin
                                          (set! x.1 ,label2)
                                          (set! x.2 -5)
                                          (jump x.1 x.2)))
                                      (define ,label2
                                        ((locals (x.2)) (assignment ((x.2 rcx))))
                                        (jump rax x.2))
                                      (jump ,label1)))
                `(module
                     (define ,label1 (begin (set! rbx ,label2) (set! rcx -5) (jump rbx)))
                   (define ,label2 (jump rax))
                   (jump ,label1)))
  (check-equal? (replace-locations '(module
                                        ((locals ())
                                         (conflicts
                                          ((x.2 (x.3 tmp-ra.2 rbp))
                                           (tmp-ra.2 (rdi x.3 x.2 rbp))
                                           (x.3 (x.2 tmp-ra.2 rbp))
                                           (rbp (r15 rdi x.3 x.2 tmp-ra.2))
                                           (rdi (r15 tmp-ra.2 rbp))
                                           (r15 (rdi rbp))))
                                         (assignment ((x.2 rcx) (tmp-ra.2 rbx) (x.3 rsp))))
                                      (define L.f.1
                                        ((locals ())
                                         (conflicts
                                          ((a.1 (b.1 z.1 x.1 rbp tmp-ra.1))
                                           (z.1 (a.1 y.1 x.1 rbp tmp-ra.1))
                                           (x.1 (b.1 a.1 z.1 y.1 rbp tmp-ra.1))
                                           (y.1 (z.1 x.1 rbp tmp-ra.1))
                                           (b.1 (x.1 a.1 rbp tmp-ra.1))
                                           (tmp-ra.1 (rax b.1 a.1 z.1 y.1 x.1 rdi rbp))
                                           (rbp (rax b.1 a.1 z.1 y.1 x.1 tmp-ra.1))
                                           (rdi (tmp-ra.1))
                                           (rax (rbp tmp-ra.1))))
                                         (assignment
                                          ((y.1 rdx) (z.1 rbx) (a.1 rdx) (x.1 rcx) (b.1 rbx) (tmp-ra.1 rsp))))
                                        (begin
                                          (set! tmp-ra.1 r15)
                                          (set! x.1 rdi)
                                          (set! y.1 1)
                                          (set! z.1 2)
                                          (set! a.1 y.1)
                                          (set! a.1 (bitwise-and a.1 x.1))
                                          (set! b.1 z.1)
                                          (set! b.1 (bitwise-ior b.1 x.1))
                                          (set! a.1 (bitwise-xor a.1 b.1))
                                          (set! rax a.1)
                                          (set! rax (arithmetic-shift-right rax 3))
                                          (jump tmp-ra.1 rbp rax)))
                                      (begin
                                        (set! tmp-ra.2 r15)
                                        (set! x.2 10)
                                        (if (begin (set! x.3 100) (not (!= x.2 x.3)))
                                            (begin (set! rdi x.2) (set! r15 tmp-ra.2) (jump L.f.1 rbp r15 rdi))
                                            (begin (set! rdi 1000) (set! r15 tmp-ra.2) (jump L.f.2 rbp r15 rdi))))))
                '(module
                     (define L.f.1
                       (begin
                         (set! rsp r15)
                         (set! rcx rdi)
                         (set! rdx 1)
                         (set! rbx 2)
                         (set! rdx rdx)
                         (set! rdx (bitwise-and rdx rcx))
                         (set! rbx rbx)
                         (set! rbx (bitwise-ior rbx rcx))
                         (set! rdx (bitwise-xor rdx rbx))
                         (set! rax rdx)
                         (set! rax (arithmetic-shift-right rax 3))
                         (jump rsp)))
                   (begin
                     (set! rbx r15)
                     (set! rcx 10)
                     (if (begin (set! rsp 100) (not (!= rcx rsp)))
                         (begin (set! rdi rcx) (set! r15 rbx) (jump L.f.1))
                         (begin (set! rdi 1000) (set! r15 rbx) (jump L.f.2))))))
  (check-equal? (replace-locations '(module ((new-frames (() ())) (locals ()) (conflicts ((tmp.87 (tmp.88 tmp-ra.95 rbp)) (tmp-ra.95 (rsi rdi tmp.88 tmp.87 rbp)) (tmp.88 (rdi tmp.87 tmp-ra.95 rbp)) (rbp (tmp.88 tmp.87 r15 rsi rdi tmp-ra.95)) (rdi (tmp.88 tmp-ra.95 r15 rsi rbp)) (rsi (tmp-ra.95 r15 rdi rbp)) (r15 (rsi rdi rbp)))) (assignment ((tmp.87 fv0) (tmp-ra.95 fv0) (tmp.88 rsp)))) (define L.*.17 ((new-frames ()) (locals ()) (conflicts ((tmp-ra.93 (rax tmp.82 tmp.80 tmp.81 tmp.78 tmp.79 tmp.42 tmp.41 rdi rsi rbp)) (tmp.79 (tmp.42 tmp.41 rbp tmp-ra.93)) (tmp.81 (tmp.41 tmp.42 rbp tmp-ra.93)) (tmp.42 (tmp.80 tmp.81 tmp.78 tmp.79 tmp.41 rbp tmp-ra.93)) (tmp.82 (rax tmp.41 rbp tmp-ra.93)) (tmp.41 (tmp.82 tmp.80 tmp.81 tmp.78 tmp.79 tmp.42 rsi rbp tmp-ra.93)) (tmp.78 (tmp.42 tmp.41 rbp tmp-ra.93)) (tmp.80 (tmp.42 tmp.41 rbp tmp-ra.93)) (rbp (rax tmp.82 tmp.80 tmp.81 tmp.78 tmp.79 tmp.42 tmp.41 tmp-ra.93)) (rsi (tmp.41 tmp-ra.93)) (rdi (tmp-ra.93)) (rax (tmp.82 rbp tmp-ra.93)))) (assignment ((tmp.82 rsp) (tmp.79 rsp) (tmp.81 rsp) (tmp.78 rsp) (tmp-ra.93 rdx) (tmp.42 rcx) (tmp.41 rbx) (tmp.80 rsp)))) (begin (set! tmp-ra.93 r15) (set! tmp.41 rdi) (set! tmp.42 rsi) (if (begin (if (begin (set! tmp.79 tmp.42) (set! tmp.79 (bitwise-and tmp.79 7)) (= tmp.79 0)) (set! tmp.78 14) (set! tmp.78 6)) (!= tmp.78 6)) (if (begin (if (begin (set! tmp.81 tmp.41) (set! tmp.81 (bitwise-and tmp.81 7)) (= tmp.81 0)) (set! tmp.80 14) (set! tmp.80 6)) (!= tmp.80 6)) (begin (set! tmp.82 tmp.42) (set! tmp.82 (arithmetic-shift-right tmp.82 3)) (set! rax tmp.41) (set! rax (* rax tmp.82)) (jump tmp-ra.93 rbp rax)) (begin (set! rax 318) (jump tmp-ra.93 rbp rax))) (begin (set! rax 318) (jump tmp-ra.93 rbp rax))))) (define L.+.16 ((new-frames ()) (locals ()) (conflicts ((tmp.83 (tmp.39 tmp.40 rbp tmp-ra.94)) (tmp.40 (rax tmp.85 tmp.86 tmp.83 tmp.84 tmp.39 rbp tmp-ra.94)) (tmp.86 (tmp.39 tmp.40 rbp tmp-ra.94)) (tmp.84 (tmp.40 tmp.39 rbp tmp-ra.94)) (tmp-ra.94 (rax tmp.85 tmp.86 tmp.83 tmp.84 tmp.40 tmp.39 rdi rsi rbp)) (tmp.85 (tmp.39 tmp.40 rbp tmp-ra.94)) (tmp.39 (tmp.85 tmp.86 tmp.83 tmp.84 tmp.40 rsi rbp tmp-ra.94)) (rbp (rax tmp.85 tmp.86 tmp.83 tmp.84 tmp.40 tmp.39 tmp-ra.94)) (rsi (tmp.39 tmp-ra.94)) (rdi (tmp-ra.94)) (rax (tmp.40 rbp tmp-ra.94)))) (assignment ((tmp.83 rbx) (tmp.86 rbx) (tmp.84 rbx) (tmp.40 rdx) (tmp-ra.94 rcx) (tmp.85 rbx) (tmp.39 rsp)))) (begin (set! tmp-ra.94 r15) (set! tmp.39 rdi) (set! tmp.40 rsi) (if (begin (if (begin (set! tmp.84 tmp.40) (set! tmp.84 (bitwise-and tmp.84 7)) (= tmp.84 0)) (set! tmp.83 14) (set! tmp.83 6)) (!= tmp.83 6)) (if (begin (if (begin (set! tmp.86 tmp.39) (set! tmp.86 (bitwise-and tmp.86 7)) (= tmp.86 0)) (set! tmp.85 14) (set! tmp.85 6)) (!= tmp.85 6)) (begin (set! rax tmp.39) (set! rax (+ rax tmp.40)) (jump tmp-ra.94 rbp rax)) (begin (set! rax 574) (jump tmp-ra.94 rbp rax))) (begin (set! rax 574) (jump tmp-ra.94 rbp rax))))) (begin (set! tmp-ra.95 r15) (begin (set! rbp (- rbp 16)) (return-point L.rp.19 (begin (set! rdi 40) (set! rsi 48) (set! r15 L.rp.19) (jump L.+.16 rbp r15 rdi rsi))) (set! rbp (+ rbp 16))) (set! tmp.87 rax) (begin (set! rbp (- rbp 16)) (return-point L.rp.20 (begin (set! rdi 32) (set! rsi 40) (set! r15 L.rp.20) (jump L.*.17 rbp r15 rdi rsi))) (set! rbp (+ rbp 16))) (set! tmp.88 rax) (set! rdi tmp.87) (set! rsi tmp.88) (set! r15 tmp-ra.95) (jump L.+.16 rbp r15 rdi rsi))))
                '(module
                     (define L.*.17
                       (begin
                         (set! rdx r15)
                         (set! rbx rdi)
                         (set! rcx rsi)
                         (if (begin
                               (if (begin (set! rsp rcx) (set! rsp (bitwise-and rsp 7)) (= rsp 0))
                                   (set! rsp 14)
                                   (set! rsp 6))
                               (!= rsp 6))
                             (if (begin
                                   (if (begin
                                         (set! rsp rbx)
                                         (set! rsp (bitwise-and rsp 7))
                                         (= rsp 0))
                                       (set! rsp 14)
                                       (set! rsp 6))
                                   (!= rsp 6))
                                 (begin
                                   (set! rsp rcx)
                                   (set! rsp (arithmetic-shift-right rsp 3))
                                   (set! rax rbx)
                                   (set! rax (* rax rsp))
                                   (jump rdx))
                                 (begin (set! rax 318) (jump rdx)))
                             (begin (set! rax 318) (jump rdx)))))
                   (define L.+.16
                     (begin
                       (set! rcx r15)
                       (set! rsp rdi)
                       (set! rdx rsi)
                       (if (begin
                             (if (begin (set! rbx rdx) (set! rbx (bitwise-and rbx 7)) (= rbx 0))
                                 (set! rbx 14)
                                 (set! rbx 6))
                             (!= rbx 6))
                           (if (begin
                                 (if (begin
                                       (set! rbx rsp)
                                       (set! rbx (bitwise-and rbx 7))
                                       (= rbx 0))
                                     (set! rbx 14)
                                     (set! rbx 6))
                                 (!= rbx 6))
                               (begin (set! rax rsp) (set! rax (+ rax rdx)) (jump rcx))
                               (begin (set! rax 574) (jump rcx)))
                           (begin (set! rax 574) (jump rcx)))))
                   (begin
                     (set! fv0 r15)
                     (begin
                       (set! rbp (- rbp 16))
                       (return-point L.rp.19
                                     (begin (set! rdi 40) (set! rsi 48) (set! r15 L.rp.19) (jump L.+.16)))
                       (set! rbp (+ rbp 16)))
                     (set! fv0 rax)
                     (begin
                       (set! rbp (- rbp 16))
                       (return-point L.rp.20
                                     (begin (set! rdi 32) (set! rsi 40) (set! r15 L.rp.20) (jump L.*.17)))
                       (set! rbp (+ rbp 16)))
                     (set! rsp rax)
                     (set! rdi fv0)
                     (set! rsi rsp)
                     (set! r15 fv0)
                     (jump L.+.16))))
  (check-equal? (replace-locations '(module
                                        ((locals ())
                                         (conflicts
                                          ((tmp-ra.238 (fv0 r9 r8 rcx rdx rsi rdi rbp))
                                           (rbp (r15 fv0 r9 r8 rcx rdx rsi rdi tmp-ra.238))
                                           (rdi (r15 fv0 r9 r8 rcx rdx rsi rbp tmp-ra.238))
                                           (rsi (r15 fv0 r9 r8 rcx rdx rbp rdi tmp-ra.238))
                                           (rdx (r15 fv0 r9 r8 rcx rbp rdi rsi tmp-ra.238))
                                           (rcx (r15 fv0 r9 r8 rbp rdi rsi rdx tmp-ra.238))
                                           (r8 (r15 fv0 r9 rbp rdi rsi rdx rcx tmp-ra.238))
                                           (r9 (r15 fv0 rbp rdi rsi rdx rcx r8 tmp-ra.238))
                                           (fv0 (r15 rbp rdi rsi rdx rcx r8 r9 tmp-ra.238))
                                           (r15 (rbp rdi rsi rdx rcx r8 r9 fv0))))
                                         (assignment ((tmp-ra.238 r15))))
                                      (define L.+.31
                                        ((locals ())
                                         (conflicts
                                          ((tmp.183 (rbp tmp-ra.232 tmp.97 tmp.96))
                                           (tmp.97 (rbp tmp-ra.232 tmp.96 tmp.184 tmp.183 tmp.186 tmp.185 rax))
                                           (tmp.184 (tmp.97 rbp tmp-ra.232 tmp.96))
                                           (tmp-ra.232
                                            (tmp.97 tmp.96 rbp rsi rdi tmp.184 tmp.183 tmp.186 tmp.185 rax))
                                           (tmp.186 (tmp.96 rbp tmp-ra.232 tmp.97))
                                           (tmp.96 (tmp.97 rbp tmp-ra.232 rsi tmp.184 tmp.183 tmp.186 tmp.185))
                                           (tmp.185 (rbp tmp-ra.232 tmp.97 tmp.96))
                                           (rax (tmp.97 rbp tmp-ra.232))
                                           (rbp (tmp.97 tmp.96 tmp-ra.232 tmp.184 tmp.183 tmp.186 tmp.185 rax))
                                           (rdi (tmp-ra.232))
                                           (rsi (tmp.96 tmp-ra.232))))
                                         (assignment
                                          ((tmp-ra.232 r15)
                                           (tmp.97 r14)
                                           (tmp.96 r13)
                                           (tmp.183 r9)
                                           (tmp.184 r9)
                                           (tmp.186 r9)
                                           (tmp.185 r9))))
                                        (begin
                                          (set! tmp-ra.232 r15)
                                          (set! tmp.96 rdi)
                                          (set! tmp.97 rsi)
                                          (if (begin
                                                (if (begin
                                                      (set! tmp.184 tmp.97)
                                                      (set! tmp.184 (bitwise-and tmp.184 7))
                                                      (= tmp.184 0))
                                                    (set! tmp.183 14)
                                                    (set! tmp.183 6))
                                                (!= tmp.183 6))
                                              (if (begin
                                                    (if (begin
                                                          (set! tmp.186 tmp.96)
                                                          (set! tmp.186 (bitwise-and tmp.186 7))
                                                          (= tmp.186 0))
                                                        (set! tmp.185 14)
                                                        (set! tmp.185 6))
                                                    (!= tmp.185 6))
                                                  (begin
                                                    (set! rax tmp.96)
                                                    (set! rax (+ rax tmp.97))
                                                    (jump tmp-ra.232 rbp rax))
                                                  (begin (set! rax 574) (jump tmp-ra.232 rbp rax)))
                                              (begin (set! rax 574) (jump tmp-ra.232 rbp rax)))))
                                      (define L.F.6
                                        ((locals ())
                                         (conflicts
                                          ((tmp-ra.233
                                            (tmp.187
                                             g.25
                                             f.24
                                             e.23
                                             d.22
                                             c.21
                                             b.20
                                             a.19
                                             rbp
                                             fv0
                                             r9
                                             r8
                                             rcx
                                             rdx
                                             rsi
                                             rdi))
                                           (a.19
                                            (g.25 f.24 e.23 d.22 c.21 b.20 rbp tmp-ra.233 fv0 r9 r8 rcx rdx rsi))
                                           (b.20
                                            (rdi g.25 f.24 e.23 d.22 c.21 rbp tmp-ra.233 a.19 fv0 r9 r8 rcx rdx))
                                           (c.21
                                            (rsi rdi g.25 f.24 e.23 d.22 rbp tmp-ra.233 a.19 b.20 fv0 r9 r8 rcx))
                                           (d.22
                                            (rdx rsi rdi g.25 f.24 e.23 rbp tmp-ra.233 a.19 b.20 c.21 fv0 r9 r8))
                                           (e.23
                                            (rcx rdx rsi rdi g.25 f.24 rbp tmp-ra.233 a.19 b.20 c.21 d.22 fv0 r9))
                                           (f.24
                                            (r8 rcx rdx rsi rdi g.25 rbp tmp-ra.233 a.19 b.20 c.21 d.22 e.23 fv0))
                                           (g.25
                                            (r9 r8 rcx rdx rsi rdi rbp tmp-ra.233 a.19 b.20 c.21 d.22 e.23 f.24))
                                           (nfv.235 (r15 rbp rdi rsi rdx rcx r8 r9 nfv.234))
                                           (nfv.234 (r15 nfv.235 rbp rdi rsi rdx rcx r8 r9))
                                           (tmp.187 (rdi rbp tmp-ra.233))
                                           (rdi
                                            (tmp.187
                                             r15
                                             nfv.235
                                             nfv.234
                                             r9
                                             r8
                                             rcx
                                             rdx
                                             rsi
                                             rbp
                                             g.25
                                             f.24
                                             e.23
                                             d.22
                                             c.21
                                             b.20
                                             tmp-ra.233))
                                           (rsi
                                            (r15
                                             nfv.235
                                             nfv.234
                                             r9
                                             r8
                                             rcx
                                             rdx
                                             rbp
                                             rdi
                                             g.25
                                             f.24
                                             e.23
                                             d.22
                                             c.21
                                             a.19
                                             tmp-ra.233))
                                           (rdx
                                            (r15
                                             nfv.235
                                             nfv.234
                                             r9
                                             r8
                                             rcx
                                             rbp
                                             rdi
                                             rsi
                                             g.25
                                             f.24
                                             e.23
                                             d.22
                                             b.20
                                             a.19
                                             tmp-ra.233))
                                           (rcx
                                            (r15
                                             nfv.235
                                             nfv.234
                                             r9
                                             r8
                                             rbp
                                             rdi
                                             rsi
                                             rdx
                                             g.25
                                             f.24
                                             e.23
                                             c.21
                                             b.20
                                             a.19
                                             tmp-ra.233))
                                           (r8
                                            (r15
                                             nfv.235
                                             nfv.234
                                             r9
                                             rbp
                                             rdi
                                             rsi
                                             rdx
                                             rcx
                                             g.25
                                             f.24
                                             d.22
                                             c.21
                                             b.20
                                             a.19
                                             tmp-ra.233))
                                           (r9
                                            (r15
                                             nfv.235
                                             nfv.234
                                             rbp
                                             rdi
                                             rsi
                                             rdx
                                             rcx
                                             r8
                                             g.25
                                             e.23
                                             d.22
                                             c.21
                                             b.20
                                             a.19
                                             tmp-ra.233))
                                           (fv0 (f.24 e.23 d.22 c.21 b.20 a.19 tmp-ra.233))
                                           (rbp
                                            (tmp.187
                                             r15
                                             nfv.235
                                             nfv.234
                                             r9
                                             r8
                                             rcx
                                             rdx
                                             rsi
                                             rdi
                                             g.25
                                             f.24
                                             e.23
                                             d.22
                                             c.21
                                             b.20
                                             a.19
                                             tmp-ra.233))
                                           (r15 (rbp rdi rsi rdx rcx r8 r9 nfv.234 nfv.235))))
                                         (assignment
                                          ((tmp-ra.233 fv1)
                                           (a.19 r15)
                                           (b.20 r14)
                                           (c.21 r13)
                                           (d.22 rcx)
                                           (e.23 r8)
                                           (f.24 r9)
                                           (g.25 rbx)
                                           (nfv.235 r14)
                                           (nfv.234 r13)
                                           (tmp.187 r15))))
                                        (begin
                                          (set! tmp-ra.233 r15)
                                          (set! a.19 rdi)
                                          (set! b.20 rsi)
                                          (set! c.21 rdx)
                                          (set! d.22 rcx)
                                          (set! e.23 r8)
                                          (set! f.24 r9)
                                          (set! g.25 fv0)
                                          (begin
                                            (set! rbp (- rbp 16))
                                            (return-point L.rp.47
                                                          (begin
                                                            (set! rdi a.19)
                                                            (set! rsi b.20)
                                                            (set! rdx c.21)
                                                            (set! rcx d.22)
                                                            (set! r8 e.23)
                                                            (set! r9 f.24)
                                                            (set! nfv.234 g.25)
                                                            (set! nfv.235 64)
                                                            (set! r15 L.rp.47)
                                                            (jump L.G.7 rbp r15 rdi rsi rdx rcx r8 r9 nfv.234 nfv.235)))
                                            (set! rbp (+ rbp 16)))
                                          (set! tmp.187 rax)
                                          (set! rdi 80)
                                          (set! rsi tmp.187)
                                          (set! r15 tmp-ra.233)
                                          (jump L.+.31 rbp r15 rdi rsi)))
                                      (define L.G.7
                                        ((locals ())
                                         (conflicts
                                          ((tmp-ra.236
                                            (fv2
                                             h.33
                                             g.32
                                             f.31
                                             e.30
                                             d.29
                                             c.28
                                             b.27
                                             a.26
                                             rbp
                                             fv1
                                             fv0
                                             r9
                                             r8
                                             rcx
                                             rdx
                                             rsi
                                             rdi))
                                           (a.26
                                            (h.33
                                             g.32
                                             f.31
                                             e.30
                                             d.29
                                             c.28
                                             b.27
                                             rbp
                                             tmp-ra.236
                                             fv1
                                             fv0
                                             r9
                                             r8
                                             rcx
                                             rdx
                                             rsi))
                                           (b.27
                                            (rdi
                                             h.33
                                             g.32
                                             f.31
                                             e.30
                                             d.29
                                             c.28
                                             rbp
                                             tmp-ra.236
                                             a.26
                                             fv1
                                             fv0
                                             r9
                                             r8
                                             rcx
                                             rdx))
                                           (c.28
                                            (rsi
                                             rdi
                                             h.33
                                             g.32
                                             f.31
                                             e.30
                                             d.29
                                             rbp
                                             tmp-ra.236
                                             b.27
                                             a.26
                                             fv1
                                             fv0
                                             r9
                                             r8
                                             rcx))
                                           (d.29
                                            (rdx
                                             rsi
                                             rdi
                                             h.33
                                             g.32
                                             f.31
                                             e.30
                                             rbp
                                             tmp-ra.236
                                             c.28
                                             b.27
                                             a.26
                                             fv1
                                             fv0
                                             r9
                                             r8))
                                           (e.30
                                            (rcx
                                             rdx
                                             rsi
                                             rdi
                                             h.33
                                             g.32
                                             f.31
                                             rbp
                                             tmp-ra.236
                                             d.29
                                             c.28
                                             b.27
                                             a.26
                                             fv1
                                             fv0
                                             r9))
                                           (f.31
                                            (r8
                                             rcx
                                             rdx
                                             rsi
                                             rdi
                                             h.33
                                             g.32
                                             rbp
                                             tmp-ra.236
                                             e.30
                                             d.29
                                             c.28
                                             b.27
                                             a.26
                                             fv1
                                             fv0))
                                           (g.32
                                            (r9
                                             r8
                                             rcx
                                             rdx
                                             rsi
                                             rdi
                                             h.33
                                             rbp
                                             tmp-ra.236
                                             f.31
                                             e.30
                                             d.29
                                             c.28
                                             b.27
                                             a.26
                                             fv1))
                                           (h.33
                                            (fv0
                                             r9
                                             r8
                                             rcx
                                             rdx
                                             rsi
                                             rdi
                                             rbp
                                             tmp-ra.236
                                             g.32
                                             f.31
                                             e.30
                                             d.29
                                             c.28
                                             b.27
                                             a.26))
                                           (rdi
                                            (r15
                                             fv2
                                             fv1
                                             fv0
                                             r9
                                             r8
                                             rcx
                                             rdx
                                             rsi
                                             rbp
                                             h.33
                                             g.32
                                             f.31
                                             e.30
                                             d.29
                                             c.28
                                             b.27
                                             tmp-ra.236))
                                           (rsi
                                            (r15
                                             fv2
                                             fv1
                                             fv0
                                             r9
                                             r8
                                             rcx
                                             rdx
                                             rbp
                                             rdi
                                             h.33
                                             g.32
                                             f.31
                                             e.30
                                             d.29
                                             c.28
                                             a.26
                                             tmp-ra.236))
                                           (rdx
                                            (r15
                                             fv2
                                             fv1
                                             fv0
                                             r9
                                             r8
                                             rcx
                                             rbp
                                             rdi
                                             rsi
                                             h.33
                                             g.32
                                             f.31
                                             e.30
                                             d.29
                                             b.27
                                             a.26
                                             tmp-ra.236))
                                           (rcx
                                            (r15
                                             fv2
                                             fv1
                                             fv0
                                             r9
                                             r8
                                             rbp
                                             rdi
                                             rsi
                                             rdx
                                             h.33
                                             g.32
                                             f.31
                                             e.30
                                             c.28
                                             b.27
                                             a.26
                                             tmp-ra.236))
                                           (r8
                                            (r15
                                             fv2
                                             fv1
                                             fv0
                                             r9
                                             rbp
                                             rdi
                                             rsi
                                             rdx
                                             rcx
                                             h.33
                                             g.32
                                             f.31
                                             d.29
                                             c.28
                                             b.27
                                             a.26
                                             tmp-ra.236))
                                           (r9
                                            (r15
                                             fv2
                                             fv1
                                             fv0
                                             rbp
                                             rdi
                                             rsi
                                             rdx
                                             rcx
                                             r8
                                             h.33
                                             g.32
                                             e.30
                                             d.29
                                             c.28
                                             b.27
                                             a.26
                                             tmp-ra.236))
                                           (fv0
                                            (r15
                                             fv2
                                             fv1
                                             rbp
                                             rdi
                                             rsi
                                             rdx
                                             rcx
                                             r8
                                             r9
                                             h.33
                                             f.31
                                             e.30
                                             d.29
                                             c.28
                                             b.27
                                             a.26
                                             tmp-ra.236))
                                           (fv1
                                            (r15
                                             fv2
                                             rbp
                                             rdi
                                             rsi
                                             rdx
                                             rcx
                                             r8
                                             r9
                                             fv0
                                             g.32
                                             f.31
                                             e.30
                                             d.29
                                             c.28
                                             b.27
                                             a.26
                                             tmp-ra.236))
                                           (rbp
                                            (r15
                                             fv2
                                             fv1
                                             fv0
                                             r9
                                             r8
                                             rcx
                                             rdx
                                             rsi
                                             rdi
                                             h.33
                                             g.32
                                             f.31
                                             e.30
                                             d.29
                                             c.28
                                             b.27
                                             a.26
                                             tmp-ra.236))
                                           (fv2 (r15 rbp rdi rsi rdx rcx r8 r9 fv0 fv1 tmp-ra.236))
                                           (r15 (rbp rdi rsi rdx rcx r8 r9 fv0 fv1 fv2))))
                                         (assignment
                                          ((tmp-ra.236 r15)
                                           (a.26 r14)
                                           (b.27 r13)
                                           (c.28 rdx)
                                           (d.29 rcx)
                                           (e.30 r8)
                                           (f.31 r9)
                                           (g.32 rbx)
                                           (h.33 rsp))))
                                        (begin
                                          (set! tmp-ra.236 r15)
                                          (set! a.26 rdi)
                                          (set! b.27 rsi)
                                          (set! c.28 rdx)
                                          (set! d.29 rcx)
                                          (set! e.30 r8)
                                          (set! f.31 r9)
                                          (set! g.32 fv0)
                                          (set! h.33 fv1)
                                          (set! rdi a.26)
                                          (set! rsi b.27)
                                          (set! rdx c.28)
                                          (set! rcx d.29)
                                          (set! r8 e.30)
                                          (set! r9 f.31)
                                          (set! fv0 g.32)
                                          (set! fv1 h.33)
                                          (set! fv2 72)
                                          (set! r15 tmp-ra.236)
                                          (jump L.H.8 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2)))
                                      (define L.H.8
                                        ((locals ())
                                         (conflicts
                                          ((tmp-ra.237
                                            (r7.49
                                             r6.48
                                             r5.47
                                             r4.46
                                             r3.45
                                             r2.44
                                             r1.43
                                             j.42
                                             h.41
                                             g.40
                                             f.39
                                             e.38
                                             d.37
                                             c.36
                                             b.35
                                             a.34
                                             rbp
                                             fv2
                                             fv1
                                             fv0
                                             r9
                                             r8
                                             rcx
                                             rdx
                                             rsi
                                             rdi))
                                           (a.34
                                            (j.42
                                             h.41
                                             g.40
                                             f.39
                                             e.38
                                             d.37
                                             c.36
                                             b.35
                                             rbp
                                             tmp-ra.237
                                             fv2
                                             fv1
                                             fv0
                                             r9
                                             r8
                                             rcx
                                             rdx
                                             rsi))
                                           (b.35
                                            (rdi
                                             j.42
                                             h.41
                                             g.40
                                             f.39
                                             e.38
                                             d.37
                                             c.36
                                             rbp
                                             tmp-ra.237
                                             a.34
                                             fv2
                                             fv1
                                             fv0
                                             r9
                                             r8
                                             rcx
                                             rdx))
                                           (c.36
                                            (rdi
                                             r1.43
                                             j.42
                                             h.41
                                             g.40
                                             f.39
                                             e.38
                                             d.37
                                             rbp
                                             tmp-ra.237
                                             a.34
                                             b.35
                                             fv2
                                             fv1
                                             fv0
                                             r9
                                             r8
                                             rcx))
                                           (d.37
                                            (rdi
                                             r2.44
                                             r1.43
                                             j.42
                                             h.41
                                             g.40
                                             f.39
                                             e.38
                                             rbp
                                             tmp-ra.237
                                             c.36
                                             a.34
                                             b.35
                                             fv2
                                             fv1
                                             fv0
                                             r9
                                             r8))
                                           (e.38
                                            (rdi
                                             r3.45
                                             r2.44
                                             r1.43
                                             j.42
                                             h.41
                                             g.40
                                             f.39
                                             rbp
                                             tmp-ra.237
                                             d.37
                                             c.36
                                             a.34
                                             b.35
                                             fv2
                                             fv1
                                             fv0
                                             r9))
                                           (f.39
                                            (rdi
                                             r4.46
                                             r3.45
                                             r2.44
                                             r1.43
                                             j.42
                                             h.41
                                             g.40
                                             rbp
                                             tmp-ra.237
                                             e.38
                                             d.37
                                             c.36
                                             a.34
                                             b.35
                                             fv2
                                             fv1
                                             fv0))
                                           (g.40
                                            (rdi
                                             r5.47
                                             r4.46
                                             r3.45
                                             r2.44
                                             r1.43
                                             j.42
                                             h.41
                                             rbp
                                             tmp-ra.237
                                             f.39
                                             e.38
                                             d.37
                                             c.36
                                             a.34
                                             b.35
                                             fv2
                                             fv1))
                                           (h.41
                                            (rdi
                                             r6.48
                                             r5.47
                                             r4.46
                                             r3.45
                                             r2.44
                                             r1.43
                                             j.42
                                             rbp
                                             tmp-ra.237
                                             g.40
                                             f.39
                                             e.38
                                             d.37
                                             c.36
                                             a.34
                                             b.35
                                             fv2))
                                           (j.42
                                            (rdi
                                             r7.49
                                             r6.48
                                             r5.47
                                             r4.46
                                             r3.45
                                             r2.44
                                             r1.43
                                             rbp
                                             tmp-ra.237
                                             h.41
                                             g.40
                                             f.39
                                             e.38
                                             d.37
                                             c.36
                                             a.34
                                             b.35))
                                           (r1.43 (rbp tmp-ra.237 j.42 h.41 g.40 f.39 e.38 d.37 c.36))
                                           (r2.44 (rbp tmp-ra.237 j.42 h.41 g.40 f.39 e.38 d.37))
                                           (r3.45 (rbp tmp-ra.237 j.42 h.41 g.40 f.39 e.38))
                                           (r4.46 (rbp tmp-ra.237 j.42 h.41 g.40 f.39))
                                           (r5.47 (rbp tmp-ra.237 j.42 h.41 g.40))
                                           (r6.48 (rbp tmp-ra.237 j.42 h.41))
                                           (r7.49 (rbp tmp-ra.237 j.42))
                                           (rdi (j.42 h.41 g.40 f.39 e.38 d.37 c.36 r15 rsi rbp b.35 tmp-ra.237))
                                           (rsi (r15 rbp rdi a.34 tmp-ra.237))
                                           (rdx (b.35 a.34 tmp-ra.237))
                                           (rcx (c.36 b.35 a.34 tmp-ra.237))
                                           (r8 (d.37 c.36 b.35 a.34 tmp-ra.237))
                                           (r9 (e.38 d.37 c.36 b.35 a.34 tmp-ra.237))
                                           (fv0 (f.39 e.38 d.37 c.36 b.35 a.34 tmp-ra.237))
                                           (fv1 (g.40 f.39 e.38 d.37 c.36 b.35 a.34 tmp-ra.237))
                                           (fv2 (h.41 g.40 f.39 e.38 d.37 c.36 b.35 a.34 tmp-ra.237))
                                           (rbp
                                            (r7.49
                                             r6.48
                                             r5.47
                                             r4.46
                                             r3.45
                                             r2.44
                                             r1.43
                                             r15
                                             rsi
                                             rdi
                                             j.42
                                             h.41
                                             g.40
                                             f.39
                                             e.38
                                             d.37
                                             c.36
                                             b.35
                                             a.34
                                             tmp-ra.237))
                                           (r15 (rbp rdi rsi))))
                                         (assignment
                                          ((tmp-ra.237 fv3)
                                           (j.42 fv0)
                                           (h.41 fv1)
                                           (g.40 fv4)
                                           (f.39 fv5)
                                           (e.38 fv6)
                                           (d.37 fv7)
                                           (c.36 fv8)
                                           (a.34 r15)
                                           (b.35 r14)
                                           (r1.43 r15)
                                           (r2.44 r15)
                                           (r3.45 r15)
                                           (r4.46 r15)
                                           (r5.47 r15)
                                           (r6.48 r15)
                                           (r7.49 r15))))
                                        (begin
                                          (set! tmp-ra.237 r15)
                                          (set! a.34 rdi)
                                          (set! b.35 rsi)
                                          (set! c.36 rdx)
                                          (set! d.37 rcx)
                                          (set! e.38 r8)
                                          (set! f.39 r9)
                                          (set! g.40 fv0)
                                          (set! h.41 fv1)
                                          (set! j.42 fv2)
                                          (begin
                                            (set! rbp (- rbp 72))
                                            (return-point L.rp.48
                                                          (begin
                                                            (set! rdi a.34)
                                                            (set! rsi b.35)
                                                            (set! r15 L.rp.48)
                                                            (jump L.+.31 rbp r15 rdi rsi)))
                                            (set! rbp (+ rbp 72)))
                                          (set! r1.43 rax)
                                          (begin
                                            (set! rbp (- rbp 72))
                                            (return-point L.rp.49
                                                          (begin
                                                            (set! rdi r1.43)
                                                            (set! rsi c.36)
                                                            (set! r15 L.rp.49)
                                                            (jump L.+.31 rbp r15 rdi rsi)))
                                            (set! rbp (+ rbp 72)))
                                          (set! r2.44 rax)
                                          (begin
                                            (set! rbp (- rbp 72))
                                            (return-point L.rp.50
                                                          (begin
                                                            (set! rdi r2.44)
                                                            (set! rsi d.37)
                                                            (set! r15 L.rp.50)
                                                            (jump L.+.31 rbp r15 rdi rsi)))
                                            (set! rbp (+ rbp 72)))
                                          (set! r3.45 rax)
                                          (begin
                                            (set! rbp (- rbp 72))
                                            (return-point L.rp.51
                                                          (begin
                                                            (set! rdi r3.45)
                                                            (set! rsi e.38)
                                                            (set! r15 L.rp.51)
                                                            (jump L.+.31 rbp r15 rdi rsi)))
                                            (set! rbp (+ rbp 72)))
                                          (set! r4.46 rax)
                                          (begin
                                            (set! rbp (- rbp 72))
                                            (return-point L.rp.52
                                                          (begin
                                                            (set! rdi r4.46)
                                                            (set! rsi f.39)
                                                            (set! r15 L.rp.52)
                                                            (jump L.+.31 rbp r15 rdi rsi)))
                                            (set! rbp (+ rbp 72)))
                                          (set! r5.47 rax)
                                          (begin
                                            (set! rbp (- rbp 72))
                                            (return-point L.rp.53
                                                          (begin
                                                            (set! rdi r5.47)
                                                            (set! rsi g.40)
                                                            (set! r15 L.rp.53)
                                                            (jump L.+.31 rbp r15 rdi rsi)))
                                            (set! rbp (+ rbp 72)))
                                          (set! r6.48 rax)
                                          (begin
                                            (set! rbp (- rbp 72))
                                            (return-point L.rp.54
                                                          (begin
                                                            (set! rdi r6.48)
                                                            (set! rsi h.41)
                                                            (set! r15 L.rp.54)
                                                            (jump L.+.31 rbp r15 rdi rsi)))
                                            (set! rbp (+ rbp 72)))
                                          (set! r7.49 rax)
                                          (set! rdi r7.49)
                                          (set! rsi j.42)
                                          (set! r15 tmp-ra.237)
                                          (jump L.+.31 rbp r15 rdi rsi)))
                                      (begin
                                        (set! tmp-ra.238 r15)
                                        (set! rdi 8)
                                        (set! rsi 16)
                                        (set! rdx 24)
                                        (set! rcx 32)
                                        (set! r8 40)
                                        (set! r9 48)
                                        (set! fv0 56)
                                        (set! r15 tmp-ra.238)
                                        (jump L.F.6 rbp r15 rdi rsi rdx rcx r8 r9 fv0))))
                '(module
                     (define L.+.31
                       (begin
                         (set! r15 r15)
                         (set! r13 rdi)
                         (set! r14 rsi)
                         (if (begin
                               (if (begin (set! r9 r14) (set! r9 (bitwise-and r9 7)) (= r9 0))
                                   (set! r9 14)
                                   (set! r9 6))
                               (!= r9 6))
                             (if (begin
                                   (if (begin (set! r9 r13) (set! r9 (bitwise-and r9 7)) (= r9 0))
                                       (set! r9 14)
                                       (set! r9 6))
                                   (!= r9 6))
                                 (begin (set! rax r13) (set! rax (+ rax r14)) (jump r15))
                                 (begin (set! rax 574) (jump r15)))
                             (begin (set! rax 574) (jump r15)))))
                   (define L.F.6
                     (begin
                       (set! fv1 r15)
                       (set! r15 rdi)
                       (set! r14 rsi)
                       (set! r13 rdx)
                       (set! rcx rcx)
                       (set! r8 r8)
                       (set! r9 r9)
                       (set! rbx fv0)
                       (begin
                         (set! rbp (- rbp 16))
                         (return-point L.rp.47
                                       (begin
                                         (set! rdi r15)
                                         (set! rsi r14)
                                         (set! rdx r13)
                                         (set! rcx rcx)
                                         (set! r8 r8)
                                         (set! r9 r9)
                                         (set! r13 rbx)
                                         (set! r14 64)
                                         (set! r15 L.rp.47)
                                         (jump L.G.7)))
                         (set! rbp (+ rbp 16)))
                       (set! r15 rax)
                       (set! rdi 80)
                       (set! rsi r15)
                       (set! r15 fv1)
                       (jump L.+.31)))
                   (define L.G.7
                     (begin
                       (set! r15 r15)
                       (set! r14 rdi)
                       (set! r13 rsi)
                       (set! rdx rdx)
                       (set! rcx rcx)
                       (set! r8 r8)
                       (set! r9 r9)
                       (set! rbx fv0)
                       (set! rsp fv1)
                       (set! rdi r14)
                       (set! rsi r13)
                       (set! rdx rdx)
                       (set! rcx rcx)
                       (set! r8 r8)
                       (set! r9 r9)
                       (set! fv0 rbx)
                       (set! fv1 rsp)
                       (set! fv2 72)
                       (set! r15 r15)
                       (jump L.H.8)))
                   (define L.H.8
                     (begin
                       (set! fv3 r15)
                       (set! r15 rdi)
                       (set! r14 rsi)
                       (set! fv8 rdx)
                       (set! fv7 rcx)
                       (set! fv6 r8)
                       (set! fv5 r9)
                       (set! fv4 fv0)
                       (set! fv1 fv1)
                       (set! fv0 fv2)
                       (begin
                         (set! rbp (- rbp 72))
                         (return-point L.rp.48
                                       (begin
                                         (set! rdi r15)
                                         (set! rsi r14)
                                         (set! r15 L.rp.48)
                                         (jump L.+.31)))
                         (set! rbp (+ rbp 72)))
                       (set! r15 rax)
                       (begin
                         (set! rbp (- rbp 72))
                         (return-point L.rp.49
                                       (begin
                                         (set! rdi r15)
                                         (set! rsi fv8)
                                         (set! r15 L.rp.49)
                                         (jump L.+.31)))
                         (set! rbp (+ rbp 72)))
                       (set! r15 rax)
                       (begin
                         (set! rbp (- rbp 72))
                         (return-point L.rp.50
                                       (begin
                                         (set! rdi r15)
                                         (set! rsi fv7)
                                         (set! r15 L.rp.50)
                                         (jump L.+.31)))
                         (set! rbp (+ rbp 72)))
                       (set! r15 rax)
                       (begin
                         (set! rbp (- rbp 72))
                         (return-point L.rp.51
                                       (begin
                                         (set! rdi r15)
                                         (set! rsi fv6)
                                         (set! r15 L.rp.51)
                                         (jump L.+.31)))
                         (set! rbp (+ rbp 72)))
                       (set! r15 rax)
                       (begin
                         (set! rbp (- rbp 72))
                         (return-point L.rp.52
                                       (begin
                                         (set! rdi r15)
                                         (set! rsi fv5)
                                         (set! r15 L.rp.52)
                                         (jump L.+.31)))
                         (set! rbp (+ rbp 72)))
                       (set! r15 rax)
                       (begin
                         (set! rbp (- rbp 72))
                         (return-point L.rp.53
                                       (begin
                                         (set! rdi r15)
                                         (set! rsi fv4)
                                         (set! r15 L.rp.53)
                                         (jump L.+.31)))
                         (set! rbp (+ rbp 72)))
                       (set! r15 rax)
                       (begin
                         (set! rbp (- rbp 72))
                         (return-point L.rp.54
                                       (begin
                                         (set! rdi r15)
                                         (set! rsi fv1)
                                         (set! r15 L.rp.54)
                                         (jump L.+.31)))
                         (set! rbp (+ rbp 72)))
                       (set! r15 rax)
                       (set! rdi r15)
                       (set! rsi fv0)
                       (set! r15 fv3)
                       (jump L.+.31)))
                   (begin
                     (set! r15 r15)
                     (set! rdi 8)
                     (set! rsi 16)
                     (set! rdx 24)
                     (set! rcx 32)
                     (set! r8 40)
                     (set! r9 48)
                     (set! fv0 56)
                     (set! r15 r15)
                     (jump L.F.6))))
  ;; test suite, make sure
  (check-equal? (replace-locations '(module
                                        ((locals ())
                                         (conflicts
                                          ((tmp-ra.52 (rsi rdi rbp))
                                           (rbp (r15 rsi rdi tmp-ra.52))
                                           (rdi (r15 rsi tmp-ra.52 rbp))
                                           (rsi (r15 tmp-ra.52 rdi rbp))
                                           (r15 (rsi rdi rbp))))
                                         (assignment ((tmp-ra.52 rsp))))
                                      (define L.f.1
                                        ((locals ())
                                         (conflicts
                                          ((tmp.38 (tmp.40 r12 tmp.39 tmp-ra.50 x.1 x.2 rbp))
                                           (tmp-ra.50
                                            (rax
                                             tmp.43
                                             tmp.41
                                             tmp.42
                                             tmp.40
                                             tmp.38
                                             tmp.39
                                             x.2
                                             x.1
                                             rdi
                                             rsi
                                             r12
                                             rbp))
                                           (tmp.42 (tmp.41 r12 rbp tmp-ra.50))
                                           (tmp.39 (tmp.38 r12 tmp-ra.50 x.1 x.2 rbp))
                                           (tmp.43 (tmp.41 rbp tmp-ra.50))
                                           (tmp.40 (x.2 x.1 tmp.38 r12 rbp tmp-ra.50))
                                           (x.2 (tmp.40 tmp.38 tmp.39 r12 tmp-ra.50 x.1 rbp))
                                           (x.1 (tmp.40 tmp.38 tmp.39 x.2 rsi r12 tmp-ra.50 rbp))
                                           (tmp.41 (tmp.43 r12 tmp.42 rbp tmp-ra.50))
                                           (rbp
                                            (rax
                                             tmp.43
                                             tmp.41
                                             tmp.42
                                             tmp.40
                                             r15
                                             r12
                                             tmp.38
                                             tmp.39
                                             x.2
                                             x.1
                                             tmp-ra.50))
                                           (r12 (tmp.41 tmp.42 tmp.40 tmp.38 rbp tmp.39 x.2 x.1 tmp-ra.50))
                                           (rsi (x.1 tmp-ra.50))
                                           (rdi (tmp-ra.50))
                                           (r15 (rbp))
                                           (rax (rbp tmp-ra.50))))
                                         (assignment
                                          ((tmp-ra.50 fv3)
                                           (tmp.38 fv2)
                                           (x.1 fv1)
                                           (x.2 fv0)
                                           (tmp.39 rsp)
                                           (tmp.43 rbx)
                                           (tmp.40 rsp)
                                           (tmp.42 rbx)
                                           (tmp.41 rsp))))
                                        (begin
                                          (set! tmp-ra.50 r15)
                                          (set! x.1 rdi)
                                          (set! x.2 rsi)
                                          (set! tmp.39 10)
                                          (set! tmp.39 (+ tmp.39 6))
                                          (begin (set! tmp.38 r12) (set! r12 (+ r12 tmp.39)))
                                          (begin
                                            (set! rbp (- rbp 32))
                                            (return-point L.rp.21 (begin (set! r15 L.rp.21) (jump L.g.1 rbp r15)))
                                            (set! rbp (+ rbp 32)))
                                          (set! tmp.40 rax)
                                          (if (true) (mset! tmp.38 tmp.40 x.1) (mset! tmp.38 tmp.40 x.2))
                                          (set! tmp.42 10)
                                          (set! tmp.42 (+ tmp.42 6))
                                          (begin (set! tmp.41 r12) (set! r12 (+ r12 tmp.42)))
                                          (set! tmp.43 8)
                                          (set! tmp.43 (bitwise-and tmp.43 8))
                                          (set! rax (mref tmp.41 tmp.43))
                                          (jump tmp-ra.50 rbp rax)))
                                      (define L.g.1
                                        ((locals ())
                                         (conflicts
                                          ((tmp-ra.51 (rax rbp)) (rbp (rax tmp-ra.51)) (rax (rbp tmp-ra.51))))
                                         (assignment ((tmp-ra.51 rsp))))
                                        (begin (set! tmp-ra.51 r15) (set! rax 8) (jump tmp-ra.51 rbp rax)))
                                      (begin
                                        (set! tmp-ra.52 r15)
                                        (set! rdi 1)
                                        (set! rsi 2)
                                        (set! r15 tmp-ra.52)
                                        (jump L.f.1 rbp r15 rdi rsi))))
                '(module
                     (define L.f.1
                       (begin
                         (set! fv3 r15)
                         (set! fv1 rdi)
                         (set! fv0 rsi)
                         (set! rsp 10)
                         (set! rsp (+ rsp 6))
                         (begin (set! fv2 r12) (set! r12 (+ r12 rsp)))
                         (begin
                           (set! rbp (- rbp 32))
                           (return-point L.rp.21 (begin (set! r15 L.rp.21) (jump L.g.1)))
                           (set! rbp (+ rbp 32)))
                         (set! rsp rax)
                         (if (true) (mset! fv2 rsp fv1) (mset! fv2 rsp fv0))
                         (set! rbx 10)
                         (set! rbx (+ rbx 6))
                         (begin (set! rsp r12) (set! r12 (+ r12 rbx)))
                         (set! rbx 8)
                         (set! rbx (bitwise-and rbx 8))
                         (set! rax (mref rsp rbx))
                         (jump fv3)))
                   (define L.g.1 (begin (set! rsp r15) (set! rax 8) (jump rsp)))
                   (begin (set! rsp r15) (set! rdi 1) (set! rsi 2) (set! r15 rsp) (jump L.f.1)))))
