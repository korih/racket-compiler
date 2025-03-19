#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v7
  rackunit)

(provide replace-locations)

;; asm-lang-v7/assignments -> nested-asm-lang-fvars-v7
;; compiles p to Nested-asm-lang v7 by replacing all abstract location with
;; physical locations using the assignment described in the assignment info
;; field, and dropping any register-allocation-related metadata from the program
(define (replace-locations p)
  (-> asm-pred-lang-v7/assignments? nested-asm-lang-fvars-v7?)

  ;; func is `(define ,label ,tail)
  ;; interp. a function definition that does not have metadata

  ;; asm-lang-v7/assignments -> (Dict-of aloc rloc)
  ;; interp. creates a dictionary of assignments
  (define (make-assignments-dict assignments)
    (for/fold ([acc (hash)])
              ([pair assignments])
      (dict-set acc (first pair) (second pair))))

  ;; asm-pred-lang-v7/assignments.label asm-pred-lang-v7/assignments.info asm-pred-lang-v7/assignments.tail -> func
  (define (replace-locations-func label info tail)
    (define assignments (make-assignments-dict (info-ref info 'assignment)))
    `(define ,label ,(replace-locations-tail tail assignments)))

  ;; asm-lang-v7/assignments.tail (Dict-of aloc rloc) -> nested-asm-lang-v7.tail
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

  ;; asm-lang-v7/assignments.effect (Dict-of aloc rloc) -> nested-asm-lang-v7.effect
  (define (replace-locations-effect e assignments)
    (match e
      [`(set! ,loc (,binop ,loc ,op))
       (define loc^ (replace-locations-loc loc assignments))
       (define op^ (replace-locations-opand op assignments))
       `(set! ,loc^ (,binop ,loc^ ,op^))]
      [`(set! ,loc ,triv)
       (define loc^ (replace-locations-loc loc assignments))
       (define triv^ (replace-locations-triv triv assignments))
       `(set! ,loc^ ,triv^)]
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

  ;; asm-lang-v7/assignments.pred (Dict-of aloc rloc) -> nested-asm-lang-v7.pred
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

  ;; asm-lang-v7/assignments.triv (Dict-of aloc rloc) -> nested-asm-lang-v7.triv
  (define (replace-locations-triv t assignments)
    (match t
      [label #:when (label? label) label]
      [op (replace-locations-opand op assignments)]))

  ;; asm-lang-v7/assignments.opand (Dict-of aloc rloc) -> nested-asm-lang-v7.opand
  (define (replace-locations-opand op assignments)
    (match op
      [int64 #:when (int64? int64) int64]
      [loc (replace-locations-loc loc assignments)]))

  ;; asm-lang-v7/assignments.loc (Dict-of aloc rloc) -> nested-asm-lang-v7.loc
  (define (replace-locations-loc loc assignments)
    (match loc
      [aloc #:when (aloc? aloc) (dict-ref assignments aloc)]
      [rloc #:when (rloc? rloc) rloc]))

  ;; asm-lang-v7/assignments.trg (Dict-of aloc rloc) -> nested-asm-lang-v7.trg
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
                         (begin (set! rdi 1000) (set! r15 rbx) (jump L.f.2)))))))
