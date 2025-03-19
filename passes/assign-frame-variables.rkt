#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v7)

(provide assign-frame-variables)

;; Asm-pred-lang-v7/spilled -> Asm-pred-lang-v7/assignments
;; Compiles Asm-pred-lang-v7/spilled to Asm-pred-lang-v7/assignments by
;; allocating all abstract locations in the locals set to free frame variables.
(define/contract (assign-frame-variables p)
  (-> asm-pred-lang-v7/spilled? asm-pred-lang-v7/assignments?)

  ;; local (Graph of conflicts) assignments -> assignments
  ;; recursive call over call-undead-set that produces an assignment
  ;; for the physical locations in the call-undead-set
  (define (graph-colouring x conflicts-graph assignments)
    (cond
      ; empty set, default assignment
      ; select x from input, remove from conflict and input to return assignment
      [else
       (define conflict-list (get-neighbors conflicts-graph x))
       ;; look for available fvar
       (define fvar-assignment (for/or ([i (in-naturals)])
                                 (define var (string->symbol (format "fv~a" i)))
                                 (if (not (and (member var conflict-list)
                                               (member `(,x ,var) assignments)))
                                     var
                                     #f)))
       `(,x ,fvar-assignment)]))

  ;; info -> info
  ;; update the assignments for a given info
  (define (assign-call-variables-info info)
    (define conflicts-graph (info-ref info 'conflicts))
    (define assignments
      (for/fold ([assignments (info-ref info 'assignment)])
                ([x (info-ref info 'locals)])
        (cons (graph-colouring x conflicts-graph assignments) assignments)))

    (define info^ (info-set info 'assignment assignments))
    info^)

  ;; (Function Definition) -> (Function Definition)
  ;; Take a function definition and update its assignments in info
  (define (assign-call-fun f)
    (match f
      [`(define ,name ,info ,tail) (define info^ (assign-call-variables-info info))
                                   `(define ,name ,info^ ,tail)]))

  (match p
    [`(module ,info ,funs ... ,tail) (define funs^ (map assign-call-fun funs))
                                     (define info^ (assign-call-variables-info info))
                                     `(module ,info^ ,@funs^ ,tail)]))

(module+ test
  (require rackunit)
  (check-equal? (assign-frame-variables '(module
                                             ((locals ()) (conflicts ()) (assignment ()))
                                           (define L.f.1
                                             ((locals ()) (conflicts ((tmp.1 ()))) (assignment ((tmp.1 rsp))))
                                             (begin (set! tmp.1 1) (set! tmp.1 (* tmp.1 2)) (jump done tmp.1)))
                                           (jump L.f.1 rbp)))
                '(module
                     ((locals ()) (conflicts ()) (assignment ()))
                   (define L.f.1
                     ((locals ()) (conflicts ((tmp.1 ()))) (assignment ((tmp.1 rsp))))
                     (begin (set! tmp.1 1) (set! tmp.1 (* tmp.1 2)) (jump done tmp.1)))
                   (jump L.f.1 rbp)))

  (check-equal? (assign-frame-variables '(module
                                             ((locals ()) (conflicts ()) (assignment ()))
                                           (define L.f.1
                                             ((locals (tmp.1))
                                              (conflicts
                                               ((tmp.1
                                                 (tmp.2 tmp.3 tmp.4 tmp.5 tmp.6 tmp.7 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                                                (tmp.2
                                                 (tmp.1 tmp.3 tmp.4 tmp.5 tmp.6 tmp.7 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                                                (tmp.3
                                                 (tmp.1 tmp.2 tmp.4 tmp.5 tmp.6 tmp.7 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                                                (tmp.4
                                                 (tmp.1 tmp.2 tmp.3 tmp.5 tmp.6 tmp.7 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                                                (tmp.5
                                                 (tmp.1 tmp.2 tmp.3 tmp.4 tmp.6 tmp.7 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                                                (tmp.6
                                                 (tmp.1 tmp.2 tmp.3 tmp.4 tmp.5 tmp.7 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                                                (tmp.7
                                                 (tmp.1 tmp.2 tmp.3 tmp.4 tmp.5 tmp.6 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                                                (tmp.8
                                                 (tmp.1 tmp.2 tmp.3 tmp.4 tmp.5 tmp.6 tmp.7 tmp.9 tmp.10 tmp.11 tmp.12))
                                                (tmp.9
                                                 (tmp.1 tmp.2 tmp.3 tmp.4 tmp.5 tmp.6 tmp.7 tmp.8 tmp.10 tmp.11 tmp.12))
                                                (tmp.10
                                                 (tmp.1 tmp.2 tmp.3 tmp.4 tmp.5 tmp.6 tmp.7 tmp.8 tmp.9 tmp.11 tmp.12))
                                                (tmp.11
                                                 (tmp.1 tmp.2 tmp.3 tmp.4 tmp.5 tmp.6 tmp.7 tmp.8 tmp.9 tmp.10 tmp.12))
                                                (tmp.12
                                                 (tmp.1
                                                  tmp.2
                                                  tmp.3
                                                  tmp.4
                                                  tmp.5
                                                  tmp.6
                                                  tmp.7
                                                  tmp.8
                                                  tmp.9
                                                  tmp.10
                                                  tmp.11))))
                                              (assignment
                                               ((tmp.2 r15)
                                                (tmp.3 r14)
                                                (tmp.4 r13)
                                                (tmp.5 r9)
                                                (tmp.6 r8)
                                                (tmp.7 rdi)
                                                (tmp.8 rsi)
                                                (tmp.9 rdx)
                                                (tmp.10 rcx)
                                                (tmp.11 rbx)
                                                (tmp.12 rsp))))
                                             (begin
                                               (set! tmp.1 2)
                                               (set! tmp.2 2)
                                               (set! tmp.3 2)
                                               (set! tmp.4 2)
                                               (set! tmp.5 2)
                                               (set! tmp.6 2)
                                               (set! tmp.7 2)
                                               (set! tmp.8 2)
                                               (set! tmp.9 2)
                                               (set! tmp.10 2)
                                               (set! tmp.11 2)
                                               (set! tmp.12 2)
                                               (jump done tmp.1)))
                                           (jump L.f.1 rbp)))
                '(module
                     ((locals ()) (conflicts ()) (assignment ()))
                   (define L.f.1
                     ((locals (tmp.1))
                      (conflicts
                       ((tmp.1
                         (tmp.2 tmp.3 tmp.4 tmp.5 tmp.6 tmp.7 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                        (tmp.2
                         (tmp.1 tmp.3 tmp.4 tmp.5 tmp.6 tmp.7 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                        (tmp.3
                         (tmp.1 tmp.2 tmp.4 tmp.5 tmp.6 tmp.7 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                        (tmp.4
                         (tmp.1 tmp.2 tmp.3 tmp.5 tmp.6 tmp.7 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                        (tmp.5
                         (tmp.1 tmp.2 tmp.3 tmp.4 tmp.6 tmp.7 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                        (tmp.6
                         (tmp.1 tmp.2 tmp.3 tmp.4 tmp.5 tmp.7 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                        (tmp.7
                         (tmp.1 tmp.2 tmp.3 tmp.4 tmp.5 tmp.6 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                        (tmp.8
                         (tmp.1 tmp.2 tmp.3 tmp.4 tmp.5 tmp.6 tmp.7 tmp.9 tmp.10 tmp.11 tmp.12))
                        (tmp.9
                         (tmp.1 tmp.2 tmp.3 tmp.4 tmp.5 tmp.6 tmp.7 tmp.8 tmp.10 tmp.11 tmp.12))
                        (tmp.10
                         (tmp.1 tmp.2 tmp.3 tmp.4 tmp.5 tmp.6 tmp.7 tmp.8 tmp.9 tmp.11 tmp.12))
                        (tmp.11
                         (tmp.1 tmp.2 tmp.3 tmp.4 tmp.5 tmp.6 tmp.7 tmp.8 tmp.9 tmp.10 tmp.12))
                        (tmp.12
                         (tmp.1
                          tmp.2
                          tmp.3
                          tmp.4
                          tmp.5
                          tmp.6
                          tmp.7
                          tmp.8
                          tmp.9
                          tmp.10
                          tmp.11))))
                      (assignment
                       ((tmp.1 fv0)
                        (tmp.2 r15)
                        (tmp.3 r14)
                        (tmp.4 r13)
                        (tmp.5 r9)
                        (tmp.6 r8)
                        (tmp.7 rdi)
                        (tmp.8 rsi)
                        (tmp.9 rdx)
                        (tmp.10 rcx)
                        (tmp.11 rbx)
                        (tmp.12 rsp))))
                     (begin
                       (set! tmp.1 2)
                       (set! tmp.2 2)
                       (set! tmp.3 2)
                       (set! tmp.4 2)
                       (set! tmp.5 2)
                       (set! tmp.6 2)
                       (set! tmp.7 2)
                       (set! tmp.8 2)
                       (set! tmp.9 2)
                       (set! tmp.10 2)
                       (set! tmp.11 2)
                       (set! tmp.12 2)
                       (jump done tmp.1)))
                   (jump L.f.1 rbp)))
  (check-equal? (assign-frame-variables '(module
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
                         (begin (set! rdi 1000) (set! r15 tmp-ra.2) (jump L.f.2 rbp r15 rdi)))))))



