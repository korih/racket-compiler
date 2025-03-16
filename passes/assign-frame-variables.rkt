#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v6)

(provide assign-frame-variables)

;; Asm-pred-lang-v6/spilled -> Asm-pred-lang-v6/assignments
;; Compiles Asm-pred-lang-v6/spilled to Asm-pred-lang-v6/assignments by
;; allocating all abstract locations in the locals set to free frame variables.
(define/contract (assign-frame-variables p)
  (-> asm-pred-lang-v6/spilled? asm-pred-lang-v6/assignments?)

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
  #;
  (check-equal? (assign-frame-variables '(module
                                             ((locals ())
                                              (conflicts
                                               ((r9 (rdi rsi rdx rcx r8 rbp))
                                                (rbp (rdi rsi rdx rcx r8 r9))
                                                (r8 (rdi rsi rdx rcx r9 rbp))
                                                (rcx (rdi rsi rdx r9 r8 rbp))
                                                (rdx (rdi rsi r9 r8 rcx rbp))
                                                (rsi (rdi r9 r8 rcx rdx rbp))
                                                (rdi (r9 r8 rcx rdx rsi rbp))))
                                              (assignment ()))
                                           (define L.f.1
                                             ((locals ())
                                              (conflicts
                                               ((a.1 (f.1 e.1 d.1 c.1 b.1 rsi rdx rcx r8 r9))
                                                (b.1 (f.1 e.1 d.1 c.1 rdx rcx r8 r9 a.1))
                                                (c.1 (f.1 e.1 d.1 rcx r8 r9 b.1 a.1))
                                                (d.1 (f.1 e.1 r8 r9 b.1 a.1 c.1))
                                                (e.1 (f.1 r9 b.1 a.1 c.1 d.1))
                                                (f.1 (b.1 a.1 c.1 d.1 e.1))
                                                (r9 (e.1 d.1 c.1 b.1 a.1))
                                                (r8 (d.1 c.1 b.1 a.1))
                                                (rcx (c.1 b.1 a.1))
                                                (rdx (b.1 a.1))
                                                (rsi (a.1))))
                                              (assignment ((f.1 rdi) (a.1 rsi) (b.1 rdx) (c.1 rcx) (d.1 rbx) (e.1 rsp))))
                                             (begin
                                               (set! a.1 rdi)
                                               (set! b.1 rsi)
                                               (set! c.1 rdx)
                                               (set! d.1 rcx)
                                               (set! e.1 r8)
                                               (set! f.1 r9)
                                               (set! a.1 (+ a.1 b.1))
                                               (set! a.1 (+ a.1 c.1))
                                               (set! a.1 (+ a.1 d.1))
                                               (set! a.1 (+ a.1 e.1))
                                               (set! a.1 (+ a.1 f.1))
                                               (jump done a.1)))
                                           (begin
                                             (set! r9 6)
                                             (set! r8 5)
                                             (set! rcx 4)
                                             (set! rdx 3)
                                             (set! rsi 2)
                                             (set! rdi 1)
                                             (jump L.f.1 rbp rdi rsi rdx rcx r8 r9))))
                '(module
                     ((locals ())
                      (conflicts
                       ((r9 (rdi rsi rdx rcx r8 rbp))
                        (rbp (rdi rsi rdx rcx r8 r9))
                        (r8 (rdi rsi rdx rcx r9 rbp))
                        (rcx (rdi rsi rdx r9 r8 rbp))
                        (rdx (rdi rsi r9 r8 rcx rbp))
                        (rsi (rdi r9 r8 rcx rdx rbp))
                        (rdi (r9 r8 rcx rdx rsi rbp))))
                      (assignment ()))
                   (define L.f.1
                     ((locals ())
                      (conflicts
                       ((a.1 (f.1 e.1 d.1 c.1 b.1 rsi rdx rcx r8 r9))
                        (b.1 (f.1 e.1 d.1 c.1 rdx rcx r8 r9 a.1))
                        (c.1 (f.1 e.1 d.1 rcx r8 r9 b.1 a.1))
                        (d.1 (f.1 e.1 r8 r9 b.1 a.1 c.1))
                        (e.1 (f.1 r9 b.1 a.1 c.1 d.1))
                        (f.1 (b.1 a.1 c.1 d.1 e.1))
                        (r9 (e.1 d.1 c.1 b.1 a.1))
                        (r8 (d.1 c.1 b.1 a.1))
                        (rcx (c.1 b.1 a.1))
                        (rdx (b.1 a.1))
                        (rsi (a.1))))
                      (assignment
                       ((f.1 rdi) (a.1 rsi) (b.1 rdx) (c.1 rcx) (d.1 rbx) (e.1 rsp))))
                     (begin
                       (set! a.1 rdi)
                       (set! b.1 rsi)
                       (set! c.1 rdx)
                       (set! d.1 rcx)
                       (set! e.1 r8)
                       (set! f.1 r9)
                       (set! a.1 (+ a.1 b.1))
                       (set! a.1 (+ a.1 c.1))
                       (set! a.1 (+ a.1 d.1))
                       (set! a.1 (+ a.1 e.1))
                       (set! a.1 (+ a.1 f.1))
                       (jump done a.1)))
                   (begin
                     (set! r9 6)
                     (set! r8 5)
                     (set! rcx 4)
                     (set! rdx 3)
                     (set! rsi 2)
                     (set! rdi 1)
                     (jump L.f.1 rbp rdi rsi rdx rcx r8 r9))))

  )



