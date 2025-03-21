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
                                 (if (and (not (member var conflict-list))
                                          (not (ormap (lambda (assignment) (symbol=? var (cadr assignment))) assignments)))
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
                         (begin (set! rdi 1000) (set! r15 tmp-ra.2) (jump L.f.2 rbp r15 rdi))))))
  (check-equal? (assign-frame-variables '(module
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
                     (jump L.F.6 rbp r15 rdi rsi rdx rcx r8 r9 fv0)))))