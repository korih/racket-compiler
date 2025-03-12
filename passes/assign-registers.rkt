#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v6
  rackunit)

(provide assign-registers)

;; asm-lang-v6/framed -> asm-lang-v6/spilled
;; perform graph-colouring register allocation, compiling p to
;; Asm-pred-lang.v6/spilled by decorating programs with their register
;; assignments
(define/contract (assign-registers p)
  (-> asm-pred-lang-v6/framed? asm-pred-lang-v6/spilled?)

  ;; func-info is `(define ,label ,info ,tail)
  ;; interp. a function definition that has metadata

  ;; func-info -> func-info 
  (define (assign-registers-func func)
    (match func
      [`(define ,label ,info ,tail)
       (define assignments (graph-colouring-with-spilling (info-ref info 'conflicts) (current-assignable-registers)))
       (define updated-info (info-set (info-remove info 'conflicts) 'assignment assignments))
       `(define ,label ,updated-info ,tail)]))

  ;; graph (List-of register) -> (List-of (list aloc loc))
  (define (graph-colouring-with-spilling conflict-graph registers)

    ;; fvar-index is Natural
    ;; keeps track of the current index of frame variables for spillovers 
    (define fvar-index 0)

    ;; -> fvar
    ;; interp. creates an fvar that is used when there are no more registers
    ;; EFFECTS: increments fvar-index by 1
    (define (make-fvar-spill)
      (define fvar (make-fvar fvar-index))
      (set! fvar-index (+ fvar-index 1))
      fvar)

    ;; graph -> (List-of (list aloc loc))
    ;; interp. performs the graph colouring algorithm
    (define (colour-graph graph)
      (cond
        [(null? graph) '()]
        [else
         (define sorted-graph (sort (map car graph)
                                    (lambda (a b)
                                      (< (length (get-neighbors graph a))
                                         (length (get-neighbors graph b))))))
         (define chosen-node (car sorted-graph))
         (define updated-graph (remove-vertex graph chosen-node))
         (define sub-assign (colour-graph updated-graph))
         (define conflicts (get-neighbors graph chosen-node))
         (define used-registers
           (map (lambda (conflict)
                  (if (aloc? conflict)
                      (first (cdr (assoc conflict sub-assign)))
                      conflict))
                conflicts))
         (define available-registers
           (filter (lambda (r) (not (member r used-registers))) registers))

         (define new-location
           (if (null? available-registers)
               (make-fvar-spill)
               (car available-registers)))

         (if (rloc? chosen-node)
             sub-assign
             (cons (list chosen-node new-location) sub-assign))]))

    (colour-graph conflict-graph))

  (match p
    [`(module ,info ,funcs ... ,tail)
     (define assignments (graph-colouring-with-spilling (info-ref info 'conflicts) (current-assignable-registers)))
     (define updated-info (info-set (info-remove info 'conflicts) 'assignment assignments))
     `(module ,updated-info ,@(map assign-registers-func funcs) ,tail)]))

(module+ test
  (check-equal? (assign-registers '(module
                                       ((locals ()) (conflicts ()))
                                     (define L.f.1
                                       ((locals (tmp.1))
                                        (conflicts ((tmp.1 ()))))
                                       (begin (set! tmp.1 1) (set! tmp.1 (* tmp.1 2)) (halt tmp.1)))
                                     (jump L.f.1 rbp)))
                '(module
                     ((locals ()) (assignment ()))
                   (define L.f.1
                     ((locals (tmp.1)) (assignment ((tmp.1 rsp))))
                     (begin (set! tmp.1 1) (set! tmp.1 (* tmp.1 2)) (halt tmp.1)))
                   (jump L.f.1 rbp)))
  (check-equal? (assign-registers '(module
                                       ((locals ())
                                        (conflicts
                                         ((r9 (rdi rsi rdx rcx r8 rbp))
                                          (rbp (rdi rsi rdx rcx r8 r9))
                                          (r8 (rdi rsi rdx rcx r9 rbp))
                                          (rcx (rdi rsi rdx r9 r8 rbp))
                                          (rdx (rdi rsi r9 r8 rcx rbp))
                                          (rsi (rdi r9 r8 rcx rdx rbp))
                                          (rdi (r9 r8 rcx rdx rsi rbp)))))
                                     (define L.f.1
                                       ((locals (f.1 e.1 d.1 c.1 b.1 a.1))
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
                                          (rsi (a.1)))))
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
                                         (halt a.1)))
                                     (begin
                                       (set! r9 6)
                                       (set! r8 5)
                                       (set! rcx 4)
                                       (set! rdx 3)
                                       (set! rsi 2)
                                       (set! rdi 1)
                                       (jump L.f.1 rbp rdi rsi rdx rcx r8 r9))))
                '(module
                     ((locals ()) (assignment ()))
                   (define L.f.1
                     ((locals (f.1 e.1 d.1 c.1 b.1 a.1))
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
                       (halt a.1)))
                   (begin
                     (set! r9 6)
                     (set! r8 5)
                     (set! rcx 4)
                     (set! rdx 3)
                     (set! rsi 2)
                     (set! rdi 1)
                     (jump L.f.1 rbp rdi rsi rdx rcx r8 r9))))
  (check-equal? (assign-registers '(module
                                       ((locals (k.1 j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                                        (conflicts
                                         ((a.1
                                           (rsi
                                            rdx
                                            rcx
                                            r8
                                            r9
                                            fv0
                                            fv1
                                            fv2
                                            fv3
                                            fv4
                                            k.1
                                            j.1
                                            i.1
                                            h.1
                                            g.1
                                            f.1
                                            e.1
                                            d.1
                                            c.1
                                            b.1
                                            rbp))
                                          (b.1
                                           (rdx
                                            rcx
                                            r8
                                            r9
                                            fv0
                                            fv1
                                            fv2
                                            fv3
                                            fv4
                                            k.1
                                            j.1
                                            i.1
                                            h.1
                                            g.1
                                            f.1
                                            e.1
                                            d.1
                                            c.1
                                            a.1
                                            rbp))
                                          (c.1
                                           (rcx
                                            r8
                                            r9
                                            fv0
                                            fv1
                                            fv2
                                            fv3
                                            fv4
                                            k.1
                                            j.1
                                            i.1
                                            h.1
                                            g.1
                                            f.1
                                            e.1
                                            d.1
                                            b.1
                                            a.1
                                            rbp))
                                          (d.1
                                           (r8 r9 fv0 fv1 fv2 fv3 fv4 k.1 j.1 i.1 h.1 g.1 f.1 e.1 c.1 b.1 a.1 rbp))
                                          (e.1
                                           (r9 fv0 fv1 fv2 fv3 fv4 k.1 j.1 i.1 h.1 g.1 f.1 d.1 c.1 b.1 a.1 rbp))
                                          (f.1 (fv0 fv1 fv2 fv3 fv4 k.1 j.1 i.1 h.1 g.1 e.1 d.1 c.1 b.1 a.1 rbp))
                                          (g.1 (fv1 fv2 fv3 fv4 k.1 j.1 i.1 h.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp))
                                          (h.1 (fv2 fv3 fv4 k.1 j.1 i.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp))
                                          (i.1 (fv3 fv4 k.1 j.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp))
                                          (j.1 (fv4 k.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp))
                                          (k.1 (j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp))
                                          (rbp
                                           (rdi
                                            rsi
                                            rdx
                                            rcx
                                            r8
                                            r9
                                            fv0
                                            fv1
                                            fv2
                                            fv3
                                            fv4
                                            k.1
                                            j.1
                                            i.1
                                            h.1
                                            g.1
                                            f.1
                                            e.1
                                            d.1
                                            c.1
                                            b.1
                                            a.1))
                                          (fv4
                                           (rdi
                                            rsi
                                            rdx
                                            rcx
                                            r8
                                            r9
                                            fv0
                                            fv1
                                            fv2
                                            fv3
                                            j.1
                                            i.1
                                            h.1
                                            g.1
                                            f.1
                                            e.1
                                            d.1
                                            c.1
                                            b.1
                                            a.1
                                            rbp))
                                          (fv3
                                           (rdi
                                            rsi
                                            rdx
                                            rcx
                                            r8
                                            r9
                                            fv0
                                            fv1
                                            fv2
                                            i.1
                                            h.1
                                            g.1
                                            f.1
                                            e.1
                                            d.1
                                            c.1
                                            b.1
                                            a.1
                                            fv4
                                            rbp))
                                          (fv2
                                           (rdi
                                            rsi
                                            rdx
                                            rcx
                                            r8
                                            r9
                                            fv0
                                            fv1
                                            h.1
                                            g.1
                                            f.1
                                            e.1
                                            d.1
                                            c.1
                                            b.1
                                            a.1
                                            fv4
                                            fv3
                                            rbp))
                                          (fv1
                                           (rdi rsi rdx rcx r8 r9 fv0 g.1 f.1 e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 rbp))
                                          (fv0 (rdi rsi rdx rcx r8 r9 f.1 e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 rbp))
                                          (r9 (rdi rsi rdx rcx r8 e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 rbp))
                                          (r8 (rdi rsi rdx rcx d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 rbp))
                                          (rcx (rdi rsi rdx c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rbp))
                                          (rdx (rdi rsi b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rbp))
                                          (rsi (rdi a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rdx rbp))
                                          (rdi (fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rdx rsi rbp)))))
                                     (define L.f.1
                                       ((locals (k.1 j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                                        (conflicts
                                         ((a.1 (rsi rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4))
                                          (b.1 (rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4))
                                          (c.1 (rcx r8 r9 fv0 fv1 fv2 fv3 fv4))
                                          (d.1 (r8 r9 fv0 fv1 fv2 fv3 fv4))
                                          (e.1 (r9 fv0 fv1 fv2 fv3 fv4))
                                          (f.1 (fv0 fv1 fv2 fv3 fv4))
                                          (g.1 (fv1 fv2 fv3 fv4))
                                          (h.1 (fv2 fv3 fv4))
                                          (i.1 (fv3 fv4))
                                          (j.1 (fv4))
                                          (k.1 ())
                                          (fv4 (j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                                          (fv3 (i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                                          (fv2 (h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                                          (fv1 (g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                                          (fv0 (f.1 e.1 d.1 c.1 b.1 a.1))
                                          (r9 (e.1 d.1 c.1 b.1 a.1))
                                          (r8 (d.1 c.1 b.1 a.1))
                                          (rcx (c.1 b.1 a.1))
                                          (rdx (b.1 a.1))
                                          (rsi (a.1)))))
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
                                         (halt 10)))
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
                                       (set! fv4 k.1)
                                       (set! fv3 j.1)
                                       (set! fv2 i.1)
                                       (set! fv1 h.1)
                                       (set! fv0 g.1)
                                       (set! r9 f.1)
                                       (set! r8 e.1)
                                       (set! rcx d.1)
                                       (set! rdx c.1)
                                       (set! rsi b.1)
                                       (set! rdi a.1)
                                       (jump L.f.1 rbp rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4))))
                '(module
                     ((locals (k.1 j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                      (assignment
                       ((k.1 r15)
                        (j.1 r14)
                        (i.1 r13)
                        (h.1 r9)
                        (g.1 r8)
                        (f.1 rdi)
                        (e.1 rsi)
                        (d.1 rcx)
                        (c.1 rdx)
                        (b.1 rbx)
                        (a.1 rsp))))
                   (define L.f.1
                     ((locals (k.1 j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                      (assignment
                       ((k.1 rsp)
                        (j.1 rsp)
                        (i.1 rsp)
                        (h.1 rsp)
                        (g.1 rsp)
                        (f.1 rsp)
                        (a.1 rsp)
                        (b.1 rsp)
                        (c.1 rsp)
                        (d.1 rsp)
                        (e.1 rsp))))
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
                       (halt 10)))
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
                     (set! fv4 k.1)
                     (set! fv3 j.1)
                     (set! fv2 i.1)
                     (set! fv1 h.1)
                     (set! fv0 g.1)
                     (set! r9 f.1)
                     (set! r8 e.1)
                     (set! rcx d.1)
                     (set! rdx c.1)
                     (set! rsi b.1)
                     (set! rdi a.1)
                     (jump L.f.1 rbp rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4))))
  (check-equal? (assign-registers '(module
                                       ((locals ())
                                        (conflicts ((rdi (rbp)) (rbp (rdi)))))
                                     (define L.f.1
                                       ((locals (x.1)) (conflicts ((x.1 ()))))
                                       (begin (set! x.1 rdi) (halt x.1)))
                                     (begin (set! rdi 1) (jump L.f.1 rbp rdi))))
                '(module
                     ((locals ()) (assignment ()))
                   (define L.f.1
                     ((locals (x.1)) (assignment ((x.1 rsp))))
                     (begin (set! x.1 rdi) (halt x.1)))
                   (begin (set! rdi 1) (jump L.f.1 rbp rdi))))
  (check-equal? (assign-registers '(module
                                       ((locals (a.1))
                                        (conflicts ((a.1 (rdi rbp)) (rbp (rdi a.1)) (rdi (rbp a.1)))))
                                     (define L.f.1
                                       ((locals (x.1)) (conflicts ((x.1 ()))))
                                       (begin (set! x.1 rdi) (halt x.1)))
                                     (begin (set! a.1 L.f.1) (set! rdi 1) (jump a.1 rbp rdi))))
                '(module
                     ((locals (a.1)) (assignment ((a.1 rsp))))
                   (define L.f.1
                     ((locals (x.1)) (assignment ((x.1 rsp))))
                     (begin (set! x.1 rdi) (halt x.1)))
                   (begin (set! a.1 L.f.1) (set! rdi 1) (jump a.1 rbp rdi))))
  (check-equal? (assign-registers '(module
                                       ((locals ()) (conflicts ((r13 (rdi rbp)) (rbp (rdi r13)) (rdi (rbp r13)))))
                                     (define L.f.1
                                       ((locals (x.1)) (conflicts ((x.1 ()))))
                                       (begin (set! x.1 rdi) (halt x.1)))
                                     (begin (set! r13 L.f.1) (set! rdi 1) (jump r13 rbp rdi))))
                '(module
                     ((locals ()) (assignment ()))
                   (define L.f.1
                     ((locals (x.1)) (assignment ((x.1 rsp))))
                     (begin (set! x.1 rdi) (halt x.1)))
                   (begin (set! r13 L.f.1) (set! rdi 1) (jump r13 rbp rdi))))
  (check-equal? (assign-registers '(module
                                       ((locals ())
                                        (conflicts
                                         ((rdx (rdi rsi rbp))
                                          (rbp (rdi rsi rdx))
                                          (rsi (rdi rdx rbp))
                                          (rdi (rdx rsi rbp)))))
                                     (define L.f.1
                                       ((locals (x.1)) (conflicts ((x.1 ()))))
                                       (begin (set! x.1 rdi) (halt x.1)))
                                     (define L.g.1
                                       ((locals (y.1 x.1 z.1))
                                        (conflicts
                                         ((z.1 (x.1 rbp))
                                          (x.1 (z.1 y.1 rsi rdx rbp))
                                          (y.1 (rdx x.1 rbp))
                                          (rbp (rdi z.1 y.1 x.1))
                                          (rdx (y.1 x.1))
                                          (rsi (x.1))
                                          (rdi (rbp)))))
                                       (begin
                                         (set! x.1 rdi)
                                         (set! y.1 rsi)
                                         (set! z.1 rdx)
                                         (set! rdi x.1)
                                         (jump L.f.1 rbp rdi)))
                                     (if (true)
                                         (begin
                                           (set! rdx 3)
                                           (set! rsi 2)
                                           (set! rdi 1)
                                           (jump L.g.1 rbp rdi rsi rdx))
                                         (begin (set! rdi 1) (jump L.f.1 rbp rdi)))))
                '(module
                     ((locals ()) (assignment ()))
                   (define L.f.1
                     ((locals (x.1)) (assignment ((x.1 rsp))))
                     (begin (set! x.1 rdi) (halt x.1)))
                   (define L.g.1
                     ((locals (y.1 x.1 z.1)) (assignment ((z.1 rsp) (x.1 rbx) (y.1 rsp))))
                     (begin
                       (set! x.1 rdi)
                       (set! y.1 rsi)
                       (set! z.1 rdx)
                       (set! rdi x.1)
                       (jump L.f.1 rbp rdi)))
                   (if (true)
                       (begin
                         (set! rdx 3)
                         (set! rsi 2)
                         (set! rdi 1)
                         (jump L.g.1 rbp rdi rsi rdx))
                       (begin (set! rdi 1) (jump L.f.1 rbp rdi)))))
  (check-equal? (assign-registers
                 '(module ((locals (x.1))
                           (conflicts ((x.1 ()))))
                    (begin
                      (set! x.1 42)
                      (halt x.1))))
                '(module
                     ((locals (x.1)) (assignment ((x.1 rsp))))
                   (begin (set! x.1 42) (halt x.1))))
  (check-equal? (parameterize ([current-assignable-registers '(r9)])
                  (assign-registers
                   '(module ((locals (x.1))
                             (conflicts ((x.1 ()))))
                      (begin
                        (set! x.1 42)
                        (halt x.1)))))
                '(module
                     ((locals (x.1)) (assignment ((x.1 r9))))
                   (begin (set! x.1 42) (halt x.1))))
  (check-equal? (parameterize ([current-assignable-registers '()])
                  (assign-registers
                   '(module ((locals (x.1))
                             (conflicts ((x.1 ()))))
                      (begin
                        (set! x.1 42)
                        (halt x.1)))))
                '(module
                     ((locals (x.1)) (assignment ((x.1 fv0))))
                   (begin (set! x.1 42) (halt x.1))))
  (check-equal? (assign-registers
                 '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                           (conflicts
                            ((x.3 (z.5 p.1 y.4 v.1 w.2))
                             (w.2 (z.5 p.1 y.4 v.1 x.3))
                             (v.1 (w.2 x.3))
                             (y.4 (t.6 z.5 p.1 w.2 x.3))
                             (p.1 (t.6 z.5 y.4 w.2 x.3))
                             (z.5 (t.6 p.1 y.4 w.2 x.3))
                             (t.6 (z.5 p.1 y.4)))))
                    (begin
                      (set! v.1 1)
                      (set! w.2 46)
                      (set! x.3 v.1)
                      (set! p.1 7)
                      (set! x.3 (+ x.3 p.1))
                      (set! y.4 x.3)
                      (set! p.1 4)
                      (set! y.4 (+ y.4 p.1))
                      (set! z.5 x.3)
                      (set! z.5 (+ z.5 w.2))
                      (set! t.6 y.4)
                      (set! p.1 -1)
                      (set! t.6 (* t.6 p.1))
                      (set! z.5 (+ z.5 t.6))
                      (halt z.5))))
                '(module
                     ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                      (assignment
                       ((v.1 rsp) (t.6 rdx) (x.3 rsi) (w.2 rdx) (y.4 rcx) (p.1 rbx) (z.5 rsp))))
                   (begin
                     (set! v.1 1)
                     (set! w.2 46)
                     (set! x.3 v.1)
                     (set! p.1 7)
                     (set! x.3 (+ x.3 p.1))
                     (set! y.4 x.3)
                     (set! p.1 4)
                     (set! y.4 (+ y.4 p.1))
                     (set! z.5 x.3)
                     (set! z.5 (+ z.5 w.2))
                     (set! t.6 y.4)
                     (set! p.1 -1)
                     (set! t.6 (* t.6 p.1))
                     (set! z.5 (+ z.5 t.6))
                     (halt z.5)))))
