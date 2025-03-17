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


  ;; keeps track of spilled variables
  (define spilled-variables (box '()))

  ;; func-info -> func-info
  (define (assign-registers-func func)
    (match func
      [`(define ,label ,info ,tail)
       (set-box! spilled-variables '())
       (define assignments (graph-colouring-with-spilling (info-ref info 'assignment) (info-ref info 'conflicts) (current-assignable-registers)))
       (define updated-info (info-set info 'assignment assignments))
       (define updated-locals (remove* (map car (info-ref updated-info 'assignment)) (info-ref updated-info 'locals)))
       (set! updated-info (info-set updated-info 'locals updated-locals))
       `(define ,label ,updated-info ,tail)]))


  ;; graph (List-of register) -> (List-of (list aloc loc))
  (define (graph-colouring-with-spilling assignments conflict-graph registers)

    (define assigned-alocs (map car assignments))
    (define graph^ (for/fold ([new-graph conflict-graph])
                     ([assignment assigned-alocs])
                     (remove-vertex new-graph assignment)))

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
           (cond 
             [(null? available-registers) '()]
             [else (car available-registers)]))
         (cond
           [(rloc? chosen-node) sub-assign]
           [(empty? new-location) (begin
                                    (set-box! spilled-variables (cons chosen-node spilled-variables))
                                    sub-assign)]
           [else (cons (list chosen-node new-location) sub-assign)])]))

    (append assignments (colour-graph graph^)))

  (match p
    [`(module ,info ,funcs ... ,tail)
     (set-box! spilled-variables '())
     (define assignments (graph-colouring-with-spilling (info-ref info 'assignment) (info-ref info 'conflicts) (current-assignable-registers)))
     (define updated-info (info-set info 'assignment assignments))
     (define updated-locals (remove* (map car (info-ref updated-info 'assignment)) (info-ref updated-info 'locals)))
     (set! updated-info (info-set updated-info 'locals updated-locals))
     `(module ,updated-info ,@(map assign-registers-func funcs) ,tail)]))


(module+ test
  (check-equal? (assign-registers '(module
                                       ((locals ())
                                        (conflicts ())
                                        (assignment ()))
                                     (define L.f.1
                                       ((locals (tmp.1))
                                        (conflicts ((tmp.1 ())))
                                        (assignment ()))
                                       (begin (set! tmp.1 1) (set! tmp.1 (* tmp.1 2)) (jump done tmp.1)))
                                     (jump L.f.1 rbp)))
                '(module
                     ((locals ()) (conflicts ()) (assignment ()))
                   (define L.f.1
                     ((locals ()) (conflicts ((tmp.1 ()))) (assignment ((tmp.1 rsp))))
                     (begin (set! tmp.1 1) (set! tmp.1 (* tmp.1 2)) (jump done tmp.1)))
                   (jump L.f.1 rbp)))

  (check-equal? (assign-registers '(module
                                       ((locals ())
                                        (conflicts ())
                                        (assignment ()))
                                     (define L.f.1
                                       ((locals (tmp.1 tmp.2 tmp.3 tmp.4 tmp.5 tmp.6 tmp.7 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                                        (conflicts ((tmp.1 (tmp.2 tmp.3 tmp.4 tmp.5 tmp.6 tmp.7 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                                                    (tmp.2 (tmp.1 tmp.3 tmp.4 tmp.5 tmp.6 tmp.7 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                                                    (tmp.3 (tmp.1 tmp.2 tmp.4 tmp.5 tmp.6 tmp.7 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                                                    (tmp.4 (tmp.1 tmp.2 tmp.3 tmp.5 tmp.6 tmp.7 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                                                    (tmp.5 (tmp.1 tmp.2 tmp.3 tmp.4 tmp.6 tmp.7 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                                                    (tmp.6 (tmp.1 tmp.2 tmp.3 tmp.4 tmp.5 tmp.7 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                                                    (tmp.7 (tmp.1 tmp.2 tmp.3 tmp.4 tmp.5 tmp.6 tmp.8 tmp.9 tmp.10 tmp.11 tmp.12))
                                                    (tmp.8 (tmp.1 tmp.2 tmp.3 tmp.4 tmp.5 tmp.6 tmp.7 tmp.9 tmp.10 tmp.11 tmp.12))
                                                    (tmp.9 (tmp.1 tmp.2 tmp.3 tmp.4 tmp.5 tmp.6 tmp.7 tmp.8 tmp.10 tmp.11 tmp.12))
                                                    (tmp.10 (tmp.1 tmp.2 tmp.3 tmp.4 tmp.5 tmp.6 tmp.7 tmp.8 tmp.9 tmp.11 tmp.12))
                                                    (tmp.11 (tmp.1 tmp.2 tmp.3 tmp.4 tmp.5 tmp.6 tmp.7 tmp.8 tmp.9 tmp.10 tmp.12))
                                                    (tmp.12 (tmp.1 tmp.2 tmp.3 tmp.4 tmp.5 tmp.6 tmp.7 tmp.8 tmp.9 tmp.10 tmp.11))))
                                        (assignment ()))
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


  (check-equal? (assign-registers '(module
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
                                          (rsi (a.1))))
                                        (assignment ()))
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


  (check-equal? (assign-registers '(module
                                       ((locals (k.1 j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                                        (assignment ())
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
                                        (assignment ())
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
                                         (jump done k.1)))
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
                     ((locals ())
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
                        (a.1 rsp)))
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
                     ((locals ())
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
                        (e.1 rsp)))
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
                       (jump done k.1)))
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
                                       ((locals (x.53))
                                        (conflicts
                                         ((x.53 (tmp-ra.146 rbp))
                                          (tmp-ra.146 (rdi x.53 rbp))
                                          (rbp (x.53 r15 rdi tmp-ra.146))
                                          (rdi (tmp-ra.146 r15 rbp))
                                          (r15 (rdi rbp))))
                                        (assignment
                                         ((tmp-ra.146 fv0))))
                                     (define L.id.20
                                       ((locals (tmp-ra.145 x.52))
                                        (conflicts
                                         ((x.52 (rbp tmp-ra.145))
                                          (tmp-ra.145 (rax x.52 rdi rbp))
                                          (rbp (rax x.52 tmp-ra.145))
                                          (rdi (tmp-ra.145))
                                          (rax (rbp tmp-ra.145))))
                                        (assignment ()))
                                       (begin (set! tmp-ra.145 r15)
                                              (set! x.52 rdi)
                                              (set! rax x.52)
                                              (jump tmp-ra.145 rbp rax)))
                                     (begin
                                       (set! tmp-ra.146 r15)
                                       (begin
                                         (set! rbp (- rbp 8))
                                         (return-point L.rp.33
                                                       (begin
                                                         (set! rdi 5)
                                                         (set! r15 L.rp.33)
                                                         (jump L.id.20 rbp r15 rdi)))
                                         (set! rbp (+ rbp 8)))
                                       (set! x.53 rax)
                                       (set! rdi x.53)
                                       (set! r15 tmp-ra.146)
                                       (jump L.id.20 rbp r15 rdi))))
                '(module
                     ((locals ())
                      (conflicts
                       ((x.53 (tmp-ra.146 rbp))
                        (tmp-ra.146 (rdi x.53 rbp))
                        (rbp (x.53 r15 rdi tmp-ra.146))
                        (rdi (tmp-ra.146 r15 rbp))
                        (r15 (rdi rbp))))
                      (assignment ((tmp-ra.146 fv0) (x.53 rsp))))
                   (define L.id.20
                     ((locals ())
                      (conflicts
                       ((x.52 (rbp tmp-ra.145))
                        (tmp-ra.145 (rax x.52 rdi rbp))
                        (rbp (rax x.52 tmp-ra.145))
                        (rdi (tmp-ra.145))
                        (rax (rbp tmp-ra.145))))
                      (assignment ((x.52 rbx) (tmp-ra.145 rsp))))
                     (begin
                       (set! tmp-ra.145 r15)
                       (set! x.52 rdi)
                       (set! rax x.52)
                       (jump tmp-ra.145 rbp rax)))
                   (begin
                     (set! tmp-ra.146 r15)
                     (begin
                       (set! rbp (- rbp 8))
                       (return-point L.rp.33
                                     (begin (set! rdi 5) (set! r15 L.rp.33) (jump L.id.20 rbp r15 rdi)))
                       (set! rbp (+ rbp 8)))
                     (set! x.53 rax)
                     (set! rdi x.53)
                     (set! r15 tmp-ra.146)
                     (jump L.id.20 rbp r15 rdi)))))
