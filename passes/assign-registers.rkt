#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v7
  rackunit)

(provide assign-registers)

;; asm-lang-v7/framed -> asm-lang-v7/spilled
;; perform graph-colouring register allocation, compiling p to
;; Asm-pred-lang.v7/spilled by decorating programs with their register
;; assignments
(define/contract (assign-registers p)
  (-> asm-pred-lang-v7/framed? asm-pred-lang-v7/spilled?)

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
                      (let ([conflict-used-register-pair (assoc conflict sub-assign)])
                        (if conflict-used-register-pair
                            (first (cdr conflict-used-register-pair))
                            conflict))
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
                     (jump L.id.20 rbp r15 rdi))))

  (check-equal? (assign-registers
                 '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                           (conflicts
                            ((x.3 (z.5 p.1 y.4 v.1 w.2))
                             (w.2 (z.5 p.1 y.4 v.1 x.3))
                             (v.1 (w.2 x.3))
                             (y.4 (t.6 z.5 p.1 w.2 x.3))
                             (p.1 (t.6 z.5 y.4 w.2 x.3))
                             (z.5 (t.6 p.1 y.4 w.2 x.3))
                             (t.6 (z.5 p.1 y.4))))
                           (assignment ()))
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
                      (jump done z.5))))
                '(module
                     ((locals ())
                      (conflicts
                       ((x.3 (z.5 p.1 y.4 v.1 w.2))
                        (w.2 (z.5 p.1 y.4 v.1 x.3))
                        (v.1 (w.2 x.3))
                        (y.4 (t.6 z.5 p.1 w.2 x.3))
                        (p.1 (t.6 z.5 y.4 w.2 x.3))
                        (z.5 (t.6 p.1 y.4 w.2 x.3))
                        (t.6 (z.5 p.1 y.4))))
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
                     (jump done z.5))))


  (check-equal? (parameterize ([current-assignable-registers '()])
                  (assign-registers
                   '(module ((locals (x.1))
                             (conflicts ((x.1 ())))
                             (assignment ()))
                      (begin
                        (set! x.1 42)
                        (jump done x.1)))))
                '(module
                     ((locals (x.1)) (conflicts ((x.1 ()))) (assignment ()))
                   (begin (set! x.1 42) (jump done x.1))))


  (check-equal? (parameterize ([current-assignable-registers '(r9)])
                  (assign-registers
                   '(module ((locals (x.1))
                             (conflicts ((x.1 ())))
                             (assignment ()))
                      (begin
                        (set! x.1 42)
                        (jump done x.1)))))
                '(module
                     ((locals ()) (conflicts ((x.1 ()))) (assignment ((x.1 r9))))
                   (begin (set! x.1 42) (jump done x.1))))

  (check-equal? (assign-registers
                 '(module ((locals (x.1))
                           (conflicts ((x.1 ())))
                           (assignment ()))
                    (begin
                      (set! x.1 42)
                      (jump done x.1))))
                '(module
                     ((locals ()) (conflicts ((x.1 ()))) (assignment ((x.1 rsp))))
                   (begin (set! x.1 42) (jump done x.1))))

  (check-equal? (assign-registers '(module
                                       ((locals ())
                                        (conflicts
                                         ((rdx (rdi rsi rbp))
                                          (rbp (rdi rsi rdx))
                                          (rsi (rdi rdx rbp))
                                          (rdi (rdx rsi rbp))))
                                        (assignment ()))
                                     (define L.f.1
                                       ((locals (x.1))
                                        (conflicts ((x.1 ())))
                                        (assignment ()))
                                       (begin (set! x.1 rdi) (jump done x.1)))
                                     (define L.g.1
                                       ((locals (y.1 x.1 z.1))
                                        (conflicts
                                         ((z.1 (x.1 rbp))
                                          (x.1 (z.1 y.1 rsi rdx rbp))
                                          (y.1 (rdx x.1 rbp))
                                          (rbp (rdi z.1 y.1 x.1))
                                          (rdx (y.1 x.1))
                                          (rsi (x.1))
                                          (rdi (rbp))))
                                        (assignment ()))
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
                     ((locals ())
                      (conflicts
                       ((rdx (rdi rsi rbp))
                        (rbp (rdi rsi rdx))
                        (rsi (rdi rdx rbp))
                        (rdi (rdx rsi rbp))))
                      (assignment ()))
                   (define L.f.1
                     ((locals ())
                      (conflicts ((x.1 ())))
                      (assignment ((x.1 rsp))))
                     (begin (set! x.1 rdi) (jump done x.1)))
                   (define L.g.1
                     ((locals ())
                      (conflicts
                       ((z.1 (x.1 rbp))
                        (x.1 (z.1 y.1 rsi rdx rbp))
                        (y.1 (rdx x.1 rbp))
                        (rbp (rdi z.1 y.1 x.1))
                        (rdx (y.1 x.1))
                        (rsi (x.1))
                        (rdi (rbp))))
                      (assignment ((z.1 rsp) (x.1 rbx) (y.1 rsp))))
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

  (check-equal? (assign-registers '(module
                                       ((locals ())
                                        (conflicts ((r13 (rdi rbp)) (rbp (rdi r13)) (rdi (rbp r13))))
                                        (assignment ()))
                                     (define L.f.1
                                       ((locals (x.1)) (conflicts ((x.1 ()))) (assignment ()))
                                       (begin (set! x.1 rdi) (jump done x.1)))
                                     (begin (set! r13 L.f.1) (set! rdi 1) (jump r13 rbp rdi))))
                '(module
                     ((locals ())
                      (conflicts ((r13 (rdi rbp)) (rbp (rdi r13)) (rdi (rbp r13))))
                      (assignment ()))
                   (define L.f.1
                     ((locals ()) (conflicts ((x.1 ()))) (assignment ((x.1 rsp))))
                     (begin (set! x.1 rdi) (jump done x.1)))
                   (begin (set! r13 L.f.1) (set! rdi 1) (jump r13 rbp rdi))))

  (check-equal? (assign-registers '(module
                                       ((locals (a.1))
                                        (conflicts ((a.1 (rdi rbp)) (rbp (rdi a.1)) (rdi (rbp a.1))))
                                        (assignment ()))
                                     (define L.f.1
                                       ((locals (x.1)) (conflicts ((x.1 ()))) (assignment ()))
                                       (begin (set! x.1 rdi) (jump done x.1)))
                                     (begin (set! a.1 L.f.1) (set! rdi 1) (jump a.1 rbp rdi))))
                '(module
                     ((locals ())
                      (conflicts ((a.1 (rdi rbp)) (rbp (rdi a.1)) (rdi (rbp a.1))))
                      (assignment ((a.1 rsp))))
                   (define L.f.1
                     ((locals ()) (conflicts ((x.1 ())))(assignment ((x.1 rsp))))
                     (begin (set! x.1 rdi) (jump done x.1)))
                   (begin (set! a.1 L.f.1) (set! rdi 1) (jump a.1 rbp rdi))))


  (check-equal? (assign-registers '(module
                                       ((locals ())
                                        (conflicts ((rdi (rbp)) (rbp (rdi))))
                                        (assignment ()))
                                     (define L.f.1
                                       ((locals (x.1)) (conflicts ((x.1 ()))) (assignment ()))
                                       (begin (set! x.1 rdi) (jump x.1)))
                                     (begin (set! rdi 1) (jump L.f.1 rbp rdi))))
                '(module
                     ((locals ()) (conflicts ((rdi (rbp)) (rbp (rdi)))) (assignment ()))
                   (define L.f.1
                     ((locals ()) (conflicts ((x.1 ()))) (assignment ((x.1 rsp))))
                     (begin (set! x.1 rdi) (jump x.1)))
                   (begin (set! rdi 1) (jump L.f.1 rbp rdi))))
  (check-equal? (assign-registers '(module
                                       ((locals (x.3 tmp-ra.2 x.2))
                                        (conflicts
                                         ((x.2 (x.3 tmp-ra.2 rbp))
                                          (tmp-ra.2 (rdi x.3 x.2 rbp))
                                          (x.3 (x.2 tmp-ra.2 rbp))
                                          (rbp (r15 rdi x.3 x.2 tmp-ra.2))
                                          (rdi (r15 tmp-ra.2 rbp))
                                          (r15 (rdi rbp))))
                                        (assignment ()))
                                     (define L.f.1
                                       ((locals (tmp-ra.1 b.1 y.1 x.1 z.1 a.1))
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
                                        (assignment ()))
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
  (check-equal? (assign-registers '(module
                                       ((new-frames ())
                                        (locals (tmp-ra.238))
                                        (call-undead ())
                                        (undead-out
                                         ((tmp-ra.238 rbp)
                                          (tmp-ra.238 rdi rbp)
                                          (tmp-ra.238 rsi rdi rbp)
                                          (tmp-ra.238 rdx rsi rdi rbp)
                                          (tmp-ra.238 rcx rdx rsi rdi rbp)
                                          (tmp-ra.238 r8 rcx rdx rsi rdi rbp)
                                          (tmp-ra.238 r9 r8 rcx rdx rsi rdi rbp)
                                          (tmp-ra.238 fv0 r9 r8 rcx rdx rsi rdi rbp)
                                          (fv0 r9 r8 rcx rdx rsi rdi r15 rbp)
                                          (fv0 r9 r8 rcx rdx rsi rdi r15 rbp)))
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
                                        (assignment ()))
                                     (define L.+.31
                                       ((new-frames ())
                                        (locals (tmp.183 tmp.97 tmp.184 tmp-ra.232 tmp.186 tmp.96 tmp.185))
                                        (undead-out
                                         ((rdi rsi tmp-ra.232 rbp)
                                          (rsi tmp.96 tmp-ra.232 rbp)
                                          (tmp.96 tmp.97 tmp-ra.232 rbp)
                                          (((((tmp.184 tmp.96 tmp.97 tmp-ra.232 rbp)
                                              (tmp.184 tmp.96 tmp.97 tmp-ra.232 rbp)
                                              (tmp.96 tmp.97 tmp-ra.232 rbp))
                                             (tmp.183 tmp.96 tmp.97 tmp-ra.232 rbp)
                                             (tmp.183 tmp.96 tmp.97 tmp-ra.232 rbp))
                                            (tmp.96 tmp.97 tmp-ra.232 rbp))
                                           (((((tmp.186 tmp.96 tmp.97 tmp-ra.232 rbp)
                                               (tmp.186 tmp.96 tmp.97 tmp-ra.232 rbp)
                                               (tmp.96 tmp.97 tmp-ra.232 rbp))
                                              (tmp.185 tmp.96 tmp.97 tmp-ra.232 rbp)
                                              (tmp.185 tmp.96 tmp.97 tmp-ra.232 rbp))
                                             (tmp.96 tmp.97 tmp-ra.232 rbp))
                                            ((tmp.97 rax tmp-ra.232 rbp) (tmp-ra.232 rax rbp) (rax rbp))
                                            ((tmp-ra.232 rax rbp) (rax rbp)))
                                           ((tmp-ra.232 rax rbp) (rax rbp)))))
                                        (call-undead ())
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
                                        (assignment ()))
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
                                       ((new-frames ())
                                        (locals (a.19 b.20 c.21 d.22 e.23 f.24 g.25 nfv.235 nfv.234 tmp.187))
                                        (undead-out
                                         ((rdi rsi rdx rcx r8 r9 fv0 tmp-ra.233 rbp)
                                          (rsi rdx rcx r8 r9 fv0 a.19 tmp-ra.233 rbp)
                                          (rdx rcx r8 r9 fv0 b.20 a.19 tmp-ra.233 rbp)
                                          (rcx r8 r9 fv0 c.21 b.20 a.19 tmp-ra.233 rbp)
                                          (r8 r9 fv0 d.22 c.21 b.20 a.19 tmp-ra.233 rbp)
                                          (r9 fv0 e.23 d.22 c.21 b.20 a.19 tmp-ra.233 rbp)
                                          (fv0 f.24 e.23 d.22 c.21 b.20 a.19 tmp-ra.233 rbp)
                                          (g.25 f.24 e.23 d.22 c.21 b.20 a.19 tmp-ra.233 rbp)
                                          ((rax tmp-ra.233 rbp)
                                           ((b.20 c.21 d.22 e.23 f.24 g.25 rdi rbp)
                                            (c.21 d.22 e.23 f.24 g.25 rsi rdi rbp)
                                            (d.22 e.23 f.24 g.25 rdx rsi rdi rbp)
                                            (e.23 f.24 g.25 rcx rdx rsi rdi rbp)
                                            (f.24 g.25 r8 rcx rdx rsi rdi rbp)
                                            (g.25 r9 r8 rcx rdx rsi rdi rbp)
                                            (nfv.234 r9 r8 rcx rdx rsi rdi rbp)
                                            (nfv.235 nfv.234 r9 r8 rcx rdx rsi rdi rbp)
                                            (nfv.235 nfv.234 r9 r8 rcx rdx rsi rdi r15 rbp)
                                            (nfv.235 nfv.234 r9 r8 rcx rdx rsi rdi r15 rbp)))
                                          (tmp.187 tmp-ra.233 rbp)
                                          (tmp.187 tmp-ra.233 rdi rbp)
                                          (tmp-ra.233 rsi rdi rbp)
                                          (rsi rdi r15 rbp)
                                          (rsi rdi r15 rbp)))
                                        (call-undead (tmp-ra.233))
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
                                        (assignment ((tmp-ra.233 fv1))))
                                       (begin
                                         (set! tmp-ra.233 r15)
                                         (set! a.19 rdi)
                                         (set! b.20 rsi)
                                         (set! c.21 rdx)
                                         (set! d.22 rcx)
                                         (set! e.23 r8)
                                         (set! f.24 r9)
                                         (set! g.25 fv0)
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
                                         (set! tmp.187 rax)
                                         (set! rdi 80)
                                         (set! rsi tmp.187)
                                         (set! r15 tmp-ra.233)
                                         (jump L.+.31 rbp r15 rdi rsi)))
                                     (define L.G.7
                                       ((new-frames ())
                                        (locals (tmp-ra.236 a.26 b.27 c.28 d.29 e.30 f.31 g.32 h.33))
                                        (undead-out
                                         ((rdi rsi rdx rcx r8 r9 fv0 fv1 tmp-ra.236 rbp)
                                          (rsi rdx rcx r8 r9 fv0 fv1 a.26 tmp-ra.236 rbp)
                                          (rdx rcx r8 r9 fv0 fv1 a.26 b.27 tmp-ra.236 rbp)
                                          (rcx r8 r9 fv0 fv1 a.26 b.27 c.28 tmp-ra.236 rbp)
                                          (r8 r9 fv0 fv1 a.26 b.27 c.28 d.29 tmp-ra.236 rbp)
                                          (r9 fv0 fv1 a.26 b.27 c.28 d.29 e.30 tmp-ra.236 rbp)
                                          (fv0 fv1 a.26 b.27 c.28 d.29 e.30 f.31 tmp-ra.236 rbp)
                                          (fv1 a.26 b.27 c.28 d.29 e.30 f.31 g.32 tmp-ra.236 rbp)
                                          (a.26 b.27 c.28 d.29 e.30 f.31 g.32 h.33 tmp-ra.236 rbp)
                                          (b.27 c.28 d.29 e.30 f.31 g.32 h.33 tmp-ra.236 rdi rbp)
                                          (c.28 d.29 e.30 f.31 g.32 h.33 tmp-ra.236 rsi rdi rbp)
                                          (d.29 e.30 f.31 g.32 h.33 tmp-ra.236 rdx rsi rdi rbp)
                                          (e.30 f.31 g.32 h.33 tmp-ra.236 rcx rdx rsi rdi rbp)
                                          (f.31 g.32 h.33 tmp-ra.236 r8 rcx rdx rsi rdi rbp)
                                          (g.32 h.33 tmp-ra.236 r9 r8 rcx rdx rsi rdi rbp)
                                          (h.33 tmp-ra.236 fv0 r9 r8 rcx rdx rsi rdi rbp)
                                          (tmp-ra.236 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)
                                          (tmp-ra.236 fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)
                                          (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi r15 rbp)
                                          (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi r15 rbp)))
                                        (call-undead ())
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
                                        (assignment ()))
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
                                       ((new-frames ())
                                        (locals (a.34 b.35 r1.43 r2.44 r3.45 r4.46 r5.47 r6.48 r7.49))
                                        (undead-out
                                         ((rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.237 rbp)
                                          (rsi rdx rcx r8 r9 fv0 fv1 fv2 a.34 tmp-ra.237 rbp)
                                          (rdx rcx r8 r9 fv0 fv1 fv2 b.35 a.34 tmp-ra.237 rbp)
                                          (rcx r8 r9 fv0 fv1 fv2 b.35 a.34 c.36 tmp-ra.237 rbp)
                                          (r8 r9 fv0 fv1 fv2 b.35 a.34 c.36 d.37 tmp-ra.237 rbp)
                                          (r9 fv0 fv1 fv2 b.35 a.34 c.36 d.37 e.38 tmp-ra.237 rbp)
                                          (fv0 fv1 fv2 b.35 a.34 c.36 d.37 e.38 f.39 tmp-ra.237 rbp)
                                          (fv1 fv2 b.35 a.34 c.36 d.37 e.38 f.39 g.40 tmp-ra.237 rbp)
                                          (fv2 b.35 a.34 c.36 d.37 e.38 f.39 g.40 h.41 tmp-ra.237 rbp)
                                          (b.35 a.34 c.36 d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                          ((rax c.36 d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                           ((b.35 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                          (c.36 r1.43 d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                          ((rax d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                           ((c.36 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                          (d.37 r2.44 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                          ((rax e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                           ((d.37 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                          (e.38 r3.45 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                          ((rax f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                           ((e.38 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                          (f.39 r4.46 g.40 h.41 j.42 tmp-ra.237 rbp)
                                          ((rax g.40 h.41 j.42 tmp-ra.237 rbp)
                                           ((f.39 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                          (g.40 r5.47 h.41 j.42 tmp-ra.237 rbp)
                                          ((rax h.41 j.42 tmp-ra.237 rbp)
                                           ((g.40 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                          (h.41 r6.48 j.42 tmp-ra.237 rbp)
                                          ((rax j.42 tmp-ra.237 rbp)
                                           ((h.41 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                          (r7.49 j.42 tmp-ra.237 rbp)
                                          (j.42 tmp-ra.237 rdi rbp)
                                          (tmp-ra.237 rsi rdi rbp)
                                          (rsi rdi r15 rbp)
                                          (rsi rdi r15 rbp)))
                                        (call-undead (c.36 d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237))
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
                                          (c.36 fv8))))
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
                                         (return-point L.rp.48
                                                       (begin
                                                         (set! rdi a.34)
                                                         (set! rsi b.35)
                                                         (set! r15 L.rp.48)
                                                         (jump L.+.31 rbp r15 rdi rsi)))
                                         (set! r1.43 rax)
                                         (return-point L.rp.49
                                                       (begin
                                                         (set! rdi r1.43)
                                                         (set! rsi c.36)
                                                         (set! r15 L.rp.49)
                                                         (jump L.+.31 rbp r15 rdi rsi)))
                                         (set! r2.44 rax)
                                         (return-point L.rp.50
                                                       (begin
                                                         (set! rdi r2.44)
                                                         (set! rsi d.37)
                                                         (set! r15 L.rp.50)
                                                         (jump L.+.31 rbp r15 rdi rsi)))
                                         (set! r3.45 rax)
                                         (return-point L.rp.51
                                                       (begin
                                                         (set! rdi r3.45)
                                                         (set! rsi e.38)
                                                         (set! r15 L.rp.51)
                                                         (jump L.+.31 rbp r15 rdi rsi)))
                                         (set! r4.46 rax)
                                         (return-point L.rp.52
                                                       (begin
                                                         (set! rdi r4.46)
                                                         (set! rsi f.39)
                                                         (set! r15 L.rp.52)
                                                         (jump L.+.31 rbp r15 rdi rsi)))
                                         (set! r5.47 rax)
                                         (return-point L.rp.53
                                                       (begin
                                                         (set! rdi r5.47)
                                                         (set! rsi g.40)
                                                         (set! r15 L.rp.53)
                                                         (jump L.+.31 rbp r15 rdi rsi)))
                                         (set! r6.48 rax)
                                         (return-point L.rp.54
                                                       (begin
                                                         (set! rdi r6.48)
                                                         (set! rsi h.41)
                                                         (set! r15 L.rp.54)
                                                         (jump L.+.31 rbp r15 rdi rsi)))
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
                     ((new-frames ())
                      (locals ())
                      (call-undead ())
                      (undead-out
                       ((tmp-ra.238 rbp)
                        (tmp-ra.238 rdi rbp)
                        (tmp-ra.238 rsi rdi rbp)
                        (tmp-ra.238 rdx rsi rdi rbp)
                        (tmp-ra.238 rcx rdx rsi rdi rbp)
                        (tmp-ra.238 r8 rcx rdx rsi rdi rbp)
                        (tmp-ra.238 r9 r8 rcx rdx rsi rdi rbp)
                        (tmp-ra.238 fv0 r9 r8 rcx rdx rsi rdi rbp)
                        (fv0 r9 r8 rcx rdx rsi rdi r15 rbp)
                        (fv0 r9 r8 rcx rdx rsi rdi r15 rbp)))
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
                     ((new-frames ())
                      (locals ())
                      (undead-out
                       ((rdi rsi tmp-ra.232 rbp)
                        (rsi tmp.96 tmp-ra.232 rbp)
                        (tmp.96 tmp.97 tmp-ra.232 rbp)
                        (((((tmp.184 tmp.96 tmp.97 tmp-ra.232 rbp)
                            (tmp.184 tmp.96 tmp.97 tmp-ra.232 rbp)
                            (tmp.96 tmp.97 tmp-ra.232 rbp))
                           (tmp.183 tmp.96 tmp.97 tmp-ra.232 rbp)
                           (tmp.183 tmp.96 tmp.97 tmp-ra.232 rbp))
                          (tmp.96 tmp.97 tmp-ra.232 rbp))
                         (((((tmp.186 tmp.96 tmp.97 tmp-ra.232 rbp)
                             (tmp.186 tmp.96 tmp.97 tmp-ra.232 rbp)
                             (tmp.96 tmp.97 tmp-ra.232 rbp))
                            (tmp.185 tmp.96 tmp.97 tmp-ra.232 rbp)
                            (tmp.185 tmp.96 tmp.97 tmp-ra.232 rbp))
                           (tmp.96 tmp.97 tmp-ra.232 rbp))
                          ((tmp.97 rax tmp-ra.232 rbp) (tmp-ra.232 rax rbp) (rax rbp))
                          ((tmp-ra.232 rax rbp) (rax rbp)))
                         ((tmp-ra.232 rax rbp) (rax rbp)))))
                      (call-undead ())
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
                        (tmp.96 r14)
                        (tmp.97 r13)
                        (tmp.185 r9)
                        (tmp.186 r9)
                        (tmp.184 r9)
                        (tmp.183 r9))))
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
                     ((new-frames ())
                      (locals ())
                      (undead-out
                       ((rdi rsi rdx rcx r8 r9 fv0 tmp-ra.233 rbp)
                        (rsi rdx rcx r8 r9 fv0 a.19 tmp-ra.233 rbp)
                        (rdx rcx r8 r9 fv0 b.20 a.19 tmp-ra.233 rbp)
                        (rcx r8 r9 fv0 c.21 b.20 a.19 tmp-ra.233 rbp)
                        (r8 r9 fv0 d.22 c.21 b.20 a.19 tmp-ra.233 rbp)
                        (r9 fv0 e.23 d.22 c.21 b.20 a.19 tmp-ra.233 rbp)
                        (fv0 f.24 e.23 d.22 c.21 b.20 a.19 tmp-ra.233 rbp)
                        (g.25 f.24 e.23 d.22 c.21 b.20 a.19 tmp-ra.233 rbp)
                        ((rax tmp-ra.233 rbp)
                         ((b.20 c.21 d.22 e.23 f.24 g.25 rdi rbp)
                          (c.21 d.22 e.23 f.24 g.25 rsi rdi rbp)
                          (d.22 e.23 f.24 g.25 rdx rsi rdi rbp)
                          (e.23 f.24 g.25 rcx rdx rsi rdi rbp)
                          (f.24 g.25 r8 rcx rdx rsi rdi rbp)
                          (g.25 r9 r8 rcx rdx rsi rdi rbp)
                          (nfv.234 r9 r8 rcx rdx rsi rdi rbp)
                          (nfv.235 nfv.234 r9 r8 rcx rdx rsi rdi rbp)
                          (nfv.235 nfv.234 r9 r8 rcx rdx rsi rdi r15 rbp)
                          (nfv.235 nfv.234 r9 r8 rcx rdx rsi rdi r15 rbp)))
                        (tmp.187 tmp-ra.233 rbp)
                        (tmp.187 tmp-ra.233 rdi rbp)
                        (tmp-ra.233 rsi rdi rbp)
                        (rsi rdi r15 rbp)
                        (rsi rdi r15 rbp)))
                      (call-undead (tmp-ra.233))
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
                        (g.25 r15)
                        (f.24 r14)
                        (e.23 r13)
                        (d.22 rcx)
                        (c.21 rdx)
                        (b.20 rsi)
                        (a.19 rdi)
                        (nfv.234 r14)
                        (nfv.235 r13)
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
                       (set! tmp.187 rax)
                       (set! rdi 80)
                       (set! rsi tmp.187)
                       (set! r15 tmp-ra.233)
                       (jump L.+.31 rbp r15 rdi rsi)))
                   (define L.G.7
                     ((new-frames ())
                      (locals ())
                      (undead-out
                       ((rdi rsi rdx rcx r8 r9 fv0 fv1 tmp-ra.236 rbp)
                        (rsi rdx rcx r8 r9 fv0 fv1 a.26 tmp-ra.236 rbp)
                        (rdx rcx r8 r9 fv0 fv1 a.26 b.27 tmp-ra.236 rbp)
                        (rcx r8 r9 fv0 fv1 a.26 b.27 c.28 tmp-ra.236 rbp)
                        (r8 r9 fv0 fv1 a.26 b.27 c.28 d.29 tmp-ra.236 rbp)
                        (r9 fv0 fv1 a.26 b.27 c.28 d.29 e.30 tmp-ra.236 rbp)
                        (fv0 fv1 a.26 b.27 c.28 d.29 e.30 f.31 tmp-ra.236 rbp)
                        (fv1 a.26 b.27 c.28 d.29 e.30 f.31 g.32 tmp-ra.236 rbp)
                        (a.26 b.27 c.28 d.29 e.30 f.31 g.32 h.33 tmp-ra.236 rbp)
                        (b.27 c.28 d.29 e.30 f.31 g.32 h.33 tmp-ra.236 rdi rbp)
                        (c.28 d.29 e.30 f.31 g.32 h.33 tmp-ra.236 rsi rdi rbp)
                        (d.29 e.30 f.31 g.32 h.33 tmp-ra.236 rdx rsi rdi rbp)
                        (e.30 f.31 g.32 h.33 tmp-ra.236 rcx rdx rsi rdi rbp)
                        (f.31 g.32 h.33 tmp-ra.236 r8 rcx rdx rsi rdi rbp)
                        (g.32 h.33 tmp-ra.236 r9 r8 rcx rdx rsi rdi rbp)
                        (h.33 tmp-ra.236 fv0 r9 r8 rcx rdx rsi rdi rbp)
                        (tmp-ra.236 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)
                        (tmp-ra.236 fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)
                        (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi r15 rbp)
                        (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi r15 rbp)))
                      (call-undead ())
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
                        (h.33 r14)
                        (g.32 r13)
                        (f.31 r9)
                        (e.30 r8)
                        (d.29 rcx)
                        (c.28 rdx)
                        (b.27 rsi)
                        (a.26 rdi))))
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
                     ((new-frames ())
                      (locals ())
                      (undead-out
                       ((rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.237 rbp)
                        (rsi rdx rcx r8 r9 fv0 fv1 fv2 a.34 tmp-ra.237 rbp)
                        (rdx rcx r8 r9 fv0 fv1 fv2 b.35 a.34 tmp-ra.237 rbp)
                        (rcx r8 r9 fv0 fv1 fv2 b.35 a.34 c.36 tmp-ra.237 rbp)
                        (r8 r9 fv0 fv1 fv2 b.35 a.34 c.36 d.37 tmp-ra.237 rbp)
                        (r9 fv0 fv1 fv2 b.35 a.34 c.36 d.37 e.38 tmp-ra.237 rbp)
                        (fv0 fv1 fv2 b.35 a.34 c.36 d.37 e.38 f.39 tmp-ra.237 rbp)
                        (fv1 fv2 b.35 a.34 c.36 d.37 e.38 f.39 g.40 tmp-ra.237 rbp)
                        (fv2 b.35 a.34 c.36 d.37 e.38 f.39 g.40 h.41 tmp-ra.237 rbp)
                        (b.35 a.34 c.36 d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                        ((rax c.36 d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                         ((b.35 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                        (c.36 r1.43 d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                        ((rax d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                         ((c.36 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                        (d.37 r2.44 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                        ((rax e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                         ((d.37 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                        (e.38 r3.45 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                        ((rax f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                         ((e.38 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                        (f.39 r4.46 g.40 h.41 j.42 tmp-ra.237 rbp)
                        ((rax g.40 h.41 j.42 tmp-ra.237 rbp)
                         ((f.39 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                        (g.40 r5.47 h.41 j.42 tmp-ra.237 rbp)
                        ((rax h.41 j.42 tmp-ra.237 rbp)
                         ((g.40 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                        (h.41 r6.48 j.42 tmp-ra.237 rbp)
                        ((rax j.42 tmp-ra.237 rbp)
                         ((h.41 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                        (r7.49 j.42 tmp-ra.237 rbp)
                        (j.42 tmp-ra.237 rdi rbp)
                        (tmp-ra.237 rsi rdi rbp)
                        (rsi rdi r15 rbp)
                        (rsi rdi r15 rbp)))
                      (call-undead (c.36 d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237))
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
                        (b.35 r15)
                        (a.34 r14)
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
                       (return-point L.rp.48
                                     (begin
                                       (set! rdi a.34)
                                       (set! rsi b.35)
                                       (set! r15 L.rp.48)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                       (set! r1.43 rax)
                       (return-point L.rp.49
                                     (begin
                                       (set! rdi r1.43)
                                       (set! rsi c.36)
                                       (set! r15 L.rp.49)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                       (set! r2.44 rax)
                       (return-point L.rp.50
                                     (begin
                                       (set! rdi r2.44)
                                       (set! rsi d.37)
                                       (set! r15 L.rp.50)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                       (set! r3.45 rax)
                       (return-point L.rp.51
                                     (begin
                                       (set! rdi r3.45)
                                       (set! rsi e.38)
                                       (set! r15 L.rp.51)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                       (set! r4.46 rax)
                       (return-point L.rp.52
                                     (begin
                                       (set! rdi r4.46)
                                       (set! rsi f.39)
                                       (set! r15 L.rp.52)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                       (set! r5.47 rax)
                       (return-point L.rp.53
                                     (begin
                                       (set! rdi r5.47)
                                       (set! rsi g.40)
                                       (set! r15 L.rp.53)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                       (set! r6.48 rax)
                       (return-point L.rp.54
                                     (begin
                                       (set! rdi r6.48)
                                       (set! rsi h.41)
                                       (set! r15 L.rp.54)
                                       (jump L.+.31 rbp r15 rdi rsi)))
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

  (parameterize ([current-assignable-registers '()])
    (check-equal? (interp-asm-pred-lang-v7/spilled (assign-registers
                                                    '(module
                                                         ((locals (tmp-ra.63))
                                                          (conflicts
                                                           ((tmp-ra.63 (rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 rbp))
                                                            (rbp (r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.63))
                                                            (fv2 (r15 rdi rsi rdx rcx r8 r9 fv0 fv1 rbp tmp-ra.63))
                                                            (fv1 (r15 rdi rsi rdx rcx r8 r9 fv0 rbp fv2 tmp-ra.63))
                                                            (fv0 (r15 rdi rsi rdx rcx r8 r9 rbp fv1 fv2 tmp-ra.63))
                                                            (r9 (r15 rdi rsi rdx rcx r8 rbp fv0 fv1 fv2 tmp-ra.63))
                                                            (r8 (r15 rdi rsi rdx rcx rbp r9 fv0 fv1 fv2 tmp-ra.63))
                                                            (rcx (r15 rdi rsi rdx rbp r8 r9 fv0 fv1 fv2 tmp-ra.63))
                                                            (rdx (r15 rdi rsi rbp rcx r8 r9 fv0 fv1 fv2 tmp-ra.63))
                                                            (rsi (r15 rdi rbp rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.63))
                                                            (rdi (r15 rbp rsi rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.63))
                                                            (r15 (rbp rdi rsi rdx rcx r8 r9 fv0 fv1 fv2))))
                                                          (assignment ()))
                                                       (define L.*.4
                                                         ((locals (tmp.44 tmp.19 tmp.45 tmp.46 tmp-ra.57 tmp.43 tmp.20 tmp.42))
                                                          (conflicts
                                                           ((tmp.42 (rbp tmp-ra.57 tmp.19 tmp.20))
                                                            (tmp.20 (rbp tmp-ra.57 tmp.19 tmp.43 tmp.42 tmp.45 tmp.44))
                                                            (tmp.43 (rbp tmp-ra.57 tmp.19 tmp.20))
                                                            (tmp-ra.57
                                                             (tmp.20 tmp.19 rbp rsi rdi tmp.43 tmp.42 tmp.45 tmp.44 tmp.46 rax))
                                                            (tmp.46 (rax rbp tmp-ra.57 tmp.19))
                                                            (tmp.45 (rbp tmp-ra.57 tmp.19 tmp.20))
                                                            (tmp.19 (tmp.20 rbp tmp-ra.57 rsi tmp.43 tmp.42 tmp.45 tmp.44 tmp.46))
                                                            (tmp.44 (rbp tmp-ra.57 tmp.19 tmp.20))
                                                            (rax (tmp.46 rbp tmp-ra.57))
                                                            (rbp (tmp.20 tmp.19 tmp-ra.57 tmp.43 tmp.42 tmp.45 tmp.44 tmp.46 rax))
                                                            (rdi (tmp-ra.57))
                                                            (rsi (tmp.19 tmp-ra.57))))
                                                          (assignment ()))
                                                         (begin
                                                           (set! tmp-ra.57 r15)
                                                           (set! tmp.19 rdi)
                                                           (set! tmp.20 rsi)
                                                           (if (begin
                                                                 (if (begin
                                                                       (begin
                                                                         (set! tmp.43 tmp.20)
                                                                         (set! tmp.43 (bitwise-and tmp.43 7)))
                                                                       (= tmp.43 0))
                                                                     (set! tmp.42 14)
                                                                     (set! tmp.42 6))
                                                                 (!= tmp.42 6))
                                                               (if (begin
                                                                     (if (begin
                                                                           (begin
                                                                             (set! tmp.45 tmp.19)
                                                                             (set! tmp.45 (bitwise-and tmp.45 7)))
                                                                           (= tmp.45 0))
                                                                         (set! tmp.44 14)
                                                                         (set! tmp.44 6))
                                                                     (!= tmp.44 6))
                                                                   (begin
                                                                     (set! tmp.46 tmp.20)
                                                                     (set! tmp.46 (arithmetic-shift-right tmp.46 3))
                                                                     (set! rax tmp.19)
                                                                     (set! rax (* rax tmp.46))
                                                                     (jump tmp-ra.57 rbp rax))
                                                                   (begin (set! rax 318) (jump tmp-ra.57 rbp rax)))
                                                               (begin (set! rax 318) (jump tmp-ra.57 rbp rax)))))
                                                       (define L.+.3
                                                         ((locals (tmp.49 tmp.21 tmp.50 tmp-ra.58 tmp.48 tmp.22 tmp.47))
                                                          (conflicts
                                                           ((tmp.47 (rbp tmp-ra.58 tmp.22 tmp.21))
                                                            (tmp.22 (rbp tmp-ra.58 tmp.21 tmp.48 tmp.47 tmp.50 tmp.49 rax))
                                                            (tmp.48 (rbp tmp-ra.58 tmp.22 tmp.21))
                                                            (tmp-ra.58 (tmp.22 tmp.21 rbp rsi rdi tmp.48 tmp.47 tmp.50 tmp.49 rax))
                                                            (tmp.50 (rbp tmp-ra.58 tmp.22 tmp.21))
                                                            (tmp.21 (tmp.22 rbp tmp-ra.58 rsi tmp.48 tmp.47 tmp.50 tmp.49))
                                                            (tmp.49 (rbp tmp-ra.58 tmp.22 tmp.21))
                                                            (rax (tmp.22 rbp tmp-ra.58))
                                                            (rbp (tmp.22 tmp.21 tmp-ra.58 tmp.48 tmp.47 tmp.50 tmp.49 rax))
                                                            (rdi (tmp-ra.58))
                                                            (rsi (tmp.21 tmp-ra.58))))
                                                          (assignment ()))
                                                         (begin
                                                           (set! tmp-ra.58 r15)
                                                           (set! tmp.21 rdi)
                                                           (set! tmp.22 rsi)
                                                           (if (begin
                                                                 (if (begin
                                                                       (begin
                                                                         (set! tmp.48 tmp.22)
                                                                         (set! tmp.48 (bitwise-and tmp.48 7)))
                                                                       (= tmp.48 0))
                                                                     (set! tmp.47 14)
                                                                     (set! tmp.47 6))
                                                                 (!= tmp.47 6))
                                                               (if (begin
                                                                     (if (begin
                                                                           (begin
                                                                             (set! tmp.50 tmp.21)
                                                                             (set! tmp.50 (bitwise-and tmp.50 7)))
                                                                           (= tmp.50 0))
                                                                         (set! tmp.49 14)
                                                                         (set! tmp.49 6))
                                                                     (!= tmp.49 6))
                                                                   (begin
                                                                     (set! rax tmp.21)
                                                                     (set! rax (+ rax tmp.22))
                                                                     (jump tmp-ra.58 rbp rax))
                                                                   (begin (set! rax 574) (jump tmp-ra.58 rbp rax)))
                                                               (begin (set! rax 574) (jump tmp-ra.58 rbp rax)))))
                                                       (define L.add.1
                                                         ((locals (tmp.51 tmp.52 tmp.53 tmp.54 tmp.55 tmp.56 h.1 g.2))
                                                          (conflicts
                                                           ((tmp-ra.59
                                                             (tmp.51
                                                              tmp.52
                                                              tmp.53
                                                              tmp.54
                                                              tmp.55
                                                              tmp.56
                                                              h.1
                                                              g.2
                                                              f.3
                                                              e.4
                                                              d.5
                                                              c.6
                                                              b.7
                                                              a.8
                                                              rbp
                                                              fv1
                                                              fv0
                                                              r9
                                                              r8
                                                              rcx
                                                              rdx
                                                              rsi
                                                              rdi))
                                                            (a.8
                                                             (tmp.51
                                                              tmp.52
                                                              tmp.53
                                                              tmp.54
                                                              tmp.55
                                                              tmp.56
                                                              h.1
                                                              g.2
                                                              f.3
                                                              e.4
                                                              d.5
                                                              c.6
                                                              b.7
                                                              rbp
                                                              tmp-ra.59
                                                              fv1
                                                              fv0
                                                              r9
                                                              r8
                                                              rcx
                                                              rdx
                                                              rsi))
                                                            (b.7
                                                             (rsi
                                                              tmp.52
                                                              tmp.53
                                                              tmp.54
                                                              tmp.55
                                                              tmp.56
                                                              h.1
                                                              g.2
                                                              f.3
                                                              e.4
                                                              d.5
                                                              c.6
                                                              rbp
                                                              tmp-ra.59
                                                              a.8
                                                              fv1
                                                              fv0
                                                              r9
                                                              r8
                                                              rcx
                                                              rdx))
                                                            (c.6
                                                             (rsi
                                                              tmp.53
                                                              tmp.54
                                                              tmp.55
                                                              tmp.56
                                                              h.1
                                                              g.2
                                                              f.3
                                                              e.4
                                                              d.5
                                                              rbp
                                                              tmp-ra.59
                                                              a.8
                                                              b.7
                                                              fv1
                                                              fv0
                                                              r9
                                                              r8
                                                              rcx))
                                                            (d.5
                                                             (rsi
                                                              tmp.54
                                                              tmp.55
                                                              tmp.56
                                                              h.1
                                                              g.2
                                                              f.3
                                                              e.4
                                                              rbp
                                                              tmp-ra.59
                                                              a.8
                                                              b.7
                                                              c.6
                                                              fv1
                                                              fv0
                                                              r9
                                                              r8))
                                                            (e.4
                                                             (rsi
                                                              tmp.55
                                                              tmp.56
                                                              h.1
                                                              g.2
                                                              f.3
                                                              rbp
                                                              tmp-ra.59
                                                              a.8
                                                              b.7
                                                              c.6
                                                              d.5
                                                              fv1
                                                              fv0
                                                              r9))
                                                            (f.3 (rsi tmp.56 h.1 g.2 rbp tmp-ra.59 a.8 b.7 c.6 d.5 e.4 fv1 fv0))
                                                            (g.2 (rsi h.1 rbp tmp-ra.59 a.8 b.7 c.6 d.5 e.4 f.3 fv1))
                                                            (h.1 (rbp tmp-ra.59 a.8 b.7 c.6 d.5 e.4 f.3 g.2))
                                                            (tmp.56 (rbp tmp-ra.59 a.8 b.7 c.6 d.5 e.4 f.3))
                                                            (tmp.55 (rbp tmp-ra.59 a.8 b.7 c.6 d.5 e.4))
                                                            (tmp.54 (rbp tmp-ra.59 a.8 b.7 c.6 d.5))
                                                            (tmp.53 (rbp tmp-ra.59 a.8 b.7 c.6))
                                                            (tmp.52 (rbp tmp-ra.59 a.8 b.7))
                                                            (tmp.51 (rbp tmp-ra.59 a.8))
                                                            (rdi (r15 rbp rsi tmp-ra.59))
                                                            (rsi (b.7 c.6 d.5 e.4 f.3 r15 rdi rbp g.2 a.8 tmp-ra.59))
                                                            (rdx (b.7 a.8 tmp-ra.59))
                                                            (rcx (c.6 b.7 a.8 tmp-ra.59))
                                                            (r8 (d.5 c.6 b.7 a.8 tmp-ra.59))
                                                            (r9 (e.4 d.5 c.6 b.7 a.8 tmp-ra.59))
                                                            (fv0 (f.3 e.4 d.5 c.6 b.7 a.8 tmp-ra.59))
                                                            (fv1 (g.2 f.3 e.4 d.5 c.6 b.7 a.8 tmp-ra.59))
                                                            (rbp
                                                             (tmp.51
                                                              tmp.52
                                                              tmp.53
                                                              tmp.54
                                                              tmp.55
                                                              tmp.56
                                                              r15
                                                              rdi
                                                              rsi
                                                              h.1
                                                              g.2
                                                              f.3
                                                              e.4
                                                              d.5
                                                              c.6
                                                              b.7
                                                              a.8
                                                              tmp-ra.59))
                                                            (r15 (rbp rdi rsi))))
                                                          (assignment
                                                           ((tmp-ra.59 fv2)
                                                            (a.8 fv3)
                                                            (b.7 fv4)
                                                            (c.6 fv5)
                                                            (d.5 fv6)
                                                            (e.4 fv7)
                                                            (f.3 fv8))))
                                                         (begin
                                                           (set! tmp-ra.59 r15)
                                                           (set! a.8 rdi)
                                                           (set! b.7 rsi)
                                                           (set! c.6 rdx)
                                                           (set! d.5 rcx)
                                                           (set! e.4 r8)
                                                           (set! f.3 r9)
                                                           (set! g.2 fv0)
                                                           (set! h.1 fv1)
                                                           (begin
                                                             (set! rbp (- rbp 72))
                                                             (return-point L.rp.5
                                                                           (begin
                                                                             (set! rsi h.1)
                                                                             (set! rdi g.2)
                                                                             (set! r15 L.rp.5)
                                                                             (jump L.+.3 rbp r15 rdi rsi)))
                                                             (set! rbp (+ rbp 72)))
                                                           (set! tmp.56 rax)
                                                           (begin
                                                             (set! rbp (- rbp 72))
                                                             (return-point L.rp.6
                                                                           (begin
                                                                             (set! rsi tmp.56)
                                                                             (set! rdi f.3)
                                                                             (set! r15 L.rp.6)
                                                                             (jump L.+.3 rbp r15 rdi rsi)))
                                                             (set! rbp (+ rbp 72)))
                                                           (set! tmp.55 rax)
                                                           (begin
                                                             (set! rbp (- rbp 72))
                                                             (return-point L.rp.7
                                                                           (begin
                                                                             (set! rsi tmp.55)
                                                                             (set! rdi e.4)
                                                                             (set! r15 L.rp.7)
                                                                             (jump L.+.3 rbp r15 rdi rsi)))
                                                             (set! rbp (+ rbp 72)))
                                                           (set! tmp.54 rax)
                                                           (begin
                                                             (set! rbp (- rbp 72))
                                                             (return-point L.rp.8
                                                                           (begin
                                                                             (set! rsi tmp.54)
                                                                             (set! rdi d.5)
                                                                             (set! r15 L.rp.8)
                                                                             (jump L.+.3 rbp r15 rdi rsi)))
                                                             (set! rbp (+ rbp 72)))
                                                           (set! tmp.53 rax)
                                                           (begin
                                                             (set! rbp (- rbp 72))
                                                             (return-point L.rp.9
                                                                           (begin
                                                                             (set! rsi tmp.53)
                                                                             (set! rdi c.6)
                                                                             (set! r15 L.rp.9)
                                                                             (jump L.+.3 rbp r15 rdi rsi)))
                                                             (set! rbp (+ rbp 72)))
                                                           (set! tmp.52 rax)
                                                           (begin
                                                             (set! rbp (- rbp 72))
                                                             (return-point L.rp.10
                                                                           (begin
                                                                             (set! rsi tmp.52)
                                                                             (set! rdi b.7)
                                                                             (set! r15 L.rp.10)
                                                                             (jump L.+.3 rbp r15 rdi rsi)))
                                                             (set! rbp (+ rbp 72)))
                                                           (set! tmp.51 rax)
                                                           (set! rsi tmp.51)
                                                           (set! rdi a.8)
                                                           (set! r15 tmp-ra.59)
                                                           (jump L.+.3 rbp r15 rdi rsi)))
                                                       (define L.add-and-multiply.2
                                                         ((locals (sum.18 h.10 g.11 f.12 e.13 d.14 c.15 b.16 a.17))
                                                          (conflicts
                                                           ((tmp-ra.60
                                                             (sum.18
                                                              i.9
                                                              h.10
                                                              g.11
                                                              f.12
                                                              e.13
                                                              d.14
                                                              c.15
                                                              b.16
                                                              a.17
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
                                                            (a.17
                                                             (nfv.61
                                                              nfv.62
                                                              i.9
                                                              h.10
                                                              g.11
                                                              f.12
                                                              e.13
                                                              d.14
                                                              c.15
                                                              b.16
                                                              rbp
                                                              tmp-ra.60
                                                              fv2
                                                              fv1
                                                              fv0
                                                              r9
                                                              r8
                                                              rcx
                                                              rdx
                                                              rsi))
                                                            (b.16
                                                             (nfv.61
                                                              nfv.62
                                                              i.9
                                                              h.10
                                                              g.11
                                                              f.12
                                                              e.13
                                                              d.14
                                                              c.15
                                                              rbp
                                                              tmp-ra.60
                                                              a.17
                                                              fv2
                                                              fv1
                                                              fv0
                                                              r9
                                                              r8
                                                              rcx
                                                              rdx))
                                                            (c.15
                                                             (nfv.61
                                                              nfv.62
                                                              i.9
                                                              h.10
                                                              g.11
                                                              f.12
                                                              e.13
                                                              d.14
                                                              rbp
                                                              tmp-ra.60
                                                              b.16
                                                              a.17
                                                              fv2
                                                              fv1
                                                              fv0
                                                              r9
                                                              r8
                                                              rcx))
                                                            (d.14
                                                             (nfv.61
                                                              nfv.62
                                                              i.9
                                                              h.10
                                                              g.11
                                                              f.12
                                                              e.13
                                                              rbp
                                                              tmp-ra.60
                                                              c.15
                                                              b.16
                                                              a.17
                                                              fv2
                                                              fv1
                                                              fv0
                                                              r9
                                                              r8))
                                                            (e.13
                                                             (nfv.61
                                                              nfv.62
                                                              i.9
                                                              h.10
                                                              g.11
                                                              f.12
                                                              rbp
                                                              tmp-ra.60
                                                              d.14
                                                              c.15
                                                              b.16
                                                              a.17
                                                              fv2
                                                              fv1
                                                              fv0
                                                              r9))
                                                            (f.12
                                                             (nfv.61
                                                              nfv.62
                                                              i.9
                                                              h.10
                                                              g.11
                                                              rbp
                                                              tmp-ra.60
                                                              e.13
                                                              d.14
                                                              c.15
                                                              b.16
                                                              a.17
                                                              fv2
                                                              fv1
                                                              fv0))
                                                            (g.11
                                                             (nfv.62 i.9 h.10 rbp tmp-ra.60 f.12 e.13 d.14 c.15 b.16 a.17 fv2 fv1))
                                                            (h.10 (i.9 rbp tmp-ra.60 g.11 f.12 e.13 d.14 c.15 b.16 a.17 fv2))
                                                            (i.9 (sum.18 rbp tmp-ra.60 h.10 g.11 f.12 e.13 d.14 c.15 b.16 a.17))
                                                            (nfv.61
                                                             (r15 rdi rsi rdx rcx r8 r9 rbp nfv.62 a.17 b.16 c.15 d.14 e.13 f.12))
                                                            (nfv.62
                                                             (r15
                                                              rdi
                                                              rsi
                                                              rdx
                                                              rcx
                                                              r8
                                                              r9
                                                              nfv.61
                                                              rbp
                                                              a.17
                                                              b.16
                                                              c.15
                                                              d.14
                                                              e.13
                                                              f.12
                                                              g.11))
                                                            (sum.18 (rsi rbp tmp-ra.60 i.9))
                                                            (rdi (r15 rbp rsi rdx rcx r8 r9 nfv.61 nfv.62 tmp-ra.60))
                                                            (rsi (sum.18 r15 rdi rbp rdx rcx r8 r9 nfv.61 nfv.62 a.17 tmp-ra.60))
                                                            (rdx (r15 rdi rsi rbp rcx r8 r9 nfv.61 nfv.62 b.16 a.17 tmp-ra.60))
                                                            (rcx (r15 rdi rsi rdx rbp r8 r9 nfv.61 nfv.62 c.15 b.16 a.17 tmp-ra.60))
                                                            (r8
                                                             (r15
                                                              rdi
                                                              rsi
                                                              rdx
                                                              rcx
                                                              rbp
                                                              r9
                                                              nfv.61
                                                              nfv.62
                                                              d.14
                                                              c.15
                                                              b.16
                                                              a.17
                                                              tmp-ra.60))
                                                            (r9
                                                             (r15
                                                              rdi
                                                              rsi
                                                              rdx
                                                              rcx
                                                              r8
                                                              rbp
                                                              nfv.61
                                                              nfv.62
                                                              e.13
                                                              d.14
                                                              c.15
                                                              b.16
                                                              a.17
                                                              tmp-ra.60))
                                                            (fv0 (f.12 e.13 d.14 c.15 b.16 a.17 tmp-ra.60))
                                                            (fv1 (g.11 f.12 e.13 d.14 c.15 b.16 a.17 tmp-ra.60))
                                                            (fv2 (h.10 g.11 f.12 e.13 d.14 c.15 b.16 a.17 tmp-ra.60))
                                                            (rbp
                                                             (sum.18
                                                              r15
                                                              rdi
                                                              rsi
                                                              rdx
                                                              rcx
                                                              r8
                                                              r9
                                                              nfv.61
                                                              nfv.62
                                                              i.9
                                                              h.10
                                                              g.11
                                                              f.12
                                                              e.13
                                                              d.14
                                                              c.15
                                                              b.16
                                                              a.17
                                                              tmp-ra.60))
                                                            (r15 (rbp rdi rsi rdx rcx r8 r9 nfv.61 nfv.62))))
                                                          (assignment ((tmp-ra.60 fv3) (i.9 fv0) (nfv.61 fv4) (nfv.62 fv5))))
                                                         (begin
                                                           (set! tmp-ra.60 r15)
                                                           (set! a.17 rdi)
                                                           (set! b.16 rsi)
                                                           (set! c.15 rdx)
                                                           (set! d.14 rcx)
                                                           (set! e.13 r8)
                                                           (set! f.12 r9)
                                                           (set! g.11 fv0)
                                                           (set! h.10 fv1)
                                                           (set! i.9 fv2)
                                                           (begin
                                                             (set! rbp (- rbp 32))
                                                             (return-point L.rp.11
                                                                           (begin
                                                                             (set! nfv.62 h.10)
                                                                             (set! nfv.61 g.11)
                                                                             (set! r9 f.12)
                                                                             (set! r8 e.13)
                                                                             (set! rcx d.14)
                                                                             (set! rdx c.15)
                                                                             (set! rsi b.16)
                                                                             (set! rdi a.17)
                                                                             (set! r15 L.rp.11)
                                                                             (jump L.add.1 rbp r15 rdi rsi rdx rcx r8 r9 nfv.61 nfv.62)))
                                                             (set! rbp (+ rbp 32)))
                                                           (set! sum.18 rax)
                                                           (set! rsi i.9)
                                                           (set! rdi sum.18)
                                                           (set! r15 tmp-ra.60)
                                                           (jump L.*.4 rbp r15 rdi rsi)))
                                                       (begin
                                                         (set! tmp-ra.63 r15)
                                                         (set! fv2 16)
                                                         (set! fv1 64)
                                                         (set! fv0 56)
                                                         (set! r9 48)
                                                         (set! r8 40)
                                                         (set! rcx 32)
                                                         (set! rdx 24)
                                                         (set! rsi 16)
                                                         (set! rdi 8)
                                                         (set! r15 tmp-ra.63)
                                                         (jump L.add-and-multiply.2 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2)))))
                  (interp-asm-pred-lang-v7/spilled '(module
                                                        ((locals (tmp-ra.63))
                                                         (conflicts
                                                          ((tmp-ra.63 (rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 rbp))
                                                           (rbp (r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.63))
                                                           (fv2 (r15 rdi rsi rdx rcx r8 r9 fv0 fv1 rbp tmp-ra.63))
                                                           (fv1 (r15 rdi rsi rdx rcx r8 r9 fv0 rbp fv2 tmp-ra.63))
                                                           (fv0 (r15 rdi rsi rdx rcx r8 r9 rbp fv1 fv2 tmp-ra.63))
                                                           (r9 (r15 rdi rsi rdx rcx r8 rbp fv0 fv1 fv2 tmp-ra.63))
                                                           (r8 (r15 rdi rsi rdx rcx rbp r9 fv0 fv1 fv2 tmp-ra.63))
                                                           (rcx (r15 rdi rsi rdx rbp r8 r9 fv0 fv1 fv2 tmp-ra.63))
                                                           (rdx (r15 rdi rsi rbp rcx r8 r9 fv0 fv1 fv2 tmp-ra.63))
                                                           (rsi (r15 rdi rbp rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.63))
                                                           (rdi (r15 rbp rsi rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.63))
                                                           (r15 (rbp rdi rsi rdx rcx r8 r9 fv0 fv1 fv2))))
                                                         (assignment ()))
                                                      (define L.*.4
                                                        ((locals (tmp.44 tmp.45 tmp.46 tmp.43 tmp.42 tmp.20 tmp.19 tmp-ra.57))
                                                         (conflicts
                                                          ((tmp.42 (rbp tmp-ra.57 tmp.19 tmp.20))
                                                           (tmp.20 (rbp tmp-ra.57 tmp.19 tmp.43 tmp.42 tmp.45 tmp.44))
                                                           (tmp.43 (rbp tmp-ra.57 tmp.19 tmp.20))
                                                           (tmp-ra.57
                                                            (tmp.20 tmp.19 rbp rsi rdi tmp.43 tmp.42 tmp.45 tmp.44 tmp.46 rax))
                                                           (tmp.46 (rax rbp tmp-ra.57 tmp.19))
                                                           (tmp.45 (rbp tmp-ra.57 tmp.19 tmp.20))
                                                           (tmp.19 (tmp.20 rbp tmp-ra.57 rsi tmp.43 tmp.42 tmp.45 tmp.44 tmp.46))
                                                           (tmp.44 (rbp tmp-ra.57 tmp.19 tmp.20))
                                                           (rax (tmp.46 rbp tmp-ra.57))
                                                           (rbp (tmp.20 tmp.19 tmp-ra.57 tmp.43 tmp.42 tmp.45 tmp.44 tmp.46 rax))
                                                           (rdi (tmp-ra.57))
                                                           (rsi (tmp.19 tmp-ra.57))))
                                                         (assignment ()))
                                                        (begin
                                                          (set! tmp-ra.57 r15)
                                                          (set! tmp.19 rdi)
                                                          (set! tmp.20 rsi)
                                                          (if (begin
                                                                (if (begin
                                                                      (begin
                                                                        (set! tmp.43 tmp.20)
                                                                        (set! tmp.43 (bitwise-and tmp.43 7)))
                                                                      (= tmp.43 0))
                                                                    (set! tmp.42 14)
                                                                    (set! tmp.42 6))
                                                                (!= tmp.42 6))
                                                              (if (begin
                                                                    (if (begin
                                                                          (begin
                                                                            (set! tmp.45 tmp.19)
                                                                            (set! tmp.45 (bitwise-and tmp.45 7)))
                                                                          (= tmp.45 0))
                                                                        (set! tmp.44 14)
                                                                        (set! tmp.44 6))
                                                                    (!= tmp.44 6))
                                                                  (begin
                                                                    (set! tmp.46 tmp.20)
                                                                    (set! tmp.46 (arithmetic-shift-right tmp.46 3))
                                                                    (set! rax tmp.19)
                                                                    (set! rax (* rax tmp.46))
                                                                    (jump tmp-ra.57 rbp rax))
                                                                  (begin (set! rax 318) (jump tmp-ra.57 rbp rax)))
                                                              (begin (set! rax 318) (jump tmp-ra.57 rbp rax)))))
                                                      (define L.+.3
                                                        ((locals (tmp.49 tmp.50 tmp.48 tmp.47 tmp.21 tmp.22 tmp-ra.58))
                                                         (conflicts
                                                          ((tmp.47 (rbp tmp-ra.58 tmp.22 tmp.21))
                                                           (tmp.22 (rbp tmp-ra.58 tmp.21 tmp.48 tmp.47 tmp.50 tmp.49 rax))
                                                           (tmp.48 (rbp tmp-ra.58 tmp.22 tmp.21))
                                                           (tmp-ra.58 (tmp.22 tmp.21 rbp rsi rdi tmp.48 tmp.47 tmp.50 tmp.49 rax))
                                                           (tmp.50 (rbp tmp-ra.58 tmp.22 tmp.21))
                                                           (tmp.21 (tmp.22 rbp tmp-ra.58 rsi tmp.48 tmp.47 tmp.50 tmp.49))
                                                           (tmp.49 (rbp tmp-ra.58 tmp.22 tmp.21))
                                                           (rax (tmp.22 rbp tmp-ra.58))
                                                           (rbp (tmp.22 tmp.21 tmp-ra.58 tmp.48 tmp.47 tmp.50 tmp.49 rax))
                                                           (rdi (tmp-ra.58))
                                                           (rsi (tmp.21 tmp-ra.58))))
                                                         (assignment ()))
                                                        (begin
                                                          (set! tmp-ra.58 r15)
                                                          (set! tmp.21 rdi)
                                                          (set! tmp.22 rsi)
                                                          (if (begin
                                                                (if (begin
                                                                      (begin
                                                                        (set! tmp.48 tmp.22)
                                                                        (set! tmp.48 (bitwise-and tmp.48 7)))
                                                                      (= tmp.48 0))
                                                                    (set! tmp.47 14)
                                                                    (set! tmp.47 6))
                                                                (!= tmp.47 6))
                                                              (if (begin
                                                                    (if (begin
                                                                          (begin
                                                                            (set! tmp.50 tmp.21)
                                                                            (set! tmp.50 (bitwise-and tmp.50 7)))
                                                                          (= tmp.50 0))
                                                                        (set! tmp.49 14)
                                                                        (set! tmp.49 6))
                                                                    (!= tmp.49 6))
                                                                  (begin
                                                                    (set! rax tmp.21)
                                                                    (set! rax (+ rax tmp.22))
                                                                    (jump tmp-ra.58 rbp rax))
                                                                  (begin (set! rax 574) (jump tmp-ra.58 rbp rax)))
                                                              (begin (set! rax 574) (jump tmp-ra.58 rbp rax)))))
                                                      (define L.add.1
                                                        ((locals (tmp.51 tmp.52 tmp.53 tmp.54 tmp.55 tmp.56 h.1 g.2))
                                                         (conflicts
                                                          ((tmp-ra.59
                                                            (tmp.51
                                                             tmp.52
                                                             tmp.53
                                                             tmp.54
                                                             tmp.55
                                                             tmp.56
                                                             h.1
                                                             g.2
                                                             f.3
                                                             e.4
                                                             d.5
                                                             c.6
                                                             b.7
                                                             a.8
                                                             rbp
                                                             fv1
                                                             fv0
                                                             r9
                                                             r8
                                                             rcx
                                                             rdx
                                                             rsi
                                                             rdi))
                                                           (a.8
                                                            (tmp.51
                                                             tmp.52
                                                             tmp.53
                                                             tmp.54
                                                             tmp.55
                                                             tmp.56
                                                             h.1
                                                             g.2
                                                             f.3
                                                             e.4
                                                             d.5
                                                             c.6
                                                             b.7
                                                             rbp
                                                             tmp-ra.59
                                                             fv1
                                                             fv0
                                                             r9
                                                             r8
                                                             rcx
                                                             rdx
                                                             rsi))
                                                           (b.7
                                                            (rsi
                                                             tmp.52
                                                             tmp.53
                                                             tmp.54
                                                             tmp.55
                                                             tmp.56
                                                             h.1
                                                             g.2
                                                             f.3
                                                             e.4
                                                             d.5
                                                             c.6
                                                             rbp
                                                             tmp-ra.59
                                                             a.8
                                                             fv1
                                                             fv0
                                                             r9
                                                             r8
                                                             rcx
                                                             rdx))
                                                           (c.6
                                                            (rsi
                                                             tmp.53
                                                             tmp.54
                                                             tmp.55
                                                             tmp.56
                                                             h.1
                                                             g.2
                                                             f.3
                                                             e.4
                                                             d.5
                                                             rbp
                                                             tmp-ra.59
                                                             a.8
                                                             b.7
                                                             fv1
                                                             fv0
                                                             r9
                                                             r8
                                                             rcx))
                                                           (d.5
                                                            (rsi
                                                             tmp.54
                                                             tmp.55
                                                             tmp.56
                                                             h.1
                                                             g.2
                                                             f.3
                                                             e.4
                                                             rbp
                                                             tmp-ra.59
                                                             a.8
                                                             b.7
                                                             c.6
                                                             fv1
                                                             fv0
                                                             r9
                                                             r8))
                                                           (e.4
                                                            (rsi
                                                             tmp.55
                                                             tmp.56
                                                             h.1
                                                             g.2
                                                             f.3
                                                             rbp
                                                             tmp-ra.59
                                                             a.8
                                                             b.7
                                                             c.6
                                                             d.5
                                                             fv1
                                                             fv0
                                                             r9))
                                                           (f.3 (rsi tmp.56 h.1 g.2 rbp tmp-ra.59 a.8 b.7 c.6 d.5 e.4 fv1 fv0))
                                                           (g.2 (rsi h.1 rbp tmp-ra.59 a.8 b.7 c.6 d.5 e.4 f.3 fv1))
                                                           (h.1 (rbp tmp-ra.59 a.8 b.7 c.6 d.5 e.4 f.3 g.2))
                                                           (tmp.56 (rbp tmp-ra.59 a.8 b.7 c.6 d.5 e.4 f.3))
                                                           (tmp.55 (rbp tmp-ra.59 a.8 b.7 c.6 d.5 e.4))
                                                           (tmp.54 (rbp tmp-ra.59 a.8 b.7 c.6 d.5))
                                                           (tmp.53 (rbp tmp-ra.59 a.8 b.7 c.6))
                                                           (tmp.52 (rbp tmp-ra.59 a.8 b.7))
                                                           (tmp.51 (rbp tmp-ra.59 a.8))
                                                           (rdi (r15 rbp rsi tmp-ra.59))
                                                           (rsi (b.7 c.6 d.5 e.4 f.3 r15 rdi rbp g.2 a.8 tmp-ra.59))
                                                           (rdx (b.7 a.8 tmp-ra.59))
                                                           (rcx (c.6 b.7 a.8 tmp-ra.59))
                                                           (r8 (d.5 c.6 b.7 a.8 tmp-ra.59))
                                                           (r9 (e.4 d.5 c.6 b.7 a.8 tmp-ra.59))
                                                           (fv0 (f.3 e.4 d.5 c.6 b.7 a.8 tmp-ra.59))
                                                           (fv1 (g.2 f.3 e.4 d.5 c.6 b.7 a.8 tmp-ra.59))
                                                           (rbp
                                                            (tmp.51
                                                             tmp.52
                                                             tmp.53
                                                             tmp.54
                                                             tmp.55
                                                             tmp.56
                                                             r15
                                                             rdi
                                                             rsi
                                                             h.1
                                                             g.2
                                                             f.3
                                                             e.4
                                                             d.5
                                                             c.6
                                                             b.7
                                                             a.8
                                                             tmp-ra.59))
                                                           (r15 (rbp rdi rsi))))
                                                         (assignment
                                                          ((tmp-ra.59 fv2)
                                                           (a.8 fv3)
                                                           (b.7 fv4)
                                                           (c.6 fv5)
                                                           (d.5 fv6)
                                                           (e.4 fv7)
                                                           (f.3 fv8))))
                                                        (begin
                                                          (set! tmp-ra.59 r15)
                                                          (set! a.8 rdi)
                                                          (set! b.7 rsi)
                                                          (set! c.6 rdx)
                                                          (set! d.5 rcx)
                                                          (set! e.4 r8)
                                                          (set! f.3 r9)
                                                          (set! g.2 fv0)
                                                          (set! h.1 fv1)
                                                          (begin
                                                            (set! rbp (- rbp 72))
                                                            (return-point L.rp.5
                                                                          (begin
                                                                            (set! rsi h.1)
                                                                            (set! rdi g.2)
                                                                            (set! r15 L.rp.5)
                                                                            (jump L.+.3 rbp r15 rdi rsi)))
                                                            (set! rbp (+ rbp 72)))
                                                          (set! tmp.56 rax)
                                                          (begin
                                                            (set! rbp (- rbp 72))
                                                            (return-point L.rp.6
                                                                          (begin
                                                                            (set! rsi tmp.56)
                                                                            (set! rdi f.3)
                                                                            (set! r15 L.rp.6)
                                                                            (jump L.+.3 rbp r15 rdi rsi)))
                                                            (set! rbp (+ rbp 72)))
                                                          (set! tmp.55 rax)
                                                          (begin
                                                            (set! rbp (- rbp 72))
                                                            (return-point L.rp.7
                                                                          (begin
                                                                            (set! rsi tmp.55)
                                                                            (set! rdi e.4)
                                                                            (set! r15 L.rp.7)
                                                                            (jump L.+.3 rbp r15 rdi rsi)))
                                                            (set! rbp (+ rbp 72)))
                                                          (set! tmp.54 rax)
                                                          (begin
                                                            (set! rbp (- rbp 72))
                                                            (return-point L.rp.8
                                                                          (begin
                                                                            (set! rsi tmp.54)
                                                                            (set! rdi d.5)
                                                                            (set! r15 L.rp.8)
                                                                            (jump L.+.3 rbp r15 rdi rsi)))
                                                            (set! rbp (+ rbp 72)))
                                                          (set! tmp.53 rax)
                                                          (begin
                                                            (set! rbp (- rbp 72))
                                                            (return-point L.rp.9
                                                                          (begin
                                                                            (set! rsi tmp.53)
                                                                            (set! rdi c.6)
                                                                            (set! r15 L.rp.9)
                                                                            (jump L.+.3 rbp r15 rdi rsi)))
                                                            (set! rbp (+ rbp 72)))
                                                          (set! tmp.52 rax)
                                                          (begin
                                                            (set! rbp (- rbp 72))
                                                            (return-point L.rp.10
                                                                          (begin
                                                                            (set! rsi tmp.52)
                                                                            (set! rdi b.7)
                                                                            (set! r15 L.rp.10)
                                                                            (jump L.+.3 rbp r15 rdi rsi)))
                                                            (set! rbp (+ rbp 72)))
                                                          (set! tmp.51 rax)
                                                          (set! rsi tmp.51)
                                                          (set! rdi a.8)
                                                          (set! r15 tmp-ra.59)
                                                          (jump L.+.3 rbp r15 rdi rsi)))
                                                      (define L.add-and-multiply.2
                                                        ((locals (sum.18 h.10 g.11 f.12 e.13 d.14 c.15 b.16 a.17))
                                                         (conflicts
                                                          ((tmp-ra.60
                                                            (sum.18
                                                             i.9
                                                             h.10
                                                             g.11
                                                             f.12
                                                             e.13
                                                             d.14
                                                             c.15
                                                             b.16
                                                             a.17
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
                                                           (a.17
                                                            (nfv.61
                                                             nfv.62
                                                             i.9
                                                             h.10
                                                             g.11
                                                             f.12
                                                             e.13
                                                             d.14
                                                             c.15
                                                             b.16
                                                             rbp
                                                             tmp-ra.60
                                                             fv2
                                                             fv1
                                                             fv0
                                                             r9
                                                             r8
                                                             rcx
                                                             rdx
                                                             rsi))
                                                           (b.16
                                                            (nfv.61
                                                             nfv.62
                                                             i.9
                                                             h.10
                                                             g.11
                                                             f.12
                                                             e.13
                                                             d.14
                                                             c.15
                                                             rbp
                                                             tmp-ra.60
                                                             a.17
                                                             fv2
                                                             fv1
                                                             fv0
                                                             r9
                                                             r8
                                                             rcx
                                                             rdx))
                                                           (c.15
                                                            (nfv.61
                                                             nfv.62
                                                             i.9
                                                             h.10
                                                             g.11
                                                             f.12
                                                             e.13
                                                             d.14
                                                             rbp
                                                             tmp-ra.60
                                                             b.16
                                                             a.17
                                                             fv2
                                                             fv1
                                                             fv0
                                                             r9
                                                             r8
                                                             rcx))
                                                           (d.14
                                                            (nfv.61
                                                             nfv.62
                                                             i.9
                                                             h.10
                                                             g.11
                                                             f.12
                                                             e.13
                                                             rbp
                                                             tmp-ra.60
                                                             c.15
                                                             b.16
                                                             a.17
                                                             fv2
                                                             fv1
                                                             fv0
                                                             r9
                                                             r8))
                                                           (e.13
                                                            (nfv.61
                                                             nfv.62
                                                             i.9
                                                             h.10
                                                             g.11
                                                             f.12
                                                             rbp
                                                             tmp-ra.60
                                                             d.14
                                                             c.15
                                                             b.16
                                                             a.17
                                                             fv2
                                                             fv1
                                                             fv0
                                                             r9))
                                                           (f.12
                                                            (nfv.61
                                                             nfv.62
                                                             i.9
                                                             h.10
                                                             g.11
                                                             rbp
                                                             tmp-ra.60
                                                             e.13
                                                             d.14
                                                             c.15
                                                             b.16
                                                             a.17
                                                             fv2
                                                             fv1
                                                             fv0))
                                                           (g.11
                                                            (nfv.62 i.9 h.10 rbp tmp-ra.60 f.12 e.13 d.14 c.15 b.16 a.17 fv2 fv1))
                                                           (h.10 (i.9 rbp tmp-ra.60 g.11 f.12 e.13 d.14 c.15 b.16 a.17 fv2))
                                                           (i.9 (sum.18 rbp tmp-ra.60 h.10 g.11 f.12 e.13 d.14 c.15 b.16 a.17))
                                                           (nfv.61
                                                            (r15 rdi rsi rdx rcx r8 r9 rbp nfv.62 a.17 b.16 c.15 d.14 e.13 f.12))
                                                           (nfv.62
                                                            (r15
                                                             rdi
                                                             rsi
                                                             rdx
                                                             rcx
                                                             r8
                                                             r9
                                                             nfv.61
                                                             rbp
                                                             a.17
                                                             b.16
                                                             c.15
                                                             d.14
                                                             e.13
                                                             f.12
                                                             g.11))
                                                           (sum.18 (rsi rbp tmp-ra.60 i.9))
                                                           (rdi (r15 rbp rsi rdx rcx r8 r9 nfv.61 nfv.62 tmp-ra.60))
                                                           (rsi (sum.18 r15 rdi rbp rdx rcx r8 r9 nfv.61 nfv.62 a.17 tmp-ra.60))
                                                           (rdx (r15 rdi rsi rbp rcx r8 r9 nfv.61 nfv.62 b.16 a.17 tmp-ra.60))
                                                           (rcx (r15 rdi rsi rdx rbp r8 r9 nfv.61 nfv.62 c.15 b.16 a.17 tmp-ra.60))
                                                           (r8
                                                            (r15
                                                             rdi
                                                             rsi
                                                             rdx
                                                             rcx
                                                             rbp
                                                             r9
                                                             nfv.61
                                                             nfv.62
                                                             d.14
                                                             c.15
                                                             b.16
                                                             a.17
                                                             tmp-ra.60))
                                                           (r9
                                                            (r15
                                                             rdi
                                                             rsi
                                                             rdx
                                                             rcx
                                                             r8
                                                             rbp
                                                             nfv.61
                                                             nfv.62
                                                             e.13
                                                             d.14
                                                             c.15
                                                             b.16
                                                             a.17
                                                             tmp-ra.60))
                                                           (fv0 (f.12 e.13 d.14 c.15 b.16 a.17 tmp-ra.60))
                                                           (fv1 (g.11 f.12 e.13 d.14 c.15 b.16 a.17 tmp-ra.60))
                                                           (fv2 (h.10 g.11 f.12 e.13 d.14 c.15 b.16 a.17 tmp-ra.60))
                                                           (rbp
                                                            (sum.18
                                                             r15
                                                             rdi
                                                             rsi
                                                             rdx
                                                             rcx
                                                             r8
                                                             r9
                                                             nfv.61
                                                             nfv.62
                                                             i.9
                                                             h.10
                                                             g.11
                                                             f.12
                                                             e.13
                                                             d.14
                                                             c.15
                                                             b.16
                                                             a.17
                                                             tmp-ra.60))
                                                           (r15 (rbp rdi rsi rdx rcx r8 r9 nfv.61 nfv.62))))
                                                         (assignment ((tmp-ra.60 fv3) (i.9 fv0) (nfv.61 fv4) (nfv.62 fv5))))
                                                        (begin
                                                          (set! tmp-ra.60 r15)
                                                          (set! a.17 rdi)
                                                          (set! b.16 rsi)
                                                          (set! c.15 rdx)
                                                          (set! d.14 rcx)
                                                          (set! e.13 r8)
                                                          (set! f.12 r9)
                                                          (set! g.11 fv0)
                                                          (set! h.10 fv1)
                                                          (set! i.9 fv2)
                                                          (begin
                                                            (set! rbp (- rbp 32))
                                                            (return-point L.rp.11
                                                                          (begin
                                                                            (set! nfv.62 h.10)
                                                                            (set! nfv.61 g.11)
                                                                            (set! r9 f.12)
                                                                            (set! r8 e.13)
                                                                            (set! rcx d.14)
                                                                            (set! rdx c.15)
                                                                            (set! rsi b.16)
                                                                            (set! rdi a.17)
                                                                            (set! r15 L.rp.11)
                                                                            (jump L.add.1 rbp r15 rdi rsi rdx rcx r8 r9 nfv.61 nfv.62)))
                                                            (set! rbp (+ rbp 32)))
                                                          (set! sum.18 rax)
                                                          (set! rsi i.9)
                                                          (set! rdi sum.18)
                                                          (set! r15 tmp-ra.60)
                                                          (jump L.*.4 rbp r15 rdi rsi)))
                                                      (begin
                                                        (set! tmp-ra.63 r15)
                                                        (set! fv2 16)
                                                        (set! fv1 64)
                                                        (set! fv0 56)
                                                        (set! r9 48)
                                                        (set! r8 40)
                                                        (set! rcx 32)
                                                        (set! rdx 24)
                                                        (set! rsi 16)
                                                        (set! rdi 8)
                                                        (set! r15 tmp-ra.63)
                                                        (jump L.add-and-multiply.2 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2))))))
  (check-equal? (assign-registers '(module
                                          ((locals (tmp-ra.45))
                                           (conflicts
                                            ((tmp-ra.45 (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp))
                                             (rbp (r15 fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi tmp-ra.45))
                                             (rdi (r15 fv2 fv1 fv0 r9 r8 rcx rdx rsi tmp-ra.45 rbp))
                                             (rsi (r15 fv2 fv1 fv0 r9 r8 rcx rdx tmp-ra.45 rdi rbp))
                                             (rdx (r15 fv2 fv1 fv0 r9 r8 rcx tmp-ra.45 rsi rdi rbp))
                                             (rcx (r15 fv2 fv1 fv0 r9 r8 tmp-ra.45 rdx rsi rdi rbp))
                                             (r8 (r15 fv2 fv1 fv0 r9 tmp-ra.45 rcx rdx rsi rdi rbp))
                                             (r9 (r15 fv2 fv1 fv0 tmp-ra.45 r8 rcx rdx rsi rdi rbp))
                                             (fv0 (r15 fv2 fv1 tmp-ra.45 r9 r8 rcx rdx rsi rdi rbp))
                                             (fv1 (r15 fv2 tmp-ra.45 fv0 r9 r8 rcx rdx rsi rdi rbp))
                                             (fv2 (r15 tmp-ra.45 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp))
                                             (r15 (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp))))
                                           (assignment ()))
                                        (define L.*.2
                                          ((locals (tmp.24 tmp.27 tmp.2 tmp.25 tmp-ra.39 tmp.23 tmp.1 tmp.26))
                                           (conflicts
                                            ((tmp.26 (tmp.1 tmp.2 rbp tmp-ra.39))
                                             (tmp.1 (tmp.27 tmp.25 tmp.26 tmp.23 tmp.24 tmp.2 rsi rbp tmp-ra.39))
                                             (tmp.23 (tmp.2 tmp.1 rbp tmp-ra.39))
                                             (tmp-ra.39
                                              (rax tmp.27 tmp.25 tmp.26 tmp.23 tmp.24 tmp.2 tmp.1 rdi rsi rbp))
                                             (tmp.25 (tmp.2 tmp.1 rbp tmp-ra.39))
                                             (tmp.2 (tmp.25 tmp.26 tmp.23 tmp.24 tmp.1 rbp tmp-ra.39))
                                             (tmp.27 (rax tmp.1 rbp tmp-ra.39))
                                             (tmp.24 (tmp.2 tmp.1 rbp tmp-ra.39))
                                             (rbp (rax tmp.27 tmp.25 tmp.26 tmp.23 tmp.24 tmp.2 tmp.1 tmp-ra.39))
                                             (rsi (tmp.1 tmp-ra.39))
                                             (rdi (tmp-ra.39))
                                             (rax (tmp.27 rbp tmp-ra.39))))
                                           (assignment ()))
                                          (begin
                                            (set! tmp-ra.39 r15)
                                            (set! tmp.1 rdi)
                                            (set! tmp.2 rsi)
                                            (if (begin
                                                  (if (begin
                                                        (begin
                                                          (set! tmp.24 tmp.2)
                                                          (set! tmp.24 (bitwise-and tmp.24 7)))
                                                        (= tmp.24 0))
                                                      (set! tmp.23 14)
                                                      (set! tmp.23 6))
                                                  (!= tmp.23 6))
                                                (if (begin
                                                      (if (begin
                                                            (begin
                                                              (set! tmp.26 tmp.1)
                                                              (set! tmp.26 (bitwise-and tmp.26 7)))
                                                            (= tmp.26 0))
                                                          (set! tmp.25 14)
                                                          (set! tmp.25 6))
                                                      (!= tmp.25 6))
                                                    (begin
                                                      (set! tmp.27 tmp.2)
                                                      (set! tmp.27 (arithmetic-shift-right tmp.27 3))
                                                      (set! rax tmp.1)
                                                      (set! rax (* rax tmp.27))
                                                      (jump tmp-ra.39 rbp rax))
                                                    (begin (set! rax 318) (jump tmp-ra.39 rbp rax)))
                                                (begin (set! rax 318) (jump tmp-ra.39 rbp rax)))))
                                        (define L.+.1
                                          ((locals (tmp.4 tmp.31 tmp-ra.40 tmp.3 tmp.28 tmp.30 tmp.29))
                                           (conflicts
                                            ((tmp.29 (tmp.4 tmp.3 rbp tmp-ra.40))
                                             (tmp.30 (tmp.3 tmp.4 rbp tmp-ra.40))
                                             (tmp.28 (tmp.3 tmp.4 rbp tmp-ra.40))
                                             (tmp.3 (tmp.30 tmp.31 tmp.28 tmp.29 tmp.4 rsi rbp tmp-ra.40))
                                             (tmp-ra.40 (rax tmp.30 tmp.31 tmp.28 tmp.29 tmp.4 tmp.3 rdi rsi rbp))
                                             (tmp.31 (tmp.3 tmp.4 rbp tmp-ra.40))
                                             (tmp.4 (rax tmp.30 tmp.31 tmp.28 tmp.29 tmp.3 rbp tmp-ra.40))
                                             (rbp (rax tmp.30 tmp.31 tmp.28 tmp.29 tmp.4 tmp.3 tmp-ra.40))
                                             (rsi (tmp.3 tmp-ra.40))
                                             (rdi (tmp-ra.40))
                                             (rax (tmp.4 rbp tmp-ra.40))))
                                           (assignment ()))
                                          (begin
                                            (set! tmp-ra.40 r15)
                                            (set! tmp.3 rdi)
                                            (set! tmp.4 rsi)
                                            (if (begin
                                                  (if (begin
                                                        (begin
                                                          (set! tmp.29 tmp.4)
                                                          (set! tmp.29 (bitwise-and tmp.29 7)))
                                                        (= tmp.29 0))
                                                      (set! tmp.28 14)
                                                      (set! tmp.28 6))
                                                  (!= tmp.28 6))
                                                (if (begin
                                                      (if (begin
                                                            (begin
                                                              (set! tmp.31 tmp.3)
                                                              (set! tmp.31 (bitwise-and tmp.31 7)))
                                                            (= tmp.31 0))
                                                          (set! tmp.30 14)
                                                          (set! tmp.30 6))
                                                      (!= tmp.30 6))
                                                    (begin
                                                      (set! rax tmp.3)
                                                      (set! rax (+ rax tmp.4))
                                                      (jump tmp-ra.40 rbp rax))
                                                    (begin (set! rax 574) (jump tmp-ra.40 rbp rax)))
                                                (begin (set! rax 574) (jump tmp-ra.40 rbp rax)))))
                                        (define L.add.10
                                          ((locals (tmp.34 tmp.37 g.67 tmp.35 tmp.36 tmp.32 h.68 tmp.33))
                                           (conflicts
                                            ((tmp.33 (rdi tmp-ra.41 a.61 b.62 rbp))
                                             (tmp-ra.41
                                              (tmp.32
                                               tmp.33
                                               tmp.34
                                               tmp.35
                                               tmp.36
                                               tmp.37
                                               h.68
                                               g.67
                                               f.66
                                               e.65
                                               d.64
                                               c.63
                                               b.62
                                               a.61
                                               rdi
                                               rsi
                                               rdx
                                               rcx
                                               r8
                                               r9
                                               fv0
                                               fv1
                                               rbp))
                                             (b.62
                                              (tmp.33
                                               tmp.34
                                               tmp.35
                                               tmp.36
                                               tmp.37
                                               h.68
                                               g.67
                                               f.66
                                               e.65
                                               d.64
                                               c.63
                                               rdx
                                               rcx
                                               r8
                                               r9
                                               fv0
                                               fv1
                                               a.61
                                               tmp-ra.41
                                               rbp))
                                             (d.64
                                              (tmp.35
                                               tmp.36
                                               tmp.37
                                               h.68
                                               g.67
                                               f.66
                                               e.65
                                               r8
                                               r9
                                               fv0
                                               fv1
                                               b.62
                                               a.61
                                               tmp-ra.41
                                               c.63
                                               rbp))
                                             (f.66
                                              (tmp.37 h.68 g.67 fv0 fv1 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp))
                                             (h.68 (rdi f.66 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 g.67 rbp))
                                             (tmp.32 (rdi a.61 tmp-ra.41 rbp))
                                             (a.61
                                              (tmp.32
                                               tmp.33
                                               tmp.34
                                               tmp.35
                                               tmp.36
                                               tmp.37
                                               h.68
                                               g.67
                                               f.66
                                               e.65
                                               d.64
                                               c.63
                                               b.62
                                               rsi
                                               rdx
                                               rcx
                                               r8
                                               r9
                                               fv0
                                               fv1
                                               tmp-ra.41
                                               rbp))
                                             (tmp.36 (rdi d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp))
                                             (tmp.35 (rdi c.63 tmp-ra.41 a.61 b.62 d.64 rbp))
                                             (g.67 (h.68 fv1 f.66 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp))
                                             (tmp.37 (rdi e.65 c.63 tmp-ra.41 a.61 b.62 d.64 f.66 rbp))
                                             (tmp.34 (rdi b.62 a.61 tmp-ra.41 c.63 rbp))
                                             (c.63
                                              (tmp.34
                                               tmp.35
                                               tmp.36
                                               tmp.37
                                               h.68
                                               g.67
                                               f.66
                                               e.65
                                               d.64
                                               rcx
                                               r8
                                               r9
                                               fv0
                                               fv1
                                               b.62
                                               a.61
                                               tmp-ra.41
                                               rbp))
                                             (e.65
                                              (tmp.36
                                               tmp.37
                                               h.68
                                               g.67
                                               f.66
                                               r9
                                               fv0
                                               fv1
                                               d.64
                                               b.62
                                               a.61
                                               tmp-ra.41
                                               c.63
                                               rbp))
                                             (rbp
                                              (tmp.32
                                               tmp.33
                                               tmp.34
                                               tmp.35
                                               tmp.36
                                               tmp.37
                                               r15
                                               rsi
                                               rdi
                                               h.68
                                               g.67
                                               f.66
                                               e.65
                                               d.64
                                               c.63
                                               b.62
                                               a.61
                                               tmp-ra.41))
                                             (fv1 (g.67 f.66 e.65 d.64 c.63 b.62 a.61 tmp-ra.41))
                                             (fv0 (f.66 e.65 d.64 c.63 b.62 a.61 tmp-ra.41))
                                             (r9 (e.65 d.64 c.63 b.62 a.61 tmp-ra.41))
                                             (r8 (d.64 c.63 b.62 a.61 tmp-ra.41))
                                             (rcx (c.63 b.62 a.61 tmp-ra.41))
                                             (rdx (b.62 a.61 tmp-ra.41))
                                             (rsi (r15 rdi rbp a.61 tmp-ra.41))
                                             (rdi
                                              (tmp.32
                                               tmp.33
                                               tmp.34
                                               tmp.35
                                               tmp.36
                                               tmp.37
                                               r15
                                               rsi
                                               h.68
                                               rbp
                                               tmp-ra.41))
                                             (r15 (rsi rdi rbp))))
                                           (assignment
                                            ((f.66 fv8)
                                             (e.65 fv7)
                                             (d.64 fv6)
                                             (c.63 fv5)
                                             (b.62 fv4)
                                             (tmp-ra.41 fv3)
                                             (a.61 fv2))))
                                          (begin
                                            (set! tmp-ra.41 r15)
                                            (set! a.61 rdi)
                                            (set! b.62 rsi)
                                            (set! c.63 rdx)
                                            (set! d.64 rcx)
                                            (set! e.65 r8)
                                            (set! f.66 r9)
                                            (set! g.67 fv0)
                                            (set! h.68 fv1)
                                            (begin
                                              (set! rbp (- rbp 72))
                                              (return-point L.rp.12
                                                            (begin
                                                              (set! rdi g.67)
                                                              (set! rsi h.68)
                                                              (set! r15 L.rp.12)
                                                              (jump L.+.1 rbp r15 rdi rsi)))
                                              (set! rbp (+ rbp 72)))
                                            (set! tmp.37 rax)
                                            (begin
                                              (set! rbp (- rbp 72))
                                              (return-point L.rp.13
                                                            (begin
                                                              (set! rdi f.66)
                                                              (set! rsi tmp.37)
                                                              (set! r15 L.rp.13)
                                                              (jump L.+.1 rbp r15 rdi rsi)))
                                              (set! rbp (+ rbp 72)))
                                            (set! tmp.36 rax)
                                            (begin
                                              (set! rbp (- rbp 72))
                                              (return-point L.rp.14
                                                            (begin
                                                              (set! rdi e.65)
                                                              (set! rsi tmp.36)
                                                              (set! r15 L.rp.14)
                                                              (jump L.+.1 rbp r15 rdi rsi)))
                                              (set! rbp (+ rbp 72)))
                                            (set! tmp.35 rax)
                                            (begin
                                              (set! rbp (- rbp 72))
                                              (return-point L.rp.15
                                                            (begin
                                                              (set! rdi d.64)
                                                              (set! rsi tmp.35)
                                                              (set! r15 L.rp.15)
                                                              (jump L.+.1 rbp r15 rdi rsi)))
                                              (set! rbp (+ rbp 72)))
                                            (set! tmp.34 rax)
                                            (begin
                                              (set! rbp (- rbp 72))
                                              (return-point L.rp.16
                                                            (begin
                                                              (set! rdi c.63)
                                                              (set! rsi tmp.34)
                                                              (set! r15 L.rp.16)
                                                              (jump L.+.1 rbp r15 rdi rsi)))
                                              (set! rbp (+ rbp 72)))
                                            (set! tmp.33 rax)
                                            (begin
                                              (set! rbp (- rbp 72))
                                              (return-point L.rp.17
                                                            (begin
                                                              (set! rdi b.62)
                                                              (set! rsi tmp.33)
                                                              (set! r15 L.rp.17)
                                                              (jump L.+.1 rbp r15 rdi rsi)))
                                              (set! rbp (+ rbp 72)))
                                            (set! tmp.32 rax)
                                            (set! rdi a.61)
                                            (set! rsi tmp.32)
                                            (set! r15 tmp-ra.41)
                                            (jump L.+.1 rbp r15 rdi rsi)))
                                        (define L.add-and-multiply.11
                                          ((locals (a.69 g.75 sum.78 h.76 c.71 d.72 b.70 f.74 e.73))
                                           (conflicts
                                            ((nfv.43 (r15 nfv.44 h.76 r9 r8 rcx rdx rsi rdi rbp))
                                             (e.73
                                              (rcx
                                               rdx
                                               rsi
                                               rdi
                                               i.77
                                               h.76
                                               g.75
                                               f.74
                                               r9
                                               fv0
                                               fv1
                                               fv2
                                               tmp-ra.42
                                               a.69
                                               b.70
                                               c.71
                                               d.72
                                               rbp))
                                             (f.74
                                              (r8
                                               rcx
                                               rdx
                                               rsi
                                               rdi
                                               i.77
                                               h.76
                                               g.75
                                               fv0
                                               fv1
                                               fv2
                                               tmp-ra.42
                                               a.69
                                               b.70
                                               c.71
                                               d.72
                                               e.73
                                               rbp))
                                             (b.70
                                              (rdi
                                               i.77
                                               h.76
                                               g.75
                                               f.74
                                               e.73
                                               d.72
                                               c.71
                                               rdx
                                               rcx
                                               r8
                                               r9
                                               fv0
                                               fv1
                                               fv2
                                               tmp-ra.42
                                               a.69
                                               rbp))
                                             (d.72
                                              (rdx
                                               rsi
                                               rdi
                                               i.77
                                               h.76
                                               g.75
                                               f.74
                                               e.73
                                               r8
                                               r9
                                               fv0
                                               fv1
                                               fv2
                                               tmp-ra.42
                                               a.69
                                               b.70
                                               c.71
                                               rbp))
                                             (nfv.44 (r15 nfv.43 r9 r8 rcx rdx rsi rdi rbp))
                                             (c.71
                                              (rsi
                                               rdi
                                               i.77
                                               h.76
                                               g.75
                                               f.74
                                               e.73
                                               d.72
                                               rcx
                                               r8
                                               r9
                                               fv0
                                               fv1
                                               fv2
                                               tmp-ra.42
                                               a.69
                                               b.70
                                               rbp))
                                             (tmp-ra.42
                                              (sum.78
                                               i.77
                                               h.76
                                               g.75
                                               f.74
                                               e.73
                                               d.72
                                               c.71
                                               b.70
                                               a.69
                                               rdi
                                               rsi
                                               rdx
                                               rcx
                                               r8
                                               r9
                                               fv0
                                               fv1
                                               fv2
                                               rbp))
                                             (i.77
                                              (rdi sum.78 tmp-ra.42 a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76 rbp))
                                             (h.76
                                              (nfv.43
                                               r9
                                               r8
                                               rcx
                                               rdx
                                               rsi
                                               rdi
                                               i.77
                                               fv2
                                               tmp-ra.42
                                               a.69
                                               b.70
                                               c.71
                                               d.72
                                               e.73
                                               f.74
                                               g.75
                                               rbp))
                                             (sum.78 (i.77 tmp-ra.42 rbp))
                                             (g.75
                                              (r9
                                               r8
                                               rcx
                                               rdx
                                               rsi
                                               rdi
                                               i.77
                                               h.76
                                               fv1
                                               fv2
                                               tmp-ra.42
                                               a.69
                                               b.70
                                               c.71
                                               d.72
                                               e.73
                                               f.74
                                               rbp))
                                             (a.69
                                              (i.77
                                               h.76
                                               g.75
                                               f.74
                                               e.73
                                               d.72
                                               c.71
                                               b.70
                                               rsi
                                               rdx
                                               rcx
                                               r8
                                               r9
                                               fv0
                                               fv1
                                               fv2
                                               tmp-ra.42
                                               rbp))
                                             (rbp
                                              (sum.78
                                               r15
                                               nfv.44
                                               nfv.43
                                               r9
                                               r8
                                               rcx
                                               rdx
                                               rsi
                                               rdi
                                               i.77
                                               h.76
                                               g.75
                                               f.74
                                               e.73
                                               d.72
                                               c.71
                                               b.70
                                               a.69
                                               tmp-ra.42))
                                             (fv2 (h.76 g.75 f.74 e.73 d.72 c.71 b.70 a.69 tmp-ra.42))
                                             (fv1 (g.75 f.74 e.73 d.72 c.71 b.70 a.69 tmp-ra.42))
                                             (fv0 (f.74 e.73 d.72 c.71 b.70 a.69 tmp-ra.42))
                                             (r9
                                              (r15
                                               nfv.44
                                               nfv.43
                                               g.75
                                               h.76
                                               r8
                                               rcx
                                               rdx
                                               rsi
                                               rdi
                                               rbp
                                               e.73
                                               d.72
                                               c.71
                                               b.70
                                               a.69
                                               tmp-ra.42))
                                             (r8
                                              (r15
                                               nfv.44
                                               nfv.43
                                               r9
                                               f.74
                                               g.75
                                               h.76
                                               rcx
                                               rdx
                                               rsi
                                               rdi
                                               rbp
                                               d.72
                                               c.71
                                               b.70
                                               a.69
                                               tmp-ra.42))
                                             (rcx
                                              (r15
                                               nfv.44
                                               nfv.43
                                               r9
                                               r8
                                               e.73
                                               f.74
                                               g.75
                                               h.76
                                               rdx
                                               rsi
                                               rdi
                                               rbp
                                               c.71
                                               b.70
                                               a.69
                                               tmp-ra.42))
                                             (rdx
                                              (r15
                                               nfv.44
                                               nfv.43
                                               r9
                                               r8
                                               rcx
                                               d.72
                                               e.73
                                               f.74
                                               g.75
                                               h.76
                                               rsi
                                               rdi
                                               rbp
                                               b.70
                                               a.69
                                               tmp-ra.42))
                                             (rsi
                                              (r15
                                               nfv.44
                                               nfv.43
                                               r9
                                               r8
                                               rcx
                                               rdx
                                               c.71
                                               d.72
                                               e.73
                                               f.74
                                               g.75
                                               h.76
                                               rdi
                                               rbp
                                               a.69
                                               tmp-ra.42))
                                             (rdi
                                              (i.77
                                               r15
                                               nfv.44
                                               nfv.43
                                               r9
                                               r8
                                               rcx
                                               rdx
                                               rsi
                                               b.70
                                               c.71
                                               d.72
                                               e.73
                                               f.74
                                               g.75
                                               h.76
                                               rbp
                                               tmp-ra.42))
                                             (r15 (nfv.44 nfv.43 r9 r8 rcx rdx rsi rdi rbp))))
                                           (assignment ((tmp-ra.42 fv3) (i.77 fv0) (nfv.43 fv4) (nfv.44 fv5))))
                                          (begin
                                            (set! tmp-ra.42 r15)
                                            (set! a.69 rdi)
                                            (set! b.70 rsi)
                                            (set! c.71 rdx)
                                            (set! d.72 rcx)
                                            (set! e.73 r8)
                                            (set! f.74 r9)
                                            (set! g.75 fv0)
                                            (set! h.76 fv1)
                                            (set! i.77 fv2)
                                            (begin
                                              (set! rbp (- rbp 32))
                                              (return-point L.rp.18
                                                            (begin
                                                              (set! rdi a.69)
                                                              (set! rsi b.70)
                                                              (set! rdx c.71)
                                                              (set! rcx d.72)
                                                              (set! r8 e.73)
                                                              (set! r9 f.74)
                                                              (set! nfv.43 g.75)
                                                              (set! nfv.44 h.76)
                                                              (set! r15 L.rp.18)
                                                              (jump L.add.10 rbp r15 rdi rsi rdx rcx r8 r9 nfv.43 nfv.44)))
                                              (set! rbp (+ rbp 32)))
                                            (set! sum.78 rax)
                                            (set! rdi sum.78)
                                            (set! rsi i.77)
                                            (set! r15 tmp-ra.42)
                                            (jump L.*.2 rbp r15 rdi rsi)))
                                        (begin
                                          (set! tmp-ra.45 r15)
                                          (set! rdi 8)
                                          (set! rsi 16)
                                          (set! rdx 24)
                                          (set! rcx 32)
                                          (set! r8 40)
                                          (set! r9 48)
                                          (set! fv0 56)
                                          (set! fv1 64)
                                          (set! fv2 16)
                                          (set! r15 tmp-ra.45)
                                          (jump L.add-and-multiply.11 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2))))
                
  '(module
   ((locals ())
    (conflicts
     ((tmp-ra.45 (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp))
      (rbp (r15 fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi tmp-ra.45))
      (rdi (r15 fv2 fv1 fv0 r9 r8 rcx rdx rsi tmp-ra.45 rbp))
      (rsi (r15 fv2 fv1 fv0 r9 r8 rcx rdx tmp-ra.45 rdi rbp))
      (rdx (r15 fv2 fv1 fv0 r9 r8 rcx tmp-ra.45 rsi rdi rbp))
      (rcx (r15 fv2 fv1 fv0 r9 r8 tmp-ra.45 rdx rsi rdi rbp))
      (r8 (r15 fv2 fv1 fv0 r9 tmp-ra.45 rcx rdx rsi rdi rbp))
      (r9 (r15 fv2 fv1 fv0 tmp-ra.45 r8 rcx rdx rsi rdi rbp))
      (fv0 (r15 fv2 fv1 tmp-ra.45 r9 r8 rcx rdx rsi rdi rbp))
      (fv1 (r15 fv2 tmp-ra.45 fv0 r9 r8 rcx rdx rsi rdi rbp))
      (fv2 (r15 tmp-ra.45 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp))
      (r15 (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp))))
    (assignment ((tmp-ra.45 rsp))))
   (define L.*.2
     ((locals ())
      (conflicts
       ((tmp.26 (tmp.1 tmp.2 rbp tmp-ra.39))
        (tmp.1 (tmp.27 tmp.25 tmp.26 tmp.23 tmp.24 tmp.2 rsi rbp tmp-ra.39))
        (tmp.23 (tmp.2 tmp.1 rbp tmp-ra.39))
        (tmp-ra.39
         (rax tmp.27 tmp.25 tmp.26 tmp.23 tmp.24 tmp.2 tmp.1 rdi rsi rbp))
        (tmp.25 (tmp.2 tmp.1 rbp tmp-ra.39))
        (tmp.2 (tmp.25 tmp.26 tmp.23 tmp.24 tmp.1 rbp tmp-ra.39))
        (tmp.27 (rax tmp.1 rbp tmp-ra.39))
        (tmp.24 (tmp.2 tmp.1 rbp tmp-ra.39))
        (rbp (rax tmp.27 tmp.25 tmp.26 tmp.23 tmp.24 tmp.2 tmp.1 tmp-ra.39))
        (rsi (tmp.1 tmp-ra.39))
        (rdi (tmp-ra.39))
        (rax (tmp.27 rbp tmp-ra.39))))
      (assignment
       ((tmp.27 rsp)
        (tmp.26 rsp)
        (tmp.23 rsp)
        (tmp.25 rsp)
        (tmp.1 rdx)
        (tmp-ra.39 rcx)
        (tmp.2 rbx)
        (tmp.24 rsp))))
     (begin
       (set! tmp-ra.39 r15)
       (set! tmp.1 rdi)
       (set! tmp.2 rsi)
       (if (begin
             (if (begin
                   (begin
                     (set! tmp.24 tmp.2)
                     (set! tmp.24 (bitwise-and tmp.24 7)))
                   (= tmp.24 0))
               (set! tmp.23 14)
               (set! tmp.23 6))
             (!= tmp.23 6))
         (if (begin
               (if (begin
                     (begin
                       (set! tmp.26 tmp.1)
                       (set! tmp.26 (bitwise-and tmp.26 7)))
                     (= tmp.26 0))
                 (set! tmp.25 14)
                 (set! tmp.25 6))
               (!= tmp.25 6))
           (begin
             (set! tmp.27 tmp.2)
             (set! tmp.27 (arithmetic-shift-right tmp.27 3))
             (set! rax tmp.1)
             (set! rax (* rax tmp.27))
             (jump tmp-ra.39 rbp rax))
           (begin (set! rax 318) (jump tmp-ra.39 rbp rax)))
         (begin (set! rax 318) (jump tmp-ra.39 rbp rax)))))
   (define L.+.1
     ((locals ())
      (conflicts
       ((tmp.29 (tmp.4 tmp.3 rbp tmp-ra.40))
        (tmp.30 (tmp.3 tmp.4 rbp tmp-ra.40))
        (tmp.28 (tmp.3 tmp.4 rbp tmp-ra.40))
        (tmp.3 (tmp.30 tmp.31 tmp.28 tmp.29 tmp.4 rsi rbp tmp-ra.40))
        (tmp-ra.40 (rax tmp.30 tmp.31 tmp.28 tmp.29 tmp.4 tmp.3 rdi rsi rbp))
        (tmp.31 (tmp.3 tmp.4 rbp tmp-ra.40))
        (tmp.4 (rax tmp.30 tmp.31 tmp.28 tmp.29 tmp.3 rbp tmp-ra.40))
        (rbp (rax tmp.30 tmp.31 tmp.28 tmp.29 tmp.4 tmp.3 tmp-ra.40))
        (rsi (tmp.3 tmp-ra.40))
        (rdi (tmp-ra.40))
        (rax (tmp.4 rbp tmp-ra.40))))
      (assignment
       ((tmp.29 rbx)
        (tmp.30 rbx)
        (tmp.28 rbx)
        (tmp.3 rdx)
        (tmp-ra.40 rcx)
        (tmp.31 rbx)
        (tmp.4 rsp))))
     (begin
       (set! tmp-ra.40 r15)
       (set! tmp.3 rdi)
       (set! tmp.4 rsi)
       (if (begin
             (if (begin
                   (begin
                     (set! tmp.29 tmp.4)
                     (set! tmp.29 (bitwise-and tmp.29 7)))
                   (= tmp.29 0))
               (set! tmp.28 14)
               (set! tmp.28 6))
             (!= tmp.28 6))
         (if (begin
               (if (begin
                     (begin
                       (set! tmp.31 tmp.3)
                       (set! tmp.31 (bitwise-and tmp.31 7)))
                     (= tmp.31 0))
                 (set! tmp.30 14)
                 (set! tmp.30 6))
               (!= tmp.30 6))
           (begin
             (set! rax tmp.3)
             (set! rax (+ rax tmp.4))
             (jump tmp-ra.40 rbp rax))
           (begin (set! rax 574) (jump tmp-ra.40 rbp rax)))
         (begin (set! rax 574) (jump tmp-ra.40 rbp rax)))))
   (define L.add.10
     ((locals ())
      (conflicts
       ((tmp.33 (rdi tmp-ra.41 a.61 b.62 rbp))
        (tmp-ra.41
         (tmp.32
          tmp.33
          tmp.34
          tmp.35
          tmp.36
          tmp.37
          h.68
          g.67
          f.66
          e.65
          d.64
          c.63
          b.62
          a.61
          rdi
          rsi
          rdx
          rcx
          r8
          r9
          fv0
          fv1
          rbp))
        (b.62
         (tmp.33
          tmp.34
          tmp.35
          tmp.36
          tmp.37
          h.68
          g.67
          f.66
          e.65
          d.64
          c.63
          rdx
          rcx
          r8
          r9
          fv0
          fv1
          a.61
          tmp-ra.41
          rbp))
        (d.64
         (tmp.35
          tmp.36
          tmp.37
          h.68
          g.67
          f.66
          e.65
          r8
          r9
          fv0
          fv1
          b.62
          a.61
          tmp-ra.41
          c.63
          rbp))
        (f.66
         (tmp.37 h.68 g.67 fv0 fv1 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp))
        (h.68 (rdi f.66 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 g.67 rbp))
        (tmp.32 (rdi a.61 tmp-ra.41 rbp))
        (a.61
         (tmp.32
          tmp.33
          tmp.34
          tmp.35
          tmp.36
          tmp.37
          h.68
          g.67
          f.66
          e.65
          d.64
          c.63
          b.62
          rsi
          rdx
          rcx
          r8
          r9
          fv0
          fv1
          tmp-ra.41
          rbp))
        (tmp.36 (rdi d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp))
        (tmp.35 (rdi c.63 tmp-ra.41 a.61 b.62 d.64 rbp))
        (g.67 (h.68 fv1 f.66 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp))
        (tmp.37 (rdi e.65 c.63 tmp-ra.41 a.61 b.62 d.64 f.66 rbp))
        (tmp.34 (rdi b.62 a.61 tmp-ra.41 c.63 rbp))
        (c.63
         (tmp.34
          tmp.35
          tmp.36
          tmp.37
          h.68
          g.67
          f.66
          e.65
          d.64
          rcx
          r8
          r9
          fv0
          fv1
          b.62
          a.61
          tmp-ra.41
          rbp))
        (e.65
         (tmp.36
          tmp.37
          h.68
          g.67
          f.66
          r9
          fv0
          fv1
          d.64
          b.62
          a.61
          tmp-ra.41
          c.63
          rbp))
        (rbp
         (tmp.32
          tmp.33
          tmp.34
          tmp.35
          tmp.36
          tmp.37
          r15
          rsi
          rdi
          h.68
          g.67
          f.66
          e.65
          d.64
          c.63
          b.62
          a.61
          tmp-ra.41))
        (fv1 (g.67 f.66 e.65 d.64 c.63 b.62 a.61 tmp-ra.41))
        (fv0 (f.66 e.65 d.64 c.63 b.62 a.61 tmp-ra.41))
        (r9 (e.65 d.64 c.63 b.62 a.61 tmp-ra.41))
        (r8 (d.64 c.63 b.62 a.61 tmp-ra.41))
        (rcx (c.63 b.62 a.61 tmp-ra.41))
        (rdx (b.62 a.61 tmp-ra.41))
        (rsi (r15 rdi rbp a.61 tmp-ra.41))
        (rdi
         (tmp.32
          tmp.33
          tmp.34
          tmp.35
          tmp.36
          tmp.37
          r15
          rsi
          h.68
          rbp
          tmp-ra.41))
        (r15 (rsi rdi rbp))))
      (assignment
       ((f.66 fv8)
        (e.65 fv7)
        (d.64 fv6)
        (c.63 fv5)
        (b.62 fv4)
        (tmp-ra.41 fv3)
        (a.61 fv2)
        (tmp.33 rsp)
        (tmp.32 rsp)
        (tmp.36 rsp)
        (tmp.35 rsp)
        (g.67 rbx)
        (h.68 rsp)
        (tmp.37 rsp)
        (tmp.34 rsp))))
     (begin
       (set! tmp-ra.41 r15)
       (set! a.61 rdi)
       (set! b.62 rsi)
       (set! c.63 rdx)
       (set! d.64 rcx)
       (set! e.65 r8)
       (set! f.66 r9)
       (set! g.67 fv0)
       (set! h.68 fv1)
       (begin
         (set! rbp (- rbp 72))
         (return-point L.rp.12
           (begin
             (set! rdi g.67)
             (set! rsi h.68)
             (set! r15 L.rp.12)
             (jump L.+.1 rbp r15 rdi rsi)))
         (set! rbp (+ rbp 72)))
       (set! tmp.37 rax)
       (begin
         (set! rbp (- rbp 72))
         (return-point L.rp.13
           (begin
             (set! rdi f.66)
             (set! rsi tmp.37)
             (set! r15 L.rp.13)
             (jump L.+.1 rbp r15 rdi rsi)))
         (set! rbp (+ rbp 72)))
       (set! tmp.36 rax)
       (begin
         (set! rbp (- rbp 72))
         (return-point L.rp.14
           (begin
             (set! rdi e.65)
             (set! rsi tmp.36)
             (set! r15 L.rp.14)
             (jump L.+.1 rbp r15 rdi rsi)))
         (set! rbp (+ rbp 72)))
       (set! tmp.35 rax)
       (begin
         (set! rbp (- rbp 72))
         (return-point L.rp.15
           (begin
             (set! rdi d.64)
             (set! rsi tmp.35)
             (set! r15 L.rp.15)
             (jump L.+.1 rbp r15 rdi rsi)))
         (set! rbp (+ rbp 72)))
       (set! tmp.34 rax)
       (begin
         (set! rbp (- rbp 72))
         (return-point L.rp.16
           (begin
             (set! rdi c.63)
             (set! rsi tmp.34)
             (set! r15 L.rp.16)
             (jump L.+.1 rbp r15 rdi rsi)))
         (set! rbp (+ rbp 72)))
       (set! tmp.33 rax)
       (begin
         (set! rbp (- rbp 72))
         (return-point L.rp.17
           (begin
             (set! rdi b.62)
             (set! rsi tmp.33)
             (set! r15 L.rp.17)
             (jump L.+.1 rbp r15 rdi rsi)))
         (set! rbp (+ rbp 72)))
       (set! tmp.32 rax)
       (set! rdi a.61)
       (set! rsi tmp.32)
       (set! r15 tmp-ra.41)
       (jump L.+.1 rbp r15 rdi rsi)))
   (define L.add-and-multiply.11
     ((locals ())
      (conflicts
       ((nfv.43 (r15 nfv.44 h.76 r9 r8 rcx rdx rsi rdi rbp))
        (e.73
         (rcx
          rdx
          rsi
          rdi
          i.77
          h.76
          g.75
          f.74
          r9
          fv0
          fv1
          fv2
          tmp-ra.42
          a.69
          b.70
          c.71
          d.72
          rbp))
        (f.74
         (r8
          rcx
          rdx
          rsi
          rdi
          i.77
          h.76
          g.75
          fv0
          fv1
          fv2
          tmp-ra.42
          a.69
          b.70
          c.71
          d.72
          e.73
          rbp))
        (b.70
         (rdi
          i.77
          h.76
          g.75
          f.74
          e.73
          d.72
          c.71
          rdx
          rcx
          r8
          r9
          fv0
          fv1
          fv2
          tmp-ra.42
          a.69
          rbp))
        (d.72
         (rdx
          rsi
          rdi
          i.77
          h.76
          g.75
          f.74
          e.73
          r8
          r9
          fv0
          fv1
          fv2
          tmp-ra.42
          a.69
          b.70
          c.71
          rbp))
        (nfv.44 (r15 nfv.43 r9 r8 rcx rdx rsi rdi rbp))
        (c.71
         (rsi
          rdi
          i.77
          h.76
          g.75
          f.74
          e.73
          d.72
          rcx
          r8
          r9
          fv0
          fv1
          fv2
          tmp-ra.42
          a.69
          b.70
          rbp))
        (tmp-ra.42
         (sum.78
          i.77
          h.76
          g.75
          f.74
          e.73
          d.72
          c.71
          b.70
          a.69
          rdi
          rsi
          rdx
          rcx
          r8
          r9
          fv0
          fv1
          fv2
          rbp))
        (i.77
         (rdi sum.78 tmp-ra.42 a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76 rbp))
        (h.76
         (nfv.43
          r9
          r8
          rcx
          rdx
          rsi
          rdi
          i.77
          fv2
          tmp-ra.42
          a.69
          b.70
          c.71
          d.72
          e.73
          f.74
          g.75
          rbp))
        (sum.78 (i.77 tmp-ra.42 rbp))
        (g.75
         (r9
          r8
          rcx
          rdx
          rsi
          rdi
          i.77
          h.76
          fv1
          fv2
          tmp-ra.42
          a.69
          b.70
          c.71
          d.72
          e.73
          f.74
          rbp))
        (a.69
         (i.77
          h.76
          g.75
          f.74
          e.73
          d.72
          c.71
          b.70
          rsi
          rdx
          rcx
          r8
          r9
          fv0
          fv1
          fv2
          tmp-ra.42
          rbp))
        (rbp
         (sum.78
          r15
          nfv.44
          nfv.43
          r9
          r8
          rcx
          rdx
          rsi
          rdi
          i.77
          h.76
          g.75
          f.74
          e.73
          d.72
          c.71
          b.70
          a.69
          tmp-ra.42))
        (fv2 (h.76 g.75 f.74 e.73 d.72 c.71 b.70 a.69 tmp-ra.42))
        (fv1 (g.75 f.74 e.73 d.72 c.71 b.70 a.69 tmp-ra.42))
        (fv0 (f.74 e.73 d.72 c.71 b.70 a.69 tmp-ra.42))
        (r9
         (r15
          nfv.44
          nfv.43
          g.75
          h.76
          r8
          rcx
          rdx
          rsi
          rdi
          rbp
          e.73
          d.72
          c.71
          b.70
          a.69
          tmp-ra.42))
        (r8
         (r15
          nfv.44
          nfv.43
          r9
          f.74
          g.75
          h.76
          rcx
          rdx
          rsi
          rdi
          rbp
          d.72
          c.71
          b.70
          a.69
          tmp-ra.42))
        (rcx
         (r15
          nfv.44
          nfv.43
          r9
          r8
          e.73
          f.74
          g.75
          h.76
          rdx
          rsi
          rdi
          rbp
          c.71
          b.70
          a.69
          tmp-ra.42))
        (rdx
         (r15
          nfv.44
          nfv.43
          r9
          r8
          rcx
          d.72
          e.73
          f.74
          g.75
          h.76
          rsi
          rdi
          rbp
          b.70
          a.69
          tmp-ra.42))
        (rsi
         (r15
          nfv.44
          nfv.43
          r9
          r8
          rcx
          rdx
          c.71
          d.72
          e.73
          f.74
          g.75
          h.76
          rdi
          rbp
          a.69
          tmp-ra.42))
        (rdi
         (i.77
          r15
          nfv.44
          nfv.43
          r9
          r8
          rcx
          rdx
          rsi
          b.70
          c.71
          d.72
          e.73
          f.74
          g.75
          h.76
          rbp
          tmp-ra.42))
        (r15 (nfv.44 nfv.43 r9 r8 rcx rdx rsi rdi rbp))))
      (assignment
       ((tmp-ra.42 fv3)
        (i.77 fv0)
        (nfv.43 fv4)
        (nfv.44 fv5)
        (sum.78 rsp)
        (e.73 r8)
        (f.74 r9)
        (b.70 rsi)
        (d.72 rcx)
        (c.71 rdx)
        (a.69 rdi)
        (h.76 rbx)
        (g.75 rsp))))
     (begin
       (set! tmp-ra.42 r15)
       (set! a.69 rdi)
       (set! b.70 rsi)
       (set! c.71 rdx)
       (set! d.72 rcx)
       (set! e.73 r8)
       (set! f.74 r9)
       (set! g.75 fv0)
       (set! h.76 fv1)
       (set! i.77 fv2)
       (begin
         (set! rbp (- rbp 32))
         (return-point L.rp.18
           (begin
             (set! rdi a.69)
             (set! rsi b.70)
             (set! rdx c.71)
             (set! rcx d.72)
             (set! r8 e.73)
             (set! r9 f.74)
             (set! nfv.43 g.75)
             (set! nfv.44 h.76)
             (set! r15 L.rp.18)
             (jump L.add.10 rbp r15 rdi rsi rdx rcx r8 r9 nfv.43 nfv.44)))
         (set! rbp (+ rbp 32)))
       (set! sum.78 rax)
       (set! rdi sum.78)
       (set! rsi i.77)
       (set! r15 tmp-ra.42)
       (jump L.*.2 rbp r15 rdi rsi)))
   (begin
     (set! tmp-ra.45 r15)
     (set! rdi 8)
     (set! rsi 16)
     (set! rdx 24)
     (set! rcx 32)
     (set! r8 40)
     (set! r9 48)
     (set! fv0 56)
     (set! fv1 64)
     (set! fv2 16)
     (set! r15 tmp-ra.45)
     (jump L.add-and-multiply.11 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2)))))
