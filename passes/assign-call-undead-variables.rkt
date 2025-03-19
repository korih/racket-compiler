#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v7)

(provide assign-call-undead-variables)

;; asm-lang-v7/conflicts -> asm-lang-v7/pre-framed
;; Compiles Asm-pred-lang-v7/conflicts to Asm-pred-lang-v7/pre-framed by
;; pre-assigning all variables in the call-undead sets to frame variables
(define/contract (assign-call-undead-variables p)
  (-> asm-pred-lang-v7/conflicts? asm-pred-lang-v7/pre-framed?)

  ;; call-undead-set (Graph of conflicts) assignments -> assignments
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
      (for/fold ([assignments '()])
                ([x (info-ref info 'call-undead)])
        (cons (graph-colouring x conflicts-graph assignments) assignments)))

    (define locals^
      (let ([local-variables (reverse (info-ref info 'locals))])
        (remove* (map car assignments) local-variables )))

    (define info^ (info-set info 'assignment assignments))
    ;(define info^^ (info-set (info-remove info^ 'locals) 'locals locals^))
    (define info^^ (info-set info^ 'locals locals^))
    info^^)

  ;; (Function Definition) -> (Function Definition)
  ;; Take a function definition and update its assignments in info
  (define (assign-call-fun f)
    (match f
      [`(define ,name ,info ,tail) (define info^ (assign-call-variables-info info))
                                   `(define ,name ,info^ ,tail)]))

  (match p
    [`(module ,info ,funs ... ,tail)
     (define info^ (assign-call-variables-info info))
     (define funs^ (for/list ([fun funs])
                     (assign-call-fun fun)))
     `(module ,info^ ,@funs^ ,tail)]))



(module+ test
  (require rackunit)
  (check-equal? (assign-call-undead-variables '(module
                                                   ((new-frames ())
                                                    (locals ())
                                                    (call-undead ())
                                                    (conflicts ()))
                                                 (jump done)))
                '(module
                     ((new-frames ())
                      (locals ())
                      (call-undead ())
                      (conflicts ())
                      (assignment ()))
                   (jump done)))
  (check-equal? (assign-call-undead-variables '(module ((new-frames (()))
                                                        (locals (z.3 tmp-ra.1 x.1 y.2))
                                                        (undead-out
                                                         ((rdi rsi tmp-ra.1 rbp)
                                                          (rsi x.1 tmp-ra.1 rbp)
                                                          (y.2 x.1 tmp-ra.1 rbp)
                                                          ((y.2 x.1 tmp-ra.1 rbp)
                                                           ((tmp-ra.1 rax rbp) (rax rbp))
                                                           (((rax tmp-ra.1 rbp)
                                                             ((y.2 rsi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                                            (z.3 tmp-ra.1 rbp)
                                                            (tmp-ra.1 rax rbp)
                                                            (rax rbp)))))
                                                        (call-undead (tmp-ra.1))
                                                        (conflicts
                                                         ((y.2 (rbp tmp-ra.1 x.1 rsi))
                                                          (x.1 (y.2 rbp tmp-ra.1 rsi))
                                                          (tmp-ra.1 (y.2 x.1 rbp rsi rdi rax z.3))
                                                          (z.3 (rbp tmp-ra.1))
                                                          (rsi (x.1 tmp-ra.1 r15 rdi rbp y.2))
                                                          (rbp (y.2 x.1 tmp-ra.1 rax z.3 r15 rdi rsi))
                                                          (rdi (tmp-ra.1 r15 rbp rsi))
                                                          (r15 (rbp rdi rsi))
                                                          (rax (rbp tmp-ra.1)))))
                                                 (begin
                                                   (set! tmp-ra.1 r15)
                                                   (set! x.1 rdi)
                                                   (set! y.2 rsi)
                                                   (if (< y.2 x.1)
                                                       (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
                                                       (begin
                                                         (return-point L.rp.1
                                                                       (begin
                                                                         (set! rsi x.1)
                                                                         (set! rdi y.2)
                                                                         (set! r15 L.rp.1)
                                                                         (jump L.swap.1 rbp r15 rdi rsi)))
                                                         (set! z.3 rax)
                                                         (set! rax z.3)
                                                         (jump tmp-ra.1 rbp rax))))))
                '(module
                     ((new-frames (()))
                      (locals (y.2 x.1 z.3))
                      (undead-out
                       ((rdi rsi tmp-ra.1 rbp)
                        (rsi x.1 tmp-ra.1 rbp)
                        (y.2 x.1 tmp-ra.1 rbp)
                        ((y.2 x.1 tmp-ra.1 rbp)
                         ((tmp-ra.1 rax rbp) (rax rbp))
                         (((rax tmp-ra.1 rbp)
                           ((y.2 rsi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                          (z.3 tmp-ra.1 rbp)
                          (tmp-ra.1 rax rbp)
                          (rax rbp)))))
                      (call-undead (tmp-ra.1))
                      (conflicts
                       ((y.2 (rbp tmp-ra.1 x.1 rsi))
                        (x.1 (y.2 rbp tmp-ra.1 rsi))
                        (tmp-ra.1 (y.2 x.1 rbp rsi rdi rax z.3))
                        (z.3 (rbp tmp-ra.1))
                        (rsi (x.1 tmp-ra.1 r15 rdi rbp y.2))
                        (rbp (y.2 x.1 tmp-ra.1 rax z.3 r15 rdi rsi))
                        (rdi (tmp-ra.1 r15 rbp rsi))
                        (r15 (rbp rdi rsi))
                        (rax (rbp tmp-ra.1))))
                      (assignment ((tmp-ra.1 fv0))))
                   (begin
                     (set! tmp-ra.1 r15)
                     (set! x.1 rdi)
                     (set! y.2 rsi)
                     (if (< y.2 x.1)
                         (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
                         (begin
                           (return-point L.rp.1
                                         (begin
                                           (set! rsi x.1)
                                           (set! rdi y.2)
                                           (set! r15 L.rp.1)
                                           (jump L.swap.1 rbp r15 rdi rsi)))
                           (set! z.3 rax)
                           (set! rax z.3)
                           (jump tmp-ra.1 rbp rax))))))
  (check-equal? (assign-call-undead-variables '(module
                                                   ((new-frames ())
                                                    (locals (tmp-ra.2))
                                                    (call-undead ())
                                                    (undead-out
                                                     ((tmp-ra.2 rbp)
                                                      (tmp-ra.2 rsi rbp)
                                                      (tmp-ra.2 rsi rdi rbp)
                                                      (rsi rdi r15 rbp)
                                                      (rsi rdi r15 rbp)))
                                                    (conflicts
                                                     ((tmp-ra.2 (rdi rsi rbp))
                                                      (rbp (r15 rdi rsi tmp-ra.2))
                                                      (rsi (r15 rdi rbp tmp-ra.2))
                                                      (rdi (r15 rbp rsi tmp-ra.2))
                                                      (r15 (rbp rdi rsi)))))
                                                 (define L.swap.1
                                                   ((new-frames (()))
                                                    (locals (z.3 tmp-ra.1 x.1 y.2))
                                                    (undead-out
                                                     ((rdi rsi tmp-ra.1 rbp)
                                                      (rsi x.1 tmp-ra.1 rbp)
                                                      (y.2 x.1 tmp-ra.1 rbp)
                                                      ((y.2 x.1 tmp-ra.1 rbp)
                                                       ((tmp-ra.1 rax rbp) (rax rbp))
                                                       (((rax tmp-ra.1 rbp)
                                                         ((y.2 rsi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                                        (z.3 tmp-ra.1 rbp)
                                                        (tmp-ra.1 rax rbp)
                                                        (rax rbp)))))
                                                    (call-undead (tmp-ra.1))
                                                    (conflicts
                                                     ((y.2 (rbp tmp-ra.1 x.1 rsi))
                                                      (x.1 (y.2 rbp tmp-ra.1 rsi))
                                                      (tmp-ra.1 (y.2 x.1 rbp rsi rdi rax z.3))
                                                      (z.3 (rbp tmp-ra.1))
                                                      (rsi (x.1 tmp-ra.1 r15 rdi rbp y.2))
                                                      (rbp (y.2 x.1 tmp-ra.1 rax z.3 r15 rdi rsi))
                                                      (rdi (tmp-ra.1 r15 rbp rsi))
                                                      (r15 (rbp rdi rsi))
                                                      (rax (rbp tmp-ra.1)))))
                                                   (begin
                                                     (set! tmp-ra.1 r15)
                                                     (set! x.1 rdi)
                                                     (set! y.2 rsi)
                                                     (if (< y.2 x.1)
                                                         (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
                                                         (begin
                                                           (return-point L.rp.1
                                                                         (begin
                                                                           (set! rsi x.1)
                                                                           (set! rdi y.2)
                                                                           (set! r15 L.rp.1)
                                                                           (jump L.swap.1 rbp r15 rdi rsi)))
                                                           (set! z.3 rax)
                                                           (set! rax z.3)
                                                           (jump tmp-ra.1 rbp rax)))))
                                                 (begin
                                                   (set! tmp-ra.2 r15)
                                                   (set! rsi 2)
                                                   (set! rdi 1)
                                                   (set! r15 tmp-ra.2)
                                                   (jump L.swap.1 rbp r15 rdi rsi))))
                '(module
                     ((new-frames ())
                      (locals (tmp-ra.2))
                      (call-undead ())
                      (undead-out
                       ((tmp-ra.2 rbp)
                        (tmp-ra.2 rsi rbp)
                        (tmp-ra.2 rsi rdi rbp)
                        (rsi rdi r15 rbp)
                        (rsi rdi r15 rbp)))
                      (conflicts
                       ((tmp-ra.2 (rdi rsi rbp))
                        (rbp (r15 rdi rsi tmp-ra.2))
                        (rsi (r15 rdi rbp tmp-ra.2))
                        (rdi (r15 rbp rsi tmp-ra.2))
                        (r15 (rbp rdi rsi))))
                      (assignment ()))
                   (define L.swap.1
                     ((new-frames (()))
                      (locals (y.2 x.1 z.3))
                      (undead-out
                       ((rdi rsi tmp-ra.1 rbp)
                        (rsi x.1 tmp-ra.1 rbp)
                        (y.2 x.1 tmp-ra.1 rbp)
                        ((y.2 x.1 tmp-ra.1 rbp)
                         ((tmp-ra.1 rax rbp) (rax rbp))
                         (((rax tmp-ra.1 rbp)
                           ((y.2 rsi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                          (z.3 tmp-ra.1 rbp)
                          (tmp-ra.1 rax rbp)
                          (rax rbp)))))
                      (call-undead (tmp-ra.1))
                      (conflicts
                       ((y.2 (rbp tmp-ra.1 x.1 rsi))
                        (x.1 (y.2 rbp tmp-ra.1 rsi))
                        (tmp-ra.1 (y.2 x.1 rbp rsi rdi rax z.3))
                        (z.3 (rbp tmp-ra.1))
                        (rsi (x.1 tmp-ra.1 r15 rdi rbp y.2))
                        (rbp (y.2 x.1 tmp-ra.1 rax z.3 r15 rdi rsi))
                        (rdi (tmp-ra.1 r15 rbp rsi))
                        (r15 (rbp rdi rsi))
                        (rax (rbp tmp-ra.1))))
                      (assignment ((tmp-ra.1 fv0))))
                     (begin
                       (set! tmp-ra.1 r15)
                       (set! x.1 rdi)
                       (set! y.2 rsi)
                       (if (< y.2 x.1)
                           (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
                           (begin
                             (return-point L.rp.1
                                           (begin
                                             (set! rsi x.1)
                                             (set! rdi y.2)
                                             (set! r15 L.rp.1)
                                             (jump L.swap.1 rbp r15 rdi rsi)))
                             (set! z.3 rax)
                             (set! rax z.3)
                             (jump tmp-ra.1 rbp rax)))))
                   (begin
                     (set! tmp-ra.2 r15)
                     (set! rsi 2)
                     (set! rdi 1)
                     (set! r15 tmp-ra.2)
                     (jump L.swap.1 rbp r15 rdi rsi))))

  (check-equal? (assign-call-undead-variables
                 '(module
                      ((locals (x.1 y.2))
                       (call-undead ())
                       (new-frames (()))
                       (undead-out ((x.1 r15)
                                    (x.1 y.2 r15)
                                    ((x.1 y.2 r15)
                                     ((r15) ())
                                     ((r15) ()))))
                       (conflicts ((y.2 (r15))
                                   (x.1 (r15))
                                   (r15 (rax y.2 x.1))
                                   (rax (r15)))))
                    (begin
                      (set! x.1 3)
                      (set! y.2 x.1)
                      (if (> y.2 x.1)
                          (begin (set! rax x.1) (jump r15))
                          (begin (set! rax y.2) (jump r15))))))
                '(module
                     ((locals (y.2 x.1))
                      (call-undead ())
                      (new-frames (()))
                      (undead-out ((x.1 r15) (x.1 y.2 r15) ((x.1 y.2 r15) ((r15) ()) ((r15) ()))))
                      (conflicts ((y.2 (r15)) (x.1 (r15)) (r15 (rax y.2 x.1)) (rax (r15))))
                      (assignment ()))
                   (begin
                     (set! x.1 3)
                     (set! y.2 x.1)
                     (if (> y.2 x.1)
                         (begin (set! rax x.1) (jump r15))
                         (begin (set! rax y.2) (jump r15))))))
  (check-equal? (assign-call-undead-variables '(module
                                                   ((new-frames ())
                                                    (locals (tmp-ra.2))
                                                    (call-undead ())
                                                    (undead-out
                                                     ((tmp-ra.2 rbp)
                                                      (tmp-ra.2 rsi rbp)
                                                      (tmp-ra.2 rsi rdi rbp)
                                                      (rsi rdi r15 rbp)
                                                      (rsi rdi r15 rbp)))
                                                    (conflicts
                                                     ((tmp-ra.2 (rdi rsi rbp))
                                                      (rbp (r15 rdi rsi tmp-ra.2))
                                                      (rsi (r15 rdi rbp tmp-ra.2))
                                                      (rdi (r15 rbp rsi tmp-ra.2))
                                                      (r15 (rbp rdi rsi)))))
                                                 (define L.swap.1
                                                   ((new-frames (()))
                                                    (locals (z.3 tmp-ra.1 x.1 y.2))
                                                    (undead-out
                                                     ((rdi rsi tmp-ra.1 rbp)
                                                      (rsi x.1 tmp-ra.1 rbp)
                                                      (y.2 x.1 tmp-ra.1 rbp)
                                                      ((y.2 x.1 tmp-ra.1 rbp)
                                                       ((tmp-ra.1 rax rbp) (rax rbp))
                                                       (((rax tmp-ra.1 rbp)
                                                         ((y.2 rsi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                                        (z.3 tmp-ra.1 rbp)
                                                        (tmp-ra.1 rax rbp)
                                                        (rax rbp)))))
                                                    (call-undead (tmp-ra.1))
                                                    (conflicts
                                                     ((y.2 (rbp tmp-ra.1 x.1 rsi))
                                                      (x.1 (y.2 rbp tmp-ra.1 rsi))
                                                      (tmp-ra.1 (y.2 x.1 rbp rsi rdi rax z.3))
                                                      (z.3 (rbp tmp-ra.1))
                                                      (rsi (x.1 tmp-ra.1 r15 rdi rbp y.2))
                                                      (rbp (y.2 x.1 tmp-ra.1 rax z.3 r15 rdi rsi))
                                                      (rdi (tmp-ra.1 r15 rbp rsi))
                                                      (r15 (rbp rdi rsi))
                                                      (rax (rbp tmp-ra.1)))))
                                                   (begin
                                                     (set! tmp-ra.1 r15)
                                                     (set! x.1 rdi)
                                                     (set! y.2 rsi)
                                                     (if (< y.2 x.1)
                                                         (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
                                                         (begin
                                                           (return-point L.rp.1
                                                                         (begin
                                                                           (set! rsi x.1)
                                                                           (set! rdi y.2)
                                                                           (set! r15 L.rp.1)
                                                                           (jump L.swap.1 rbp r15 rdi rsi)))
                                                           (set! z.3 rax)
                                                           (set! rax z.3)
                                                           (jump tmp-ra.1 rbp rax)))))
                                                 (begin
                                                   (set! tmp-ra.2 r15)
                                                   (set! rsi 2)
                                                   (set! rdi 1)
                                                   (set! r15 tmp-ra.2)
                                                   (jump L.swap.1 rbp r15 rdi rsi))))
                '(module
                     ((new-frames ())
                      (locals (tmp-ra.2))
                      (call-undead ())
                      (undead-out
                       ((tmp-ra.2 rbp)
                        (tmp-ra.2 rsi rbp)
                        (tmp-ra.2 rsi rdi rbp)
                        (rsi rdi r15 rbp)
                        (rsi rdi r15 rbp)))
                      (conflicts
                       ((tmp-ra.2 (rdi rsi rbp))
                        (rbp (r15 rdi rsi tmp-ra.2))
                        (rsi (r15 rdi rbp tmp-ra.2))
                        (rdi (r15 rbp rsi tmp-ra.2))
                        (r15 (rbp rdi rsi))))
                      (assignment ()))
                   (define L.swap.1
                     ((new-frames (()))
                      (locals (y.2 x.1 z.3))
                      (undead-out
                       ((rdi rsi tmp-ra.1 rbp)
                        (rsi x.1 tmp-ra.1 rbp)
                        (y.2 x.1 tmp-ra.1 rbp)
                        ((y.2 x.1 tmp-ra.1 rbp)
                         ((tmp-ra.1 rax rbp) (rax rbp))
                         (((rax tmp-ra.1 rbp)
                           ((y.2 rsi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                          (z.3 tmp-ra.1 rbp)
                          (tmp-ra.1 rax rbp)
                          (rax rbp)))))
                      (call-undead (tmp-ra.1))
                      (conflicts
                       ((y.2 (rbp tmp-ra.1 x.1 rsi))
                        (x.1 (y.2 rbp tmp-ra.1 rsi))
                        (tmp-ra.1 (y.2 x.1 rbp rsi rdi rax z.3))
                        (z.3 (rbp tmp-ra.1))
                        (rsi (x.1 tmp-ra.1 r15 rdi rbp y.2))
                        (rbp (y.2 x.1 tmp-ra.1 rax z.3 r15 rdi rsi))
                        (rdi (tmp-ra.1 r15 rbp rsi))
                        (r15 (rbp rdi rsi))
                        (rax (rbp tmp-ra.1))))
                      (assignment ((tmp-ra.1 fv0))))
                     (begin
                       (set! tmp-ra.1 r15)
                       (set! x.1 rdi)
                       (set! y.2 rsi)
                       (if (< y.2 x.1)
                           (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
                           (begin
                             (return-point L.rp.1
                                           (begin
                                             (set! rsi x.1)
                                             (set! rdi y.2)
                                             (set! r15 L.rp.1)
                                             (jump L.swap.1 rbp r15 rdi rsi)))
                             (set! z.3 rax)
                             (set! rax z.3)
                             (jump tmp-ra.1 rbp rax)))))
                   (begin
                     (set! tmp-ra.2 r15)
                     (set! rsi 2)
                     (set! rdi 1)
                     (set! r15 tmp-ra.2)
                     (jump L.swap.1 rbp r15 rdi rsi))))
  (check-equal? (assign-call-undead-variables '(module
                                                   ((new-frames ())
                                                    (locals (x.3 tmp-ra.2 x.2))
                                                    (call-undead ())
                                                    (undead-out
                                                     ((tmp-ra.2 rbp)
                                                      (x.2 tmp-ra.2 rbp)
                                                      (((x.3 x.2 tmp-ra.2 rbp) (x.2 tmp-ra.2 rbp))
                                                       ((tmp-ra.2 rdi rbp) (rdi r15 rbp) (rbp r15 rdi))
                                                       ((tmp-ra.2 rdi rbp) (rdi r15 rbp) (rbp r15 rdi)))))
                                                    (conflicts
                                                     ((x.2 (x.3 tmp-ra.2 rbp))
                                                      (tmp-ra.2 (rdi x.3 x.2 rbp))
                                                      (x.3 (x.2 tmp-ra.2 rbp))
                                                      (rbp (r15 rdi x.3 x.2 tmp-ra.2))
                                                      (rdi (r15 tmp-ra.2 rbp))
                                                      (r15 (rdi rbp)))))
                                                 (define L.f.1
                                                   ((new-frames ())
                                                    (locals (tmp-ra.1 b.1 y.1 x.1 z.1 a.1))
                                                    (undead-out
                                                     ((rdi rbp tmp-ra.1)
                                                      (x.1 rbp tmp-ra.1)
                                                      (y.1 x.1 rbp tmp-ra.1)
                                                      (y.1 z.1 x.1 rbp tmp-ra.1)
                                                      (a.1 z.1 x.1 rbp tmp-ra.1)
                                                      (z.1 x.1 a.1 rbp tmp-ra.1)
                                                      (x.1 b.1 a.1 rbp tmp-ra.1)
                                                      (b.1 a.1 rbp tmp-ra.1)
                                                      (a.1 rbp tmp-ra.1)
                                                      (rax rbp tmp-ra.1)
                                                      (rax rbp tmp-ra.1)
                                                      (rbp rax)))
                                                    (call-undead ())
                                                    (conflicts
                                                     ((a.1 (b.1 z.1 x.1 rbp tmp-ra.1))
                                                      (z.1 (a.1 y.1 x.1 rbp tmp-ra.1))
                                                      (x.1 (b.1 a.1 z.1 y.1 rbp tmp-ra.1))
                                                      (y.1 (z.1 x.1 rbp tmp-ra.1))
                                                      (b.1 (x.1 a.1 rbp tmp-ra.1))
                                                      (tmp-ra.1 (rax b.1 a.1 z.1 y.1 x.1 rdi rbp))
                                                      (rbp (rax b.1 a.1 z.1 y.1 x.1 tmp-ra.1))
                                                      (rdi (tmp-ra.1))
                                                      (rax (rbp tmp-ra.1)))))
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
                     ((new-frames ())
                      (locals (x.2 tmp-ra.2 x.3))
                      (call-undead ())
                      (undead-out
                       ((tmp-ra.2 rbp)
                        (x.2 tmp-ra.2 rbp)
                        (((x.3 x.2 tmp-ra.2 rbp) (x.2 tmp-ra.2 rbp))
                         ((tmp-ra.2 rdi rbp) (rdi r15 rbp) (rbp r15 rdi))
                         ((tmp-ra.2 rdi rbp) (rdi r15 rbp) (rbp r15 rdi)))))
                      (conflicts
                       ((x.2 (x.3 tmp-ra.2 rbp))
                        (tmp-ra.2 (rdi x.3 x.2 rbp))
                        (x.3 (x.2 tmp-ra.2 rbp))
                        (rbp (r15 rdi x.3 x.2 tmp-ra.2))
                        (rdi (r15 tmp-ra.2 rbp))
                        (r15 (rdi rbp))))
                      (assignment ()))
                   (define L.f.1
                     ((new-frames ())
                      (locals (a.1 z.1 x.1 y.1 b.1 tmp-ra.1))
                      (undead-out
                       ((rdi rbp tmp-ra.1)
                        (x.1 rbp tmp-ra.1)
                        (y.1 x.1 rbp tmp-ra.1)
                        (y.1 z.1 x.1 rbp tmp-ra.1)
                        (a.1 z.1 x.1 rbp tmp-ra.1)
                        (z.1 x.1 a.1 rbp tmp-ra.1)
                        (x.1 b.1 a.1 rbp tmp-ra.1)
                        (b.1 a.1 rbp tmp-ra.1)
                        (a.1 rbp tmp-ra.1)
                        (rax rbp tmp-ra.1)
                        (rax rbp tmp-ra.1)
                        (rbp rax)))
                      (call-undead ())
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
                         (begin (set! rdi 1000) (set! r15 tmp-ra.2) (jump L.f.2 rbp r15 rdi)))))))
