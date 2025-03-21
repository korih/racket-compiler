#lang racket

(require
  rackunit
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v7
  rackunit)

(provide assign-call-undead-variables)

;; asm-lang-v7/conflicts -> asm-lang-v7/pre-framed
;; Compiles Asm-pred-lang-v7/conflicts to Asm-pred-lang-v7/pre-framed by
;; pre-assigning all variables in the call-undead sets to frame variables
(define/contract (assign-call-undead-variables p)
  (-> asm-pred-lang-v7/conflicts? asm-pred-lang-v7/pre-framed?)

  ;; func is `(define ,label ,info ,tail)
  ;; interp. a function definition

  ;; (List-of asm-lang-v7/conflicts.loc) (Graph-of asm-lang-v7/conflicts.loc) (List-of (list aloc fvar)) -> (List-of (list aloc fvar))
  ;; interp. recursively assigns each variable in call-undead-set to the first
  ;; compatible frame variable without conflicts
  (define (graph-colouring call-undead-set conflicts-graph assignments)
    (if (null? call-undead-set)
        assignments
        (let* ([x (car call-undead-set)]
               [rest (cdr call-undead-set)]
               [assignments^ (graph-colouring rest conflicts-graph assignments)]
               ;; Collect vars assigned to each frame variable
               [frame-assignments (foldl (lambda (pair acc)
                                           (let* ([var (car pair)]
                                                  [fv (cadr pair)]
                                                  [existing (assoc fv acc)])
                                             (if existing
                                                 (cons (list fv (cons var (cadr existing))) (remove existing acc))
                                                 (cons (list fv (list var)) acc))))
                                         '()
                                         assignments^)]
               [conflict-vars (get-neighbors conflicts-graph x)])

          ;; Recursively find the first valid frame slot
          (define (find-fvar i)
            (let* ([candidate-fv (make-fvar i)]
                   [assigned-vars (let ([entry (assoc candidate-fv frame-assignments)])
                                    (if entry (cadr entry) '()))]
                   [incompatible?
                    (or (member candidate-fv (get-neighbors conflicts-graph x))
                        (ormap (lambda (y)
                                 (or (member y (get-neighbors conflicts-graph x))
                                     (member x (get-neighbors conflicts-graph y))))
                               assigned-vars))])
              (if incompatible?
                  (find-fvar (add1 i))
                  candidate-fv)))

          ;; Assign x to the first safe frame var
          (cons (list x (find-fvar 0)) assignments^))))

  ;; asm-lang-v7/conflicts.info -> asm-lang-v7/pre-framed.info
  (define (assign-call-undead-variables-info info)
    (define assignments (graph-colouring (info-ref info 'call-undead) (info-ref info 'conflicts) '()))
    (define locals
      (let ([local-variables (reverse (info-ref info 'locals))])
        (remove* (map car assignments) local-variables)))
    (info-set (info-set info 'locals locals) 'assignment assignments))

  ;; func -> func
  (define (assign-call-undead-variables-func func)
    (match func
      [`(define ,label ,info ,tail)
       `(define ,label ,(assign-call-undead-variables-info info) ,tail)]))

  (match p
    [`(module ,info ,funcs ... ,tail)
     `(module ,(assign-call-undead-variables-info info) ,@(map assign-call-undead-variables-func funcs) ,tail)]))

(module+ test
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
                         (begin (set! rdi 1000) (set! r15 tmp-ra.2) (jump L.f.2 rbp r15 rdi))))))
  (check-equal? (assign-call-undead-variables '(module
                                                   ((new-frames ())
                                                    (locals (tmp.88 tmp-ra.95 tmp.87))
                                                    (call-undead (tmp.87 tmp-ra.95))
                                                    (undead-out
                                                     ((rbp tmp-ra.95)
                                                      ((tmp-ra.95 rbp)
                                                       ((rbp rax tmp-ra.95)
                                                        ((rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
                                                       (rax rbp tmp-ra.95))
                                                      (rbp tmp-ra.95 tmp.87)
                                                      ((tmp-ra.95 tmp.87 rbp)
                                                       ((rbp rax tmp.87 tmp-ra.95)
                                                        ((rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
                                                       (rax tmp.87 tmp-ra.95 rbp))
                                                      (tmp.87 tmp.88 tmp-ra.95 rbp)
                                                      (tmp.88 tmp-ra.95 rdi rbp)
                                                      (tmp-ra.95 rsi rdi rbp)
                                                      (rsi rdi r15 rbp)
                                                      (rbp r15 rdi rsi)))
                                                    (conflicts
                                                     ((tmp.87 (tmp.88 rbp tmp-ra.95))
                                                      (tmp-ra.95 (rsi rdi tmp.88 tmp.87 rbp))
                                                      (tmp.88 (rdi tmp.87 tmp-ra.95 rbp))
                                                      (rbp (tmp.88 tmp.87 rax r15 rsi rdi tmp-ra.95))
                                                      (rdi (tmp.88 tmp-ra.95 r15 rsi rbp))
                                                      (rsi (tmp-ra.95 r15 rdi rbp))
                                                      (r15 (rsi rdi rbp))
                                                      (rax (rbp)))))
                                                 (define L.*.17
                                                   ((new-frames ())
                                                    (locals (tmp.80 tmp.78 tmp.41 tmp.82 tmp.42 tmp.81 tmp.79 tmp-ra.93))
                                                    (undead-out
                                                     ((rdi rsi rbp tmp-ra.93)
                                                      (rsi tmp.41 rbp tmp-ra.93)
                                                      (tmp.42 tmp.41 rbp tmp-ra.93)
                                                      (((((tmp.79 tmp.42 tmp.41 rbp tmp-ra.93)
                                                          (tmp.79 tmp.42 tmp.41 rbp tmp-ra.93)
                                                          (tmp.42 tmp.41 rbp tmp-ra.93))
                                                         (tmp.78 tmp.42 tmp.41 rbp tmp-ra.93)
                                                         (tmp.78 tmp.42 tmp.41 rbp tmp-ra.93))
                                                        (tmp.42 tmp.41 rbp tmp-ra.93))
                                                       (((((tmp.81 tmp.42 tmp.41 rbp tmp-ra.93)
                                                           (tmp.81 tmp.42 tmp.41 rbp tmp-ra.93)
                                                           (tmp.42 tmp.41 rbp tmp-ra.93))
                                                          (tmp.80 tmp.42 tmp.41 rbp tmp-ra.93)
                                                          (tmp.80 tmp.42 tmp.41 rbp tmp-ra.93))
                                                         (tmp.42 tmp.41 rbp tmp-ra.93))
                                                        ((tmp.82 tmp.41 rbp tmp-ra.93)
                                                         (tmp.41 tmp.82 rbp tmp-ra.93)
                                                         (tmp.82 rax rbp tmp-ra.93)
                                                         (rax rbp tmp-ra.93)
                                                         (rbp rax))
                                                        ((rax rbp tmp-ra.93) (rbp rax)))
                                                       ((rax rbp tmp-ra.93) (rbp rax)))))
                                                    (call-undead ())
                                                    (conflicts
                                                     ((tmp-ra.93
                                                       (rax tmp.82 tmp.80 tmp.81 tmp.78 tmp.79 tmp.42 tmp.41 rdi rsi rbp))
                                                      (tmp.79 (tmp.42 tmp.41 rbp tmp-ra.93))
                                                      (tmp.81 (tmp.41 tmp.42 rbp tmp-ra.93))
                                                      (tmp.42 (tmp.80 tmp.81 tmp.78 tmp.79 tmp.41 rbp tmp-ra.93))
                                                      (tmp.82 (rax tmp.41 rbp tmp-ra.93))
                                                      (tmp.41 (tmp.82 tmp.80 tmp.81 tmp.78 tmp.79 tmp.42 rsi rbp tmp-ra.93))
                                                      (tmp.78 (tmp.42 tmp.41 rbp tmp-ra.93))
                                                      (tmp.80 (tmp.42 tmp.41 rbp tmp-ra.93))
                                                      (rbp (rax tmp.82 tmp.80 tmp.81 tmp.78 tmp.79 tmp.42 tmp.41 tmp-ra.93))
                                                      (rsi (tmp.41 tmp-ra.93))
                                                      (rdi (tmp-ra.93))
                                                      (rax (tmp.82 rbp tmp-ra.93)))))
                                                   (begin
                                                     (set! tmp-ra.93 r15)
                                                     (set! tmp.41 rdi)
                                                     (set! tmp.42 rsi)
                                                     (if (begin
                                                           (if (begin
                                                                 (set! tmp.79 tmp.42)
                                                                 (set! tmp.79 (bitwise-and tmp.79 7))
                                                                 (= tmp.79 0))
                                                               (set! tmp.78 14)
                                                               (set! tmp.78 6))
                                                           (!= tmp.78 6))
                                                         (if (begin
                                                               (if (begin
                                                                     (set! tmp.81 tmp.41)
                                                                     (set! tmp.81 (bitwise-and tmp.81 7))
                                                                     (= tmp.81 0))
                                                                   (set! tmp.80 14)
                                                                   (set! tmp.80 6))
                                                               (!= tmp.80 6))
                                                             (begin
                                                               (set! tmp.82 tmp.42)
                                                               (set! tmp.82 (arithmetic-shift-right tmp.82 3))
                                                               (set! rax tmp.41)
                                                               (set! rax (* rax tmp.82))
                                                               (jump tmp-ra.93 rbp rax))
                                                             (begin (set! rax 318) (jump tmp-ra.93 rbp rax)))
                                                         (begin (set! rax 318) (jump tmp-ra.93 rbp rax)))))
                                                 (define L.+.16
                                                   ((new-frames ())
                                                    (locals (tmp.39 tmp.85 tmp-ra.94 tmp.84 tmp.86 tmp.40 tmp.83))
                                                    (undead-out
                                                     ((rdi rsi rbp tmp-ra.94)
                                                      (rsi tmp.39 rbp tmp-ra.94)
                                                      (tmp.39 tmp.40 rbp tmp-ra.94)
                                                      (((((tmp.84 tmp.39 tmp.40 rbp tmp-ra.94)
                                                          (tmp.84 tmp.39 tmp.40 rbp tmp-ra.94)
                                                          (tmp.39 tmp.40 rbp tmp-ra.94))
                                                         (tmp.83 tmp.39 tmp.40 rbp tmp-ra.94)
                                                         (tmp.83 tmp.39 tmp.40 rbp tmp-ra.94))
                                                        (tmp.39 tmp.40 rbp tmp-ra.94))
                                                       (((((tmp.86 tmp.39 tmp.40 rbp tmp-ra.94)
                                                           (tmp.86 tmp.39 tmp.40 rbp tmp-ra.94)
                                                           (tmp.39 tmp.40 rbp tmp-ra.94))
                                                          (tmp.85 tmp.39 tmp.40 rbp tmp-ra.94)
                                                          (tmp.85 tmp.39 tmp.40 rbp tmp-ra.94))
                                                         (tmp.39 tmp.40 rbp tmp-ra.94))
                                                        ((tmp.40 rax rbp tmp-ra.94) (rax rbp tmp-ra.94) (rbp rax))
                                                        ((rax rbp tmp-ra.94) (rbp rax)))
                                                       ((rax rbp tmp-ra.94) (rbp rax)))))
                                                    (call-undead ())
                                                    (conflicts
                                                     ((tmp.83 (tmp.39 tmp.40 rbp tmp-ra.94))
                                                      (tmp.40 (rax tmp.85 tmp.86 tmp.83 tmp.84 tmp.39 rbp tmp-ra.94))
                                                      (tmp.86 (tmp.39 tmp.40 rbp tmp-ra.94))
                                                      (tmp.84 (tmp.40 tmp.39 rbp tmp-ra.94))
                                                      (tmp-ra.94 (rax tmp.85 tmp.86 tmp.83 tmp.84 tmp.40 tmp.39 rdi rsi rbp))
                                                      (tmp.85 (tmp.39 tmp.40 rbp tmp-ra.94))
                                                      (tmp.39 (tmp.85 tmp.86 tmp.83 tmp.84 tmp.40 rsi rbp tmp-ra.94))
                                                      (rbp (rax tmp.85 tmp.86 tmp.83 tmp.84 tmp.40 tmp.39 tmp-ra.94))
                                                      (rsi (tmp.39 tmp-ra.94))
                                                      (rdi (tmp-ra.94))
                                                      (rax (tmp.40 rbp tmp-ra.94)))))
                                                   (begin
                                                     (set! tmp-ra.94 r15)
                                                     (set! tmp.39 rdi)
                                                     (set! tmp.40 rsi)
                                                     (if (begin
                                                           (if (begin
                                                                 (set! tmp.84 tmp.40)
                                                                 (set! tmp.84 (bitwise-and tmp.84 7))
                                                                 (= tmp.84 0))
                                                               (set! tmp.83 14)
                                                               (set! tmp.83 6))
                                                           (!= tmp.83 6))
                                                         (if (begin
                                                               (if (begin
                                                                     (set! tmp.86 tmp.39)
                                                                     (set! tmp.86 (bitwise-and tmp.86 7))
                                                                     (= tmp.86 0))
                                                                   (set! tmp.85 14)
                                                                   (set! tmp.85 6))
                                                               (!= tmp.85 6))
                                                             (begin
                                                               (set! rax tmp.39)
                                                               (set! rax (+ rax tmp.40))
                                                               (jump tmp-ra.94 rbp rax))
                                                             (begin (set! rax 574) (jump tmp-ra.94 rbp rax)))
                                                         (begin (set! rax 574) (jump tmp-ra.94 rbp rax)))))
                                                 (begin
                                                   (set! tmp-ra.95 r15)
                                                   (begin
                                                     (set! rbp (- rbp 16))
                                                     (return-point L.rp.19
                                                                   (begin
                                                                     (set! rdi 40)
                                                                     (set! rsi 48)
                                                                     (set! r15 L.rp.19)
                                                                     (jump L.+.16 rbp r15 rdi rsi)))
                                                     (set! rbp (+ rbp 16)))
                                                   (set! tmp.87 rax)
                                                   (begin
                                                     (set! rbp (- rbp 16))
                                                     (return-point L.rp.20
                                                                   (begin
                                                                     (set! rdi 32)
                                                                     (set! rsi 40)
                                                                     (set! r15 L.rp.20)
                                                                     (jump L.*.17 rbp r15 rdi rsi)))
                                                     (set! rbp (+ rbp 16)))
                                                   (set! tmp.88 rax)
                                                   (set! rdi tmp.87)
                                                   (set! rsi tmp.88)
                                                   (set! r15 tmp-ra.95)
                                                   (jump L.+.16 rbp r15 rdi rsi))))
                '(module
                     ((new-frames ())
                      (locals (tmp.88))
                      (call-undead (tmp.87 tmp-ra.95))
                      (undead-out
                       ((rbp tmp-ra.95)
                        ((tmp-ra.95 rbp)
                         ((rbp rax tmp-ra.95)
                          ((rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
                         (rax rbp tmp-ra.95))
                        (rbp tmp-ra.95 tmp.87)
                        ((tmp-ra.95 tmp.87 rbp)
                         ((rbp rax tmp.87 tmp-ra.95)
                          ((rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
                         (rax tmp.87 tmp-ra.95 rbp))
                        (tmp.87 tmp.88 tmp-ra.95 rbp)
                        (tmp.88 tmp-ra.95 rdi rbp)
                        (tmp-ra.95 rsi rdi rbp)
                        (rsi rdi r15 rbp)
                        (rbp r15 rdi rsi)))
                      (conflicts
                       ((tmp.87 (tmp.88 rbp tmp-ra.95))
                        (tmp-ra.95 (rsi rdi tmp.88 tmp.87 rbp))
                        (tmp.88 (rdi tmp.87 tmp-ra.95 rbp))
                        (rbp (tmp.88 tmp.87 rax r15 rsi rdi tmp-ra.95))
                        (rdi (tmp.88 tmp-ra.95 r15 rsi rbp))
                        (rsi (tmp-ra.95 r15 rdi rbp))
                        (r15 (rsi rdi rbp))
                        (rax (rbp))))
                      (assignment ((tmp.87 fv1) (tmp-ra.95 fv0))))
                   (define L.*.17
                     ((new-frames ())
                      (locals (tmp-ra.93 tmp.79 tmp.81 tmp.42 tmp.82 tmp.41 tmp.78 tmp.80))
                      (undead-out
                       ((rdi rsi rbp tmp-ra.93)
                        (rsi tmp.41 rbp tmp-ra.93)
                        (tmp.42 tmp.41 rbp tmp-ra.93)
                        (((((tmp.79 tmp.42 tmp.41 rbp tmp-ra.93)
                            (tmp.79 tmp.42 tmp.41 rbp tmp-ra.93)
                            (tmp.42 tmp.41 rbp tmp-ra.93))
                           (tmp.78 tmp.42 tmp.41 rbp tmp-ra.93)
                           (tmp.78 tmp.42 tmp.41 rbp tmp-ra.93))
                          (tmp.42 tmp.41 rbp tmp-ra.93))
                         (((((tmp.81 tmp.42 tmp.41 rbp tmp-ra.93)
                             (tmp.81 tmp.42 tmp.41 rbp tmp-ra.93)
                             (tmp.42 tmp.41 rbp tmp-ra.93))
                            (tmp.80 tmp.42 tmp.41 rbp tmp-ra.93)
                            (tmp.80 tmp.42 tmp.41 rbp tmp-ra.93))
                           (tmp.42 tmp.41 rbp tmp-ra.93))
                          ((tmp.82 tmp.41 rbp tmp-ra.93)
                           (tmp.41 tmp.82 rbp tmp-ra.93)
                           (tmp.82 rax rbp tmp-ra.93)
                           (rax rbp tmp-ra.93)
                           (rbp rax))
                          ((rax rbp tmp-ra.93) (rbp rax)))
                         ((rax rbp tmp-ra.93) (rbp rax)))))
                      (call-undead ())
                      (conflicts
                       ((tmp-ra.93
                         (rax tmp.82 tmp.80 tmp.81 tmp.78 tmp.79 tmp.42 tmp.41 rdi rsi rbp))
                        (tmp.79 (tmp.42 tmp.41 rbp tmp-ra.93))
                        (tmp.81 (tmp.41 tmp.42 rbp tmp-ra.93))
                        (tmp.42 (tmp.80 tmp.81 tmp.78 tmp.79 tmp.41 rbp tmp-ra.93))
                        (tmp.82 (rax tmp.41 rbp tmp-ra.93))
                        (tmp.41 (tmp.82 tmp.80 tmp.81 tmp.78 tmp.79 tmp.42 rsi rbp tmp-ra.93))
                        (tmp.78 (tmp.42 tmp.41 rbp tmp-ra.93))
                        (tmp.80 (tmp.42 tmp.41 rbp tmp-ra.93))
                        (rbp (rax tmp.82 tmp.80 tmp.81 tmp.78 tmp.79 tmp.42 tmp.41 tmp-ra.93))
                        (rsi (tmp.41 tmp-ra.93))
                        (rdi (tmp-ra.93))
                        (rax (tmp.82 rbp tmp-ra.93))))
                      (assignment ()))
                     (begin
                       (set! tmp-ra.93 r15)
                       (set! tmp.41 rdi)
                       (set! tmp.42 rsi)
                       (if (begin
                             (if (begin
                                   (set! tmp.79 tmp.42)
                                   (set! tmp.79 (bitwise-and tmp.79 7))
                                   (= tmp.79 0))
                                 (set! tmp.78 14)
                                 (set! tmp.78 6))
                             (!= tmp.78 6))
                           (if (begin
                                 (if (begin
                                       (set! tmp.81 tmp.41)
                                       (set! tmp.81 (bitwise-and tmp.81 7))
                                       (= tmp.81 0))
                                     (set! tmp.80 14)
                                     (set! tmp.80 6))
                                 (!= tmp.80 6))
                               (begin
                                 (set! tmp.82 tmp.42)
                                 (set! tmp.82 (arithmetic-shift-right tmp.82 3))
                                 (set! rax tmp.41)
                                 (set! rax (* rax tmp.82))
                                 (jump tmp-ra.93 rbp rax))
                               (begin (set! rax 318) (jump tmp-ra.93 rbp rax)))
                           (begin (set! rax 318) (jump tmp-ra.93 rbp rax)))))
                   (define L.+.16
                     ((new-frames ())
                      (locals (tmp.83 tmp.40 tmp.86 tmp.84 tmp-ra.94 tmp.85 tmp.39))
                      (undead-out
                       ((rdi rsi rbp tmp-ra.94)
                        (rsi tmp.39 rbp tmp-ra.94)
                        (tmp.39 tmp.40 rbp tmp-ra.94)
                        (((((tmp.84 tmp.39 tmp.40 rbp tmp-ra.94)
                            (tmp.84 tmp.39 tmp.40 rbp tmp-ra.94)
                            (tmp.39 tmp.40 rbp tmp-ra.94))
                           (tmp.83 tmp.39 tmp.40 rbp tmp-ra.94)
                           (tmp.83 tmp.39 tmp.40 rbp tmp-ra.94))
                          (tmp.39 tmp.40 rbp tmp-ra.94))
                         (((((tmp.86 tmp.39 tmp.40 rbp tmp-ra.94)
                             (tmp.86 tmp.39 tmp.40 rbp tmp-ra.94)
                             (tmp.39 tmp.40 rbp tmp-ra.94))
                            (tmp.85 tmp.39 tmp.40 rbp tmp-ra.94)
                            (tmp.85 tmp.39 tmp.40 rbp tmp-ra.94))
                           (tmp.39 tmp.40 rbp tmp-ra.94))
                          ((tmp.40 rax rbp tmp-ra.94) (rax rbp tmp-ra.94) (rbp rax))
                          ((rax rbp tmp-ra.94) (rbp rax)))
                         ((rax rbp tmp-ra.94) (rbp rax)))))
                      (call-undead ())
                      (conflicts
                       ((tmp.83 (tmp.39 tmp.40 rbp tmp-ra.94))
                        (tmp.40 (rax tmp.85 tmp.86 tmp.83 tmp.84 tmp.39 rbp tmp-ra.94))
                        (tmp.86 (tmp.39 tmp.40 rbp tmp-ra.94))
                        (tmp.84 (tmp.40 tmp.39 rbp tmp-ra.94))
                        (tmp-ra.94 (rax tmp.85 tmp.86 tmp.83 tmp.84 tmp.40 tmp.39 rdi rsi rbp))
                        (tmp.85 (tmp.39 tmp.40 rbp tmp-ra.94))
                        (tmp.39 (tmp.85 tmp.86 tmp.83 tmp.84 tmp.40 rsi rbp tmp-ra.94))
                        (rbp (rax tmp.85 tmp.86 tmp.83 tmp.84 tmp.40 tmp.39 tmp-ra.94))
                        (rsi (tmp.39 tmp-ra.94))
                        (rdi (tmp-ra.94))
                        (rax (tmp.40 rbp tmp-ra.94))))
                      (assignment ()))
                     (begin
                       (set! tmp-ra.94 r15)
                       (set! tmp.39 rdi)
                       (set! tmp.40 rsi)
                       (if (begin
                             (if (begin
                                   (set! tmp.84 tmp.40)
                                   (set! tmp.84 (bitwise-and tmp.84 7))
                                   (= tmp.84 0))
                                 (set! tmp.83 14)
                                 (set! tmp.83 6))
                             (!= tmp.83 6))
                           (if (begin
                                 (if (begin
                                       (set! tmp.86 tmp.39)
                                       (set! tmp.86 (bitwise-and tmp.86 7))
                                       (= tmp.86 0))
                                     (set! tmp.85 14)
                                     (set! tmp.85 6))
                                 (!= tmp.85 6))
                               (begin
                                 (set! rax tmp.39)
                                 (set! rax (+ rax tmp.40))
                                 (jump tmp-ra.94 rbp rax))
                               (begin (set! rax 574) (jump tmp-ra.94 rbp rax)))
                           (begin (set! rax 574) (jump tmp-ra.94 rbp rax)))))
                   (begin
                     (set! tmp-ra.95 r15)
                     (begin
                       (set! rbp (- rbp 16))
                       (return-point L.rp.19
                                     (begin
                                       (set! rdi 40)
                                       (set! rsi 48)
                                       (set! r15 L.rp.19)
                                       (jump L.+.16 rbp r15 rdi rsi)))
                       (set! rbp (+ rbp 16)))
                     (set! tmp.87 rax)
                     (begin
                       (set! rbp (- rbp 16))
                       (return-point L.rp.20
                                     (begin
                                       (set! rdi 32)
                                       (set! rsi 40)
                                       (set! r15 L.rp.20)
                                       (jump L.*.17 rbp r15 rdi rsi)))
                       (set! rbp (+ rbp 16)))
                     (set! tmp.88 rax)
                     (set! rdi tmp.87)
                     (set! rsi tmp.88)
                     (set! r15 tmp-ra.95)
                     (jump L.+.16 rbp r15 rdi rsi))))
  (check-equal? (assign-call-undead-variables '(module
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
                                                      (r15 (rbp rdi rsi rdx rcx r8 r9 fv0)))))
                                                 (define L.+.31
                                                   ((new-frames ())
                                                    (locals (tmp.183 tmp.96 tmp.185 tmp.184 tmp.97 tmp-ra.232 tmp.186))
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
                                                     ((tmp.186 (tmp.96 rbp tmp-ra.232 tmp.97))
                                                      (tmp-ra.232
                                                       (tmp.97 tmp.96 rbp rsi rdi tmp.184 tmp.183 tmp.186 tmp.185 rax))
                                                      (tmp.97 (rbp tmp-ra.232 tmp.96 tmp.184 tmp.183 tmp.186 tmp.185 rax))
                                                      (tmp.184 (tmp.97 rbp tmp-ra.232 tmp.96))
                                                      (tmp.185 (rbp tmp-ra.232 tmp.97 tmp.96))
                                                      (tmp.96 (tmp.97 rbp tmp-ra.232 rsi tmp.184 tmp.183 tmp.186 tmp.185))
                                                      (tmp.183 (rbp tmp-ra.232 tmp.97 tmp.96))
                                                      (rax (tmp.97 rbp tmp-ra.232))
                                                      (rbp (tmp.97 tmp.96 tmp-ra.232 tmp.184 tmp.183 tmp.186 tmp.185 rax))
                                                      (rdi (tmp-ra.232))
                                                      (rsi (tmp.96 tmp-ra.232)))))
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
                                                    (locals
                                                     (b.20 f.24 g.25 a.19 nfv.234 d.22 nfv.235 e.23 tmp-ra.233 tmp.187 c.21))
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
                                                     ((c.21
                                                       (rsi rdi g.25 f.24 e.23 d.22 rbp tmp-ra.233 a.19 b.20 fv0 r9 r8 rcx))
                                                      (tmp.187 (rdi rbp tmp-ra.233))
                                                      (tmp-ra.233
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
                                                      (e.23
                                                       (rcx rdx rsi rdi g.25 f.24 rbp tmp-ra.233 a.19 b.20 c.21 d.22 fv0 r9))
                                                      (nfv.235 (r15 rbp rdi rsi rdx rcx r8 r9 nfv.234))
                                                      (d.22
                                                       (rdx rsi rdi g.25 f.24 e.23 rbp tmp-ra.233 a.19 b.20 c.21 fv0 r9 r8))
                                                      (nfv.234 (r15 nfv.235 rbp rdi rsi rdx rcx r8 r9))
                                                      (a.19
                                                       (g.25 f.24 e.23 d.22 c.21 b.20 rbp tmp-ra.233 fv0 r9 r8 rcx rdx rsi))
                                                      (g.25
                                                       (r9 r8 rcx rdx rsi rdi rbp tmp-ra.233 a.19 b.20 c.21 d.22 e.23 f.24))
                                                      (f.24
                                                       (r8 rcx rdx rsi rdi g.25 rbp tmp-ra.233 a.19 b.20 c.21 d.22 e.23 fv0))
                                                      (b.20
                                                       (rdi g.25 f.24 e.23 d.22 c.21 rbp tmp-ra.233 a.19 fv0 r9 r8 rcx rdx))
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
                                                      (r15 (rbp rdi rsi rdx rcx r8 r9 nfv.234 nfv.235)))))
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
                                                    (locals (g.32 c.28 tmp-ra.236 h.33 f.31 e.30 b.27 d.29 a.26))
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
                                                     ((a.26
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
                                                      (tmp-ra.236
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
                                                      (r15 (rbp rdi rsi rdx rcx r8 r9 fv0 fv1 fv2)))))
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
                                                    (locals
                                                     (tmp-ra.237
                                                      d.37
                                                      r5.47
                                                      f.39
                                                      r6.48
                                                      r3.45
                                                      r4.46
                                                      a.34
                                                      j.42
                                                      g.40
                                                      r2.44
                                                      e.38
                                                      b.35
                                                      h.41
                                                      c.36
                                                      r7.49
                                                      r1.43))
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
                                                     ((r1.43 (rbp tmp-ra.237 j.42 h.41 g.40 f.39 e.38 d.37 c.36))
                                                      (r7.49 (rbp tmp-ra.237 j.42))
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
                                                      (r2.44 (rbp tmp-ra.237 j.42 h.41 g.40 f.39 e.38 d.37))
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
                                                      (r4.46 (rbp tmp-ra.237 j.42 h.41 g.40 f.39))
                                                      (r3.45 (rbp tmp-ra.237 j.42 h.41 g.40 f.39 e.38))
                                                      (r6.48 (rbp tmp-ra.237 j.42 h.41))
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
                                                      (r5.47 (rbp tmp-ra.237 j.42 h.41 g.40))
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
                                                      (tmp-ra.237
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
                                                      (r15 (rbp rdi rsi)))))
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
                      (locals (tmp.186 tmp-ra.232 tmp.97 tmp.184 tmp.185 tmp.96 tmp.183))
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
                       ((tmp.186 (tmp.96 rbp tmp-ra.232 tmp.97))
                        (tmp-ra.232
                         (tmp.97 tmp.96 rbp rsi rdi tmp.184 tmp.183 tmp.186 tmp.185 rax))
                        (tmp.97 (rbp tmp-ra.232 tmp.96 tmp.184 tmp.183 tmp.186 tmp.185 rax))
                        (tmp.184 (tmp.97 rbp tmp-ra.232 tmp.96))
                        (tmp.185 (rbp tmp-ra.232 tmp.97 tmp.96))
                        (tmp.96 (tmp.97 rbp tmp-ra.232 rsi tmp.184 tmp.183 tmp.186 tmp.185))
                        (tmp.183 (rbp tmp-ra.232 tmp.97 tmp.96))
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
                      (locals (c.21 tmp.187 e.23 nfv.235 d.22 nfv.234 a.19 g.25 f.24 b.20))
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
                       ((c.21
                         (rsi rdi g.25 f.24 e.23 d.22 rbp tmp-ra.233 a.19 b.20 fv0 r9 r8 rcx))
                        (tmp.187 (rdi rbp tmp-ra.233))
                        (tmp-ra.233
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
                        (e.23
                         (rcx rdx rsi rdi g.25 f.24 rbp tmp-ra.233 a.19 b.20 c.21 d.22 fv0 r9))
                        (nfv.235 (r15 rbp rdi rsi rdx rcx r8 r9 nfv.234))
                        (d.22
                         (rdx rsi rdi g.25 f.24 e.23 rbp tmp-ra.233 a.19 b.20 c.21 fv0 r9 r8))
                        (nfv.234 (r15 nfv.235 rbp rdi rsi rdx rcx r8 r9))
                        (a.19
                         (g.25 f.24 e.23 d.22 c.21 b.20 rbp tmp-ra.233 fv0 r9 r8 rcx rdx rsi))
                        (g.25
                         (r9 r8 rcx rdx rsi rdi rbp tmp-ra.233 a.19 b.20 c.21 d.22 e.23 f.24))
                        (f.24
                         (r8 rcx rdx rsi rdi g.25 rbp tmp-ra.233 a.19 b.20 c.21 d.22 e.23 fv0))
                        (b.20
                         (rdi g.25 f.24 e.23 d.22 c.21 rbp tmp-ra.233 a.19 fv0 r9 r8 rcx rdx))
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
                      (locals (a.26 d.29 b.27 e.30 f.31 h.33 tmp-ra.236 c.28 g.32))
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
                       ((a.26
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
                        (tmp-ra.236
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
                      (locals (r1.43 r7.49 b.35 r2.44 a.34 r4.46 r3.45 r6.48 r5.47))
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
                       ((r1.43 (rbp tmp-ra.237 j.42 h.41 g.40 f.39 e.38 d.37 c.36))
                        (r7.49 (rbp tmp-ra.237 j.42))
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
                        (r2.44 (rbp tmp-ra.237 j.42 h.41 g.40 f.39 e.38 d.37))
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
                        (r4.46 (rbp tmp-ra.237 j.42 h.41 g.40 f.39))
                        (r3.45 (rbp tmp-ra.237 j.42 h.41 g.40 f.39 e.38))
                        (r6.48 (rbp tmp-ra.237 j.42 h.41))
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
                        (r5.47 (rbp tmp-ra.237 j.42 h.41 g.40))
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
                        (tmp-ra.237
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
                       ((c.36 fv8)
                        (d.37 fv7)
                        (e.38 fv6)
                        (f.39 fv5)
                        (g.40 fv4)
                        (h.41 fv1)
                        (j.42 fv0)
                        (tmp-ra.237 fv3))))
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
                     (jump L.F.6 rbp r15 rdi rsi rdx rcx r8 r9 fv0)))))
