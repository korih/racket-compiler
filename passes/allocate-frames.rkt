#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v7)

(provide allocate-frames)

;; asm-pred-lang-v7/pre-framed -> asm-pred-lang-v7/framed
;; Allocated frames for each non-tail call, and assign
;; all new-frame variables to frame variables in the new frame.
(define/contract (allocate-frames p)
  (-> asm-pred-lang-v7/pre-framed? asm-pred-lang-v7/framed?)

  ;; info -> info
  ;; remove new-frames and call-undead while adding new assignments
  (define (compile-info i)
    ;; new frames for assignment
    (define new-frames (if (empty? (info-ref i 'new-frames))
                           '()
                           (first (info-ref i 'new-frames))))
    (define locals^ (remove* new-frames (info-ref i 'locals)))
    ;; existing assignments
    (define assignments (map cadr (info-ref i 'assignment)))
    ;; all variables that could conflict with assignments
    (define conflicts (map car (info-ref i 'conflicts)))
    ;; all frame variables
    (define conflict-frame-variables (append assignments conflicts))
    (define new-assignments (for/fold ([assignments^ (info-ref i 'assignment)])
                                      ([frame new-frames])
                              (define frame-assignment (for/or ([i (in-naturals)])
                                                         (define fvar (string->symbol (format "fv~a" i)))
                                                         (cond
                                                           [(member fvar conflict-frame-variables) #f]
                                                           [else (set! conflict-frame-variables (cons fvar conflict-frame-variables))
                                                                 `(,frame ,fvar)])))
                              (append assignments^ `(,frame-assignment))))

    (define info^ i)
    (set! info^ (info-remove info^ 'undead-out))
    (set! info^ (info-remove info^ 'call-undead))
    (set! info^ (info-remove info^ 'new-frames))
    (set! info^ (info-set info^ 'locals (reverse locals^))) ;reverse so it matches tests
    (info-set info^ 'assignment new-assignments))

  ;; funs -> funs
  ;; compile the function definition
  (define (compile-fun f)
    (match f
      [`(define ,label ,info ,tail) (define info^ (compile-info info))
                                    (define tail^ (compile-tail tail info^))
                                    `(define ,label ,info^ ,tail^)]))

  ;; asm-pred-lang-v7/pre-framed.tail info -> asm-pred-lang-v7/framed.tail
  ;; compile the tails
  (define (compile-tail t info)
    (match t
      [`(jump ,trg ,locs ...) `(jump ,trg ,@locs)]
      [`(begin ,effects ... ,tail) (define effects^ (for/fold ([compiled-effects '()])
                                                              ([effect effects])
                                                      (append compiled-effects (compile-effect effect info))))

                                   (define tail^ (compile-tail tail info))
                                   `(begin ,@effects^ ,tail^)]
      [`(if ,pred ,t1 ,t2) (define pred^ (compile-pred pred info))
                           (define t1^ (compile-tail t1 info))
                           (define t2^ (compile-tail t2 info))
                           `(if ,pred^ ,t1^ ,t2^)]))

  ;; asm-pred-lang-v7/pre-framed.effect -> asm-pred-lang-v7/framed.effect
  ;; compile the effects, looking for return-points to set stack
  (define (compile-effect e info)
    (define frame-size (* 8 (length (info-ref info 'assignment))))
    (match e
      [`(begin ,effects ...) (define effects^ (for/fold ([compiled-effects '()])
                                                        ([effect effects])
                                                (append compiled-effects (compile-effect effect info))))
                             `((begin ,@effects^))]

      [`(return-point ,label ,tail) (define tail^ (compile-tail tail info))
                                    (define fbp (current-frame-base-pointer-register))
                                    `((begin
                                        (set! ,fbp (- ,fbp ,frame-size))
                                        (return-point ,label ,tail^)
                                        (set! ,fbp (+ ,fbp ,frame-size))))]

      [`(if ,pred ,e1 ,e2) (define pred^ (compile-pred pred info))
                           (define e1^ (compile-effect e1 info))
                           (define e2^ (compile-effect e2 info))
                           `((if ,pred^ ,@e1^ ,@e2^))]
      ;; no other cases that can have return-point
      [_ `(,e)]))

  ;; asm-pred-lang-v7/pre-framed.pred -> asm-pred-lang-v7/framed.pred
  ;; compile the preds
  (define (compile-pred p info)
    (match p
      [`(begin ,effects ... ,pred) (define effects^ (for/fold ([compiled-effects '()])
                                                              ([effect effects])
                                                      (append compiled-effects (compile-effect effect info))))
                                   (define pred^ (compile-pred pred info))
                                   `(begin ,@effects^ ,pred^)]
      ;; all other cases can't have an effect
      [_ `,p]))

  (match p
    [`(module ,info ,funs ... ,tail) (define info^ (compile-info info))
                                     (define funs^ (map compile-fun funs))
                                     (define tail^ (compile-tail tail info))
                                     `(module ,info^ ,@funs^ ,tail^)]))

(module+ test
  (require rackunit)

  (check-equal? (allocate-frames '(module
                                      ((new-frames ())
                                       (locals (tmp-ra.10))
                                       (call-undead ())
                                       (undead-out
                                        ((tmp-ra.10 rbp)
                                         (tmp-ra.10 fv1 rbp)
                                         (tmp-ra.10 fv1 fv0 rbp)
                                         (fv1 fv0 r15 rbp)
                                         (fv1 fv0 r15 rbp)))
                                       (conflicts
                                        ((tmp-ra.10 (fv0 fv1 rbp))
                                         (rbp (r15 fv0 fv1 tmp-ra.10))
                                         (fv1 (r15 fv0 rbp tmp-ra.10))
                                         (fv0 (r15 rbp fv1 tmp-ra.10))
                                         (r15 (rbp fv0 fv1))))
                                       (assignment ()))
                                    (define L.swap.1
                                      ((new-frames ((nfv.8 nfv.9)))
                                       (locals (y.2 x.1 z.3 nfv.9 nfv.8))
                                       (undead-out
                                        ((fv0 fv1 tmp-ra.7 rbp)
                                         (fv1 x.1 tmp-ra.7 rbp)
                                         (y.2 x.1 tmp-ra.7 rbp)
                                         ((y.2 x.1 tmp-ra.7 rbp)
                                          ((tmp-ra.7 rax rbp) (rax rbp))
                                          (((rax tmp-ra.7 rbp)
                                            ((y.2 nfv.9 rbp)
                                             (nfv.9 nfv.8 rbp)
                                             (nfv.9 nfv.8 r15 rbp)
                                             (nfv.9 nfv.8 r15 rbp)))
                                           (z.3 tmp-ra.7 rbp)
                                           (tmp-ra.7 rax rbp)
                                           (rax rbp)))))
                                       (call-undead (tmp-ra.7))
                                       (conflicts
                                        ((y.2 (rbp tmp-ra.7 x.1 nfv.9))
                                         (x.1 (y.2 rbp tmp-ra.7 fv1))
                                         (tmp-ra.7 (y.2 x.1 rbp fv1 fv0 rax z.3))
                                         (z.3 (rbp tmp-ra.7))
                                         (nfv.9 (r15 nfv.8 rbp y.2))
                                         (nfv.8 (r15 rbp nfv.9))
                                         (rbp (y.2 x.1 tmp-ra.7 rax z.3 r15 nfv.8 nfv.9))
                                         (r15 (rbp nfv.8 nfv.9))
                                         (rax (rbp tmp-ra.7))
                                         (fv0 (tmp-ra.7))
                                         (fv1 (x.1 tmp-ra.7))))
                                       (assignment ((tmp-ra.7 fv2))))
                                      (begin
                                        (set! tmp-ra.7 r15)
                                        (set! x.1 fv0)
                                        (set! y.2 fv1)
                                        (if (< y.2 x.1)
                                            (begin (set! rax x.1) (jump tmp-ra.7 rbp rax))
                                            (begin
                                              (return-point L.rp.3
                                                            (begin
                                                              (set! nfv.9 x.1)
                                                              (set! nfv.8 y.2)
                                                              (set! r15 L.rp.3)
                                                              (jump L.swap.1 rbp r15 nfv.8 nfv.9)))
                                              (set! z.3 rax)
                                              (set! rax z.3)
                                              (jump tmp-ra.7 rbp rax)))))
                                    (begin
                                      (set! tmp-ra.10 r15)
                                      (set! fv1 2)
                                      (set! fv0 1)
                                      (set! r15 tmp-ra.10)
                                      (jump L.swap.1 rbp r15 fv0 fv1))))
                '(module
                     ((locals (tmp-ra.10))
                      (conflicts
                       ((tmp-ra.10 (fv0 fv1 rbp))
                        (rbp (r15 fv0 fv1 tmp-ra.10))
                        (fv1 (r15 fv0 rbp tmp-ra.10))
                        (fv0 (r15 rbp fv1 tmp-ra.10))
                        (r15 (rbp fv0 fv1))))
                      (assignment ()))
                   (define L.swap.1
                     ((locals (z.3 x.1 y.2))
                      (conflicts
                       ((y.2 (rbp tmp-ra.7 x.1 nfv.9))
                        (x.1 (y.2 rbp tmp-ra.7 fv1))
                        (tmp-ra.7 (y.2 x.1 rbp fv1 fv0 rax z.3))
                        (z.3 (rbp tmp-ra.7))
                        (nfv.9 (r15 nfv.8 rbp y.2))
                        (nfv.8 (r15 rbp nfv.9))
                        (rbp (y.2 x.1 tmp-ra.7 rax z.3 r15 nfv.8 nfv.9))
                        (r15 (rbp nfv.8 nfv.9))
                        (rax (rbp tmp-ra.7))
                        (fv0 (tmp-ra.7))
                        (fv1 (x.1 tmp-ra.7))))
                      (assignment ((tmp-ra.7 fv2) (nfv.8 fv3) (nfv.9 fv4))))
                     (begin
                       (set! tmp-ra.7 r15)
                       (set! x.1 fv0)
                       (set! y.2 fv1)
                       (if (< y.2 x.1)
                           (begin (set! rax x.1) (jump tmp-ra.7 rbp rax))
                           (begin
                             (begin
                               (set! rbp (- rbp 24))
                               (return-point L.rp.3
                                             (begin
                                               (set! nfv.9 x.1)
                                               (set! nfv.8 y.2)
                                               (set! r15 L.rp.3)
                                               (jump L.swap.1 rbp r15 nfv.8 nfv.9)))
                               (set! rbp (+ rbp 24)))
                             (set! z.3 rax)
                             (set! rax z.3)
                             (jump tmp-ra.7 rbp rax)))))
                   (begin
                     (set! tmp-ra.10 r15)
                     (set! fv1 2)
                     (set! fv0 1)
                     (set! r15 tmp-ra.10)
                     (jump L.swap.1 rbp r15 fv0 fv1))))

  (check-equal? (allocate-frames '(module
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
                '(module
                     ((locals (x.1 y.2))
                      (conflicts ((y.2 (r15)) (x.1 (r15)) (r15 (rax y.2 x.1)) (rax (r15))))
                      (assignment ()))
                   (begin
                     (set! x.1 3)
                     (set! y.2 x.1)
                     (if (> y.2 x.1)
                         (begin (set! rax x.1) (jump r15))
                         (begin (set! rax y.2) (jump r15))))))

  (check-equal? (allocate-frames '(module
                                      ((locals (y.2 x.1))
                                       (call-undead ())
                                       (new-frames (()))
                                       (undead-out ((x.1 r15) (x.1 y.2 r15) ((x.1 y.2 r15) ((r15) ()) ((r15) ()))))
                                       (conflicts ((y.2 (r15)) (x.1 (r15)) (r15 (rax y.2 x.1)) (rax (r15))))
                                       (assignment ()))
                                    (begin
                                      (set! x.1 3)
                                      (set! y.2 x.1)
                                      (if (begin
                                            (set! x.1 4)
                                            (set! y.2 3)
                                            (> x.1 y.2))
                                          (set! x.1 5)
                                          (set! x.1 10))
                                      (if (> y.2 x.1)
                                          (begin (set! rax x.1) (jump r15))
                                          (begin (set! rax y.2) (jump r15))))))

                '(module
                     ((locals (x.1 y.2))
                      (conflicts ((y.2 (r15)) (x.1 (r15)) (r15 (rax y.2 x.1)) (rax (r15))))
                      (assignment ()))
                   (begin
                     (set! x.1 3)
                     (set! y.2 x.1)
                     (if (begin (set! x.1 4) (set! y.2 3) (> x.1 y.2))
                         (set! x.1 5)
                         (set! x.1 10))
                     (if (> y.2 x.1)
                         (begin (set! rax x.1) (jump r15))
                         (begin (set! rax y.2) (jump r15))))))

  (check-equal? (allocate-frames '(module
                                      ((locals (y.2 x.1))
                                       (call-undead ())
                                       (new-frames (()))
                                       (undead-out ((x.1 r15) (x.1 y.2 r15) ((x.1 y.2 r15) ((r15) ()) ((r15) ()))))
                                       (conflicts ((y.2 (r15)) (x.1 (r15)) (r15 (rax y.2 x.1)) (rax (r15))))
                                       (assignment ()))
                                    (begin
                                      (set! x.1 3)
                                      (set! y.2 x.1)
                                      (if (begin
                                            (set! x.1 4)
                                            (return-point L.rp.1 (jump L.haha.2))
                                            (> x.1 y.2))
                                          (set! x.1 5)
                                          (set! x.1 10))
                                      (if (> y.2 x.1)
                                          (begin (set! rax x.1) (jump r15))
                                          (begin (set! rax y.2) (jump r15))))))

                '(module
                     ((locals (x.1 y.2))
                      (conflicts ((y.2 (r15)) (x.1 (r15)) (r15 (rax y.2 x.1)) (rax (r15))))
                      (assignment ()))
                   (begin
                     (set! x.1 3)
                     (set! y.2 x.1)
                     (if (begin
                           (set! x.1 4)
                           (begin
                             (set! rbp (- rbp 0))
                             (return-point L.rp.1 (jump L.haha.2))
                             (set! rbp (+ rbp 0)))
                           (> x.1 y.2))
                         (set! x.1 5)
                         (set! x.1 10))
                     (if (> y.2 x.1)
                         (begin (set! rax x.1) (jump r15))
                         (begin (set! rax y.2) (jump r15))))))

  (check-equal? (allocate-frames '(module ((new-frames ())
                                           (locals (tmp-ra.137))
                                           (call-undead ())
                                           (undead-out
                                            ((tmp-ra.137 rbp)
                                             (tmp-ra.137 rdi rbp)
                                             (rdi r15 rbp)
                                             (rbp r15 rdi)))
                                           (conflicts
                                            ((tmp-ra.137 (rdi rbp))
                                             (rbp (r15 rdi tmp-ra.137))
                                             (rdi (r15 tmp-ra.137 rbp))
                                             (r15 (rdi rbp))))
                                           (assignment ()))
                                    (define L.fact.21 ((new-frames (()))
                                                       (locals (y.57 z.56))
                                                       (undead-out ((rdi rbp tmp-ra.136)
                                                                    (x.55 rbp tmp-ra.136)
                                                                    ((x.55 rbp tmp-ra.136)
                                                                     ((rax rbp tmp-ra.136) (rbp rax))
                                                                     ((z.56 tmp-ra.136 x.55 rbp)
                                                                      (tmp-ra.136 x.55 z.56 rbp)
                                                                      ((rax x.55 rbp tmp-ra.136)
                                                                       ((rdi rbp)
                                                                        (rdi r15 rbp)
                                                                        (rbp r15 rdi)))
                                                                      (x.55 y.57 rbp tmp-ra.136)
                                                                      (y.57 rax rbp tmp-ra.136)
                                                                      (rax rbp tmp-ra.136) (rbp rax)))))
                                                       (call-undead (x.55 tmp-ra.136))
                                                       (conflicts ((y.57 (rax x.55 rbp tmp-ra.136))
                                                                   (x.55 (y.57 z.56 rbp tmp-ra.136))
                                                                   (z.56 (x.55 tmp-ra.136 rbp))
                                                                   (tmp-ra.136 (y.57 z.56 rax x.55 rdi rbp))
                                                                   (rbp (y.57 r15 rdi z.56 rax x.55 tmp-ra.136))
                                                                   (rdi (r15 rbp tmp-ra.136))
                                                                   (rax (y.57 rbp tmp-ra.136))
                                                                   (r15 (rdi rbp))))
                                                       (assignment ((tmp-ra.136 fv0) (x.55 fv0))))
                                      (begin
                                        (set! tmp-ra.136 r15)
                                        (set! x.55 rdi)
                                        (if (= x.55 0)
                                            (begin
                                              (set! rax 1)
                                              (jump tmp-ra.136 rbp rax))
                                            (begin
                                              (set! z.56 x.55)
                                              (set! z.56 (+ z.56 -1))
                                              (return-point L.rp.32
                                                            (begin (set! rdi z.56)
                                                                   (set! r15 L.rp.32)
                                                                   (jump L.fact.21 rbp r15 rdi)))
                                              (set! y.57 rax)
                                              (set! rax x.55)
                                              (set! rax (* rax y.57))
                                              (jump tmp-ra.136 rbp rax)))))
                                    (begin
                                      (set! tmp-ra.137 r15)
                                      (set! rdi 10)
                                      (set! r15 tmp-ra.137)
                                      (jump L.fact.21 rbp r15 rdi))))
                '(module
                     ((locals (tmp-ra.137))
                      (conflicts
                       ((tmp-ra.137 (rdi rbp))
                        (rbp (r15 rdi tmp-ra.137))
                        (rdi (r15 tmp-ra.137 rbp))
                        (r15 (rdi rbp))))
                      (assignment ()))
                   (define L.fact.21
                     ((locals (z.56 y.57))
                      (conflicts
                       ((y.57 (rax x.55 rbp tmp-ra.136))
                        (x.55 (y.57 z.56 rbp tmp-ra.136))
                        (z.56 (x.55 tmp-ra.136 rbp))
                        (tmp-ra.136 (y.57 z.56 rax x.55 rdi rbp))
                        (rbp (y.57 r15 rdi z.56 rax x.55 tmp-ra.136))
                        (rdi (r15 rbp tmp-ra.136))
                        (rax (y.57 rbp tmp-ra.136))
                        (r15 (rdi rbp))))
                      (assignment ((tmp-ra.136 fv0) (x.55 fv0))))
                     (begin
                       (set! tmp-ra.136 r15)
                       (set! x.55 rdi)
                       (if (= x.55 0)
                           (begin (set! rax 1) (jump tmp-ra.136 rbp rax))
                           (begin
                             (set! z.56 x.55)
                             (set! z.56 (+ z.56 -1))
                             (begin
                               (set! rbp (- rbp 16))
                               (return-point L.rp.32
                                             (begin
                                               (set! rdi z.56)
                                               (set! r15 L.rp.32)
                                               (jump L.fact.21 rbp r15 rdi)))
                               (set! rbp (+ rbp 16)))
                             (set! y.57 rax)
                             (set! rax x.55)
                             (set! rax (* rax y.57))
                             (jump tmp-ra.136 rbp rax)))))
                   (begin
                     (set! tmp-ra.137 r15)
                     (set! rdi 10)
                     (set! r15 tmp-ra.137)
                     (jump L.fact.21 rbp r15 rdi))))

  (check-equal? (allocate-frames '(module
                                      ((new-frames ())
                                       (locals (tmp-ra.102))
                                       (call-undead ())
                                       (conflicts
                                        ((tmp-ra.102 (rdi rbp))
                                         (rbp (r15 rdi tmp-ra.102))
                                         (rdi (r15 tmp-ra.102 rbp))
                                         (r15 (rdi rbp))))
                                       (assignment ()))
                                    (define L.fact.19
                                      ((new-frames ())
                                       (locals (z.37 y.38))
                                       (call-undead (x.36 tmp-ra.101))
                                       (conflicts
                                        ((tmp-ra.101 (y.38 z.37 rax x.36 rdi rbp))
                                         (y.38 (rax x.36 rbp tmp-ra.101))
                                         (z.37 (x.36 tmp-ra.101 rbp))
                                         (x.36 (y.38 z.37 rbp tmp-ra.101))
                                         (rbp (y.38 r15 rdi z.37 rax x.36 tmp-ra.101))
                                         (rdi (r15 rbp tmp-ra.101))
                                         (rax (y.38 rbp tmp-ra.101))
                                         (r15 (rdi rbp))))
                                       (assignment ((tmp-ra.101 fv0) (x.36 fv0))))
                                      (begin
                                        (set! tmp-ra.101 r15)
                                        (set! x.36 rdi)
                                        (if (= x.36 0)
                                            (begin (set! rax 1) (jump tmp-ra.101 rbp rax))
                                            (begin
                                              (set! z.37 x.36)
                                              (set! z.37 (+ z.37 -1))
                                              (return-point L.rp.25
                                                            (begin
                                                              (set! rdi z.37)
                                                              (set! r15 L.rp.25)
                                                              (jump L.fact.19 rbp r15 rdi)))
                                              (set! y.38 rax)
                                              (set! rax x.36)
                                              (set! rax (* rax y.38))
                                              (jump tmp-ra.101 rbp rax)))))
                                    (begin
                                      (set! tmp-ra.102 r15)
                                      (set! rdi 5)
                                      (set! r15 tmp-ra.102)
                                      (jump L.fact.19 rbp r15 rdi))))
                '(module
                     ((locals (tmp-ra.102))
                      (conflicts
                       ((tmp-ra.102 (rdi rbp))
                        (rbp (r15 rdi tmp-ra.102))
                        (rdi (r15 tmp-ra.102 rbp))
                        (r15 (rdi rbp))))
                      (assignment ()))
                   (define L.fact.19
                     ((locals (y.38 z.37))
                      (conflicts
                       ((tmp-ra.101 (y.38 z.37 rax x.36 rdi rbp))
                        (y.38 (rax x.36 rbp tmp-ra.101))
                        (z.37 (x.36 tmp-ra.101 rbp))
                        (x.36 (y.38 z.37 rbp tmp-ra.101))
                        (rbp (y.38 r15 rdi z.37 rax x.36 tmp-ra.101))
                        (rdi (r15 rbp tmp-ra.101))
                        (rax (y.38 rbp tmp-ra.101))
                        (r15 (rdi rbp))))
                      (assignment ((tmp-ra.101 fv0) (x.36 fv0))))
                     (begin
                       (set! tmp-ra.101 r15)
                       (set! x.36 rdi)
                       (if (= x.36 0)
                           (begin (set! rax 1) (jump tmp-ra.101 rbp rax))
                           (begin
                             (set! z.37 x.36)
                             (set! z.37 (+ z.37 -1))
                             (begin
                               (set! rbp (- rbp 16))
                               (return-point L.rp.25
                                             (begin
                                               (set! rdi z.37)
                                               (set! r15 L.rp.25)
                                               (jump L.fact.19 rbp r15 rdi)))
                               (set! rbp (+ rbp 16)))
                             (set! y.38 rax)
                             (set! rax x.36)
                             (set! rax (* rax y.38))
                             (jump tmp-ra.101 rbp rax)))))
                   (begin
                     (set! tmp-ra.102 r15)
                     (set! rdi 5)
                     (set! r15 tmp-ra.102)
                     (jump L.fact.19 rbp r15 rdi))))
  (check-equal? (allocate-frames '(module
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
                '(module
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
                         (begin (set! rdi 1000) (set! r15 tmp-ra.2) (jump L.f.2 rbp r15 rdi)))))))
