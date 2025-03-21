#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v7
  rackunit)

(provide allocate-frames)

;; asm-pred-lang-v7/pre-framed -> asm-pred-lang-v7/framed
;; compiles p to Asm-pred-lang v7/framed by allocating frames for each non-tail
;; call, and assigning all new-frame variables to frame variables in the new
;; frame
(define/contract (allocate-frames p)
  (-> asm-pred-lang-v7/pre-framed? asm-pred-lang-v7/framed?)

  ;; func is `(define ,label ,info ,tail)
  ;; interp. a function definition

  ;; asm-pred-lang-v7/pre-framed -> integer
  ;; interp. computes the number of bytes needed to allocate the frame by
  ;; calculating the maximum of assigned frame variable slots and call-undead
  ;; size, multiplied by the word size
  (define (compute-frame-size info)
    (define assignments (info-ref info 'assignment))
    (define max-idx
      (for/fold ([max-idx -1]) ([a assignments])
        (if (fvar? (second a))
            (max max-idx (fvar->index (second a)))
            max-idx)))
    (* (max (add1 max-idx) (length (info-ref info 'call-undead)))
       (current-word-size-bytes)))

  ;; asm-pred-lang-v7/pre-framed.info -> asm-pred-lang-v7/framed.info
  (define (allocate-frames-info info)
    ;; new frames for assignment
    (define new-frames (if (empty? (info-ref info 'new-frames))
                           '()
                           (first (info-ref info 'new-frames))))
    (define locals^ (remove* new-frames (info-ref info 'locals)))
    ;; existing assignments
    (define assignments (map cadr (info-ref info 'assignment)))
    ;; all variables that could conflict with assignments
    (define conflicts (map car (info-ref info 'conflicts)))
    ;; all frame variables
    (define conflict-frame-variables (append assignments conflicts))
    (define new-assignments (for/fold ([assignments^ (info-ref info 'assignment)])
                                      ([frame new-frames])
                              (define frame-assignment (for/or ([i (in-naturals)])
                                                         (define fvar (make-fvar i))
                                                         (cond
                                                           [(member fvar conflict-frame-variables) #f]
                                                           [else (set! conflict-frame-variables (cons fvar conflict-frame-variables))
                                                                 `(,frame ,fvar)])))
                              (append assignments^ `(,frame-assignment))))
    (info-set (info-set
               (info-remove
                (info-remove
                 (info-remove info 'undead-out)
                 'call-undead)
                'new-frames)
               'locals (reverse locals^))
              'assignment new-assignments))

  ;; func -> func
  (define (allocate-frames-func f)
    (match f
      [`(define ,label ,info ,tail)
       `(define ,label ,(allocate-frames-info info) ,(allocate-frames-tail tail info))]))

  ;; asm-pred-lang-v7/pre-framed.tail asm-pred-lang-v7/pre-framed.info -> asm-pred-lang-v7/framed.tail
  (define (allocate-frames-tail t info)
    (match t
      [`(jump ,trg ,locs ...) `(jump ,trg ,@locs)]
      [`(begin ,effects ... ,tail)
       (define effects^ (for/fold ([compiled-effects '()])
                                  ([effect effects])
                          (append compiled-effects (list (allocate-frames-effect effect info)))))
       `(begin ,@effects^ ,(allocate-frames-tail tail info))]
      [`(if ,pred ,t1 ,t2)
       `(if ,(allocate-frames-pred pred info)
            ,(allocate-frames-tail t1 info)
            ,(allocate-frames-tail t2 info))]))

  ;; asm-pred-lang-v7/pre-framed.effect asm-pred-lang-v7/pre-framed.info -> asm-pred-lang-v7/framed.effect
  (define (allocate-frames-effect e info)
    (match e
      [`(begin ,effects ...)
       (define effects^ (for/fold ([compiled-effects '()])
                                  ([effect effects])
                          (append compiled-effects (list (allocate-frames-effect effect info)))))
       `(begin ,@effects^)]
      [`(return-point ,label ,tail)
       (define frame-size (compute-frame-size info))
       (define fbp (current-frame-base-pointer-register))
       `(begin
          (set! ,fbp (- ,fbp ,frame-size))
          (return-point ,label ,(allocate-frames-tail tail info))
          (set! ,fbp (+ ,fbp ,frame-size)))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(allocate-frames-pred pred info)
            ,(allocate-frames-effect e1 info)
            ,(allocate-frames-effect e2 info))]
      ;; Wildcard collapse case used because any other effect does not require
      ;; frame allocation and can be returned unchanged
      [_ e]))

  ;; asm-pred-lang-v7/pre-framed.pred asm-pred-lang-v7/pre-framed.info -> asm-pred-lang-v7/framed.pred
  (define (allocate-frames-pred p info)
    (match p
      [`(begin ,effects ... ,pred)
       (define effects^ (for/fold ([compiled-effects '()])
                                  ([effect effects])
                          (append compiled-effects (list (allocate-frames-effect effect info)))))
       `(begin ,@effects^ ,(allocate-frames-pred pred info))]
      ;; Wildcard collapse case used because all other predicates do not contain
      ;; nested effects or require frame allocation and can be returned as-is
      [_ p]))

  (match p
    [`(module ,info ,funcs ... ,tail)
     `(module ,(allocate-frames-info info) ,@(map allocate-frames-func funcs) ,(allocate-frames-tail tail info))]))

(module+ test
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
                         (begin (set! rdi 1000) (set! r15 tmp-ra.2) (jump L.f.2 rbp r15 rdi))))))
  (check-equal? (allocate-frames '(module ((new-frames (() ())) (locals (tmp.88)) (call-undead (tmp-ra.95 tmp.87)) (undead-out ((tmp-ra.95 rbp) ((rax tmp-ra.95 rbp) ((rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi))) (tmp-ra.95 tmp.87 rbp) ((rax tmp.87 tmp-ra.95 rbp) ((rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi))) (tmp.87 tmp.88 tmp-ra.95 rbp) (tmp.88 tmp-ra.95 rdi rbp) (tmp-ra.95 rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi))) (conflicts ((tmp.87 (tmp.88 tmp-ra.95 rbp)) (tmp-ra.95 (rsi rdi tmp.88 tmp.87 rbp)) (tmp.88 (rdi tmp.87 tmp-ra.95 rbp)) (rbp (tmp.88 tmp.87 r15 rsi rdi tmp-ra.95)) (rdi (tmp.88 tmp-ra.95 r15 rsi rbp)) (rsi (tmp-ra.95 r15 rdi rbp)) (r15 (rsi rdi rbp)))) (assignment ((tmp.87 fv0) (tmp-ra.95 fv0)))) (define L.*.17 ((new-frames ()) (locals (tmp-ra.93 tmp.79 tmp.81 tmp.42 tmp.82 tmp.41 tmp.78 tmp.80)) (undead-out ((rdi rsi rbp tmp-ra.93) (rsi tmp.41 rbp tmp-ra.93) (tmp.42 tmp.41 rbp tmp-ra.93) (((((tmp.79 tmp.42 tmp.41 rbp tmp-ra.93) (tmp.79 tmp.42 tmp.41 rbp tmp-ra.93) (tmp.42 tmp.41 rbp tmp-ra.93)) (tmp.78 tmp.42 tmp.41 rbp tmp-ra.93) (tmp.78 tmp.42 tmp.41 rbp tmp-ra.93)) (tmp.42 tmp.41 rbp tmp-ra.93)) (((((tmp.81 tmp.42 tmp.41 rbp tmp-ra.93) (tmp.81 tmp.42 tmp.41 rbp tmp-ra.93) (tmp.42 tmp.41 rbp tmp-ra.93)) (tmp.80 tmp.42 tmp.41 rbp tmp-ra.93) (tmp.80 tmp.42 tmp.41 rbp tmp-ra.93)) (tmp.42 tmp.41 rbp tmp-ra.93)) ((tmp.82 tmp.41 rbp tmp-ra.93) (tmp.41 tmp.82 rbp tmp-ra.93) (tmp.82 rax rbp tmp-ra.93) (rax rbp tmp-ra.93) (rbp rax)) ((rax rbp tmp-ra.93) (rbp rax))) ((rax rbp tmp-ra.93) (rbp rax))))) (call-undead ()) (conflicts ((tmp-ra.93 (rax tmp.82 tmp.80 tmp.81 tmp.78 tmp.79 tmp.42 tmp.41 rdi rsi rbp)) (tmp.79 (tmp.42 tmp.41 rbp tmp-ra.93)) (tmp.81 (tmp.41 tmp.42 rbp tmp-ra.93)) (tmp.42 (tmp.80 tmp.81 tmp.78 tmp.79 tmp.41 rbp tmp-ra.93)) (tmp.82 (rax tmp.41 rbp tmp-ra.93)) (tmp.41 (tmp.82 tmp.80 tmp.81 tmp.78 tmp.79 tmp.42 rsi rbp tmp-ra.93)) (tmp.78 (tmp.42 tmp.41 rbp tmp-ra.93)) (tmp.80 (tmp.42 tmp.41 rbp tmp-ra.93)) (rbp (rax tmp.82 tmp.80 tmp.81 tmp.78 tmp.79 tmp.42 tmp.41 tmp-ra.93)) (rsi (tmp.41 tmp-ra.93)) (rdi (tmp-ra.93)) (rax (tmp.82 rbp tmp-ra.93)))) (assignment ())) (begin (set! tmp-ra.93 r15) (set! tmp.41 rdi) (set! tmp.42 rsi) (if (begin (if (begin (set! tmp.79 tmp.42) (set! tmp.79 (bitwise-and tmp.79 7)) (= tmp.79 0)) (set! tmp.78 14) (set! tmp.78 6)) (!= tmp.78 6)) (if (begin (if (begin (set! tmp.81 tmp.41) (set! tmp.81 (bitwise-and tmp.81 7)) (= tmp.81 0)) (set! tmp.80 14) (set! tmp.80 6)) (!= tmp.80 6)) (begin (set! tmp.82 tmp.42) (set! tmp.82 (arithmetic-shift-right tmp.82 3)) (set! rax tmp.41) (set! rax (* rax tmp.82)) (jump tmp-ra.93 rbp rax)) (begin (set! rax 318) (jump tmp-ra.93 rbp rax))) (begin (set! rax 318) (jump tmp-ra.93 rbp rax))))) (define L.+.16 ((new-frames ()) (locals (tmp.83 tmp.40 tmp.86 tmp.84 tmp-ra.94 tmp.85 tmp.39)) (undead-out ((rdi rsi rbp tmp-ra.94) (rsi tmp.39 rbp tmp-ra.94) (tmp.39 tmp.40 rbp tmp-ra.94) (((((tmp.84 tmp.39 tmp.40 rbp tmp-ra.94) (tmp.84 tmp.39 tmp.40 rbp tmp-ra.94) (tmp.39 tmp.40 rbp tmp-ra.94)) (tmp.83 tmp.39 tmp.40 rbp tmp-ra.94) (tmp.83 tmp.39 tmp.40 rbp tmp-ra.94)) (tmp.39 tmp.40 rbp tmp-ra.94)) (((((tmp.86 tmp.39 tmp.40 rbp tmp-ra.94) (tmp.86 tmp.39 tmp.40 rbp tmp-ra.94) (tmp.39 tmp.40 rbp tmp-ra.94)) (tmp.85 tmp.39 tmp.40 rbp tmp-ra.94) (tmp.85 tmp.39 tmp.40 rbp tmp-ra.94)) (tmp.39 tmp.40 rbp tmp-ra.94)) ((tmp.40 rax rbp tmp-ra.94) (rax rbp tmp-ra.94) (rbp rax)) ((rax rbp tmp-ra.94) (rbp rax))) ((rax rbp tmp-ra.94) (rbp rax))))) (call-undead ()) (conflicts ((tmp.83 (tmp.39 tmp.40 rbp tmp-ra.94)) (tmp.40 (rax tmp.85 tmp.86 tmp.83 tmp.84 tmp.39 rbp tmp-ra.94)) (tmp.86 (tmp.39 tmp.40 rbp tmp-ra.94)) (tmp.84 (tmp.40 tmp.39 rbp tmp-ra.94)) (tmp-ra.94 (rax tmp.85 tmp.86 tmp.83 tmp.84 tmp.40 tmp.39 rdi rsi rbp)) (tmp.85 (tmp.39 tmp.40 rbp tmp-ra.94)) (tmp.39 (tmp.85 tmp.86 tmp.83 tmp.84 tmp.40 rsi rbp tmp-ra.94)) (rbp (rax tmp.85 tmp.86 tmp.83 tmp.84 tmp.40 tmp.39 tmp-ra.94)) (rsi (tmp.39 tmp-ra.94)) (rdi (tmp-ra.94)) (rax (tmp.40 rbp tmp-ra.94)))) (assignment ())) (begin (set! tmp-ra.94 r15) (set! tmp.39 rdi) (set! tmp.40 rsi) (if (begin (if (begin (set! tmp.84 tmp.40) (set! tmp.84 (bitwise-and tmp.84 7)) (= tmp.84 0)) (set! tmp.83 14) (set! tmp.83 6)) (!= tmp.83 6)) (if (begin (if (begin (set! tmp.86 tmp.39) (set! tmp.86 (bitwise-and tmp.86 7)) (= tmp.86 0)) (set! tmp.85 14) (set! tmp.85 6)) (!= tmp.85 6)) (begin (set! rax tmp.39) (set! rax (+ rax tmp.40)) (jump tmp-ra.94 rbp rax)) (begin (set! rax 574) (jump tmp-ra.94 rbp rax))) (begin (set! rax 574) (jump tmp-ra.94 rbp rax))))) (begin (set! tmp-ra.95 r15) (return-point L.rp.19 (begin (set! rdi 40) (set! rsi 48) (set! r15 L.rp.19) (jump L.+.16 rbp r15 rdi rsi))) (set! tmp.87 rax) (return-point L.rp.20 (begin (set! rdi 32) (set! rsi 40) (set! r15 L.rp.20) (jump L.*.17 rbp r15 rdi rsi))) (set! tmp.88 rax) (set! rdi tmp.87) (set! rsi tmp.88) (set! r15 tmp-ra.95) (jump L.+.16 rbp r15 rdi rsi))))
                '(module
                     ((locals (tmp.88))
                      (conflicts
                       ((tmp.87 (tmp.88 tmp-ra.95 rbp))
                        (tmp-ra.95 (rsi rdi tmp.88 tmp.87 rbp))
                        (tmp.88 (rdi tmp.87 tmp-ra.95 rbp))
                        (rbp (tmp.88 tmp.87 r15 rsi rdi tmp-ra.95))
                        (rdi (tmp.88 tmp-ra.95 r15 rsi rbp))
                        (rsi (tmp-ra.95 r15 rdi rbp))
                        (r15 (rsi rdi rbp))))
                      (assignment ((tmp.87 fv0) (tmp-ra.95 fv0))))
                   (define L.*.17
                     ((locals (tmp.80 tmp.78 tmp.41 tmp.82 tmp.42 tmp.81 tmp.79 tmp-ra.93))
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
                     ((locals (tmp.39 tmp.85 tmp-ra.94 tmp.84 tmp.86 tmp.40 tmp.83))
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
  (check-equal? (allocate-frames '(module
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
                     ((locals (tmp-ra.238))
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
                     ((locals (tmp.185 tmp.96 tmp.186 tmp-ra.232 tmp.184 tmp.97 tmp.183))
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
                     ((locals (tmp.187 nfv.234 nfv.235 g.25 f.24 e.23 d.22 c.21 b.20 a.19))
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
                     ((locals (h.33 g.32 f.31 e.30 d.29 c.28 b.27 a.26 tmp-ra.236))
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
                     ((locals (r7.49 r6.48 r5.47 r4.46 r3.45 r2.44 r1.43 b.35 a.34))
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
