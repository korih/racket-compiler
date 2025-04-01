#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8
  rackunit)

(provide implement-fvars)

;; nested-asm-lang-fvars-v8 -> nested-asm-lang-v8
;; compiles p to Nested-asm-lang v8 by reifying fvars into displacement mode
;; operands
(define/contract (implement-fvars p)
  (-> nested-asm-lang-fvars-v8? nested-asm-lang-v8?)

  ;; base-pointer-offset is Integer
  ;; interp. keeps track of the frame base pointer offset after
  ;; allocating/deallocating a frame
  (define base-pointer-offset 0)

  ;; fvar -> addr
  ;; convert fvar into displacement mode operand
  (define (fvar->addr fvar)
    `(,(current-frame-base-pointer-register) - ,(+ (* (fvar->index fvar)
                                                      (current-word-size-bytes))
                                                   base-pointer-offset)))

  ;; nested-asm-lang-fvars-v8.binop Integer -> void
  ;; EFFECTS: mutates `base-pointer-offset` by applying the given binop with the
  ;; offset
  (define (update-base-pointer-offset! binop offset)
    (match binop
      ['+ (set! base-pointer-offset (+ base-pointer-offset offset))]
      ['* (set! base-pointer-offset (* base-pointer-offset offset))]
      ['- (set! base-pointer-offset (- base-pointer-offset offset))]

      ;; Wildcard TODO: should not be able to put some binops here
      [_ (set! base-pointer-offset offset)]))

  ;; nested-asm-lang-fvars-v8.tail -> nested-asm-lang-v8.tail
  (define (implement-fvars-tail tail)
    (match tail
      [`(jump ,trg)
       `(jump ,(implement-fvars-trg trg))]
      [`(begin ,es ... ,t)
       `(begin ,@(map implement-fvars-effect es) ,(implement-fvars-tail t))]
      [`(if ,pred ,t1 ,t2)
       `(if ,(implement-fvars-pred pred)
            ,(implement-fvars-tail t1)
            ,(implement-fvars-tail t2))]))

  ;; nested-asm-lang-fvars-v8.effect -> nested-asm-lang-v8.effect
  (define (implement-fvars-effect effect)
    (match effect
      [`(set! ,loc1 (mref ,loc2 ,index))
       (define loc1^ (implement-fvars-loc loc1))
       (define loc2^ (implement-fvars-loc loc2))
       (define index^ (implement-fvars-opand index))
       `(set! ,loc1^ (mref ,loc2^ ,index^))]
      [`(set! ,loc (,binop ,loc ,opand))
       (when (and (eq? (current-frame-base-pointer-register) loc)
                  (int64? opand))
         (update-base-pointer-offset! binop opand))
       (define loc^ (implement-fvars-loc loc))
       `(set! ,loc^ (,binop ,loc^ ,(implement-fvars-opand opand)))]
      [`(set! ,loc ,triv)
       (when (and (eq? (current-frame-base-pointer-register) loc)
                  (int64? triv))
         (update-base-pointer-offset! '= triv))
       `(set! ,(implement-fvars-loc loc) ,(implement-fvars-triv triv))]
      [`(mset! ,loc ,index ,triv)
       `(mset! ,(implement-fvars-loc loc) ,(implement-fvars-opand index) ,(implement-fvars-triv triv))]
      [`(begin ,es ...)
       `(begin ,@(map implement-fvars-effect es))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(implement-fvars-pred pred)
            ,(implement-fvars-effect e1)
            ,(implement-fvars-effect e2))]
      [`(return-point ,label ,tail)
       `(return-point ,label ,(implement-fvars-tail tail))]))

  ;; nested-asm-lang-fvars-v8.pred -> nested-asm-lang-v8.pred
  (define (implement-fvars-pred pred)
    (match pred
      ['(true) pred]
      ['(false) pred]
      [`(not ,p) `(not ,(implement-fvars-pred p))]
      [`(begin ,es ... ,p)
       `(begin ,@(map implement-fvars-effect es) ,(implement-fvars-pred p))]
      [`(if ,p1 ,p2 ,p3)
       `(if ,(implement-fvars-pred p1)
            ,(implement-fvars-pred p2)
            ,(implement-fvars-pred p3))]
      [`(,relop ,loc ,opand)
       `(,relop ,(implement-fvars-loc loc) ,(implement-fvars-opand opand))]))

  ;; nested-asm-lang-fvars-v8.loc -> nested-asm-lang-v8.loc
  (define (implement-fvars-loc loc)
    (match loc
      [reg #:when (register? reg) reg]
      [fvar #:when (fvar? fvar) (fvar->addr fvar)]))

  ;; nested-asm-lang-fvars-v8.trg -> nested-asm-lang-v8.trg
  (define (implement-fvars-trg trg)
    (match trg
      [label #:when (label? label) label]
      [loc (implement-fvars-loc loc)]))

  ;; nested-asm-lang-fvars-v8.opand -> nested-asm-lang-v8.opand
  (define (implement-fvars-opand opand)
    (match opand
      [int64 #:when (int64? int64) int64]
      [loc (implement-fvars-loc loc)]))

  ;; nested-asm-lang-fvars-v8.triv -> nested-asm-lang-v8.triv
  (define (implement-fvars-triv triv)
    (match triv
      [label #:when (label? label) label]
      [opand (implement-fvars-opand opand)]))

  (match p
    [`(module (define ,ls ,ts) ... ,tail)
     (define funcs (map (lambda (l t) `(define ,l ,(implement-fvars-tail t))) ls ts))
     `(module ,@funcs ,(implement-fvars-tail tail))]))

(module+ test
  (check-equal? (implement-fvars '(module (begin
                                            (set! fv0 r15)
                                            (set! rbp (- rbp 8))
                                            (set! r15 fv0)
                                            (set! rbp (+ rbp 8))
                                            (jump fv0))))
                '(module
                     (begin
                       (set! (rbp - 0) r15)
                       (set! rbp (- rbp 8))
                       (set! r15 (rbp - -8))
                       (set! rbp (+ rbp 8))
                       (jump (rbp - 0)))))
  (check-equal? (implement-fvars '(module (begin
                                            (set! fv0 r15)
                                            (set! rbp (+ rbp 8))
                                            (set! rbp (* rbp 3))
                                            (set! r15 fv0)
                                            (jump fv0))))
                '(module
                     (begin
                       (set! (rbp - 0) r15)
                       (set! rbp (+ rbp 8))
                       (set! rbp (* rbp 3))
                       (set! r15 (rbp - 24))
                       (jump (rbp - 24)))))
  (check-equal? (implement-fvars '(module (begin
                                            (set! fv0 r15)
                                            (set! rbp (- rbp 32))
                                            (return-point L.rp.1
                                                          (if (< fv0 fv1)
                                                              (jump fv0)
                                                              (begin
                                                                (set! fv0 (- fv0 1))
                                                                (jump L.rp.1))))
                                            (set! rbp (+ rbp 32))
                                            (jump L.rp.1))))
                '(module
                     (begin
                       (set! (rbp - 0) r15)
                       (set! rbp (- rbp 32))
                       (return-point L.rp.1
                                     (if (< (rbp - -32) (rbp - -24))
                                         (jump (rbp - -32))
                                         (begin (set! (rbp - -32) (- (rbp - -32) 1)) (jump L.rp.1))))
                       (set! rbp (+ rbp 32))
                       (jump L.rp.1))))
  (check-equal? (implement-fvars '(module
                                      (define L.swap.1
                                        (begin
                                          (set! fv2 r15)
                                          (set! r14 fv0)
                                          (set! r15 fv1)
                                          (if (< r15 r14)
                                              (begin (set! rax r14) (jump fv2))
                                              (begin
                                                (begin
                                                  (set! rbp (- rbp 24))
                                                  (return-point L.rp.6
                                                                (begin
                                                                  (set! fv4 r14)
                                                                  (set! fv3 r15)
                                                                  (set! r15 L.rp.6)
                                                                  (jump L.swap.1)))
                                                  (set! rbp (+ rbp 24)))
                                                (set! r15 rax)
                                                (set! rax r15)
                                                (jump fv2)))))
                                    (begin
                                      (set! r15 r15)
                                      (set! fv1 2)
                                      (set! fv0 1)
                                      (set! r15 r15)
                                      (jump L.swap.1))))
                '(module
                     (define L.swap.1
                       (begin
                         (set! (rbp - 16) r15)
                         (set! r14 (rbp - 0))
                         (set! r15 (rbp - 8))
                         (if (< r15 r14)
                             (begin (set! rax r14) (jump (rbp - 16)))
                             (begin
                               (begin
                                 (set! rbp (- rbp 24))
                                 (return-point L.rp.6
                                               (begin
                                                 (set! (rbp - 8) r14)
                                                 (set! (rbp - 0) r15)
                                                 (set! r15 L.rp.6)
                                                 (jump L.swap.1)))
                                 (set! rbp (+ rbp 24)))
                               (set! r15 rax)
                               (set! rax r15)
                               (jump (rbp - 16))))))
                   (begin
                     (set! r15 r15)
                     (set! (rbp - 8) 2)
                     (set! (rbp - 0) 1)
                     (set! r15 r15)
                     (jump L.swap.1))))
  (check-equal? (implement-fvars '(module
                                      (define L.f.1
                                        (begin
                                          (set! rsp r15)
                                          (set! rcx rdi)
                                          (set! rdx 1)
                                          (set! rbx 2)
                                          (set! rdx rdx)
                                          (set! rdx (bitwise-and rdx rcx))
                                          (set! rbx rbx)
                                          (set! rbx (bitwise-ior rbx rcx))
                                          (set! rdx (bitwise-xor rdx rbx))
                                          (set! rax rdx)
                                          (set! rax (arithmetic-shift-right rax 3))
                                          (jump rsp)))
                                    (begin
                                      (set! rbx r15)
                                      (set! rcx 10)
                                      (if (begin (set! rsp 100) (not (!= rcx rsp)))
                                          (begin (set! rdi rcx) (set! r15 rbx) (jump L.f.1))
                                          (begin (set! rdi 1000) (set! r15 rbx) (jump L.f.2))))))
                '(module
                     (define L.f.1
                       (begin
                         (set! rsp r15)
                         (set! rcx rdi)
                         (set! rdx 1)
                         (set! rbx 2)
                         (set! rdx rdx)
                         (set! rdx (bitwise-and rdx rcx))
                         (set! rbx rbx)
                         (set! rbx (bitwise-ior rbx rcx))
                         (set! rdx (bitwise-xor rdx rbx))
                         (set! rax rdx)
                         (set! rax (arithmetic-shift-right rax 3))
                         (jump rsp)))
                   (begin
                     (set! rbx r15)
                     (set! rcx 10)
                     (if (begin (set! rsp 100) (not (!= rcx rsp)))
                         (begin (set! rdi rcx) (set! r15 rbx) (jump L.f.1))
                         (begin (set! rdi 1000) (set! r15 rbx) (jump L.f.2))))))
  (check-equal? (implement-fvars '(module
                                      (define L.f.1
                                        (begin
                                          (set! fv3 r15)
                                          (set! fv1 rdi)
                                          (set! fv0 rsi)
                                          (set! rsp 10)
                                          (set! rsp (+ rsp 6))
                                          (begin (set! fv2 r12) (set! r12 (+ r12 rsp)))
                                          (begin
                                            (set! rbp (- rbp 32))
                                            (return-point L.rp.21 (begin (set! r15 L.rp.21) (jump L.g.1)))
                                            (set! rbp (+ rbp 32)))
                                          (set! rsp rax)
                                          (if (true) (mset! fv2 rsp fv1) (mset! fv2 rsp fv0))
                                          (set! rbx 10)
                                          (set! rbx (+ rbx 6))
                                          (begin (set! rsp r12) (set! r12 (+ r12 rbx)))
                                          (set! rbx 8)
                                          (set! rbx (bitwise-and rbx 8))
                                          (set! rax (mref rsp rbx))
                                          (jump fv3)))
                                    (define L.g.1 (begin (set! rsp r15) (set! rax 8) (jump rsp)))
                                    (begin (set! rsp r15) (set! rdi 1) (set! rsi 2) (set! r15 rsp) (jump L.f.1))))
                '(module
                     (define L.f.1
                       (begin
                         (set! (rbp - 24) r15)
                         (set! (rbp - 8) rdi)
                         (set! (rbp - 0) rsi)
                         (set! rsp 10)
                         (set! rsp (+ rsp 6))
                         (begin (set! (rbp - 16) r12) (set! r12 (+ r12 rsp)))
                         (begin
                           (set! rbp (- rbp 32))
                           (return-point L.rp.21 (begin (set! r15 L.rp.21) (jump L.g.1)))
                           (set! rbp (+ rbp 32)))
                         (set! rsp rax)
                         (if (true)
                             (mset! (rbp - 16) rsp (rbp - 8))
                             (mset! (rbp - 16) rsp (rbp - 0)))
                         (set! rbx 10)
                         (set! rbx (+ rbx 6))
                         (begin (set! rsp r12) (set! r12 (+ r12 rbx)))
                         (set! rbx 8)
                         (set! rbx (bitwise-and rbx 8))
                         (set! rax (mref rsp rbx))
                         (jump (rbp - 24))))
                   (define L.g.1 (begin (set! rsp r15) (set! rax 8) (jump rsp)))
                   (begin (set! rsp r15) (set! rdi 1) (set! rsi 2) (set! r15 rsp) (jump L.f.1))))

  (check-equal? (interp-nested-asm-lang-v8 (implement-fvars '(module (begin
                                                                       (set! r15 r15)
                                                                       (set! r13 rdi)
                                                                       (set! r14 rsi)
                                                                       (if (begin
                                                                             (if (begin (set! r9 10) (set! r9 (bitwise-and r9 7)) (= r9 0))
                                                                                 (set! r9 14)
                                                                                 (set! r9 6))
                                                                             (!= r9 6))
                                                                           (if (begin
                                                                                 (if (begin (set! r9 r13) (set! r9 (bitwise-and r9 7)) (= r9 0))
                                                                                     (set! r9 14)
                                                                                     (set! r9 6))
                                                                                 (!= r9 6))
                                                                               (begin (set! rax r13) (set! rax (+ rax r14)) (jump r15))
                                                                               (begin (set! rax 574) (jump r15)))
                                                                           (begin (set! rax 574) (jump r15)))))))
                (interp-nested-asm-lang-v8 '(module
                                                (begin
                                                  (set! r15 r15)
                                                  (set! r13 rdi)
                                                  (set! r14 rsi)
                                                  (if (begin
                                                        (if (begin (set! r9 10) (set! r9 (bitwise-and r9 7)) (= r9 0))
                                                            (set! r9 14)
                                                            (set! r9 6))
                                                        (!= r9 6))
                                                      (if (begin
                                                            (if (begin (set! r9 r13) (set! r9 (bitwise-and r9 7)) (= r9 0))
                                                                (set! r9 14)
                                                                (set! r9 6))
                                                            (!= r9 6))
                                                          (begin (set! rax r13) (set! rax (+ rax r14)) (jump r15))
                                                          (begin (set! rax 574) (jump r15)))
                                                      (begin (set! rax 574) (jump r15)))))))

  (check-equal? (interp-nested-asm-lang-v8 (implement-fvars '(module
                                                                 (define L.+.31
                                                                   (begin
                                                                     (set! r15 r15)
                                                                     (set! r13 rdi)
                                                                     (set! r14 10)
                                                                     (if (begin
                                                                           (if (begin (set! r9 r14) (set! r9 (bitwise-and r9 7)) (= r9 0))
                                                                               (set! r9 14)
                                                                               (set! r9 6))
                                                                           (!= r9 6))
                                                                         (if (begin
                                                                               (if (begin (set! r9 r13) (set! r9 (bitwise-and r9 7)) (= r9 0))
                                                                                   (set! r9 14)
                                                                                   (set! r9 6))
                                                                               (!= r9 6))
                                                                             (begin (set! rax r13) (set! rax (+ rax r14)) (jump r15))
                                                                             (begin (set! rax 574) (jump r15)))
                                                                         (begin (set! rax 574) (jump r15)))))
                                                               (define L.F.6
                                                                 (begin
                                                                   (set! fv1 r15)
                                                                   (set! r15 rdi)
                                                                   (set! r14 rsi)
                                                                   (set! r13 rdx)
                                                                   (set! rcx rcx)
                                                                   (set! r8 r8)
                                                                   (set! r9 r9)
                                                                   (set! rbx fv0)
                                                                   (begin
                                                                     (set! rbp (- rbp 16))
                                                                     (return-point L.rp.47
                                                                                   (begin
                                                                                     (set! rdi r15)
                                                                                     (set! rsi r14)
                                                                                     (set! rdx r13)
                                                                                     (set! rcx rcx)
                                                                                     (set! r8 r8)
                                                                                     (set! r9 r9)
                                                                                     (set! r13 rbx)
                                                                                     (set! r14 64)
                                                                                     (set! r15 L.rp.47)
                                                                                     (jump L.G.7)))
                                                                     (set! rbp (+ rbp 16)))
                                                                   (set! r15 rax)
                                                                   (set! rdi 80)
                                                                   (set! rsi r15)
                                                                   (set! r15 fv1)
                                                                   (jump L.+.31)))
                                                               (define L.G.7
                                                                 (begin
                                                                   (set! r15 r15)
                                                                   (set! r14 rdi)
                                                                   (set! r13 rsi)
                                                                   (set! rdx rdx)
                                                                   (set! rcx rcx)
                                                                   (set! r8 r8)
                                                                   (set! r9 r9)
                                                                   (set! rbx fv0)
                                                                   (set! rsp fv1)
                                                                   (set! rdi r14)
                                                                   (set! rsi r13)
                                                                   (set! rdx rdx)
                                                                   (set! rcx rcx)
                                                                   (set! r8 r8)
                                                                   (set! r9 r9)
                                                                   (set! fv0 rbx)
                                                                   (set! fv1 rsp)
                                                                   (set! fv2 72)
                                                                   (set! r15 r15)
                                                                   (jump L.H.8)))
                                                               (define L.H.8
                                                                 (begin
                                                                   (set! fv3 r15)
                                                                   (set! r15 rdi)
                                                                   (set! r14 rsi)
                                                                   (set! fv8 rdx)
                                                                   (set! fv7 rcx)
                                                                   (set! fv6 r8)
                                                                   (set! fv5 r9)
                                                                   (set! fv4 fv0)
                                                                   (set! fv1 fv1)
                                                                   (set! fv0 fv2)
                                                                   (begin
                                                                     (set! rbp (- rbp 72))
                                                                     (return-point L.rp.48
                                                                                   (begin
                                                                                     (set! rdi r15)
                                                                                     (set! rsi r14)
                                                                                     (set! r15 L.rp.48)
                                                                                     (jump L.+.31)))
                                                                     (set! rbp (+ rbp 72)))
                                                                   (set! r15 rax)
                                                                   (begin
                                                                     (set! rbp (- rbp 72))
                                                                     (return-point L.rp.49
                                                                                   (begin
                                                                                     (set! rdi r15)
                                                                                     (set! rsi fv8)
                                                                                     (set! r15 L.rp.49)
                                                                                     (jump L.+.31)))
                                                                     (set! rbp (+ rbp 72)))
                                                                   (set! r15 rax)
                                                                   (begin
                                                                     (set! rbp (- rbp 72))
                                                                     (return-point L.rp.50
                                                                                   (begin
                                                                                     (set! rdi r15)
                                                                                     (set! rsi fv7)
                                                                                     (set! r15 L.rp.50)
                                                                                     (jump L.+.31)))
                                                                     (set! rbp (+ rbp 72)))
                                                                   (set! r15 rax)
                                                                   (begin
                                                                     (set! rbp (- rbp 72))
                                                                     (return-point L.rp.51
                                                                                   (begin
                                                                                     (set! rdi r15)
                                                                                     (set! rsi fv6)
                                                                                     (set! r15 L.rp.51)
                                                                                     (jump L.+.31)))
                                                                     (set! rbp (+ rbp 72)))
                                                                   (set! r15 rax)
                                                                   (begin
                                                                     (set! rbp (- rbp 72))
                                                                     (return-point L.rp.52
                                                                                   (begin
                                                                                     (set! rdi r15)
                                                                                     (set! rsi fv5)
                                                                                     (set! r15 L.rp.52)
                                                                                     (jump L.+.31)))
                                                                     (set! rbp (+ rbp 72)))
                                                                   (set! r15 rax)
                                                                   (begin
                                                                     (set! rbp (- rbp 72))
                                                                     (return-point L.rp.53
                                                                                   (begin
                                                                                     (set! rdi r15)
                                                                                     (set! rsi fv4)
                                                                                     (set! r15 L.rp.53)
                                                                                     (jump L.+.31)))
                                                                     (set! rbp (+ rbp 72)))
                                                                   (set! r15 rax)
                                                                   (begin
                                                                     (set! rbp (- rbp 72))
                                                                     (return-point L.rp.54
                                                                                   (begin
                                                                                     (set! rdi r15)
                                                                                     (set! rsi fv1)
                                                                                     (set! r15 L.rp.54)
                                                                                     (jump L.+.31)))
                                                                     (set! rbp (+ rbp 72)))
                                                                   (set! r15 rax)
                                                                   (set! rdi r15)
                                                                   (set! rsi fv0)
                                                                   (set! r15 fv3)
                                                                   (jump L.+.31)))
                                                               (begin
                                                                 (set! r15 r15)
                                                                 (set! rdi 8)
                                                                 (set! rsi 16)
                                                                 (set! rdx 24)
                                                                 (set! rcx 32)
                                                                 (set! r8 40)
                                                                 (set! r9 48)
                                                                 (set! fv0 56)
                                                                 (set! r15 r15)
                                                                 (jump L.F.6)))))
                (interp-nested-asm-lang-v8 '(module
                                                (define L.+.31
                                                  (begin
                                                    (set! r15 r15)
                                                    (set! r13 rdi)
                                                    (set! r14 10)
                                                    (if (begin
                                                          (if (begin (set! r9 r14) (set! r9 (bitwise-and r9 7)) (= r9 0))
                                                              (set! r9 14)
                                                              (set! r9 6))
                                                          (!= r9 6))
                                                        (if (begin
                                                              (if (begin (set! r9 r13) (set! r9 (bitwise-and r9 7)) (= r9 0))
                                                                  (set! r9 14)
                                                                  (set! r9 6))
                                                              (!= r9 6))
                                                            (begin (set! rax r13) (set! rax (+ rax r14)) (jump r15))
                                                            (begin (set! rax 574) (jump r15)))
                                                        (begin (set! rax 574) (jump r15)))))
                                              (define L.F.6
                                                (begin
                                                  (set! (rbp - 8) r15)
                                                  (set! r15 rdi)
                                                  (set! r14 rsi)
                                                  (set! r13 rdx)
                                                  (set! rcx rcx)
                                                  (set! r8 r8)
                                                  (set! r9 r9)
                                                  (set! rbx (rbp - 0))
                                                  (begin
                                                    (set! rbp (- rbp 16))
                                                    (return-point L.rp.47
                                                                  (begin
                                                                    (set! rdi r15)
                                                                    (set! rsi r14)
                                                                    (set! rdx r13)
                                                                    (set! rcx rcx)
                                                                    (set! r8 r8)
                                                                    (set! r9 r9)
                                                                    (set! r13 rbx)
                                                                    (set! r14 64)
                                                                    (set! r15 L.rp.47)
                                                                    (jump L.G.7)))
                                                    (set! rbp (+ rbp 16)))
                                                  (set! r15 rax)
                                                  (set! rdi 80)
                                                  (set! rsi r15)
                                                  (set! r15 (rbp - 8))
                                                  (jump L.+.31)))
                                              (define L.G.7
                                                (begin
                                                  (set! r15 r15)
                                                  (set! r14 rdi)
                                                  (set! r13 rsi)
                                                  (set! rdx rdx)
                                                  (set! rcx rcx)
                                                  (set! r8 r8)
                                                  (set! r9 r9)
                                                  (set! rbx (rbp - 0))
                                                  (set! rsp (rbp - 8))
                                                  (set! rdi r14)
                                                  (set! rsi r13)
                                                  (set! rdx rdx)
                                                  (set! rcx rcx)
                                                  (set! r8 r8)
                                                  (set! r9 r9)
                                                  (set! (rbp - 0) rbx)
                                                  (set! (rbp - 8) rsp)
                                                  (set! (rbp - 16) 72)
                                                  (set! r15 r15)
                                                  (jump L.H.8)))
                                              (define L.H.8
                                                (begin
                                                  (set! (rbp - 24) r15)
                                                  (set! r15 rdi)
                                                  (set! r14 rsi)
                                                  (set! (rbp - 64) rdx)
                                                  (set! (rbp - 56) rcx)
                                                  (set! (rbp - 48) r8)
                                                  (set! (rbp - 40) r9)
                                                  (set! (rbp - 32) (rbp - 0))
                                                  (set! (rbp - 8) (rbp - 8))
                                                  (set! (rbp - 0) (rbp - 16))
                                                  (begin
                                                    (set! rbp (- rbp 72))
                                                    (return-point L.rp.48
                                                                  (begin
                                                                    (set! rdi r15)
                                                                    (set! rsi r14)
                                                                    (set! r15 L.rp.48)
                                                                    (jump L.+.31)))
                                                    (set! rbp (+ rbp 72)))
                                                  (set! r15 rax)
                                                  (begin
                                                    (set! rbp (- rbp 72))
                                                    (return-point L.rp.49
                                                                  (begin
                                                                    (set! rdi r15)
                                                                    (set! rsi (rbp - -8))
                                                                    (set! r15 L.rp.49)
                                                                    (jump L.+.31)))
                                                    (set! rbp (+ rbp 72)))
                                                  (set! r15 rax)
                                                  (begin
                                                    (set! rbp (- rbp 72))
                                                    (return-point L.rp.50
                                                                  (begin
                                                                    (set! rdi r15)
                                                                    (set! rsi (rbp - -16))
                                                                    (set! r15 L.rp.50)
                                                                    (jump L.+.31)))
                                                    (set! rbp (+ rbp 72)))
                                                  (set! r15 rax)
                                                  (begin
                                                    (set! rbp (- rbp 72))
                                                    (return-point L.rp.51
                                                                  (begin
                                                                    (set! rdi r15)
                                                                    (set! rsi (rbp - -24))
                                                                    (set! r15 L.rp.51)
                                                                    (jump L.+.31)))
                                                    (set! rbp (+ rbp 72)))
                                                  (set! r15 rax)
                                                  (begin
                                                    (set! rbp (- rbp 72))
                                                    (return-point L.rp.52
                                                                  (begin
                                                                    (set! rdi r15)
                                                                    (set! rsi (rbp - -32))
                                                                    (set! r15 L.rp.52)
                                                                    (jump L.+.31)))
                                                    (set! rbp (+ rbp 72)))
                                                  (set! r15 rax)
                                                  (begin
                                                    (set! rbp (- rbp 72))
                                                    (return-point L.rp.53
                                                                  (begin
                                                                    (set! rdi r15)
                                                                    (set! rsi (rbp - -40))
                                                                    (set! r15 L.rp.53)
                                                                    (jump L.+.31)))
                                                    (set! rbp (+ rbp 72)))
                                                  (set! r15 rax)
                                                  (begin
                                                    (set! rbp (- rbp 72))
                                                    (return-point L.rp.54
                                                                  (begin
                                                                    (set! rdi r15)
                                                                    (set! rsi (rbp - -64))
                                                                    (set! r15 L.rp.54)
                                                                    (jump L.+.31)))
                                                    (set! rbp (+ rbp 72)))
                                                  (set! r15 rax)
                                                  (set! rdi r15)
                                                  (set! rsi (rbp - 0))
                                                  (set! r15 (rbp - 24))
                                                  (jump L.+.31)))
                                              (begin
                                                (set! r15 r15)
                                                (set! rdi 8)
                                                (set! rsi 16)
                                                (set! rdx 24)
                                                (set! rcx 32)
                                                (set! r8 40)
                                                (set! r9 48)
                                                (set! (rbp - 0) 56)
                                                (set! r15 r15)
                                                (jump L.F.6))))))
