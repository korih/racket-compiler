#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v7
  rackunit)

(provide implement-fvars)

;; nested-asm-lang-fvars-v7 -> nested-asm-lang-v7
;; compiles p to Nested-asm-lang v7 by reifying fvars into displacement mode
;; operands
(define/contract (implement-fvars p)
  (-> nested-asm-lang-fvars-v7? nested-asm-lang-v7?)

  ;; base-pointer-offset is Integer
  ;; keeps track of the frame base pointer offset after allocating/deallocating
  ;; a frame
  (define base-pointer-offset 0)

  ;; fvar -> addr
  ;; convert fvar into displacement mode operand
  (define (fvar->addr fvar)
    `(,(current-frame-base-pointer-register) - ,(+ (* (fvar->index fvar)
                                                      (current-word-size-bytes))
                                                   base-pointer-offset)))

  ;; Symbol Integer ->
  ;; EFFECTS: mutates `base-pointer-offset` by applying the given binop with the
  ;; offset
  (define (update-base-pointer-offset! binop offset)
    (match binop
      ['+ (set! base-pointer-offset (+ base-pointer-offset offset))]
      ['* (set! base-pointer-offset (* base-pointer-offset offset))]
      ['- (set! base-pointer-offset (- base-pointer-offset offset))]
      [else (set! base-pointer-offset offset)]))

  ;; nested-asm-lang-fvars-v7.tail -> nested-asm-lang-v7.tail
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

  ;; nested-asm-lang-fvars-v7.effect -> nested-asm-lang-v7.effect
  (define (implement-fvars-effect effect)
    (match effect
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
      [`(begin ,es ...)
       `(begin ,@(map implement-fvars-effect es))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(implement-fvars-pred pred)
            ,(implement-fvars-effect e1)
            ,(implement-fvars-effect e2))]
      [`(return-point ,label ,tail)
       `(return-point ,label ,(implement-fvars-tail tail))]))

  ;; nested-asm-lang-fvars-v7.pred -> nested-asm-lang-v7.pred
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

  ;; nested-asm-lang-fvars-v7.loc -> nested-asm-lang-v7.loc
  (define (implement-fvars-loc loc)
    (match loc
      [reg #:when (register? reg) reg]
      [fvar #:when (fvar? fvar) (fvar->addr fvar)]))

  ;; nested-asm-lang-fvars-v7.trg -> nested-asm-lang-v7.trg
  (define (implement-fvars-trg trg)
    (match trg
      [label #:when (label? label) label]
      [loc (implement-fvars-loc loc)]))

  ;; nested-asm-lang-fvars-v7.opand -> nested-asm-lang-v7.opand
  (define (implement-fvars-opand opand)
    (match opand
      [int64 #:when (int64? int64) int64]
      [loc (implement-fvars-loc loc)]))

  ;; nested-asm-lang-fvars-v7.triv -> nested-asm-lang-v7.triv
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
                         (begin (set! rdi 1000) (set! r15 rbx) (jump L.f.2)))))))