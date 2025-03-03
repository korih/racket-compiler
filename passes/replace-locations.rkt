#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5
  rackunit)

(provide replace-locations)

;; asm-lang-v5/assignments -> nested-asm-lang-v5
;; compiles p to Nested-asm-lang v5 by replacing all abstract location with
;; physical locations using the assignment described in the assignment info
;; field, and dropping any register-allocation-related metadata from the program
(define (replace-locations p)
  (-> asm-pred-lang-v5/assignments? nested-asm-lang-v5?)

  ;; func is `(define ,label ,tail)
  ;; interp. a function definition that does not have metadata

  ;; asm-lang-v5/assignments -> (Dict-of aloc rloc)
  ;; interp. creates a dictionary of assignments
  (define (make-assignments-dict assignments)
    (for/fold ([acc (hash)])
              ([pair assignments])
      (dict-set acc (first pair) (second pair))))

  ;; asm-pred-lang-v5/assignments.label asm-pred-lang-v5/assignments.info asm-pred-lang-v5/assignments.tail -> func
  (define (replace-locations-func label info tail)
    (define assignments (make-assignments-dict (info-ref info 'assignment)))
    `(define ,label ,(replace-locations-tail tail assignments)))

  ;; asm-lang-v5/assignments.tail (Dict-of aloc rloc) -> nested-asm-lang-v5.tail
  (define (replace-locations-tail t assignments)
    (match t
      [`(halt ,op)
       `(halt ,(replace-locations-opand op assignments))]
      [`(begin ,fx ... ,tail)
       (define compiled-fx (for/list ([e fx])
                             (replace-locations-effect e assignments)))
       (define compiled-tail (replace-locations-tail tail assignments))
       `(begin ,@compiled-fx ,compiled-tail)]
      [`(if ,pred ,t1 ,t2)
       (define pred^ (replace-locations-pred pred assignments))
       (define t1^ (replace-locations-tail t1 assignments))
       (define t2^ (replace-locations-tail t2 assignments))
       `(if ,pred^ ,t1^ ,t2^)]
      [`(jump ,trg ,locs ...)
       `(jump ,(replace-locations-trg trg assignments))]))

  ;; asm-lang-v5/assignments.effect (Dict-of aloc rloc) -> nested-asm-lang-v5.effect
  (define (replace-locations-effect e assignments)
    (match e
      [`(set! ,loc (,binop ,loc ,op))
       (define loc^ (replace-locations-loc loc assignments))
       (define op^ (replace-locations-opand op assignments))
       `(set! ,loc^ (,binop ,loc^ ,op^))]
      [`(set! ,loc ,triv)
       (define loc^ (replace-locations-loc loc assignments))
       (define triv^ (replace-locations-triv triv assignments))
       `(set! ,loc^ ,triv^)]
      [`(begin ,fx ... ,e)
       (define compiled-fx (for/list ([e fx]) (replace-locations-effect e assignments)))
       (define compiled-e (replace-locations-effect e assignments))
       `(begin ,@compiled-fx ,compiled-e)]
      [`(if ,pred ,e1 ,e2)
       (define pred^ (replace-locations-pred pred assignments))
       (define e1^ (replace-locations-effect e1 assignments))
       (define e2^ (replace-locations-effect e2 assignments))
       `(if ,pred^ ,e1^ ,e2^)]))

  ;; asm-lang-v5/assignments.pred (Dict-of aloc rloc) -> nested-asm-lang-v5.pred
  (define (replace-locations-pred p assignments)
    (match p
      ['(true) p]
      ['(false) p]
      [`(begin ,effects ... ,pred)
       (define effects^
         (for/list ([effect effects])
           (replace-locations-effect effect assignments)))
       (define pred^ (replace-locations-pred pred assignments))
       `(begin ,@effects^ ,pred^)]
      [`(not ,pred) `(not ,(replace-locations-pred pred assignments))]
      [`(,relop ,loc ,op)
       (define loc^ (replace-locations-loc loc assignments))
       (define op^ (replace-locations-opand op assignments))
       `(,relop ,loc^ ,op^)]
      [`(if ,p1 ,p2 ,p3)
       (define p1^ (replace-locations-pred p1 assignments))
       (define p2^ (replace-locations-pred p2 assignments))
       (define p3^ (replace-locations-pred p3 assignments))
       `(if ,p1^ ,p2^ ,p3^)]))

  ;; asm-lang-v5/assignments.triv (Dict-of aloc rloc) -> nested-asm-lang-v5.triv
  (define (replace-locations-triv t assignments)
    (match t
      [label #:when (label? label) label]
      [op (replace-locations-opand op assignments)]))

  ;; asm-lang-v5/assignments.opand (Dict-of aloc rloc) -> nested-asm-lang-v5.opand
  (define (replace-locations-opand op assignments)
    (match op
      [int64 #:when (int64? int64) int64]
      [loc (replace-locations-loc loc assignments)]))

  ;; asm-lang-v5/assignments.loc (Dict-of aloc rloc) -> nested-asm-lang-v5.loc
  (define (replace-locations-loc loc assignments)
    (match loc
      [aloc #:when (aloc? aloc) (dict-ref assignments aloc)]
      [rloc #:when (rloc? rloc) rloc]))

  ;; asm-lang-v5/assignments.trg (Dict-of aloc rloc) -> nested-asm-lang-v5.trg
  (define (replace-locations-trg trg assignments)
    (match trg
      [label #:when (label? label) label]
      [loc (replace-locations-loc loc assignments)]))

  (match p
    [`(module ,info (define ,labels ,infos ,tails) ... ,tail)
     `(module ,@(for/list ([label labels] [info infos] [tail tails])
                  (replace-locations-func label info tail))
        ,(replace-locations-tail tail (make-assignments-dict (info-ref info 'assignment))))]))

(module+ test
  (check-equal? (replace-locations '(module ((locals ()) (assignment ())) (define L.id.13 ((locals (x.2)) (assignment ((x.2 rsp)))) (begin (set! x.2 rdi) (halt x.2))) (begin (set! rdi 5) (jump L.id.13 rbp rdi))))
                '(module
                     (define L.id.13 (begin (set! rsp rdi) (halt rsp)))
                   (begin (set! rdi 5) (jump L.id.13))))
  (check-equal? (replace-locations '(module ((locals (x.1 y.2)) (assignment ((y.2 rsp) (x.1 rsp)))) (begin (set! x.1 3) (set! y.2 x.1) (if (> y.2 x.1) (halt x.1) (halt y.2)))))
                '(module
                     (begin (set! rsp 3) (set! rsp rsp) (if (> rsp rsp) (halt rsp) (halt rsp)))))
  (check-equal? (replace-locations '(module ((locals (tmp.18 y.6)) (assignment ((y.6 rbx) (tmp.18 rsp)))) (begin (set! y.6 200) (if (begin (set! tmp.18 3) (< tmp.18 y.6)) (halt 1) (halt 0)))))
                '(module
                     (begin
                       (set! rbx 200)
                       (if (begin (set! rsp 3) (< rsp rbx)) (halt 1) (halt 0)))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (begin (set! x.1 1) (if (> x.1 0) (set! x.1 2) (set! x.1 3)) (halt x.1))))
                '(module
                     (begin (set! rax 1) (if (> rax 0) (set! rax 2) (set! rax 3)) (halt rax))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (if (true) (true) (false)) (halt 1) (halt 0))))
                '(module (if (if (true) (true) (false)) (halt 1) (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (begin (set! x.1 1) (begin (set! x.1 2) (set! x.1 3)) (> x.1 0)) (halt 1) (halt 0))))
                '(module
                     (if (begin (set! rax 1) (begin (set! rax 2) (set! rax 3)) (> rax 0))
                         (halt 1)
                         (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (begin (set! x.1 1) (set! x.1 1) (> x.1 0)) (halt 1) (halt 0))))
                '(module (if (begin (set! rax 1) (set! rax 1) (> rax 0)) (halt 1) (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (begin (set! x.1 1) (> x.1 0)) (halt 1) (halt 0))))
                '(module (if (begin (set! rax 1) (> rax 0)) (halt 1) (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (not (> x.1 1)) (halt 1) (halt 0))))
                '(module (if (not (> rax 1)) (halt 1) (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (true) (halt 1) (halt 0))))
                '(module (if (true) (halt 1) (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (> x.1 1) (halt 1) (halt 0))))
                '(module (if (> rax 1) (halt 1) (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (begin (if (true)
                                                 (begin
                                                   (set! x.1 (+ x.1 1))
                                                   (if (> x.1 0) (set! x.1 1) (set! x.1 0))
                                                   (halt x.1))
                                                 (begin
                                                   (set! x.1 (+ x.1 2))
                                                   (if (if (true) (false) (false))
                                                       (set! x.1 2)
                                                       (set! x.1 3))
                                                   (halt x.1))))))
                '(module
                     (begin
                       (if (true)
                           (begin
                             (set! rax (+ rax 1))
                             (if (> rax 0) (set! rax 1) (set! rax 0))
                             (halt rax))
                           (begin
                             (set! rax (+ rax 2))
                             (if (if (true) (false) (false)) (set! rax 2) (set! rax 3))
                             (halt rax))))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (begin (set! x.1 (+ x.1 1)) (halt x.1))))
                '(module (begin (set! rax (+ rax 1)) (halt rax))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax)))) (begin (set! x.1 0) (halt x.1))))
                '(module (begin (set! rax 0) (halt rax))))
  (check-equal? (replace-locations '(module ((locals (x.1 y.1 w.1)) (assignment ((x.1 rax) (y.1 rbx) (w.1 r9))))
                                      (begin (set! x.1 0)
                                             (set! y.1 x.1)
                                             (set! w.1 1)
                                             (set! w.1 (+ w.1 y.1))
                                             (halt w.1))))
                '(module (begin (set! rax 0) (set! rbx rax) (set! r9 1) (set! r9 (+ r9 rbx)) (halt r9))))

  (define label2 (fresh-label))
  (check-equal? (replace-locations `(module ((locals (x.1)) (assignment ((x.1 r8))))
                                      (define ,label2
                                        ((locals (x.3)) (assignment ((x.3 r9))))
                                        (begin (set! x.3 (+ x.3 1)) (halt x.3)))
                                      (begin (set! x.1 0) (jump ,label2 x.1))))
                `(module
                     (define ,label2 (begin (set! r9 (+ r9 1)) (halt r9)))
                   (begin (set! r8 0) (jump ,label2))))

  (define label1 (fresh-label))
  (define label3 (fresh-label))
  (check-equal? (replace-locations `(module ((locals (x.2 x.3)) (assignment ((x.2 r7) (x.3 r8))))
                                      (define ,label1
                                        ((locals (x.2)) (assignment ((x.2 r7))))
                                        (begin (set! x.2 0)
                                               (set! x.2 (* x.2 2))
                                               (halt x.2)))
                                      (define ,label3
                                        ((locals (x.3)) (assignment ((x.3 r8))))
                                        (begin
                                          (set! x.3 -1)
                                          (set! x.3 (+ x.3 2))
                                          (halt x.3)))
                                      (begin (set! x.2 1) (jump ,label1 x.2))))
                `(module (define ,label1 (begin (set! r7 0)
                                                (set! r7 (* r7 2))
                                                (halt r7)))
                   (define ,label3 (begin (set! r8 -1)
                                          (set! r8 (+ r8 2))
                                          (halt r8)))
                   (begin (set! r7 1) (jump ,label1))))

  (define label4 (fresh-label))
  (define label5 (fresh-label))
  (check-equal? (replace-locations `(module ((locals (x.1 x.2 x.3)) (assignment ((x.1 rax) (x.2 rbx) (x.3 rcx))))
                                      (define ,label4
                                        ((locals (x.4 x.5)) (assignment ((x.4 r1) (x.5 r2))))
                                        (begin (set! x.4 (+ x.4 x.5)) (jump ,label5 x.4 x.5)))
                                      (define ,label5
                                        ((locals (x.5 x.7)) (assignment ((x.5 r2) (x.7 fv0))))
                                        (begin (set! x.5 (* x.5 x.7)) (halt x.5)))
                                      (begin (set! x.1 1)
                                             (set! x.2 x.1)
                                             (set! x.2 (+ x.2 x.1))
                                             (set! x.3 x.2)
                                             (jump ,label4 x.3))))
                `(module (define ,label4 (begin (set! r1 (+ r1 r2))
                                               (jump ,label5)))
                   (define ,label5 (begin (set! r2 (* r2 fv0)) (halt r2)))
                   (begin (set! rax 1)
                          (set! rbx rax)
                          (set! rbx (+ rbx rax))
                          (set! rcx rbx)
                          (jump ,label4))))

    (define label6 (fresh-label))
    (define label7 (fresh-label))
    (check-equal? (replace-locations `(module ((locals (x.4 x.5 x.6)) (assignment ((x.4 fv0) (x.5 fv1) (x.6 r0))))
                                        (define ,label6 ((locals (x.4)) (assignment ((x.4 r1))))
                                          (jump ,label7 x.4))
                                        (define ,label7 ((locals (x.5)) (assignment ((x.5 r2))))
                                          (jump ,label6 x.5))
                                        (begin (set! x.4 0)
                                          (set! x.5 3)
                                          (set! x.5 (+ x.5 x.4))
                                          (set! x.6 x.5)
                                          (set! x.6 (* x.6 x.5))
                                          (jump ,label6 x.6))))
                  `(module (define ,label6 (jump ,label7))
                     (define ,label7 (jump ,label6))
                     (begin (set! fv0 0)
                       (set! fv1 3)
                       (set! fv1 (+ fv1 fv0))
                       (set! r0 fv1)
                       (set! r0 (* r0 fv1))
                       (jump ,label6)))))
