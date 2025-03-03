#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5
  rackunit)

(provide select-instructions)

;; imp-cmf-lang-v5 -> asm-pred-lang-v5
;; compiles p to Asm-pred-lang v4 by selecting appropriate sequences of abstract
;; assembly instructions to implement the operations of the source language
(define/contract (select-instructions p)
  (-> imp-cmf-lang-v5? any #;asm-pred-lang-v5?)

  ; imp-cmf-lang-v5.value -> (list (List-of asm-pred-lang-v5.effect) aloc)
  ; Assigns the value v to a fresh temporary, returning two values: the list of
  ; statements the implement the assignment in Loc-lang, and the aloc that the
  ; value is stored in
  (define (assign-tmp v)
    (match v
      [`(,binop ,op1 ,op2)
       (match-let ([`(,stmts1 ,loc1) (select-triv op1)]
                   [`(,stmts2 ,loc2) (select-triv op2)])
         (list (append stmts1 stmts2 (list `(set! ,loc1 (,binop ,loc1 ,loc2)))) loc1))]
      [triv (select-triv triv)]))

  (define (select-func f)
    (match f
      [`(define ,label ,tail)
       `(define ,label () ,(select-tail tail))]))

  ;; imp-cmf-lang-v5.tail -> asm-pred-lang-v5.tail
  (define (select-tail e)
    (match e
      [`(jump ,trg ,locs ...) `(jump ,trg ,@locs)]
      [`(if ,pred ,tail1 ,tail2)
       `(if ,(select-pred pred)
            ,(select-tail tail1)
            ,(select-tail tail2))]
      [`(begin ,fx ... ,tail)
       (define compiled-fx (for/foldr ([instructions empty])
                             ([e fx])
                             (append (select-effect e) instructions)))
       (define tail-compiled (select-tail tail))
       (cond
         [(empty? compiled-fx) tail-compiled]
         [else (match tail-compiled
                 [`(begin ,inner-effects^ ... ,inner-tail^)
                  `(begin ,@compiled-fx ,@inner-effects^ ,inner-tail^)]
                 [_ `(begin ,@compiled-fx ,tail-compiled)])])]
      [value (select-value value)]))

  ;; imp-cmf-lang-v5.value -> (List-of asm-pred-lang-v5.effect)
  (define (select-value e)
    (match e
      [`(,binop ,op1 ,op2)
       (define op1-tmp (fresh 'tmp))
       `(begin 
          (set! ,op1-tmp ,op1)
          (set! ,op1-tmp (,binop ,op1-tmp ,op2))
          (halt ,op1-tmp))]
      [triv `(halt ,triv)]))

  ;; aloc imp-cmf-lang-v5.effect -> (List-of asm-pred-lang-v5.effect)
  (define (convert-set-expr x v)
    (match v
      [`(,binop ,op1 ,op2)
       (cond
         [(or (int64? op1) (not (eq? x op1))) (list `(set! ,x ,op1) `(set! ,x (,binop ,x ,op2)))]
         [else (list `(set! ,x (,binop ,x ,op2)))])]
      [triv (list `(set! ,x ,triv))]))

  ;; imp-cmf-lang-v5.effect -> (List-of asm-pred-lang-v5.effect)
  (define (select-effect e)
    (match e
      [`(set! ,x ,v) (convert-set-expr x v)]
      [`(begin ,fx ... ,e)
       (define compiled-fx (for/foldr ([fx-acc empty])
                             ([e fx])
                             (append (select-effect e) fx-acc)))
       `(,@compiled-fx ,@(select-effect e))]
      [`(if ,pred ,e1 ,e2)
       (list `(if ,(select-pred pred)
                  ,@(select-effect e1)
                  ,@(select-effect e2)))]))

  ;; imp-cmf-lang-v5.pred -> asm-pred-lang-v5.pred
  (define (select-pred p)
    (match p
      [`(not ,pred)
       `(not ,(select-pred pred))]
      [`(begin ,fx ... ,pred)
       (define compiled-fx (for/foldr ([fx-acc empty])
                             ([e fx])
                             (append (select-effect e) fx-acc)))
       `(begin ,@compiled-fx ,(select-pred pred))]
      [`(if ,pred1 ,pred2 ,pred3)
       `(if ,(select-pred pred1) ,(select-pred pred2) ,(select-pred pred3))]
      [`(,relop ,triv1 ,triv2)
       (match-let ([`(,stmts ,loc) (select-triv triv1)])
         (if (empty? stmts)
             `(,relop ,loc ,triv2)
             `(begin ,@stmts (,relop ,loc ,triv2))))]
      ['(true) p]
      ['(false) p]))

  ;; imp-cmf-lang-v5.triv -> (list (List-of asm-pred-lang-v5.effect) asm-pred-lang-v5.trg)
  (define (select-triv t)
    (match t
      [label #:when (label? label) (list empty label)]
      [opand (select-opand opand)]))

  ;; imp-cmf-lang-v5.opand -> (list (List-of asm-pred-lang-v5.effect) asm-pred-lang-v5.trg)
  (define (select-opand op)
    (match op
      [int64
       #:when (int64? int64)
       (define tmp (fresh 'tmp))
       (list (list `(set! ,tmp ,int64)) tmp)]
      [loc (select-loc loc)]))

  ;; imp-cmf-lang-v5.loc -> (list (List-of asm-pred-lang-v5.effect) asm-pred-lang-v5.trg)
  (define (select-loc loc)
    (match loc
      [aloc #:when (aloc? aloc) (list empty aloc)]
      [rloc #:when (rloc? rloc) (list empty rloc)]))

  ;; imp-cmf-lang-v5.trg -> (list (List-of asm-pred-lang-v5.effect) asm-pred-lang-v5.trg)
  (define (select-trg trg)
    (match trg
      [label #:when (label? label) (list empty label)]
      [loc (select-loc loc)]))

  (match p
    [`(module ,funcs ... ,tail)
     `(module () ,@(map select-func funcs) ,(select-tail tail))]))

(module+ test
  (check-equal? (select-instructions '(module (begin (set! foo.1 1) (+ foo.1 foo.1))))
                '(module
                     ()
                   (begin
                     (set! foo.1 1)
                     (set! tmp.1 foo.1)
                     (set! tmp.1 (+ tmp.1 foo.1))
                     (halt tmp.1))))
  (check-equal? (select-instructions '(module (begin (set! foo.12 1) (begin (set! bar.13 2) (+ foo.12 bar.13)))))
                '(module
                     ()
                   (begin
                     (set! foo.12 1)
                     (set! bar.13 2)
                     (set! tmp.2 foo.12)
                     (set! tmp.2 (+ tmp.2 bar.13))
                     (halt tmp.2))))
  (check-equal? (select-instructions '(module (begin (set! foo.3 1) (begin (begin (set! x.5 1) (set! bar.4 (+ x.5 5))) (+ foo.3 bar.4)))))
                '(module
                     ()
                   (begin
                     (set! foo.3 1)
                     (set! x.5 1)
                     (set! bar.4 x.5)
                     (set! bar.4 (+ bar.4 5))
                     (set! tmp.3 foo.3)
                     (set! tmp.3 (+ tmp.3 bar.4))
                     (halt tmp.3))))
  (check-equal? (select-instructions '(module (+ 2 2)))
                '(module () (begin (set! tmp.4 2) (set! tmp.4 (+ tmp.4 2)) (halt tmp.4))))
  (check-equal? (select-instructions '(module (define L.start.1 1) (begin (set! x.1 5) x.1)))
                '(module () (define L.start.1 () (halt 1)) (begin (set! x.1 5) (halt x.1))))
  (check-equal? (select-instructions '(module (begin (begin (begin 5)))))
                '(module () (halt 5)))
  (check-equal? (select-instructions
                 '(module
                      (define L.start.1 (begin
                                          (set! x.1 0)
                                          (if
                                           (if (> rax rbx)
                                               (< 1 fv1)
                                               (> x.1 5))
                                           (set! x.1 1)
                                           (set! x.1 2))
                                          x.1))
                    (jump L.start.1 x.1)))
                '(module
                     ()
                   (define L.start.1
                     ()
                     (begin
                       (set! x.1 0)
                       (if (if (> rax rbx) (begin (set! tmp.5 1) (< tmp.5 fv1)) (> x.1 5))
                           (set! x.1 1)
                           (set! x.1 2))
                       (halt x.1)))
                   (jump L.start.1 x.1)))
  (check-equal? (select-instructions '(module (begin (set! x.1 5) x.1)))
                '(module () (begin (set! x.1 5) (halt x.1))))
  (check-equal? (select-instructions '(module (begin (set! x.1 (+ 2 2)) x.1)))
                '(module () (begin (set! x.1 2) (set! x.1 (+ x.1 2)) (halt x.1))))
  (check-equal? (select-instructions '(module (begin (begin (set! x.1 1)) x.1)))
                '(module () (begin (set! x.1 1) (halt x.1))))
  (check-equal? (select-instructions '(module (begin (set! x.1 2) (set! x.1 (+ x.1 x.1)) x.1)))
                '(module () (begin (set! x.1 2) (set! x.1 (+ x.1 x.1)) (halt x.1))))
  (check-equal? (select-instructions '(module (begin (set! x.1 2) (begin (set! x.1 (+ x.1 2))) x.1)))
                '(module () (begin (set! x.1 2) (set! x.1 (+ x.1 2)) (halt x.1))))
  (check-equal? (select-instructions '(module (begin
                                                (begin
                                                  (set! x.1 1)
                                                  (set! x.2 2)
                                                  (if (begin
                                                        (set! x.2 (+ x.2 1))
                                                        (= x.2 3))
                                                      (set! x.1 (+ x.1 x.2))
                                                      (set! x.1 (* x.1 x.2)))
                                                  (if (if (not (> x.1 1)) (false) (true))
                                                      x.1
                                                      x.2)))))
                '(module
                     ()
                   (begin
                     (set! x.1 1)
                     (set! x.2 2)
                     (if (begin (set! x.2 (+ x.2 1)) (= x.2 3))
                         (set! x.1 (+ x.1 x.2))
                         (set! x.1 (* x.1 x.2)))
                     (if (if (not (> x.1 1)) (false) (true)) (halt x.1) (halt x.2)))))
  (check-equal? (select-instructions '(module (begin
                                                (set! foo.3 1)
                                                (begin
                                                  (begin
                                                    (set! x.5 1)
                                                    (set! bar.4 (+ x.5 5)))
                                                  (+ foo.3 bar.4)))))
                '(module
                     ()
                   (begin
                     (set! foo.3 1)
                     (set! x.5 1)
                     (set! bar.4 x.5)
                     (set! bar.4 (+ bar.4 5))
                     (set! tmp.6 foo.3)
                     (set! tmp.6 (+ tmp.6 bar.4))
                     (halt tmp.6))))
  (check-equal? (select-instructions '(module (begin (set! x.1 1) (set! y.1 1) (set! z.1 (+ x.1 y.1)) z.1)))
                '(module
                     ()
                   (begin
                     (set! x.1 1)
                     (set! y.1 1)
                     (set! z.1 x.1)
                     (set! z.1 (+ z.1 y.1))
                     (halt z.1))))
  (check-equal? (select-instructions '(module (begin (set! x.1 0) (if (true) (begin (set! y.2 (+ x.1 17)) (set! x.5 12)) (begin (set! x.5 15))) x.5))
                                     )1)
  (check-equal? (select-instructions '(module (begin (set! x.6 (+ 2 3)) (set! x.7 (+ x.6 x.6)) (begin (set! y.2 5) x.6))))
                '(module
                     ()
                   (begin
                     (set! x.6 2)
                     (set! x.6 (+ x.6 3))
                     (set! x.7 x.6)
                     (set! x.7 (+ x.7 x.6))
                     (set! y.2 5)
                     (halt x.6)))))
