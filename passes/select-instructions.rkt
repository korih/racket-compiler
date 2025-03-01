#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5
  rackunit)

(provide select-instructions)

;; Exercise 17
;; imp-cmf-lang-v4 -> asm-pred-lang-v4
;; compiles p to Asm-pred-lang v4 by selecting appropriate sequences of abstract
;; assembly instructions to implement the operations of the source language
(define/contract (select-instructions p)
  (-> imp-cmf-lang-v5? asm-pred-lang-v5?)

  ; imp-cmf-lang-v4.value -> (list (List-of asm-pred-lang-v4.effect) aloc)
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

  ;; imp-cmf-lang-v4.tail -> asm-pred-lang-v4.tail
  (define (select-tail e)
    (match e
      [`(jump ,trg ,loc ...) `(jump ,trg ,@loc)]
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
                  `(begin ,@compiled-fx ,@inner-effects^ ,@inner-tail^)]
                 [_ `(begin ,@compiled-fx ,tail-compiled)])])]

      [value (select-value value)]))

  ;; imp-cmf-lang-v4.value -> (list (List-of asm-pred-lang-v4.effect) aloc)
  (define (select-value e)
    (match e
      [`(,binop ,op1 ,op2)
       (define op1-tmp (fresh 'tmp))
       `(begin 
          (set! ,op1-tmp ,op1)
          (set! ,op1-tmp (,binop ,op1-tmp ,op2))
          (halt ,op1-tmp))]
      [triv `(halt ,triv)]))

  ;; aloc imp-cmf-lang-v4.effect -> (List-of asm-pred-lang-v4.effect)
  (define (convert-set-expr x v)
    (match v
      [`(,binop ,op1 ,op2)
       (cond
         [(int64? op1) (list `(set! ,x ,op1) `(set! ,x (,binop ,x ,op2)))]
         [else (list `(set! ,x (,binop ,x ,op2)))])]
      [triv (list `(set! ,x ,triv))]))

  ;; imp-cmf-lang-v4.effect -> (List-of asm-pred-lang-v4.effect)
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

  ;; imp-cmf-lang-v4.triv -> (list (List-of asm-pred-lang-v4.effect) aloc)
  ;; opand -> int or loc and label
  (define (select-triv t)
    (match t
      [x #:when (int64? x)
         (define tmp (fresh 'tmp))
         (list (list `(set! ,tmp ,x)) tmp)]
      [x (list empty x)]))

  ;; imp-cmf-lang-v4.pred -> asm-pred-lang-v4.pred
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

  (define (select-func f)
    (match f
      [`(define ,label ,tail) `(define ,label () ,(select-tail tail))]))

  (match p
    [`(module ,funcs ... ,tail)
     `(module () ,@(map select-func funcs) ,(select-tail tail))]))

(module+ test
  (check-equal? (select-instructions '(module (+ 2 2)))
                '(module () (begin (set! tmp.1 2) (set! tmp.1 (+ tmp.1 2)) (halt tmp.1))))
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
                       (if (if (> rax rbx) (begin (set! tmp.2 1) (< tmp.2 fv1)) (> x.1 5))
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
                     (if (if (not (> x.1 1)) (false) (true)) (halt x.1) (halt x.2))))))
