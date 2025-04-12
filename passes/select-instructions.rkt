#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide select-instructions)

;; imp-cmf-lang-v8 -> asm-alloc-lang-v8
;; compiles p to Asm-alloc-lang v8 by selecting appropriate sequences of abstract
;; assembly instructions to implement the operations of the source language
(define (select-instructions p)
  (-> imp-cmf-lang-v8? asm-alloc-lang-v8?)

  ;; func-info is `(define ,label ,info ,tail)
  ;; interp. a function definition that has metadata

  ;; func-info -> func-info
  (define (select-func f)
    (match f
      [`(define ,label ,info ,tail)
       `(define ,label ,info ,(select-tail tail))]))

  ;; imp-cmf-lang-v8.tail -> asm-alloc-lang-v8.tail
  (define (select-tail t)
    (match t
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
                 [_ `(begin ,@compiled-fx ,tail-compiled)])])]))

  ;; imp-cmf-lang-v8.value loc -> (List-of asm-alloc-lang-v8.effect)
  (define (select-value value loc)
    (match value
      [`(mref ,l ,op)
       `((set! ,loc (mref ,l ,op)))]
      [`(alloc ,op)
       `((set! ,loc (alloc ,op)))]
      [`(,binop ,op1 ,op2)
       (define-values (stmts1 loc1) (select-opand op1 (if (and (eq? binop '-) (eq? loc op2))
                                                          (fresh 'tmp)
                                                          loc)))
       (if (and (eq? binop '-) (not (eq? loc1 loc)))
           (append stmts1
                   `((set! ,loc1 (,binop ,loc1 ,op2))
                     (set! ,loc ,loc1)))
           (append stmts1
                   `((set! ,loc (,binop ,loc ,op2)))))]
      [triv
       `((set! ,loc ,triv))]))

  ;; imp-cmf-lang-v8.effect -> (List-of asm-alloc-lang-v8.effect)
  (define (select-effect e)
    (match e
      [`(set! ,loc ,value)
       (select-value value loc)]
      [`(mset! ,loc ,opand ,triv)
       `((mset! ,loc ,opand ,triv))]
      [`(begin ,fx ... ,e)
       (define compiled-fx (for/foldr ([fx-acc empty])
                             ([e fx])
                             (append (select-effect e) fx-acc)))
       `(,@compiled-fx ,@(select-effect e))]
      [`(if ,pred ,e1 ,e2)
       (define e1^ (match e1
                     [`(begin ,e ...) `((begin ,@(select-effect e1)))]
                     [_ (select-effect e1)]))
       (define e2^ (match e2
                     [`(begin ,e ...) `((begin ,@(select-effect e2)))]
                     [_ (select-effect e2)]))
       (list `(if ,(select-pred pred)
                  ,@e1^
                  ,@e2^))]
      [`(return-point ,label ,tail)
       (list `(return-point ,label ,(select-tail tail)))]))

  ;; imp-cmf-lang-v8.pred -> asm-alloc-lang-v8.pred
  (define (select-pred p)
    (match p
      ['(true) p]
      ['(false) p]
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
       (define-values (stmts loc) (select-triv triv1))
       (if (empty? stmts)
           `(,relop ,loc ,triv2)
           `(begin ,@stmts (,relop ,loc ,triv2)))]))

  ;; imp-cmf-lang-v8.triv -> (List-of asm-alloc-lang-v8.effect) asm-alloc-lang-v8.triv
  (define (select-triv t)
    (match t
      [label #:when (label? label) (values empty label)]
      [int64 #:when (int64? int64)
             (define tmp (fresh 'tmp))
             (values (list `(set! ,tmp ,int64)) tmp)]
      [loc (values empty loc)]))

  ;; imp-cmf-lang-v8.opand loc -> (List-of asm-alloc-lang-v8.effect) asm-alloc-lang-v8.opand
  (define (select-opand op loc)
    (match op
      [int64 #:when (int64? int64)
             (values (list `(set! ,loc ,int64)) loc)]
      [l
       (if (equal? l loc)
           (values empty l)
           (values (list `(set! ,loc ,l)) loc))]))

  (match p
    [`(module ,info ,funcs ... ,tail)
     `(module ,info ,@(map select-func funcs) ,(select-tail tail))]))

