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
  ;; interp. converts the tail body of a function into an explicit sequence of
  ;; effects and a final tail
  (define (select-instructions-func func)
    (match func
      [`(define ,label ,info ,tail)
       `(define ,label ,info ,(call-with-values (lambda () (select-instructions-tail tail))
                                                (lambda (fx tail)
                                                  (make-begin fx tail))))]))

  ;; imp-cmf-lang-v8.tail -> (List-of asm-alloc-lang-v8.effect) asm-alloc-lang-v8.tail
  ;; interp. lowers tail-position expressions, collecting effects and returning
  ;; the transformed tail and a list of side effects that precede it
  (define (select-instructions-tail tail)
    (match tail
      [`(jump ,trg ,locs ...)
       (values empty `(jump ,trg ,@locs))]
      [`(if ,pred ,tail1 ,tail2)
       (values empty
               `(if ,(select-instructions-pred pred)
                    ,(call-with-values (lambda () (select-instructions-tail tail1))
                                       (lambda (fx tail) (make-begin fx tail)))
                    ,(call-with-values (lambda () (select-instructions-tail tail2))
                                       (lambda (fx tail) (make-begin fx tail)))))]
      [`(begin ,fx ... ,tail)
       (define compiled-fx (for/foldr ([instructions empty])
                             ([e fx])
                             (append (select-instructions-effect e) instructions)))
       (define-values (more-compiled-fx tail-compiled) (select-instructions-tail tail))
       (values (append compiled-fx more-compiled-fx)
               tail-compiled)]))

  ;; imp-cmf-lang-v8.value loc -> (List-of asm-alloc-lang-v8.effect)
  ;; interp. generates assembly instructions to compute the given value and store
  ;; the result in loc
  (define (select-instructions-value value loc)
    (match value
      [`(mref ,l ,op)
       `((set! ,loc (mref ,l ,op)))]
      [`(alloc ,op)
       `((set! ,loc (alloc ,op)))]
      [`(,binop ,op1 ,op2)
       (define-values (stmts1 loc1)
         (select-instructions-opand op1 (if (and (eq? binop '-) (eq? loc op2))
                                            (fresh 'tmp)
                                            loc)))
       (if (and (eq? binop '-) (not (eq? loc1 loc)))
           (append stmts1
                   `((set! ,loc1 (,binop ,loc1 ,op2))
                     (set! ,loc ,loc1)))
           (append stmts1
                   `((set! ,loc (,binop ,loc ,op2)))))]
      [triv `((set! ,loc ,triv))]))

  ;; imp-cmf-lang-v8.effect -> (List-of asm-alloc-lang-v8.effect)
  ;; interp. lowers high-level effects into flat instruction sequences
  (define (select-instructions-effect e)
    (match e
      [`(set! ,loc ,value)
       (select-instructions-value value loc)]
      [`(mset! ,loc ,opand ,triv)
       `((mset! ,loc ,opand ,triv))]
      [`(begin ,fx ... ,e)
       (define compiled-fx (for/foldr ([fx-acc empty])
                             ([e fx])
                             (append (select-instructions-effect e) fx-acc)))
       `(,@compiled-fx ,@(select-instructions-effect e))]
      [`(if ,pred ,e1 ,e2)
       (define e1^ (match e1
                     [`(begin ,e ...) `((begin ,@(select-instructions-effect e1)))]
                     [_ (select-instructions-effect e1)]))
       (define e2^ (match e2
                     [`(begin ,e ...) `((begin ,@(select-instructions-effect e2)))]
                     [_ (select-instructions-effect e2)]))
       (list `(if ,(select-instructions-pred pred)
                  ,@e1^
                  ,@e2^))]
      [`(return-point ,label ,tail)
       (list `(return-point ,label ,(call-with-values (lambda () (select-instructions-tail tail))
                                                      (lambda (fx tail) (make-begin fx tail)))))]))

  ;; imp-cmf-lang-v8.pred -> asm-alloc-lang-v8.pred
  ;; interp. lowers compound predicate expressions into instruction sequences,
  ;; lifting side-effectful computations into a preceding begin
  (define (select-instructions-pred pred)
    (match pred
      ['(true) pred]
      ['(false) pred]
      [`(not ,pred)
       `(not ,(select-instructions-pred pred))]
      [`(begin ,fx ... ,pred)
       (define compiled-fx (for/foldr ([fx-acc empty])
                             ([e fx])
                             (append (select-instructions-effect e) fx-acc)))
       `(begin ,@compiled-fx ,(select-instructions-pred pred))]
      [`(if ,pred1 ,pred2 ,pred3)
       `(if ,(select-instructions-pred pred1) ,(select-instructions-pred pred2) ,(select-instructions-pred pred3))]
      [`(,relop ,triv1 ,triv2)
       (define-values (stmts loc) (select-triv triv1))
       (if (empty? stmts)
           `(,relop ,loc ,triv2)
           `(begin ,@stmts (,relop ,loc ,triv2)))]))

  ;; imp-cmf-lang-v8.triv -> (List-of asm-alloc-lang-v8.effect) asm-alloc-lang-v8.triv
  ;; interp. ensures a triv is a location or constant
  (define (select-triv triv)
    (match triv
      [label #:when (label? label) (values empty label)]
      [int64 #:when (int64? int64)
             (define tmp (fresh 'tmp))
             (values (list `(set! ,tmp ,int64)) tmp)]
      [loc (values empty loc)]))

  ;; imp-cmf-lang-v8.opand loc -> (List-of asm-alloc-lang-v8.effect) asm-alloc-lang-v8.opand
  ;; interp. ensures an operand is in the location loc
  (define (select-instructions-opand op loc)
    (match op
      [int64
       #:when (int64? int64)
       (values (list `(set! ,loc ,int64)) loc)]
      [l
       (if (equal? l loc)
           (values empty l)
           (values (list `(set! ,loc ,l)) loc))]))

  (match p
    [`(module ,info ,funcs ... ,tail)
     `(module ,info
        ,@(map select-instructions-func funcs)
        ,(call-with-values (lambda () (select-instructions-tail tail))
                           (lambda (fx tail) (make-begin fx tail))))]))
