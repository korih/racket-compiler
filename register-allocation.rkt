#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2)

(provide uncover-locals
         assign-fvars
         replace-locations
         assign-homes)

;; decorate the program with the local variables being used
(define/contract (uncover-locals p)
  (-> asm-lang-v2? asm-lang-v2/locals?)
  (define locals (mutable-set))
  ;; interp. produce an annotated program with the local variables being used
  (define/contract (uncover-locals p)
    (-> asm-lang-v2? asm-lang-v2/locals?)
    (match p
      [`(module ,_ ,tail) (let ([uncovered-tail (uncover-locals/tail tail)])
                            `(module ((locals ,(set->list locals))) ,uncovered-tail))]))
  ;; asm-lang-v2-tail -> asm-lang-v2-tail
  ;; interp. discovers the local variables being used in the program
  (define (uncover-locals/tail t)
    (match t
      [`(halt ,t) `(halt ,(uncover-locals/triv t))]
      [`(begin ,fx ... ,tail) (let ([compiled-fx (for/list ([e fx]) (uncover-locals/effect e))]
                                    [compiled-tail (uncover-locals/tail tail)])
                                `(begin ,@compiled-fx ,compiled-tail))]))
  ;; asm-lang-v2-effect -> asm-lang-v2-effect
  ;; interp. discovers the local variables being used in the program
  (define (uncover-locals/effect e)
    (match e
      [`(set! ,x ,v) (set-add! locals x) `(set! ,x ,(uncover-locals/triv v))]
      [`(begin ,fx ... ,e) (let ([compiled-fx (for/list ([e fx]) (uncover-locals/effect e))]
                                 [compiled-e (uncover-locals/effect e)])
                             `(begin ,@compiled-fx ,compiled-e))]
      [`(set! ,x (,binop ,x ,v)) (set-add! locals x) `(set! ,x (,binop ,x ,(uncover-locals/triv v)))]))
  ;; asm-lang-v2-triv -> asm-lang-v2-triv
  ;; interp. discovers the local variables being used in the program
  (define (uncover-locals/triv t)
    (match t
      [x #:when (aloc? x) (set-add! locals x) x]
      [x x]))
  (uncover-locals p))

;; interp. annotate abstract locations with frame variables
(define/contract (assign-fvars p)
  (-> asm-lang-v2/locals? asm-lang-v2/assignments?)
  ;; interp. annotate abstract locations with frame variables
  (define (assign-fvars p)
    (-> asm-lang-v2/locals? asm-lang-v2/assignments?)
    (match p
      [`(module ((locals (,ls ...))) ,tail)
       (let ([assignments (for/list ([l ls] [i (in-naturals)]) `(,l ,(make-fvar i)))])
         `(module ((locals ,ls) (assignment ,assignments)) ,tail))]))
  (assign-fvars p))

;; interp. replaces the abstract locations with the concrete locations
(define (replace-locations p)
  (-> asm-lang-v2/assignments? nested-asm-lang-v2?)
  (define assignments (make-hash))

  ;; interp. replaces the abstract locations with the concrete locations
  (define/contract (replace-locations p)
    (-> asm-lang-v2/assignments? nested-asm-lang-v2?)
    (match p
      [`(module ((locals (,_ ...)) (assignment ((,xs ,regx) ...))) ,tail)
       (for ([x xs] [reg regx])
         (dict-set! assignments x reg))
       (replace-locations/tail tail)]))
  ;; (asm-lang-v2/assignments-tail) -> (nested-asm-lang-v2-tail)
  ;; interp. replaces the abstract locations with the concrete locations
  (define (replace-locations/tail t)
    (match t
      [`(halt ,triv) `(halt ,(replace-locations/triv triv))]
      [`(begin ,fx ... ,tail) (let ([compiled-fx (for/list ([e fx]) (replace-locations/effect e))]
                                    [compiled-tail (replace-locations/tail tail)])
                                `(begin ,@compiled-fx ,compiled-tail))]))
  ;; (asm-lang-v2/assignments-effect) -> (nested-asm-lang-v2-effect)
  ;; interp. replaces the abstract locations with the concrete locations
  (define (replace-locations/effect e)
    (match e
      [`(set! ,x (,binop ,x ,v)) (let ([reg (dict-ref assignments x)])
                                   `(set! ,reg (,binop ,reg ,(replace-locations/triv v))))]
      [`(set! ,x ,v) `(set! ,(dict-ref assignments x) ,(replace-locations/triv v))]
      [`(begin ,fx ... ,e) (let ([compiled-fx (for/list ([e fx]) (replace-locations/effect e))]
                                 [compiled-e (replace-locations/effect e)])
                             `(begin ,@compiled-fx ,compiled-e))]))
  ;; (asm-lang-v2-triv) -> (nested-asm-lang-v2-triv)
  ;; interp. replaces any abstract locations with the concrete locations
  (define (replace-locations/triv t)
    (match t
      [`,x #:when (aloc? x) (dict-ref assignments x)]
      [`,x x]))

  (replace-locations p))

;; interp. compiles program and replaces abstract locations with concrete locations
(define (assign-homes p)
  (-> asm-lang-v2? nested-asm-lang-v2?)
  (replace-locations (assign-fvars (uncover-locals p))))
