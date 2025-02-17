#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2
  rackunit)

(provide uncover-locals)

;; asm-lang-v2 -> asm-lang-v2/locals
;; compiles p to asm-lang-v2/locals by analysing which abstract locations are
;; used in the program and decorating the program with the set of variables in
;; an info field
(define/contract (uncover-locals p)
  (-> asm-lang-v2? asm-lang-v2/locals?)

  ;; acc is (Set-of aloc)
  ;; the unique abstract locations used in the program p
  (define unique-alocs (mutable-set))

  (define (uncover-locals-tail t)
    (match t
      [`(halt ,tr)
       (uncover-locals-triv tr)]
      [`(begin ,ef ... ,ta)
       (for-each uncover-locals-effect ef)
       (uncover-locals-tail ta)]))

  (define (uncover-locals-effect e)
    (match e
      [`(set! ,aloc1 (,binop ,aloc1 ,triv))
       (set-add! unique-alocs aloc1)
       (uncover-locals-triv triv)]
      [`(set! ,aloc ,triv)
       (set-add! unique-alocs aloc)
       (uncover-locals-triv triv)]
      [`(begin ,ef ...)
       (for-each uncover-locals-effect ef)]))

  (define (uncover-locals-triv t)
    (match t
      [aloc #:when (aloc? aloc) (set-add! unique-alocs aloc)]
      [int64 #:when (int64? int64) (void)]))

  (match p
    [`(module () ,t)
     (uncover-locals-tail t)
     `(module ,(info-set '() 'locals (set->list unique-alocs)) ,t)]))

(test-case
 "uncover-locals"
 (check-equal? (uncover-locals '(module () (begin (set! x.1 0) (halt x.1))))
               '(module ((locals (x.1))) (begin (set! x.1 0) (halt x.1))))
 (match-let ([`(module ((locals (,ls ...))) ,_) (uncover-locals '(module () (begin (set! x.1 0)
                                                                                   (set! y.1 x.1)
                                                                                   (set! y.1 (+ y.1 x.1))
                                                                                   (halt y.1))))])
   (check-equal? (list->set ls) (set 'x.1 'y.1))))