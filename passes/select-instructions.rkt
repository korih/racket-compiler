#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2
  cpsc411/langs/v3
  rackunit)

(provide select-instructions)

;; imp-cmf-lang-v3 -> asm-lang-v2
;; intep. compile value abstractions into a sequence of instructions
(define/contract (select-instructions p)
  (-> imp-cmf-lang-v3? asm-lang-v2?)

  ;; (imp-cmf-lang-v3-tail) -> bool
  ;; interp. compiler that returns true if the expression is a valid value in tail position
  (define (tail-value? e)
    (match e
      [`(begin ,fx ... ,tail) #f]
      [_ #t]))

  ; (Imp-cmf-lang-v3 value) -> (List-of (Asm-lang-v2 effect)) and (Asm-lang-v2 aloc)
  ; Assigns the value v to a fresh temporary, returning two values: the list of
  ; statements the implement the assignment in Loc-lang, and the aloc that the
  ; value is stored in.
  (define (assign-tmp v)
    (match v
      [`(,binop ,op1 ,op2)
       (match-let ([`(,stmts1 ,loc1) (select-triv op1)]
                   [`(,stmts2 ,loc2) (select-triv op2)])
         (list (append stmts1 stmts2 (list `(set! ,loc1 (,binop ,loc1 ,loc2)))) loc1))]
      [triv (select-triv triv)]))


  ;; imp-cmf-lang-v3-tail -> asm-lang-v2-tail
  ;; interp. produce the asm-lang-v2-tail and halt with the trivial value
  (define (select-tail e)
    (match e
      [`(begin ,fx ... ,tail)
       (define compiled-fx (for/foldr ([instructions empty])
                             ([e fx])
                             (append (select-effect e) instructions)))
       (define tail-compiled (select-tail tail))
       (match tail-compiled
         [`(begin ,inner-compiled-fx ... ,inner-compiled-tail)
          #:when (tail-value? tail)
          `(begin ,@compiled-fx ,@inner-compiled-fx ,inner-compiled-tail)]
         [_ `(begin ,@compiled-fx ,tail-compiled)])]
      [value (match-let ([`(,stmts ,loc) (select-value value)])
               (if (empty? stmts)
                   `(halt ,loc)
                   `(begin ,@stmts (halt ,loc))))]))

  ;; imp-cmf-lang-v3-value -> (list (listof asm-lang-v2-effect) asm-lang-v2-aloc)
  ;; interp. compiles value expression and creates temporary abstract locations
  ;; to store intermediate values
  (define (select-value e)
    (match e
      [`(,binop ,op1 ,op2)
       (define op1-tmp (fresh 'tmp))
       (define op2-tmp (fresh 'tmp))
       (list (list `(set! ,op1-tmp ,op1)
                   `(set! ,op2-tmp ,op2)
                   `(set! ,op1-tmp (,binop ,op1-tmp ,op2-tmp)))
             op1-tmp)]
      [triv (select-triv triv)]))

  ;; imp-cmf-lang-v3-effect -> (listof asm-lang-v2-effect)
  ;; interp. convert expressions of the form (set! x v) into (set! x triv) and
  ;; (set! x (binop x triv))
  (define (convert-set-expr x v)
    (match v
      [`(,binop ,op1 ,op2)
       (list `(set! ,x ,op1) `(set! ,x (,binop ,x ,op2)))]
      [triv (list `(set! ,x ,triv))]))

  ;; imp-cmf-lang-v3-effect -> (listof asm-lang-v2-effect)
  ;; interp. compiles effect expression into a sequence of instructions,
  ;; resolving values to abstract locations
  (define (select-effect e)
    (match e
      [`(set! ,x ,v) (convert-set-expr x v)]
      [`(begin ,fx ... ,e)
       (define compiled-fx (for/foldr ([fx-acc empty])
                             ([e fx])
                             (append (select-effect e) fx-acc)))
       (list `(begin ,@compiled-fx ,@(select-effect e)))]))

  ;; imp-cmf-lang-v3-triv -> (list (listof asm-lang-v2-effect) asm-lang-v2-aloc)
  ;; interp. compiles trivial expressions into a sequence of instructions and
  ;; returns the abstract location
  (define (select-triv t)
    (match t
      [x #:when (aloc? x) (list empty x)]
      [x
       (define tmp (fresh 'tmp))
       (list (list `(set! ,tmp ,x)) tmp)]))

  (match p
    [`(module ,tail)
     `(module () ,(select-tail tail))]))

(test-case
 "select-instructions"
 (define cmf-lang-v3-1 '(module (+ 2 2)))
 (check-equal? (interp-imp-cmf-lang-v3 cmf-lang-v3-1) (interp-asm-lang-v2 (select-instructions cmf-lang-v3-1)))

 (check-equal? (select-instructions '(module (begin (set! x.1 5) x.1)))
               '(module () (begin (set! x.1 5) (halt x.1))))
 (check-equal? (select-instructions '(module (begin (set! x.1 (+ 2 2)) x.1)))
               '(module () (begin (set! x.1 2) (set! x.1 (+ x.1 2)) (halt x.1))))

 (define cmf-lang-v3-2 '(module (begin (set! x.1 2) (set! x.2 2) (+ x.1 x.2))))
 (check-equal? (interp-imp-cmf-lang-v3 cmf-lang-v3-2) (interp-asm-lang-v2 (select-instructions cmf-lang-v3-2)))

 (check-equal? (select-instructions '(module (begin (begin (set! x.1 1)) x.1)))
               '(module () (begin (begin (set! x.1 1)) (halt x.1))))

 (define cmf-lang-v3-3 '(module (begin (set! foo.12 1) (begin (begin (set! x.14 1) (set! bar.13 (+ x.14 5)))
                                                              (+ foo.12 bar.13)))))
 (check-equal? (interp-imp-cmf-lang-v3 cmf-lang-v3-3) (interp-asm-lang-v2 (select-instructions cmf-lang-v3-3))))