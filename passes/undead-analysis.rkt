#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide undead-analysis)

;; asm-pred-lang-v8/locals -> asm-pred-lang-v8/undead
;; compiles p to Asm-pred-lang v8/undead by performing undeadness analysis,
;; decorating the program with undead-set tree
(define/contract (undead-analysis p)
  (-> asm-pred-lang-v8/locals? asm-pred-lang-v8/undead?)

  ;; func-info is `(define ,label ,info ,tail)
  ;; interp. a function definition that has metadata

  ;; call-undead is (Set-of aloc)
  ;; interp. the set of abstract locations in the undead-out set of a return-point

  ;; func-info -> func-info
  (define (analyze-func f)
    (match f
      [`(define ,label ,info ,tail)
       (define-values (undead-tree _ call-undead) (analyze-tail tail))
       (define updated-info (info-set (info-set info 'undead-out undead-tree) 'call-undead call-undead))
       `(define ,label ,updated-info ,tail)]))

  ;; asm-pred-lang-v8/locals.tail -> (values undead-set-tree undead-set call-undead)
  (define (analyze-tail t)
    (match t
      [`(begin ,effects ... ,tail)
       (define-values (t-ust undead-out call-undead-tail)
         (analyze-tail tail))
       (define-values (ust undead-in call-undead)
         (for/foldr ([inner-ust (list t-ust)]
                     [undead-out undead-out]
                     [call-undead-acc call-undead-tail])
           ([effect effects])
           (define-values (ust undead-in call-undead-effect)
             (analyze-effects effect undead-out))
           (values (cons ust inner-ust) undead-in (set-union call-undead-acc call-undead-effect))))
       (values ust undead-in call-undead)]
      [`(if ,pred ,t1 ,t2)
       (define-values (t1-ust t1-undead-out t1-call-undead) (analyze-tail t1))
       (define-values (t2-ust t2-undead-out t2-call-undead) (analyze-tail t2))
       (define tail-undead-in (set-union t1-undead-out t2-undead-out))
       (define ust^ (list t1-ust t2-ust))
       (define-values (p-ust p-undead-out p-call-undead) (analyze-pred pred tail-undead-in))
       (values (cons p-ust ust^) p-undead-out (set-union p-call-undead t1-call-undead t2-call-undead))]
      [`(jump ,trg ,locs ...)
       (values locs (set-union (analyze-trg trg) locs) '())]))

  ;; asm-pred-lang-v8/locals.effect undead-set -> (values undead-set-tree undead-set call-undead)
  (define (analyze-effects e undead-out)
    (match e
      [`(begin ,effects ...)
       (define-values (rev-ust undead-in call-undead)
         (for/foldr ([rev-ust '()]
                     [undead-out undead-out]
                     [call-undead '()])
           ([effect effects])
           (define-values (ust undead-in call-undead-effect)
             (analyze-effects effect undead-out))
           (values
            (cons ust rev-ust)
            undead-in
            (set-union call-undead call-undead-effect))))
       (values rev-ust undead-in call-undead)]
      [`(set! ,loc1 (mref ,loc2 ,index))
       (define undead-loc1 (analyze-loc loc1))
       (define undead-out^ (if (empty? undead-loc1)
                               undead-out
                               (set-remove undead-out (first undead-loc1))))
       (define undead-in (set-union undead-out^ (analyze-loc loc2) (analyze-opand index)))
       (values undead-out undead-in '())]
      [`(set! ,loc (,binop ,loc ,triv))
       (define undead-loc (analyze-loc loc))
       (define undead-set (if (empty? undead-loc)
                              undead-out
                              (set-add (set-remove undead-out (first undead-loc)) (first undead-loc))))
       (define undead-in (set-union undead-set (analyze-triv triv)))
       (values undead-out undead-in '())]
      [`(set! ,loc ,triv)
       (define undead-loc (analyze-loc loc))
       (define undead-set (if (empty? undead-loc)
                              undead-out
                              (set-remove undead-out (first undead-loc))))
       (define undead-in (set-union undead-set (analyze-triv triv)))
       (values undead-out undead-in '())]
      [`(mset! ,loc ,index ,triv)
       (define undead-in (set-union undead-out (analyze-loc loc) (analyze-opand index) (analyze-triv triv)))
       (values undead-out undead-in '())]
      [`(if ,pred ,e1 ,e2)
       (define-values (e1-ust e1-undead-out e1-call-undead) (analyze-effects e1 undead-out))
       (define-values (e2-ust e2-undead-out e2-call-undead) (analyze-effects e2 undead-out))
       (define effect-undead-in (set-union e1-undead-out e2-undead-out))
       (define ust (list e1-ust e2-ust))
       (define-values (p-ust p-undead-out p-call-undead) (analyze-pred pred effect-undead-in))
       (values (cons p-ust ust) p-undead-out (set-union p-call-undead e1-call-undead e2-call-undead))]
      [`(return-point ,label ,tail)
       (define-values (t-ust t-undead-out t-call-undead) (analyze-tail tail))
       (values (list undead-out t-ust)
               (set-remove (set-union t-undead-out undead-out) (current-return-value-register))
               (set-subtract (set-union t-call-undead undead-out) (current-register-set)))]))

  ;; asm-pred-lang-v8/locals.pred undead-set -> (values undead-set-tree undead-set call-undead)
  (define (analyze-pred p undead-out)
    (match p
      [`(not ,pred)
       (analyze-pred pred undead-out)]
      [`(begin ,effects ... ,pred)
       (define-values (p-out p-undead-out p-call-undead)
         (analyze-pred pred undead-out))
       (define-values (e-ust e-undead-out e-call-undead)
         (for/foldr ([ust^ (list p-out)]
                     [undead-in^ p-undead-out]
                     [call-undead^ p-call-undead])
           ([effect effects])
           (define-values (ust undead-in call-undead)
             (analyze-effects effect undead-in^))
           (values (cons ust ust^) undead-in (set-union call-undead call-undead^))))
       (values e-ust e-undead-out e-call-undead)]
      [`(if ,p1 ,p2 ,p3)
       (define-values (p3-ust p3-undead-in p3-call-undead) (analyze-pred p3 undead-out))
       (define-values (p2-ust p2-undead-in p2-call-undead) (analyze-pred p2 undead-out))
       (define pred-undead-in (set-union p3-undead-in p2-undead-in))
       (define ust (list p3-ust p2-ust))
       (define-values (p1-ust p1-undead-out p1-call-undead) (analyze-pred p1 pred-undead-in))
       (values (cons p1-ust ust) p1-undead-out (set-union p1-call-undead p2-call-undead p3-call-undead))]
      [`(,relop ,loc ,op)
       (define undead-loc (analyze-loc loc))
       (define undead-set (if (empty? undead-loc)
                              undead-out
                              (set-add undead-out (first undead-loc))))
       (define undead-in (set-union undead-set (analyze-opand op)))
       (values undead-out undead-in '())]
      ['(true) (values undead-out undead-out '())]
      ['(false) (values undead-out undead-out '())]))

  ;; asm-pred-lang-v8/locals.triv -> (List-of loc)
  (define (analyze-triv triv)
    (match triv
      [label #:when (label? label) '()]
      [opand (analyze-opand opand)]))

  ;; asm-pred-lang-v8/locals.loc -> (List-of loc)
  (define (analyze-loc loc)
    (match loc
      [rloc #:when (rloc? rloc) (list rloc)]
      [aloc #:when (aloc? aloc) (list aloc)]))

  ;; asm-pred-lang-v8/locals.trg -> (List-of loc)
  (define (analyze-trg trg)
    (match trg
      [label #:when (label? label) '()]
      [loc (analyze-loc loc)]))

  ;; asm-pred-lang-v8/locals.opand -> (List-of loc)
  (define (analyze-opand op)
    (match op
      [int64 #:when (int64? int64) '()]
      [loc (analyze-loc loc)]))

  (match p
    [`(module ,info ,funcs ... ,tail)
     (define-values (undead-tree _ call-undead) (analyze-tail tail))
     (define updated-info (info-set (info-set info 'call-undead call-undead) 'undead-out undead-tree))
     `(module ,updated-info ,@(map analyze-func funcs) ,tail)]))

