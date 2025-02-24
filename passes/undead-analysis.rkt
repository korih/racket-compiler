#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4
  rackunit)

(provide undead-analysis)

;; asm-lang-v2/locals -> asm-lang-v2/undead
;; compiles p to asm-lang-v2/undead by performing undeadness analysis,
;; decorating the program with undead-set tree
(define/contract (undead-analysis p)
  (-> asm-pred-lang-v4/locals? asm-pred-lang-v4/undead?)

  ;; (asm-pred-lang-v4/locals tail) -> (ListOf undead-set-tree) (Listof undead-in-set)
  ;; go through the tail and analyze the undead sets
  (define (analyze-tail t)
    (match t
      [`(begin ,effects ... ,tail)

       (define-values (t-ust undead-out)
         (analyze-tail tail))

       (define-values (ust undead-in)
         (for/foldr ([rev-ust (list t-ust)]
                     [undead-out undead-out])
           ([effect effects])
           (define-values (ust undead-in)
             (analyze-effects effect undead-out))
           (values (cons ust rev-ust) undead-in)))
       (values ust undead-in)]

      [`(if ,pred ,t1 ,t2)
       (define-values (t1-ust t1-undead-out) (analyze-tail t1))
       (define-values (t2-ust t2-undead-out) (analyze-tail t2))

       ;; union the set of undead-outs
       (define tail-undead-in (set-union t1-undead-out t2-undead-out))

       ;; cons new ust to ust
       (define ust^ (list t1-ust t2-ust))

       ;; analyze pred might need to take undead-in
       (define-values (p-ust p-undead-out) (analyze-pred pred tail-undead-in))

       (values (cons p-ust ust^) p-undead-out)]

      [`(halt ,triv) (define undead-in
                       (analyze-triv triv))
                     (values '() undead-in)]))

  ;; (asm-pred-lang-v4/locals effects) -> (ListOf undead-set-tree) (Listof undead-in-set)
  ;; look through effects and create a undead-set-tree and undead-in-set for current effect
  (define (analyze-effects e undead-out)
    (match e
      [`(begin ,effects ...)
       (define-values (e-ust undead-in)
         (for/foldr ([acc-ust '()]
                     [undead-out^ undead-out])
           ([effect effects])
           (define-values (ust undead-in)
             (analyze-effects effect undead-out^))
           (values (cons ust acc-ust) undead-in)))
       (values e-ust undead-in)]
      [`(set! ,aloc_1 (,binop ,aloc_1 ,triv))
       (define undead-in (set-union
                          (set-add
                           (set-remove undead-out aloc_1)
                           aloc_1)
                          (analyze-triv triv)))
       (values undead-out undead-in)]
      [`(set! ,aloc ,triv)
       (define undead-in (set-union
                          (set-remove undead-out aloc)
                          (analyze-triv triv)))
       (values undead-out undead-in)]
      [`(if ,pred ,e1 ,e2)
       (define-values (e1-ust e1-undead-out) (analyze-effects e1 undead-out))
       (define-values (e2-ust e2-undead-out) (analyze-effects e2 undead-out))

       (define effect-undead-in (set-union e1-undead-out e2-undead-out))
       (define ust (list e1-ust e2-ust))

       (define-values (p-ust p-undead-out) (analyze-pred pred effect-undead-in))

       (values (cons ust p-ust) p-undead-out)]))

  ;; (asm-pred-lang-v4/locals pred) -> (ListOf undead-set-tree) (Listof undead-in-set)
  ;; go through the pred and analyze the undead sets
  (define (analyze-pred p undead-out)
    (match p
      [`(,relop ,aloc ,triv)
       (define undead-in (set-union (set-add undead-out aloc) (analyze-triv triv)))
       (values undead-out undead-in)]
      [`(not ,pred)
       (define-values (ust undead-out) (analyze-pred pred))
       (values ust undead-out)]
      [`(begin ,effects ... ,pred)
       (define-values (p-out p-undead-out)
         (analyze-pred pred))

       (define-values (e-ust e-undead-out)
         (for/foldr ([ust^ (list p-out)]
                     [undead-in^ p-undead-out])
           ([effect effects])
           (define-values (ust undead-in)
             (analyze-effects effect undead-in^))
           (values (cons ust ust^) undead-in)))
       (values e-ust e-undead-out)]

      [`(if ,p1 ,p2 ,p3)
       (define-values (p3-ust p3-undead-in) (analyze-pred p3 undead-out))
       (define-values (p2-ust p2-undead-in) (analyze-pred p2 undead-out))

       (define pred-undead-in (set-union p3-undead-in p2-undead-in))
       (define ust (list p3-ust p2-ust))

       (define-values (p1-ust p1-undead-out) (analyze-pred p1 pred-undead-in))
       (values (cons p1-ust ust) p1-undead-out)] ; just call pred for each one then combine?

      ;; catch case where pred is true or false
      [_ '()]))

  ;; (asm-pred-lang-v4/locals triv) -> (Listof undead-in-set)
  ;; analyze the triv value, if its an aloc return it as a list
  ;; for the undead-in set, if not then return empty list
  (define (analyze-triv triv)
    (match triv
      [x #:when (aloc? x) (list x)]
      [_ '()]))

  ;; info (asm-pred-lang-v4/locals tail) -> (Listof undead-in-set)
  ;; analyze the tail and add the undead set tree to the info
  (define (compile-info i tail)
    (match i
      [`,info
       (define-values (ust^ _)
         (analyze-tail tail))
       (info-set info 'undead-out ust^)]))

  (match p
    [`(module ,info ,tail)
     `(module ,(compile-info info tail) ,tail)]))

(module+ test

  (check-equal? (undead-analysis
                 '(module ((locals ()))
                    (halt x.1)))
                '(module
                     ((locals ()) (undead-out ()))
                   (halt x.1)))
  (check-equal? (undead-analysis
                 '(module ((locals ()))
                    (begin
                      (set! x.1 1)
                      (halt x.1))))
                '(module
                     ((locals ()) (undead-out ((x.1) ())))
                   (begin
                     (set! x.1 1)
                     (halt x.1))))
  (check-equal? (undead-analysis
                 '(module ((locals (x.1)))
                    (begin
                      (set! x.1 42)
                      (halt x.1))))
                '(module
                     ((locals (x.1)) (undead-out ((x.1) ())))
                   (begin (set! x.1 42) (halt x.1)))
                "Testing basic small program")
  (check-equal? (undead-analysis '(module
                                      ((locals (x.1 y.2)))
                                    (begin
                                      (set! y.2 1)
                                      (set! x.1 2)
                                      (set! y.2 (* y.2 x.1))
                                      (begin
                                        (set! x.1 (+ x.1 -1))
                                        (set! y.2 (* y.2 x.1))
                                        (begin
                                          (set! x.1 (+ x.1 -1))
                                          (set! y.2 (* y.2 x.1))))
                                      (halt y.2))))
                '(module
                     ((locals (x.1 y.2))
                      (undead-out ((y.2)
                                   (y.2 x.1)
                                   (x.1 y.2)
                                   ((y.2 x.1)
                                    (x.1 y.2)
                                    ((x.1 y.2)
                                     (y.2)))
                                   ())))
                   (begin
                     (set! y.2 1)
                     (set! x.1 2)
                     (set! y.2 (* y.2 x.1))
                     (begin
                       (set! x.1 (+ x.1 -1))
                       (set! y.2 (* y.2 x.1))
                       (begin
                         (set! x.1 (+ x.1 -1))
                         (set! y.2 (* y.2 x.1))))
                     (halt y.2))))
  (check-equal? (undead-analysis
                 '(module ((locals ()))
                    (halt 1)))
                '(module
                     ((locals ()) (undead-out ()))
                   (halt 1)))
  (check-equal? (undead-analysis
                 '(module ((locals (x.1 y.2 z.3)))
                    (begin
                      (set! x.1 42)
                      (set! x.1 x.1)
                      (set! z.3 x.1)
                      (set! z.3 z.3)
                      (set! z.3 (+ z.3 z.3))
                      (halt z.3))))
                '(module
                     ((locals (x.1 y.2 z.3))
                      (undead-out ((x.1) (x.1) (z.3) (z.3) (z.3) ())))
                   (begin
                     (set! x.1 42)
                     (set! x.1 x.1)
                     (set! z.3 x.1)
                     (set! z.3 z.3)
                     (set! z.3 (+ z.3 z.3))
                     (halt z.3))))
  (check-equal? (undead-analysis
                 '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1)))
                    (begin
                      (set! v.1 1)
                      (set! w.2 46)
                      (set! x.3 v.1)
                      (set! p.1 7)
                      (set! x.3 (+ x.3 p.1))
                      (set! y.4 x.3)
                      (set! p.1 4)
                      (set! y.4 (+ y.4 p.1))
                      (set! z.5 x.3)
                      (set! z.5 (+ z.5 w.2))
                      (set! t.6 y.4)
                      (set! p.1 -1)
                      (set! t.6 (* t.6 p.1))
                      (set! z.5 (+ z.5 t.6))
                      (halt z.5))))
                '(module
                     ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                      (undead-out
                       ((v.1)
                        (v.1 w.2)
                        (x.3 w.2)
                        (p.1 x.3 w.2)
                        (x.3 w.2)
                        (y.4 x.3 w.2)
                        (p.1 y.4 x.3 w.2)
                        (x.3 w.2 y.4)
                        (w.2 z.5 y.4)
                        (y.4 z.5)
                        (t.6 z.5)
                        (p.1 t.6 z.5)
                        (t.6 z.5)
                        (z.5)
                        ())))
                   (begin
                     (set! v.1 1)
                     (set! w.2 46)
                     (set! x.3 v.1)
                     (set! p.1 7)
                     (set! x.3 (+ x.3 p.1))
                     (set! y.4 x.3)
                     (set! p.1 4)
                     (set! y.4 (+ y.4 p.1))
                     (set! z.5 x.3)
                     (set! z.5 (+ z.5 w.2))
                     (set! t.6 y.4)
                     (set! p.1 -1)
                     (set! t.6 (* t.6 p.1))
                     (set! z.5 (+ z.5 t.6))
                     (halt z.5))))
  (check-equal? (undead-analysis '(module
                                      ((locals (x.1 y.2 b.3 c.4)))
                                    (begin
                                      (set! x.1 5)
                                      (set! y.2 x.1)
                                      (begin
                                        (set! b.3 x.1)
                                        (set! b.3 (+ b.3 y.2))
                                        (set! c.4 b.3)
                                        (if (= c.4 b.3)
                                            (halt c.4)
                                            (begin
                                              (set! x.1 c.4)
                                              (halt c.4)))))))
                '(module
                     ((locals (x.1 y.2 b.3 c.4))
                      (undead-out
                       ((x.1) (x.1 y.2) ((y.2 b.3) (b.3) (b.3 c.4) ((c.4) () ((c.4) ()))))))
                   (begin
                     (set! x.1 5)
                     (set! y.2 x.1)
                     (begin
                       (set! b.3 x.1)
                       (set! b.3 (+ b.3 y.2))
                       (set! c.4 b.3)
                       (if (= c.4 b.3) (halt c.4) (begin (set! x.1 c.4) (halt c.4))))))))
