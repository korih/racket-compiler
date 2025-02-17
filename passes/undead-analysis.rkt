#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2
  cpsc411/langs/v2-reg-alloc
  rackunit)

(provide undead-analysis)

;; Exercise 1
;; asm-lang-v2/locals -> asm-lang-v2/undead
;; compiles p to asm-lang-v2/undead by performing undeadness analysis,
;; decorating the program with undead-set tree
(define/contract (undead-analysis p)
  (-> asm-lang-v2/locals? asm-lang-v2/undead?)

  (define (analyze-tail t)
    (match t
      [`(begin ,effects ... ,tail)

       (define-values (t-ust undead-out)
         (analyze-tail tail))

       (define-values (rev-ust undead-in)
         (for/foldr ([rev-ust (list t-ust)]
                     [undead-out undead-out])
           ([effect effects])
           (define-values (ust undead-in)
             (analyze-effects effect undead-out))
           (values (cons ust rev-ust) undead-in)))
       (values rev-ust undead-in)]
      [`(halt ,triv) (define undead-in
                       (analyze-triv triv))
                     (values '() undead-in)]))

  (define (analyze-effects e undead-out)
    (match e
      [`(begin ,effects ...)
       (define-values (rev-ust undead-in)
         (for/foldr ([rev-ust '()]
                     [undead-out undead-out])
           ([effect effects])
           (define-values (ust undead-in)
             (analyze-effects effect undead-out))
           (values (cons ust rev-ust) undead-in)))
       (values rev-ust undead-in)]
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
       (values undead-out undead-in)]))

  (define (analyze-triv triv)
    (match triv
      [x #:when (aloc? x) (list x)]
      [_ '()]))

  (define (compile-info i tail)
    (match i
      [`,info
       (define-values (ust^ _)
         (analyze-tail tail))
       (info-set info 'undead-out ust^)]))

  (match p
    [`(module ,info ,tail)
     `(module ,(compile-info info tail) ,tail)]))

(test-case
 "undead-analysis tests"
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
                    (halt z.5)))))