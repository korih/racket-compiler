#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v7
  rackunit)

(provide undead-analysis)

;; asm-pred-lang-v7/locals -> asm-pred-lang-v7/undead
;; compiles p to Asm-pred-lang v7/undead by performing undeadness analysis,
;; decorating the program with undead-set tree
(define/contract (undead-analysis p)
  (-> asm-pred-lang-v7/locals? asm-pred-lang-v7/undead?)

  ;; func-info is `(define ,label ,info ,tail)
  ;; interp. a function definition that has metadata

  ;; call-undead is (setof? aloc)
  ;; this is the set of abstract locations in the undead-out set of a return-point

  ;; func-info -> func-info
  (define (analyze-func f)
    (match f
      [`(define ,label ,info ,tail)
       (define-values (undead-tree _ call-undead) (analyze-tail tail))
       (define updated-info (info-set (info-set info 'undead-out undead-tree) 'call-undead call-undead))
       `(define ,label ,updated-info ,tail)]))

  ;; asm-pred-lang-v7/locals.tail -> (values undead-set-tree undead-set call-undead)
  (define (analyze-tail t)
    (match t
      [`(begin ,effects ... ,tail)
       (define-values (t-ust undead-out call-undead-tail)
         (analyze-tail tail))
       (define-values (ust undead-in call-undead)
         (for/foldr ([rev-ust (list t-ust)]
                     [undead-out undead-out]
                     [call-undead-acc call-undead-tail])
           ([effect effects])
           (define-values (ust undead-in call-undead-effect)
             (analyze-effects effect undead-out))
           (values (cons ust rev-ust) undead-in (set-union call-undead-acc call-undead-effect))))
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

  ;; asm-pred-lang-v7/locals.effect undead-set -> (values undead-set-tree undead-set call-undead)
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

  ;; asm-pred-lang-v7/locals.pred undead-set -> (values undead-set-tree undead-set call-undead)
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

  ;; asm-pred-lang-v7/locals.triv -> (List-of loc)
  (define (analyze-triv triv)
    (match triv
      [label #:when (label? label) '()]
      [opand (analyze-opand opand)]))

  ;; asm-pred-lang-v7/locals.loc -> (List-of loc)
  (define (analyze-loc loc)
    (match loc
      [rloc #:when (rloc? rloc) (list rloc)]
      [aloc #:when (aloc? aloc) (list aloc)]))

  ;; asm-pred-lang-v7/locals.trg -> (List-of loc)
  (define (analyze-trg trg)
    (match trg
      [label #:when (label? label) '()]
      [loc (analyze-loc loc)]))

  ;; asm-pred-lang-v7/locals.opand -> (List-of loc)
  (define (analyze-opand op)
    (match op
      [int64 #:when (int64? int64) '()]
      [loc (analyze-loc loc)]))

  (match p
    [`(module ,info ,funcs ... ,tail)
     (define-values (undead-tree _ call-undead) (analyze-tail tail))
     (define updated-info (info-set (info-set info 'call-undead call-undead) 'undead-out undead-tree))
     `(module ,updated-info ,@(map analyze-func funcs) ,tail)]))

(module+ test
  (define ns (make-base-namespace))
  (eval '(require racket/set) ns)

  (check-equal? (undead-analysis '(module
                                      ((new-frames ()) (locals ()))
                                    (define L.f.1
                                      ((new-frames ()) (locals (tmp.1)))
                                      (begin (set! tmp.1 1) (set! tmp.1 (* tmp.1 2)) (jump tmp.1)))
                                    (jump L.f.1 rbp)))
                '(module
                     ((new-frames ()) (locals ()) (call-undead ()) (undead-out (rbp)))
                   (define L.f.1
                     ((new-frames ())
                      (locals (tmp.1))
                      (undead-out ((tmp.1) (tmp.1) ()))
                      (call-undead ()))
                     (begin (set! tmp.1 1) (set! tmp.1 (* tmp.1 2)) (jump tmp.1)))
                   (jump L.f.1 rbp)))
  (match (undead-analysis '(module
                               ((new-frames ()) (locals (k.1 j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1)))
                             (define L.f.1
                               ((new-frames ()) (locals (k.1 j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1)))
                               (begin
                                 (set! a.1 rdi)
                                 (set! b.1 rsi)
                                 (set! c.1 rdx)
                                 (set! d.1 rcx)
                                 (set! e.1 r8)
                                 (set! f.1 r9)
                                 (set! g.1 fv0)
                                 (set! h.1 fv1)
                                 (set! i.1 fv2)
                                 (set! j.1 fv3)
                                 (set! k.1 fv4)
                                 (jump L.f.1)))
                             (begin
                               (set! a.1 1)
                               (set! b.1 2)
                               (set! c.1 3)
                               (set! d.1 4)
                               (set! e.1 5)
                               (set! f.1 6)
                               (set! g.1 7)
                               (set! h.1 8)
                               (set! i.1 9)
                               (set! j.1 10)
                               (set! k.1 11)
                               (set! fv4 k.1)
                               (set! fv3 j.1)
                               (set! fv2 i.1)
                               (set! fv1 h.1)
                               (set! fv0 g.1)
                               (set! r9 f.1)
                               (set! r8 e.1)
                               (set! rcx d.1)
                               (set! rdx c.1)
                               (set! rsi b.1)
                               (set! rdi a.1)
                               (jump L.f.1 rbp rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4))))
    [`(module ((new-frames ()) (locals (,ls ...)) (call-undead ()) (undead-out (,uos ...)))
        ,func ,tail)
     (check-equal? func
                   '(define L.f.1
                      ((new-frames ())
                       (locals (k.1 j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                       (undead-out
                        ((rsi rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4)
                         (rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4)
                         (rcx r8 r9 fv0 fv1 fv2 fv3 fv4)
                         (r8 r9 fv0 fv1 fv2 fv3 fv4)
                         (r9 fv0 fv1 fv2 fv3 fv4)
                         (fv0 fv1 fv2 fv3 fv4)
                         (fv1 fv2 fv3 fv4)
                         (fv2 fv3 fv4)
                         (fv3 fv4)
                         (fv4)
                         ()
                         ()))
                       (call-undead ()))
                      (begin
                        (set! a.1 rdi)
                        (set! b.1 rsi)
                        (set! c.1 rdx)
                        (set! d.1 rcx)
                        (set! e.1 r8)
                        (set! f.1 r9)
                        (set! g.1 fv0)
                        (set! h.1 fv1)
                        (set! i.1 fv2)
                        (set! j.1 fv3)
                        (set! k.1 fv4)
                        (jump L.f.1))))
     (check-equal? tail
                   '(begin
                      (set! a.1 1)
                      (set! b.1 2)
                      (set! c.1 3)
                      (set! d.1 4)
                      (set! e.1 5)
                      (set! f.1 6)
                      (set! g.1 7)
                      (set! h.1 8)
                      (set! i.1 9)
                      (set! j.1 10)
                      (set! k.1 11)
                      (set! fv4 k.1)
                      (set! fv3 j.1)
                      (set! fv2 i.1)
                      (set! fv1 h.1)
                      (set! fv0 g.1)
                      (set! r9 f.1)
                      (set! r8 e.1)
                      (set! rcx d.1)
                      (set! rdx c.1)
                      (set! rsi b.1)
                      (set! rdi a.1)
                      (jump L.f.1 rbp rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4)))
     (for ([uo uos]
           [expected (list (set 'a.1 'rbp)
                           (set 'b.1 'a.1 'rbp)
                           (set 'c.1 'b.1 'a.1 'rbp)
                           (set 'd.1 'c.1 'b.1 'a.1 'rbp)
                           (set 'e.1 'd.1 'c.1 'b.1 'a.1 'rbp)
                           (set 'f.1 'e.1 'd.1 'c.1 'b.1 'a.1 'rbp)
                           (set 'g.1 'f.1 'e.1 'd.1 'c.1 'b.1 'a.1 'rbp)
                           (set 'h.1 'g.1 'f.1 'e.1 'd.1 'c.1 'b.1 'a.1 'rbp)
                           (set 'i.1 'h.1 'g.1 'f.1 'e.1 'd.1 'c.1 'b.1 'a.1 'rbp)
                           (set 'j.1 'i.1 'h.1 'g.1 'f.1 'e.1 'd.1 'c.1 'b.1 'a.1 'rbp)
                           (set 'k.1 'j.1 'i.1 'h.1 'g.1 'f.1 'e.1 'd.1 'c.1 'b.1 'a.1 'rbp)
                           (set 'j.1 'i.1 'h.1 'g.1 'f.1 'e.1 'd.1 'c.1 'b.1 'a.1 'fv4 'rbp)
                           (set 'i.1 'h.1 'g.1 'f.1 'e.1 'd.1 'c.1 'b.1 'a.1 'fv4 'fv3 'rbp)
                           (set 'h.1 'g.1 'f.1 'e.1 'd.1 'c.1 'b.1 'a.1 'fv4 'fv3 'fv2 'rbp)
                           (set 'g.1 'f.1 'e.1 'd.1 'c.1 'b.1 'a.1 'fv4 'fv3 'fv2 'fv1 'rbp)
                           (set 'f.1 'e.1 'd.1 'c.1 'b.1 'a.1 'fv4 'fv3 'fv2 'fv1 'fv0 'rbp)
                           (set 'e.1 'd.1 'c.1 'b.1 'a.1 'fv4 'fv3 'fv2 'fv1 'fv0 'r9  'rbp)
                           (set 'd.1 'c.1 'b.1 'a.1 'fv4 'fv3 'fv2 'fv1 'fv0 'r9  'r8  'rbp)
                           (set 'c.1 'b.1 'a.1 'fv4 'fv3 'fv2 'fv1 'fv0 'r9  'r8  'rcx 'rbp)
                           (set 'b.1 'a.1 'fv4 'fv3 'fv2 'fv1 'fv0 'r9  'r8  'rcx 'rdx 'rbp)
                           (set 'a.1 'fv4 'fv3 'fv2 'fv1 'fv0 'r9  'r8  'rcx 'rdx 'rsi 'rbp)
                           (set 'fv4 'fv3 'fv2 'fv1 'fv0 'r9  'r8  'rcx 'rdx 'rsi 'rdi 'rbp)
                           (set 'fv4 'fv3 'fv2 'fv1 'fv0 'r9  'r8  'rcx 'rdx 'rsi 'rdi 'rbp))])
       (check-true (set=? (list->set uo) expected)))])
  (match (undead-analysis '(module
                               ((new-frames ()) (locals ()))
                             (define L.f.1
                               ((new-frames ()) (locals (f.1 e.1 d.1 c.1 b.1 a.1)))
                               (begin
                                 (set! a.1 rdi)
                                 (set! b.1 rsi)
                                 (set! c.1 rdx)
                                 (set! d.1 rcx)
                                 (set! e.1 r8)
                                 (set! f.1 r9)
                                 (set! a.1 (+ a.1 b.1))
                                 (set! a.1 (+ a.1 c.1))
                                 (set! a.1 (+ a.1 d.1))
                                 (set! a.1 (+ a.1 e.1))
                                 (set! a.1 (+ a.1 f.1))
                                 (jump a.1)))
                             (begin
                               (set! r9 6)
                               (set! r8 5)
                               (set! rcx 4)
                               (set! rdx 3)
                               (set! rsi 2)
                               (set! rdi 1)
                               (jump L.f.1 rbp rdi rsi rdx rcx r8 r9))))
    [`(module ((new-frames ()) (locals ()) (call-undead ()) (undead-out (,uo ...))) ,defn ,tail)
     (check-equal? defn
                   '(define L.f.1
                      ((new-frames ())
                       (locals (f.1 e.1 d.1 c.1 b.1 a.1))
                       (undead-out
                        ((rsi rdx rcx r8 r9 a.1)
                         (rdx rcx r8 r9 b.1 a.1)
                         (rcx r8 r9 b.1 a.1 c.1)
                         (r8 r9 b.1 a.1 c.1 d.1)
                         (r9 b.1 a.1 c.1 d.1 e.1)
                         (b.1 a.1 c.1 d.1 e.1 f.1)
                         (c.1 a.1 d.1 e.1 f.1)
                         (d.1 a.1 e.1 f.1)
                         (e.1 a.1 f.1)
                         (f.1 a.1)
                         (a.1)
                         ()))
                       (call-undead ()))
                      (begin
                        (set! a.1 rdi)
                        (set! b.1 rsi)
                        (set! c.1 rdx)
                        (set! d.1 rcx)
                        (set! e.1 r8)
                        (set! f.1 r9)
                        (set! a.1 (+ a.1 b.1))
                        (set! a.1 (+ a.1 c.1))
                        (set! a.1 (+ a.1 d.1))
                        (set! a.1 (+ a.1 e.1))
                        (set! a.1 (+ a.1 f.1))
                        (jump a.1))))
     (check-equal? tail
                   '(begin
                      (set! r9 6)
                      (set! r8 5)
                      (set! rcx 4)
                      (set! rdx 3)
                      (set! rsi 2)
                      (set! rdi 1)
                      (jump L.f.1 rbp rdi rsi rdx rcx r8 r9)))
     (for ([uo uo]
           [expected '((r9 rbp)
                       (r9 r8 rbp)
                       (r9 r8 rcx rbp)
                       (r9 r8 rcx rdx rbp)
                       (r9 r8 rcx rdx rsi rbp)
                       (r9 r8 rcx rdx rsi rdi rbp)
                       (r9 r8 rcx rdx rsi rdi rbp))])
       (check-true (set=? (list->set uo) (list->set expected))))])
  (match (undead-analysis '(module ((new-frames ()) (locals ()))
                             (define L.f.1 ((new-frames ()) (locals (x.1))) (begin (set! x.1 rdi) (jump x.1)))
                             (begin (set! rdi 1) (jump L.f.1 rbp rdi))))
    [`(module ((new-frames ()) (locals ()) (call-undead ()) (undead-out (,uo ...)))
        (define L.f.1 ((new-frames ()) (locals (x.1)) (undead-out ((x.1) ())) (call-undead ()))
          (begin (set! x.1 rdi) (jump x.1)))
        (begin (set! rdi 1) (jump L.f.1 rbp rdi)))
     (for ([uo uo]
           [expected '((rdi rbp)
                       (rdi rbp))])
       (check-true (set=? (list->set uo) (list->set expected))))])
  (check-match (undead-analysis '(module ((new-frames ()) (locals ()))
                                   (define L.f.1 ((new-frames ()) (locals (x.1))) (begin (set! x.1 rdi) (jump x.1)))
                                   (begin (set! r13 L.f.1) (set! rdi 1) (jump r13 rbp rdi))))
               `(module
                    ((new-frames ())
                     (locals ())
                     (call-undead ())
                     (undead-out (,uo ...)))
                  (define L.f.1
                    ((new-frames ()) (locals (x.1)) (undead-out ((x.1) ())) (call-undead ()))
                    (begin (set! x.1 rdi) (jump x.1)))
                  (begin (set! r13 L.f.1) (set! rdi 1) (jump r13 rbp rdi)))
               (for/and ([uo uo]
                         [expected '((r13 rbp)
                                     (r13 rdi rbp)
                                     (rdi rbp))])
                 (set=? (list->set uo) (list->set expected))))
  (check-match (undead-analysis '(module
                                     ((new-frames ()) (locals ()))
                                   (define L.f.1 ((new-frames ()) (locals (x.1))) (begin (set! x.1 rdi) (jump x.1)))
                                   (define L.g.1
                                     ((new-frames ()) (locals (y.1 x.1 z.1)))
                                     (begin
                                       (set! x.1 rdi)
                                       (set! y.1 rsi)
                                       (set! z.1 rdx)
                                       (set! rdi x.1)
                                       (jump L.f.1 rbp rdi)))
                                   (if (true)
                                       (begin
                                         (set! rdx 3)
                                         (set! rsi 2)
                                         (set! rdi 1)
                                         (jump L.g.1 rbp rdi rsi rdx))
                                       (begin (set! rdi 1) (jump L.f.1 rbp rdi)))))
               `(module
                    ((new-frames ())
                     (locals ())
                     (call-undead ())
                     (undead-out
                      ((rbp)
                       (,uo1 ...)
                       (,uo2 ...))))
                  (define L.f.1
                    ((new-frames ()) (locals (x.1)) (undead-out ((x.1) ())) (call-undead ()))
                    (begin (set! x.1 rdi) (jump x.1)))
                  (define L.g.1
                    ((new-frames ())
                     (locals (y.1 x.1 z.1))
                     (undead-out
                      (,uo3 ...))
                     (call-undead ()))
                    (begin
                      (set! x.1 rdi)
                      (set! y.1 rsi)
                      (set! z.1 rdx)
                      (set! rdi x.1)
                      (jump L.f.1 rbp rdi)))
                  (if (true)
                      (begin (set! rdx 3) (set! rsi 2) (set! rdi 1) (jump L.g.1 rbp rdi rsi rdx))
                      (begin (set! rdi 1) (jump L.f.1 rbp rdi))))
               (and (for/and ([uo uo1]
                              [expected '((rdx rbp) (rdx rsi rbp) (rdx rsi rdi rbp) (rdx rsi rdi rbp))])
                      (set=? (list->set uo) (list->set expected)))
                    (for/and ([uo uo2]
                              [expected '((rdi rbp) (rdi rbp))])
                      (set=? (list->set uo) (list->set expected)))
                    (for/and ([uo uo3]
                              [expected '((rsi rdx x.1 rbp) (rdx x.1 rbp) (x.1 rbp) (rdi rbp) (rdi rbp))])
                      (set=? (list->set uo) (list->set expected)))))
  (check-equal? (undead-analysis
                 '(module ((new-frames ()) (locals ()))
                    (jump x.1)))
                '(module
                     ((new-frames ()) (locals ()) (call-undead ()) (undead-out ()))
                   (jump x.1)))
  (check-equal? (undead-analysis
                 '(module ((new-frames ()) (locals ()))
                    (begin
                      (set! x.1 1)
                      (jump x.1))))
                '(module
                     ((new-frames ()) (locals ()) (call-undead ()) (undead-out ((x.1) ())))
                   (begin (set! x.1 1) (jump x.1))))
  (check-equal? (undead-analysis
                 '(module ((new-frames ()) (locals (x.1)))
                    (begin
                      (set! x.1 42)
                      (jump x.1))))
                '(module
                     ((new-frames ()) (locals (x.1)) (call-undead ()) (undead-out ((x.1) ())))
                   (begin (set! x.1 42) (jump x.1)))
                "Testing basic small program")
  (check-equal? (undead-analysis '(module
                                      ((new-frames ()) (locals (x.1 y.2)))
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
                                      (jump y.2))))
                '(module
                     ((new-frames ())
                      (locals (x.1 y.2))
                      (call-undead ())
                      (undead-out
                       ((y.2) (y.2 x.1) (x.1 y.2) ((y.2 x.1) (x.1 y.2) ((x.1 y.2) (y.2))) ())))
                   (begin
                     (set! y.2 1)
                     (set! x.1 2)
                     (set! y.2 (* y.2 x.1))
                     (begin
                       (set! x.1 (+ x.1 -1))
                       (set! y.2 (* y.2 x.1))
                       (begin (set! x.1 (+ x.1 -1)) (set! y.2 (* y.2 x.1))))
                     (jump y.2))))
  (check-equal? (undead-analysis
                 '(module ((new-frames ()) (locals ()))
                    (jump r8)))
                '(module
                     ((new-frames ()) (locals ()) (call-undead ()) (undead-out ()))
                   (jump r8)))
  (check-equal? (undead-analysis
                 '(module ((new-frames ()) (locals (x.1 y.2 z.3)))
                    (begin
                      (set! x.1 42)
                      (set! x.1 x.1)
                      (set! z.3 x.1)
                      (set! z.3 z.3)
                      (set! z.3 (+ z.3 z.3))
                      (jump z.3))))
                '(module
                     ((new-frames ())
                      (locals (x.1 y.2 z.3))
                      (call-undead ())
                      (undead-out ((x.1) (x.1) (z.3) (z.3) (z.3) ())))
                   (begin
                     (set! x.1 42)
                     (set! x.1 x.1)
                     (set! z.3 x.1)
                     (set! z.3 z.3)
                     (set! z.3 (+ z.3 z.3))
                     (jump z.3))))
  (check-equal? (undead-analysis
                 '(module ((new-frames ()) (locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1)))
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
                      (jump z.5))))
                '(module
                     ((new-frames ())
                      (locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                      (call-undead ())
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
                     (jump z.5))))
  (check-equal? (undead-analysis '(module
                                      ((new-frames ()) (locals (x.1 y.2 b.3 c.4)))
                                    (begin
                                      (set! x.1 5)
                                      (set! y.2 x.1)
                                      (begin
                                        (set! b.3 x.1)
                                        (set! b.3 (+ b.3 y.2))
                                        (set! c.4 b.3)
                                        (if (= c.4 b.3)
                                            (jump c.4)
                                            (begin
                                              (set! x.1 c.4)
                                              (jump c.4)))))))
                '(module
                     ((new-frames ())
                      (locals (x.1 y.2 b.3 c.4))
                      (call-undead ())
                      (undead-out
                       ((x.1) (x.1 y.2) ((y.2 b.3) (b.3) (b.3 c.4) ((c.4) () ((c.4) ()))))))
                   (begin
                     (set! x.1 5)
                     (set! y.2 x.1)
                     (begin
                       (set! b.3 x.1)
                       (set! b.3 (+ b.3 y.2))
                       (set! c.4 b.3)
                       (if (= c.4 b.3) (jump c.4) (begin (set! x.1 c.4) (jump c.4)))))))
  (check-equal? (undead-analysis '(module ((new-frames ()) (locals (x.1 x.2 x.3)))
                                    (if (if (begin (set! x.1 1)
                                                   (set! x.2 x.1)
                                                   (set! x.3 x.1)
                                                   (set! x.3 (+ x.3 x.2))
                                                   (true))
                                            (true)
                                            (false))
                                        (jump x.3)
                                        (jump x.2))))
                '(module
                     ((new-frames ())
                      (locals (x.1 x.2 x.3))
                      (call-undead ())
                      (undead-out
                       ((((x.1) (x.1 x.2) (x.3 x.2) (x.2 x.3) (x.2 x.3)) (x.2 x.3) (x.2 x.3))
                        ()
                        ())))
                   (if (if (begin
                             (set! x.1 1)
                             (set! x.2 x.1)
                             (set! x.3 x.1)
                             (set! x.3 (+ x.3 x.2))
                             (true))
                           (true)
                           (false))
                       (jump x.3)
                       (jump x.2))))
  (check-equal? (undead-analysis '(module ((new-frames ()) (locals (x.1 x.2 x.3)))
                                    (begin (set! x.1 1)
                                           (set! x.2 x.1)
                                           (set! x.2 (+ x.2 x.1))
                                           (if (> x.2 2)
                                               (set! x.3 3)
                                               (set! x.3 4))
                                           (set! x.2 (+ x.2 x.3))
                                           (jump x.2))))
                '(module
                     ((new-frames ())
                      (locals (x.1 x.2 x.3))
                      (call-undead ())
                      (undead-out ((x.1) (x.1 x.2) (x.2) ((x.2) (x.3 x.2) (x.3 x.2)) (x.2) ())))
                   (begin
                     (set! x.1 1)
                     (set! x.2 x.1)
                     (set! x.2 (+ x.2 x.1))
                     (if (> x.2 2) (set! x.3 3) (set! x.3 4))
                     (set! x.2 (+ x.2 x.3))
                     (jump x.2))))
  (check-match (undead-analysis '(module ((new-frames ()) (locals (x.1 y.2 z.3)))
                                   (begin
                                     (set! x.1 10)
                                     (set! y.2 20)
                                     (return-point L.label.4 (jump z.3))
                                     (jump x.1))))
               `(module
                    ((new-frames ())
                     (locals (x.1 y.2 z.3))
                     (call-undead (x.1))
                     (undead-out ((,uo1 ...) (,uo2 ...) ((x.1) ()) ())))
                  (begin
                    (set! x.1 10)
                    (set! y.2 20)
                    (return-point L.label.4 (jump z.3))
                    (jump x.1)))
               (and (set=? (list->set uo1) (list->set '(x.1 z.3)))
                    (set=? (list->set uo2) (list->set '(x.1 z.3)))))
  (check-match (undead-analysis '(module ((new-frames ()) (locals (x.1 y.2)))
                                   (begin
                                     (set! x.1 5)
                                     (set! y.2 15)
                                     (return-point L.label.1 (begin (set! x.1 (- x.1 2)) (jump y.2)))
                                     (jump L.label.2 x.1 y.2))))
               `(module
                    ((new-frames ())
                     (locals (x.1 y.2))
                     (call-undead (,cu ...))
                     (undead-out ((x.1) (,uo1 ...) ((y.2 x.1) ((y.2) ())) (,uo2 ...))))
                  (begin
                    (set! x.1 5)
                    (set! y.2 15)
                    (return-point L.label.1 (begin (set! x.1 (- x.1 2)) (jump y.2)))
                    (jump L.label.2 x.1 y.2)))
               (and (set=? (list->set uo1) (list->set '(x.1 y.2)))
                    (set=? (list->set uo2) (list->set '(x.1 y.2)))
                    (set=? (list->set cu) (list->set '(x.1 y.2)))))
  (check-match (undead-analysis '(module ((new-frames ()) (locals (x.1 y.2)))
                                   (begin
                                     (set! x.1 5)
                                     (set! y.2 10)
                                     (return-point L.label.1 (if (< x.1 y.2) (jump x.1) (jump y.2)))
                                     (jump L.label.2))))
               `(module
                    ((new-frames ())
                     (locals (x.1 y.2))
                     (call-undead ())
                     (undead-out ((x.1) (,uo ...) (() ((y.2 x.1) () ())) ())))
                  (begin
                    (set! x.1 5)
                    (set! y.2 10)
                    (return-point L.label.1 (if (< x.1 y.2) (jump x.1) (jump y.2)))
                    (jump L.label.2)))
               (set=? (list->set uo) (list->set '(x.1 y.2))))
  (check-equal? (undead-analysis '(module ((new-frames ()) (locals (x.1 y.2)))
                                    (begin
                                      (set! x.1 10)
                                      (set! y.2 3)
                                      (return-point L.label.1 (begin (set! x.1 (- x.1 y.2)) (jump x.1)))
                                      (jump x.1))))
                '(module
                     ((new-frames ())
                      (locals (x.1 y.2))
                      (call-undead (x.1))
                      (undead-out ((x.1) (y.2 x.1) ((x.1) ((x.1) ())) ())))
                   (begin
                     (set! x.1 10)
                     (set! y.2 3)
                     (return-point L.label.1 (begin (set! x.1 (- x.1 y.2)) (jump x.1)))
                     (jump x.1))))
  (check-match (undead-analysis '(module ((new-frames ()) (locals (x.1 y.2)))
                                   (begin
                                     (set! x.1 8)
                                     (set! y.2 4)
                                     (begin (return-point L.label.1 (begin (return-point L.label.2 (jump y.2)) (jump x.1)))
                                            (jump y.2)))))
               `(module
                    ((new-frames ())
                     (locals (x.1 y.2))
                     (call-undead (,cu ...))
                     (undead-out ((x.1) (x.1 y.2) (((y.2) (((x.1) ()) ())) ()))))
                  (begin
                    (set! x.1 8)
                    (set! y.2 4)
                    (begin
                      (return-point L.label.1
                                    (begin (return-point L.label.2 (jump y.2)) (jump x.1)))
                      (jump y.2))))
               (set=? (list->set cu) (list->set '(x.1 y.2))))
  (check-equal? (undead-analysis '(module ((new-frames ()) (locals (x.1 y.2 z.3)))
                                    (begin
                                      (set! x.1 10)
                                      (set! y.2 5)
                                      (return-point L.label.1 (begin (set! z.3 (- z.3 y.2)) (set! y.2 (- y.2 z.3)) (jump z.3)))
                                      (jump y.2))))
                '(module
                     ((new-frames ())
                      (locals (x.1 y.2 z.3))
                      (call-undead (y.2))
                      (undead-out ((z.3) (z.3 y.2) ((y.2) ((y.2 z.3) (z.3) ())) ())))
                   (begin
                     (set! x.1 10)
                     (set! y.2 5)
                     (return-point L.label.1
                                   (begin (set! z.3 (- z.3 y.2)) (set! y.2 (- y.2 z.3)) (jump z.3)))
                     (jump y.2))))
  (check-match (undead-analysis '(module ((new-frames ()) (locals (x.1 y.2 z.3)))
                                   (begin
                                     (set! x.1 7)
                                     (set! y.2 14)
                                     (return-point L.label.1 (if (< x.1 y.2)
                                                                 (begin (set! z.3 (- z.3 2)) (jump z.3))
                                                                 (begin (set! z.3 (- z.3 4)) (jump z.3))))
                                     (jump z.3))))
               `(module
                    ((new-frames ())
                     (locals (x.1 y.2 z.3))
                     (call-undead (z.3))
                     (undead-out
                      ((x.1 z.3) (,uo ...) ((z.3) ((z.3) ((z.3) ()) ((z.3) ()))) ())))
                  (begin
                    (set! x.1 7)
                    (set! y.2 14)
                    (return-point L.label.1
                                  (if (< x.1 y.2)
                                      (begin (set! z.3 (- z.3 2)) (jump z.3))
                                      (begin (set! z.3 (- z.3 4)) (jump z.3))))
                    (jump z.3)))
               (set=? (list->set uo) (list->set '(x.1 y.2 z.3))))
  (check-match (undead-analysis
                '(module
                     ((new-frames ()) (locals (ra.12)))
                   (define L.fact.4
                     ((new-frames ((nfv.16)))
                      (locals (ra.13 x.9 tmp.14 tmp.15 new-n.10 nfv.16 factn-1.11 tmp.17)))
                     (begin
                       (set! x.9 fv0)
                       (set! ra.13 r15)
                       (if (= x.9 0)
                           (begin (set! rax 1) (jump ra.13 rbp rax))
                           (begin
                             (set! tmp.14 -1)
                             (set! tmp.15 x.9)
                             (set! tmp.15 (+ tmp.15 tmp.14))
                             (set! new-n.10 tmp.15)
                             (return-point
                              L.rp.6
                              (begin
                                (set! nfv.16 new-n.10)
                                (set! r15 L.rp.6)
                                (jump L.fact.4 rbp r15 nfv.16)))
                             (set! factn-1.11 rax)
                             (set! tmp.17 x.9)
                             (set! tmp.17 (* tmp.17 factn-1.11))
                             (set! rax tmp.17)
                             (jump ra.13 rbp rax)))))
                   (begin
                     (set! ra.12 r15)
                     (set! fv0 5)
                     (set! r15 ra.12)
                     (jump L.fact.4 rbp r15 fv0))))
               `(module
                    ((new-frames ())
                     (locals (ra.12))
                     (call-undead ())
                     (undead-out ((ra.12 rbp) (ra.12 fv0 rbp) (fv0 r15 rbp) (,uo1 ...))))
                  (define L.fact.4
                    ((new-frames ((nfv.16)))
                     (locals (ra.13 x.9 tmp.14 tmp.15 new-n.10 nfv.16 factn-1.11 tmp.17))
                     (undead-out
                      ((r15 x.9 rbp)
                       (,uo2 ...)
                       ((,uo3 ...)
                        ((,uo4 ...) (,uo5 ...))
                        (,uo6 ...
                         ((,uo7 ...) ((nfv.16 rbp) (nfv.16 r15 rbp) (,uo8 ...)))
                         ,uo9 ...))))
                     (call-undead (x.9 ra.13)))
                    (begin
                      (set! x.9 fv0)
                      (set! ra.13 r15)
                      (if (= x.9 0)
                          (begin (set! rax 1) (jump ra.13 rbp rax))
                          (begin
                            (set! tmp.14 -1)
                            (set! tmp.15 x.9)
                            (set! tmp.15 (+ tmp.15 tmp.14))
                            (set! new-n.10 tmp.15)
                            (return-point L.rp.6
                                          (begin
                                            (set! nfv.16 new-n.10)
                                            (set! r15 L.rp.6)
                                            (jump L.fact.4 rbp r15 nfv.16)))
                            (set! factn-1.11 rax)
                            (set! tmp.17 x.9)
                            (set! tmp.17 (* tmp.17 factn-1.11))
                            (set! rax tmp.17)
                            (jump ra.13 rbp rax)))))
                  (begin
                    (set! ra.12 r15)
                    (set! fv0 5)
                    (set! r15 ra.12)
                    (jump L.fact.4 rbp r15 fv0)))
               (and (set=? (list->set uo1) (set 'fv0 'r15 'rbp))
                    (set=? (list->set uo2) (set 'x.9 'ra.13 'rbp))
                    (set=? (list->set uo3) (set 'x.9 'ra.13 'rbp))
                    (set=? (list->set uo4) (set 'rax 'rbp 'ra.13))
                    (set=? (list->set uo5) (set 'rax 'rbp))
                    (for/and ([uo uo6]
                              [expected '((tmp.14 x.9 ra.13 rbp)
                                          (tmp.14 tmp.15 x.9 ra.13 rbp)
                                          (tmp.15 x.9 ra.13 rbp)
                                          (new-n.10 x.9 ra.13 rbp))])
                      (set=? (list->set uo) (list->set expected)))
                    (set=? (list->set uo7) (set 'rax 'x.9 'ra.13 'rbp))
                    (set=? (list->set uo8) (set 'nfv.16 'r15 'rbp))
                    (for/and ([uo uo9]
                              [expected '((x.9 factn-1.11 ra.13 rbp)
                                          (factn-1.11 tmp.17 ra.13 rbp)
                                          (tmp.17 ra.13 rbp)
                                          (ra.13 rax rbp)
                                          (rax rbp))])
                      (set=? (list->set uo) (list->set expected)))))
  (check-match (undead-analysis '(module
                                     ((new-frames ())
                                      (locals (tmp-ra.6))
                                      (call-undead ())
                                      (undead-out
                                       ((tmp-ra.6 rbp)
                                        (tmp-ra.6 fv1 rbp)
                                        (tmp-ra.6 fv1 fv0 rbp)
                                        (fv1 fv0 r15 rbp)
                                        (fv1 fv0 r15 rbp))))
                                   (define L.swap.1
                                     ((new-frames ((nfv.4 nfv.5)))
                                      (locals (nfv.4 nfv.5 z.3 tmp-ra.3 x.1 y.2))
                                      (undead-out
                                       ((fv0 fv1 tmp-ra.3 rbp)
                                        (fv1 x.1 tmp-ra.3 rbp)
                                        (y.2 x.1 tmp-ra.3 rbp)
                                        ((y.2 x.1 tmp-ra.3 rbp)
                                         ((tmp-ra.3 rax rbp) (rax rbp))
                                         (((rax tmp-ra.3 rbp)
                                           ((y.2 nfv.5 rbp)
                                            (nfv.5 nfv.4 rbp)
                                            (nfv.5 nfv.4 r15 rbp)
                                            (nfv.5 nfv.4 r15 rbp)))
                                          (z.3 tmp-ra.3 rbp)
                                          (tmp-ra.3 rax rbp)
                                          (rax rbp)))))
                                      (call-undead (tmp-ra.3)))
                                     (begin
                                       (set! tmp-ra.3 r15)
                                       (set! x.1 fv0)
                                       (set! y.2 fv1)
                                       (if (< y.2 x.1)
                                           (begin (set! rax x.1) (jump tmp-ra.3 rbp rax))
                                           (begin
                                             (return-point L.rp.2
                                                           (begin
                                                             (set! nfv.5 x.1)
                                                             (set! nfv.4 y.2)
                                                             (set! r15 L.rp.2)
                                                             (jump L.swap.1 rbp r15 nfv.4 nfv.5)))
                                             (set! z.3 rax)
                                             (set! rax z.3)
                                             (jump tmp-ra.3 rbp rax)))))
                                   (begin
                                     (set! tmp-ra.6 r15)
                                     (set! fv1 2)
                                     (set! fv0 1)
                                     (set! r15 tmp-ra.6)
                                     (jump L.swap.1 rbp r15 fv0 fv1))))
               `(module
                    ((new-frames ())
                     (locals (tmp-ra.6))
                     (call-undead ())
                     (undead-out
                      ((tmp-ra.6 rbp)
                       (tmp-ra.6 fv1 rbp)
                       (tmp-ra.6 fv1 fv0 rbp)
                       (fv1 fv0 r15 rbp)
                       (,uo0 ...))))
                  (define L.swap.1
                    ((new-frames ((nfv.4 nfv.5)))
                     (locals (nfv.4 nfv.5 z.3 tmp-ra.3 x.1 y.2))
                     (undead-out
                      (,uo1 ...
                       ((,uo2 ...)
                        (,uo3 ...)
                        (((,uo4 ...)
                          ((y.2 nfv.5 rbp)
                           (nfv.5 nfv.4 rbp)
                           (nfv.5 nfv.4 r15 rbp)
                           (,uo5 ...)))
                         ,uo6 ...))))
                     (call-undead (tmp-ra.3)))
                    (begin
                      (set! tmp-ra.3 r15)
                      (set! x.1 fv0)
                      (set! y.2 fv1)
                      (if (< y.2 x.1)
                          (begin (set! rax x.1) (jump tmp-ra.3 rbp rax))
                          (begin
                            (return-point L.rp.2
                                          (begin
                                            (set! nfv.5 x.1)
                                            (set! nfv.4 y.2)
                                            (set! r15 L.rp.2)
                                            (jump L.swap.1 rbp r15 nfv.4 nfv.5)))
                            (set! z.3 rax)
                            (set! rax z.3)
                            (jump tmp-ra.3 rbp rax)))))
                  (begin
                    (set! tmp-ra.6 r15)
                    (set! fv1 2)
                    (set! fv0 1)
                    (set! r15 tmp-ra.6)
                    (jump L.swap.1 rbp r15 fv0 fv1)))
               (and (set=? (list->set uo0) (set 'fv1 'fv0 'r15 'rbp))
                    (for/and ([uo uo1]
                              [expected '((fv0 fv1 tmp-ra.3 rbp)
                                          (fv1 x.1 tmp-ra.3 rbp)
                                          (y.2 x.1 tmp-ra.3 rbp))])
                      (set=? (list->set uo) (list->set expected)))
                    (set=? (list->set uo2) (set 'y.2 'x.1 'tmp-ra.3 'rbp))
                    (for/and ([uo uo3]
                              [expected '((tmp-ra.3 rax rbp) (rax rbp))])
                      (set=? (list->set uo) (list->set expected)))
                    (set=? (list->set uo4) (set 'rax 'tmp-ra.3 'rbp))
                    (set=? (list->set uo5) (set 'nfv.5 'nfv.4 'r15 'rbp))
                    (for/and ([uo uo6]
                              [expected '((z.3 tmp-ra.3 rbp) (tmp-ra.3 rax rbp) (rax rbp))])
                      (set=? (list->set uo) (list->set expected)))))

  (parameterize ([current-parameter-registers '()])
    (check-match (undead-analysis '(module
                                       ((new-frames ()) (locals (ra.12)))
                                     (define L.fact.4
                                       ((new-frames ((nfv.16)))
                                        (locals (ra.13 x.9 tmp.14 tmp.15 new-n.10 nfv.16 factn-1.11 tmp.17)))
                                       (begin
                                         (set! x.9 fv0)
                                         (set! ra.13 r15)
                                         (if (= x.9 0)
                                             (begin (set! rax 1) (jump ra.13 rbp rax))
                                             (begin
                                               (set! tmp.14 -1)
                                               (set! tmp.15 x.9)
                                               (set! tmp.15 (+ tmp.15 tmp.14))
                                               (set! new-n.10 tmp.15)
                                               (return-point
                                                L.rp.6
                                                (begin
                                                  (set! nfv.16 new-n.10)
                                                  (set! r15 L.rp.6)
                                                  (jump L.fact.4 rbp r15 nfv.16)))
                                               (set! factn-1.11 rax)
                                               (set! tmp.17 x.9)
                                               (set! tmp.17 (* tmp.17 factn-1.11))
                                               (set! rax tmp.17)
                                               (jump ra.13 rbp rax)))))
                                     (begin
                                       (set! ra.12 r15)
                                       (set! fv0 5)
                                       (set! r15 ra.12)
                                       (jump L.fact.4 rbp r15 fv0))))
                 `(module
                      ((new-frames ())
                       (locals (ra.12))
                       (call-undead ())
                       (undead-out ((ra.12 rbp) (ra.12 fv0 rbp) (fv0 r15 rbp) (,uo0 ...))))
                    (define L.fact.4
                      ((new-frames ((nfv.16)))
                       (locals (ra.13 x.9 tmp.14 tmp.15 new-n.10 nfv.16 factn-1.11 tmp.17))
                       (undead-out
                        ((r15 x.9 rbp)
                         (,uo1 ...)
                         ((,uo1 ...)
                          (,uo2 ...)
                          (,uo3 ...
                           ((,uo4 ...) ((nfv.16 rbp) (nfv.16 r15 rbp) (,uo5 ...)))
                           ,uo6 ...))))
                       (call-undead (x.9 ra.13)))
                      (begin
                        (set! x.9 fv0)
                        (set! ra.13 r15)
                        (if (= x.9 0)
                            (begin (set! rax 1) (jump ra.13 rbp rax))
                            (begin
                              (set! tmp.14 -1)
                              (set! tmp.15 x.9)
                              (set! tmp.15 (+ tmp.15 tmp.14))
                              (set! new-n.10 tmp.15)
                              (return-point L.rp.6
                                            (begin
                                              (set! nfv.16 new-n.10)
                                              (set! r15 L.rp.6)
                                              (jump L.fact.4 rbp r15 nfv.16)))
                              (set! factn-1.11 rax)
                              (set! tmp.17 x.9)
                              (set! tmp.17 (* tmp.17 factn-1.11))
                              (set! rax tmp.17)
                              (jump ra.13 rbp rax)))))
                    (begin
                      (set! ra.12 r15)
                      (set! fv0 5)
                      (set! r15 ra.12)
                      (jump L.fact.4 rbp r15 fv0)))
                 (and (set=? (list->set uo0) (set 'fv0 'r15 'rbp))
                      (set=? (list->set uo1) (set 'x.9 'ra.13 'rbp))
                      (for/and ([uo uo2]
                                [expected '((ra.13 rax rbp)
                                            (rax rbp))])
                        (set=? (list->set uo) (list->set expected)))
                      (for/and ([uo uo3]
                                [expected '((tmp.14 x.9 ra.13 rbp)
                                            (tmp.14 tmp.15 x.9 ra.13 rbp)
                                            (tmp.15 x.9 ra.13 rbp)
                                            (new-n.10 x.9 ra.13 rbp))])
                        (set=? (list->set uo) (list->set expected)))
                      (set=? (list->set uo4) (set 'rax 'x.9 'ra.13 'rbp))
                      (set=? (list->set uo5) (set 'nfv.16 'r15 'rbp))
                      (for/and ([uo uo6]
                                [expected '((x.9 factn-1.11 ra.13 rbp)
                                            (factn-1.11 tmp.17 ra.13 rbp)
                                            (tmp.17 ra.13 rbp)
                                            (ra.13 rax rbp)
                                            (rax rbp))])
                        (set=? (list->set uo) (list->set expected))))))
  (check-equal? (undead-analysis '(module
                                      ((new-frames ()) (locals (x.3 tmp-ra.2 x.2)))
                                    (define L.f.1
                                      ((new-frames ()) (locals (tmp-ra.1 b.1 y.1 x.1 z.1 a.1)))
                                      (begin
                                        (set! tmp-ra.1 r15)
                                        (set! x.1 rdi)
                                        (set! y.1 1)
                                        (set! z.1 2)
                                        (set! a.1 y.1)
                                        (set! a.1 (bitwise-and a.1 x.1))
                                        (set! b.1 z.1)
                                        (set! b.1 (bitwise-ior b.1 x.1))
                                        (set! a.1 (bitwise-xor a.1 b.1))
                                        (set! rax a.1)
                                        (set! rax (arithmetic-shift-right rax 3))
                                        (jump tmp-ra.1 rbp rax)))
                                    (begin
                                      (set! tmp-ra.2 r15)
                                      (set! x.2 10)
                                      (if (begin (set! x.3 100) (not (!= x.2 x.3)))
                                          (begin (set! rdi x.2) (set! r15 tmp-ra.2) (jump L.f.1 rbp r15 rdi))
                                          (begin (set! rdi 1000) (set! r15 tmp-ra.2) (jump L.f.2 rbp r15 rdi))))))
                '(module
                     ((new-frames ())
                      (locals (x.3 tmp-ra.2 x.2))
                      (call-undead ())
                      (undead-out
                       ((tmp-ra.2 rbp)
                        (x.2 tmp-ra.2 rbp)
                        (((x.3 x.2 tmp-ra.2 rbp) (x.2 tmp-ra.2 rbp))
                         ((tmp-ra.2 rdi rbp) (rdi r15 rbp) (rbp r15 rdi))
                         ((tmp-ra.2 rdi rbp) (rdi r15 rbp) (rbp r15 rdi))))))
                   (define L.f.1
                     ((new-frames ())
                      (locals (tmp-ra.1 b.1 y.1 x.1 z.1 a.1))
                      (undead-out
                       ((rdi rbp tmp-ra.1)
                        (x.1 rbp tmp-ra.1)
                        (y.1 x.1 rbp tmp-ra.1)
                        (y.1 z.1 x.1 rbp tmp-ra.1)
                        (a.1 z.1 x.1 rbp tmp-ra.1)
                        (z.1 x.1 a.1 rbp tmp-ra.1)
                        (x.1 b.1 a.1 rbp tmp-ra.1)
                        (b.1 a.1 rbp tmp-ra.1)
                        (a.1 rbp tmp-ra.1)
                        (rax rbp tmp-ra.1)
                        (rax rbp tmp-ra.1)
                        (rbp rax)))
                      (call-undead ()))
                     (begin
                       (set! tmp-ra.1 r15)
                       (set! x.1 rdi)
                       (set! y.1 1)
                       (set! z.1 2)
                       (set! a.1 y.1)
                       (set! a.1 (bitwise-and a.1 x.1))
                       (set! b.1 z.1)
                       (set! b.1 (bitwise-ior b.1 x.1))
                       (set! a.1 (bitwise-xor a.1 b.1))
                       (set! rax a.1)
                       (set! rax (arithmetic-shift-right rax 3))
                       (jump tmp-ra.1 rbp rax)))
                   (begin
                     (set! tmp-ra.2 r15)
                     (set! x.2 10)
                     (if (begin (set! x.3 100) (not (!= x.2 x.3)))
                         (begin (set! rdi x.2) (set! r15 tmp-ra.2) (jump L.f.1 rbp r15 rdi))
                         (begin (set! rdi 1000) (set! r15 tmp-ra.2) (jump L.f.2 rbp r15 rdi)))))))
