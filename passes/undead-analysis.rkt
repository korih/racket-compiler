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
                         (begin (set! rdi 1000) (set! r15 tmp-ra.2) (jump L.f.2 rbp r15 rdi))))))
  (check-equal? (undead-analysis '(module
                                      ((new-frames ()) (locals (tmp.88 tmp-ra.95 tmp.87)))
                                    (define L.*.17
                                      ((new-frames ())
                                       (locals (tmp.80 tmp.78 tmp.41 tmp.82 tmp.42 tmp.81 tmp.79 tmp-ra.93)))
                                      (begin
                                        (set! tmp-ra.93 r15)
                                        (set! tmp.41 rdi)
                                        (set! tmp.42 rsi)
                                        (if (begin
                                              (if (begin
                                                    (set! tmp.79 tmp.42)
                                                    (set! tmp.79 (bitwise-and tmp.79 7))
                                                    (= tmp.79 0))
                                                  (set! tmp.78 14)
                                                  (set! tmp.78 6))
                                              (!= tmp.78 6))
                                            (if (begin
                                                  (if (begin
                                                        (set! tmp.81 tmp.41)
                                                        (set! tmp.81 (bitwise-and tmp.81 7))
                                                        (= tmp.81 0))
                                                      (set! tmp.80 14)
                                                      (set! tmp.80 6))
                                                  (!= tmp.80 6))
                                                (begin
                                                  (set! tmp.82 tmp.42)
                                                  (set! tmp.82 (arithmetic-shift-right tmp.82 3))
                                                  (set! rax tmp.41)
                                                  (set! rax (* rax tmp.82))
                                                  (jump tmp-ra.93 rbp rax))
                                                (begin (set! rax 318) (jump tmp-ra.93 rbp rax)))
                                            (begin (set! rax 318) (jump tmp-ra.93 rbp rax)))))
                                    (define L.+.16
                                      ((new-frames ())
                                       (locals (tmp.39 tmp.85 tmp-ra.94 tmp.84 tmp.86 tmp.40 tmp.83)))
                                      (begin
                                        (set! tmp-ra.94 r15)
                                        (set! tmp.39 rdi)
                                        (set! tmp.40 rsi)
                                        (if (begin
                                              (if (begin
                                                    (set! tmp.84 tmp.40)
                                                    (set! tmp.84 (bitwise-and tmp.84 7))
                                                    (= tmp.84 0))
                                                  (set! tmp.83 14)
                                                  (set! tmp.83 6))
                                              (!= tmp.83 6))
                                            (if (begin
                                                  (if (begin
                                                        (set! tmp.86 tmp.39)
                                                        (set! tmp.86 (bitwise-and tmp.86 7))
                                                        (= tmp.86 0))
                                                      (set! tmp.85 14)
                                                      (set! tmp.85 6))
                                                  (!= tmp.85 6))
                                                (begin
                                                  (set! rax tmp.39)
                                                  (set! rax (+ rax tmp.40))
                                                  (jump tmp-ra.94 rbp rax))
                                                (begin (set! rax 574) (jump tmp-ra.94 rbp rax)))
                                            (begin (set! rax 574) (jump tmp-ra.94 rbp rax)))))
                                    (begin
                                      (set! tmp-ra.95 r15)
                                      (begin
                                        (set! rbp (- rbp 16))
                                        (return-point L.rp.19
                                                      (begin
                                                        (set! rdi 40)
                                                        (set! rsi 48)
                                                        (set! r15 L.rp.19)
                                                        (jump L.+.16 rbp r15 rdi rsi)))
                                        (set! rbp (+ rbp 16)))
                                      (set! tmp.87 rax)
                                      (begin
                                        (set! rbp (- rbp 16))
                                        (return-point L.rp.20
                                                      (begin
                                                        (set! rdi 32)
                                                        (set! rsi 40)
                                                        (set! r15 L.rp.20)
                                                        (jump L.*.17 rbp r15 rdi rsi)))
                                        (set! rbp (+ rbp 16)))
                                      (set! tmp.88 rax)
                                      (set! rdi tmp.87)
                                      (set! rsi tmp.88)
                                      (set! r15 tmp-ra.95)
                                      (jump L.+.16 rbp r15 rdi rsi))))
                '(module
                     ((new-frames ())
                      (locals (tmp.88 tmp-ra.95 tmp.87))
                      (call-undead (tmp.87 tmp-ra.95))
                      (undead-out
                       ((rbp tmp-ra.95)
                        ((tmp-ra.95 rbp)
                         ((rbp rax tmp-ra.95)
                          ((rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
                         (rax rbp tmp-ra.95))
                        (rbp tmp-ra.95 tmp.87)
                        ((tmp-ra.95 tmp.87 rbp)
                         ((rbp rax tmp.87 tmp-ra.95)
                          ((rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
                         (rax tmp.87 tmp-ra.95 rbp))
                        (tmp.87 tmp.88 tmp-ra.95 rbp)
                        (tmp.88 tmp-ra.95 rdi rbp)
                        (tmp-ra.95 rsi rdi rbp)
                        (rsi rdi r15 rbp)
                        (rbp r15 rdi rsi))))
                   (define L.*.17
                     ((new-frames ())
                      (locals (tmp.80 tmp.78 tmp.41 tmp.82 tmp.42 tmp.81 tmp.79 tmp-ra.93))
                      (undead-out
                       ((rdi rsi rbp tmp-ra.93)
                        (rsi tmp.41 rbp tmp-ra.93)
                        (tmp.42 tmp.41 rbp tmp-ra.93)
                        (((((tmp.79 tmp.42 tmp.41 rbp tmp-ra.93)
                            (tmp.79 tmp.42 tmp.41 rbp tmp-ra.93)
                            (tmp.42 tmp.41 rbp tmp-ra.93))
                           (tmp.78 tmp.42 tmp.41 rbp tmp-ra.93)
                           (tmp.78 tmp.42 tmp.41 rbp tmp-ra.93))
                          (tmp.42 tmp.41 rbp tmp-ra.93))
                         (((((tmp.81 tmp.42 tmp.41 rbp tmp-ra.93)
                             (tmp.81 tmp.42 tmp.41 rbp tmp-ra.93)
                             (tmp.42 tmp.41 rbp tmp-ra.93))
                            (tmp.80 tmp.42 tmp.41 rbp tmp-ra.93)
                            (tmp.80 tmp.42 tmp.41 rbp tmp-ra.93))
                           (tmp.42 tmp.41 rbp tmp-ra.93))
                          ((tmp.82 tmp.41 rbp tmp-ra.93)
                           (tmp.41 tmp.82 rbp tmp-ra.93)
                           (tmp.82 rax rbp tmp-ra.93)
                           (rax rbp tmp-ra.93)
                           (rbp rax))
                          ((rax rbp tmp-ra.93) (rbp rax)))
                         ((rax rbp tmp-ra.93) (rbp rax)))))
                      (call-undead ()))
                     (begin
                       (set! tmp-ra.93 r15)
                       (set! tmp.41 rdi)
                       (set! tmp.42 rsi)
                       (if (begin
                             (if (begin
                                   (set! tmp.79 tmp.42)
                                   (set! tmp.79 (bitwise-and tmp.79 7))
                                   (= tmp.79 0))
                                 (set! tmp.78 14)
                                 (set! tmp.78 6))
                             (!= tmp.78 6))
                           (if (begin
                                 (if (begin
                                       (set! tmp.81 tmp.41)
                                       (set! tmp.81 (bitwise-and tmp.81 7))
                                       (= tmp.81 0))
                                     (set! tmp.80 14)
                                     (set! tmp.80 6))
                                 (!= tmp.80 6))
                               (begin
                                 (set! tmp.82 tmp.42)
                                 (set! tmp.82 (arithmetic-shift-right tmp.82 3))
                                 (set! rax tmp.41)
                                 (set! rax (* rax tmp.82))
                                 (jump tmp-ra.93 rbp rax))
                               (begin (set! rax 318) (jump tmp-ra.93 rbp rax)))
                           (begin (set! rax 318) (jump tmp-ra.93 rbp rax)))))
                   (define L.+.16
                     ((new-frames ())
                      (locals (tmp.39 tmp.85 tmp-ra.94 tmp.84 tmp.86 tmp.40 tmp.83))
                      (undead-out
                       ((rdi rsi rbp tmp-ra.94)
                        (rsi tmp.39 rbp tmp-ra.94)
                        (tmp.39 tmp.40 rbp tmp-ra.94)
                        (((((tmp.84 tmp.39 tmp.40 rbp tmp-ra.94)
                            (tmp.84 tmp.39 tmp.40 rbp tmp-ra.94)
                            (tmp.39 tmp.40 rbp tmp-ra.94))
                           (tmp.83 tmp.39 tmp.40 rbp tmp-ra.94)
                           (tmp.83 tmp.39 tmp.40 rbp tmp-ra.94))
                          (tmp.39 tmp.40 rbp tmp-ra.94))
                         (((((tmp.86 tmp.39 tmp.40 rbp tmp-ra.94)
                             (tmp.86 tmp.39 tmp.40 rbp tmp-ra.94)
                             (tmp.39 tmp.40 rbp tmp-ra.94))
                            (tmp.85 tmp.39 tmp.40 rbp tmp-ra.94)
                            (tmp.85 tmp.39 tmp.40 rbp tmp-ra.94))
                           (tmp.39 tmp.40 rbp tmp-ra.94))
                          ((tmp.40 rax rbp tmp-ra.94) (rax rbp tmp-ra.94) (rbp rax))
                          ((rax rbp tmp-ra.94) (rbp rax)))
                         ((rax rbp tmp-ra.94) (rbp rax)))))
                      (call-undead ()))
                     (begin
                       (set! tmp-ra.94 r15)
                       (set! tmp.39 rdi)
                       (set! tmp.40 rsi)
                       (if (begin
                             (if (begin
                                   (set! tmp.84 tmp.40)
                                   (set! tmp.84 (bitwise-and tmp.84 7))
                                   (= tmp.84 0))
                                 (set! tmp.83 14)
                                 (set! tmp.83 6))
                             (!= tmp.83 6))
                           (if (begin
                                 (if (begin
                                       (set! tmp.86 tmp.39)
                                       (set! tmp.86 (bitwise-and tmp.86 7))
                                       (= tmp.86 0))
                                     (set! tmp.85 14)
                                     (set! tmp.85 6))
                                 (!= tmp.85 6))
                               (begin
                                 (set! rax tmp.39)
                                 (set! rax (+ rax tmp.40))
                                 (jump tmp-ra.94 rbp rax))
                               (begin (set! rax 574) (jump tmp-ra.94 rbp rax)))
                           (begin (set! rax 574) (jump tmp-ra.94 rbp rax)))))
                   (begin
                     (set! tmp-ra.95 r15)
                     (begin
                       (set! rbp (- rbp 16))
                       (return-point L.rp.19
                                     (begin
                                       (set! rdi 40)
                                       (set! rsi 48)
                                       (set! r15 L.rp.19)
                                       (jump L.+.16 rbp r15 rdi rsi)))
                       (set! rbp (+ rbp 16)))
                     (set! tmp.87 rax)
                     (begin
                       (set! rbp (- rbp 16))
                       (return-point L.rp.20
                                     (begin
                                       (set! rdi 32)
                                       (set! rsi 40)
                                       (set! r15 L.rp.20)
                                       (jump L.*.17 rbp r15 rdi rsi)))
                       (set! rbp (+ rbp 16)))
                     (set! tmp.88 rax)
                     (set! rdi tmp.87)
                     (set! rsi tmp.88)
                     (set! r15 tmp-ra.95)
                     (jump L.+.16 rbp r15 rdi rsi))))
  (check-equal? (undead-analysis  '(module
                                       ((new-frames ()) (locals (tmp-ra.45)))
                                     (define L.*.2
                                       ((new-frames ())
                                        (locals (tmp.24 tmp.27 tmp.2 tmp.25 tmp-ra.39 tmp.23 tmp.1 tmp.26)))
                                       (begin
                                         (set! tmp-ra.39 r15)
                                         (set! tmp.1 rdi)
                                         (set! tmp.2 rsi)
                                         (if (begin
                                               (if (begin
                                                     (begin
                                                       (set! tmp.24 tmp.2)
                                                       (set! tmp.24 (bitwise-and tmp.24 7)))
                                                     (= tmp.24 0))
                                                   (set! tmp.23 14)
                                                   (set! tmp.23 6))
                                               (!= tmp.23 6))
                                             (if (begin
                                                   (if (begin
                                                         (begin
                                                           (set! tmp.26 tmp.1)
                                                           (set! tmp.26 (bitwise-and tmp.26 7)))
                                                         (= tmp.26 0))
                                                       (set! tmp.25 14)
                                                       (set! tmp.25 6))
                                                   (!= tmp.25 6))
                                                 (begin
                                                   (set! tmp.27 tmp.2)
                                                   (set! tmp.27 (arithmetic-shift-right tmp.27 3))
                                                   (set! rax tmp.1)
                                                   (set! rax (* rax tmp.27))
                                                   (jump tmp-ra.39 rbp rax))
                                                 (begin (set! rax 318) (jump tmp-ra.39 rbp rax)))
                                             (begin (set! rax 318) (jump tmp-ra.39 rbp rax)))))
                                     (define L.+.1
                                       ((new-frames ())
                                        (locals (tmp.4 tmp.31 tmp-ra.40 tmp.3 tmp.28 tmp.30 tmp.29)))
                                       (begin
                                         (set! tmp-ra.40 r15)
                                         (set! tmp.3 rdi)
                                         (set! tmp.4 rsi)
                                         (if (begin
                                               (if (begin
                                                     (begin
                                                       (set! tmp.29 tmp.4)
                                                       (set! tmp.29 (bitwise-and tmp.29 7)))
                                                     (= tmp.29 0))
                                                   (set! tmp.28 14)
                                                   (set! tmp.28 6))
                                               (!= tmp.28 6))
                                             (if (begin
                                                   (if (begin
                                                         (begin
                                                           (set! tmp.31 tmp.3)
                                                           (set! tmp.31 (bitwise-and tmp.31 7)))
                                                         (= tmp.31 0))
                                                       (set! tmp.30 14)
                                                       (set! tmp.30 6))
                                                   (!= tmp.30 6))
                                                 (begin
                                                   (set! rax tmp.3)
                                                   (set! rax (+ rax tmp.4))
                                                   (jump tmp-ra.40 rbp rax))
                                                 (begin (set! rax 574) (jump tmp-ra.40 rbp rax)))
                                             (begin (set! rax 574) (jump tmp-ra.40 rbp rax)))))
                                     (define L.add.10
                                       ((new-frames (() () () () () ()))
                                        (locals
                                         (e.65
                                          c.63
                                          tmp.34
                                          tmp.37
                                          g.67
                                          tmp.35
                                          tmp.36
                                          a.61
                                          tmp.32
                                          h.68
                                          f.66
                                          d.64
                                          b.62
                                          tmp-ra.41
                                          tmp.33)))
                                       (begin
                                         (set! tmp-ra.41 r15)
                                         (set! a.61 rdi)
                                         (set! b.62 rsi)
                                         (set! c.63 rdx)
                                         (set! d.64 rcx)
                                         (set! e.65 r8)
                                         (set! f.66 r9)
                                         (set! g.67 fv0)
                                         (set! h.68 fv1)
                                         (return-point L.rp.12
                                                       (begin
                                                         (set! rdi g.67)
                                                         (set! rsi h.68)
                                                         (set! r15 L.rp.12)
                                                         (jump L.+.1 rbp r15 rdi rsi)))
                                         (set! tmp.37 rax)
                                         (return-point L.rp.13
                                                       (begin
                                                         (set! rdi f.66)
                                                         (set! rsi tmp.37)
                                                         (set! r15 L.rp.13)
                                                         (jump L.+.1 rbp r15 rdi rsi)))
                                         (set! tmp.36 rax)
                                         (return-point L.rp.14
                                                       (begin
                                                         (set! rdi e.65)
                                                         (set! rsi tmp.36)
                                                         (set! r15 L.rp.14)
                                                         (jump L.+.1 rbp r15 rdi rsi)))
                                         (set! tmp.35 rax)
                                         (return-point L.rp.15
                                                       (begin
                                                         (set! rdi d.64)
                                                         (set! rsi tmp.35)
                                                         (set! r15 L.rp.15)
                                                         (jump L.+.1 rbp r15 rdi rsi)))
                                         (set! tmp.34 rax)
                                         (return-point L.rp.16
                                                       (begin
                                                         (set! rdi c.63)
                                                         (set! rsi tmp.34)
                                                         (set! r15 L.rp.16)
                                                         (jump L.+.1 rbp r15 rdi rsi)))
                                         (set! tmp.33 rax)
                                         (return-point L.rp.17
                                                       (begin
                                                         (set! rdi b.62)
                                                         (set! rsi tmp.33)
                                                         (set! r15 L.rp.17)
                                                         (jump L.+.1 rbp r15 rdi rsi)))
                                         (set! tmp.32 rax)
                                         (set! rdi a.61)
                                         (set! rsi tmp.32)
                                         (set! r15 tmp-ra.41)
                                         (jump L.+.1 rbp r15 rdi rsi)))
                                     (define L.add-and-multiply.11
                                       ((new-frames ((nfv.43 nfv.44)))
                                        (locals
                                         (a.69
                                          g.75
                                          sum.78
                                          h.76
                                          i.77
                                          tmp-ra.42
                                          c.71
                                          nfv.44
                                          d.72
                                          b.70
                                          f.74
                                          e.73
                                          nfv.43)))
                                       (begin
                                         (set! tmp-ra.42 r15)
                                         (set! a.69 rdi)
                                         (set! b.70 rsi)
                                         (set! c.71 rdx)
                                         (set! d.72 rcx)
                                         (set! e.73 r8)
                                         (set! f.74 r9)
                                         (set! g.75 fv0)
                                         (set! h.76 fv1)
                                         (set! i.77 fv2)
                                         (return-point L.rp.18
                                                       (begin
                                                         (set! rdi a.69)
                                                         (set! rsi b.70)
                                                         (set! rdx c.71)
                                                         (set! rcx d.72)
                                                         (set! r8 e.73)
                                                         (set! r9 f.74)
                                                         (set! nfv.43 g.75)
                                                         (set! nfv.44 h.76)
                                                         (set! r15 L.rp.18)
                                                         (jump L.add.10 rbp r15 rdi rsi rdx rcx r8 r9 nfv.43 nfv.44)))
                                         (set! sum.78 rax)
                                         (set! rdi sum.78)
                                         (set! rsi i.77)
                                         (set! r15 tmp-ra.42)
                                         (jump L.*.2 rbp r15 rdi rsi)))
                                     (begin
                                       (set! tmp-ra.45 r15)
                                       (set! rdi 8)
                                       (set! rsi 16)
                                       (set! rdx 24)
                                       (set! rcx 32)
                                       (set! r8 40)
                                       (set! r9 48)
                                       (set! fv0 56)
                                       (set! fv1 64)
                                       (set! fv2 16)
                                       (set! r15 tmp-ra.45)
                                       (jump L.add-and-multiply.11 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2))))
                '(module
                     ((new-frames ())
                      (locals (tmp-ra.45))
                      (call-undead ())
                      (undead-out
                       ((tmp-ra.45 rbp)
                        (tmp-ra.45 rdi rbp)
                        (tmp-ra.45 rsi rdi rbp)
                        (tmp-ra.45 rdx rsi rdi rbp)
                        (tmp-ra.45 rcx rdx rsi rdi rbp)
                        (tmp-ra.45 r8 rcx rdx rsi rdi rbp)
                        (tmp-ra.45 r9 r8 rcx rdx rsi rdi rbp)
                        (tmp-ra.45 fv0 r9 r8 rcx rdx rsi rdi rbp)
                        (tmp-ra.45 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)
                        (tmp-ra.45 fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)
                        (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi r15 rbp)
                        (rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2))))
                   (define L.*.2
                     ((new-frames ())
                      (locals (tmp.24 tmp.27 tmp.2 tmp.25 tmp-ra.39 tmp.23 tmp.1 tmp.26))
                      (undead-out
                       ((rdi rsi rbp tmp-ra.39)
                        (rsi tmp.1 rbp tmp-ra.39)
                        (tmp.2 tmp.1 rbp tmp-ra.39)
                        ((((((tmp.24 tmp.2 tmp.1 rbp tmp-ra.39)
                             (tmp.24 tmp.2 tmp.1 rbp tmp-ra.39))
                            (tmp.2 tmp.1 rbp tmp-ra.39))
                           (tmp.23 tmp.2 tmp.1 rbp tmp-ra.39)
                           (tmp.23 tmp.2 tmp.1 rbp tmp-ra.39))
                          (tmp.2 tmp.1 rbp tmp-ra.39))
                         ((((((tmp.26 tmp.2 tmp.1 rbp tmp-ra.39)
                              (tmp.26 tmp.2 tmp.1 rbp tmp-ra.39))
                             (tmp.2 tmp.1 rbp tmp-ra.39))
                            (tmp.25 tmp.2 tmp.1 rbp tmp-ra.39)
                            (tmp.25 tmp.2 tmp.1 rbp tmp-ra.39))
                           (tmp.2 tmp.1 rbp tmp-ra.39))
                          ((tmp.27 tmp.1 rbp tmp-ra.39)
                           (tmp.1 tmp.27 rbp tmp-ra.39)
                           (tmp.27 rax rbp tmp-ra.39)
                           (rax rbp tmp-ra.39)
                           (rbp rax))
                          ((rax rbp tmp-ra.39) (rbp rax)))
                         ((rax rbp tmp-ra.39) (rbp rax)))))
                      (call-undead ()))
                     (begin
                       (set! tmp-ra.39 r15)
                       (set! tmp.1 rdi)
                       (set! tmp.2 rsi)
                       (if (begin
                             (if (begin
                                   (begin
                                     (set! tmp.24 tmp.2)
                                     (set! tmp.24 (bitwise-and tmp.24 7)))
                                   (= tmp.24 0))
                                 (set! tmp.23 14)
                                 (set! tmp.23 6))
                             (!= tmp.23 6))
                           (if (begin
                                 (if (begin
                                       (begin
                                         (set! tmp.26 tmp.1)
                                         (set! tmp.26 (bitwise-and tmp.26 7)))
                                       (= tmp.26 0))
                                     (set! tmp.25 14)
                                     (set! tmp.25 6))
                                 (!= tmp.25 6))
                               (begin
                                 (set! tmp.27 tmp.2)
                                 (set! tmp.27 (arithmetic-shift-right tmp.27 3))
                                 (set! rax tmp.1)
                                 (set! rax (* rax tmp.27))
                                 (jump tmp-ra.39 rbp rax))
                               (begin (set! rax 318) (jump tmp-ra.39 rbp rax)))
                           (begin (set! rax 318) (jump tmp-ra.39 rbp rax)))))
                   (define L.+.1
                     ((new-frames ())
                      (locals (tmp.4 tmp.31 tmp-ra.40 tmp.3 tmp.28 tmp.30 tmp.29))
                      (undead-out
                       ((rdi rsi rbp tmp-ra.40)
                        (rsi tmp.3 rbp tmp-ra.40)
                        (tmp.3 tmp.4 rbp tmp-ra.40)
                        ((((((tmp.29 tmp.3 tmp.4 rbp tmp-ra.40)
                             (tmp.29 tmp.3 tmp.4 rbp tmp-ra.40))
                            (tmp.3 tmp.4 rbp tmp-ra.40))
                           (tmp.28 tmp.3 tmp.4 rbp tmp-ra.40)
                           (tmp.28 tmp.3 tmp.4 rbp tmp-ra.40))
                          (tmp.3 tmp.4 rbp tmp-ra.40))
                         ((((((tmp.31 tmp.3 tmp.4 rbp tmp-ra.40)
                              (tmp.31 tmp.3 tmp.4 rbp tmp-ra.40))
                             (tmp.3 tmp.4 rbp tmp-ra.40))
                            (tmp.30 tmp.3 tmp.4 rbp tmp-ra.40)
                            (tmp.30 tmp.3 tmp.4 rbp tmp-ra.40))
                           (tmp.3 tmp.4 rbp tmp-ra.40))
                          ((tmp.4 rax rbp tmp-ra.40) (rax rbp tmp-ra.40) (rbp rax))
                          ((rax rbp tmp-ra.40) (rbp rax)))
                         ((rax rbp tmp-ra.40) (rbp rax)))))
                      (call-undead ()))
                     (begin
                       (set! tmp-ra.40 r15)
                       (set! tmp.3 rdi)
                       (set! tmp.4 rsi)
                       (if (begin
                             (if (begin
                                   (begin
                                     (set! tmp.29 tmp.4)
                                     (set! tmp.29 (bitwise-and tmp.29 7)))
                                   (= tmp.29 0))
                                 (set! tmp.28 14)
                                 (set! tmp.28 6))
                             (!= tmp.28 6))
                           (if (begin
                                 (if (begin
                                       (begin
                                         (set! tmp.31 tmp.3)
                                         (set! tmp.31 (bitwise-and tmp.31 7)))
                                       (= tmp.31 0))
                                     (set! tmp.30 14)
                                     (set! tmp.30 6))
                                 (!= tmp.30 6))
                               (begin
                                 (set! rax tmp.3)
                                 (set! rax (+ rax tmp.4))
                                 (jump tmp-ra.40 rbp rax))
                               (begin (set! rax 574) (jump tmp-ra.40 rbp rax)))
                           (begin (set! rax 574) (jump tmp-ra.40 rbp rax)))))
                   (define L.add.10
                     ((new-frames (() () () () () ()))
                      (locals
                       (e.65
                        c.63
                        tmp.34
                        tmp.37
                        g.67
                        tmp.35
                        tmp.36
                        a.61
                        tmp.32
                        h.68
                        f.66
                        d.64
                        b.62
                        tmp-ra.41
                        tmp.33))
                      (undead-out
                       ((rdi rsi rdx rcx r8 r9 fv0 fv1 tmp-ra.41 rbp)
                        (rsi rdx rcx r8 r9 fv0 fv1 a.61 tmp-ra.41 rbp)
                        (rdx rcx r8 r9 fv0 fv1 b.62 a.61 tmp-ra.41 rbp)
                        (rcx r8 r9 fv0 fv1 b.62 a.61 tmp-ra.41 c.63 rbp)
                        (r8 r9 fv0 fv1 d.64 b.62 a.61 tmp-ra.41 c.63 rbp)
                        (r9 fv0 fv1 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp)
                        (fv0 fv1 f.66 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp)
                        (fv1 f.66 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 g.67 rbp)
                        (f.66 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 g.67 h.68 rbp)
                        ((rax e.65 c.63 tmp-ra.41 a.61 b.62 d.64 f.66 rbp)
                         ((h.68 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
                        (e.65 c.63 tmp-ra.41 a.61 b.62 d.64 f.66 tmp.37 rbp)
                        ((rax d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp)
                         ((tmp.37 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
                        (d.64 b.62 a.61 tmp-ra.41 c.63 e.65 tmp.36 rbp)
                        ((rax c.63 tmp-ra.41 a.61 b.62 d.64 rbp)
                         ((tmp.36 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
                        (c.63 tmp-ra.41 a.61 b.62 d.64 tmp.35 rbp)
                        ((rax b.62 a.61 tmp-ra.41 c.63 rbp)
                         ((tmp.35 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
                        (b.62 a.61 tmp-ra.41 c.63 tmp.34 rbp)
                        ((rax tmp-ra.41 a.61 b.62 rbp)
                         ((tmp.34 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
                        (tmp-ra.41 a.61 b.62 tmp.33 rbp)
                        ((rax a.61 tmp-ra.41 rbp)
                         ((tmp.33 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
                        (a.61 tmp.32 tmp-ra.41 rbp)
                        (tmp.32 tmp-ra.41 rdi rbp)
                        (tmp-ra.41 rsi rdi rbp)
                        (rsi rdi r15 rbp)
                        (rbp r15 rdi rsi)))
                      (call-undead (f.66 e.65 d.64 c.63 b.62 tmp-ra.41 a.61)))
                     (begin
                       (set! tmp-ra.41 r15)
                       (set! a.61 rdi)
                       (set! b.62 rsi)
                       (set! c.63 rdx)
                       (set! d.64 rcx)
                       (set! e.65 r8)
                       (set! f.66 r9)
                       (set! g.67 fv0)
                       (set! h.68 fv1)
                       (return-point L.rp.12
                                     (begin
                                       (set! rdi g.67)
                                       (set! rsi h.68)
                                       (set! r15 L.rp.12)
                                       (jump L.+.1 rbp r15 rdi rsi)))
                       (set! tmp.37 rax)
                       (return-point L.rp.13
                                     (begin
                                       (set! rdi f.66)
                                       (set! rsi tmp.37)
                                       (set! r15 L.rp.13)
                                       (jump L.+.1 rbp r15 rdi rsi)))
                       (set! tmp.36 rax)
                       (return-point L.rp.14
                                     (begin
                                       (set! rdi e.65)
                                       (set! rsi tmp.36)
                                       (set! r15 L.rp.14)
                                       (jump L.+.1 rbp r15 rdi rsi)))
                       (set! tmp.35 rax)
                       (return-point L.rp.15
                                     (begin
                                       (set! rdi d.64)
                                       (set! rsi tmp.35)
                                       (set! r15 L.rp.15)
                                       (jump L.+.1 rbp r15 rdi rsi)))
                       (set! tmp.34 rax)
                       (return-point L.rp.16
                                     (begin
                                       (set! rdi c.63)
                                       (set! rsi tmp.34)
                                       (set! r15 L.rp.16)
                                       (jump L.+.1 rbp r15 rdi rsi)))
                       (set! tmp.33 rax)
                       (return-point L.rp.17
                                     (begin
                                       (set! rdi b.62)
                                       (set! rsi tmp.33)
                                       (set! r15 L.rp.17)
                                       (jump L.+.1 rbp r15 rdi rsi)))
                       (set! tmp.32 rax)
                       (set! rdi a.61)
                       (set! rsi tmp.32)
                       (set! r15 tmp-ra.41)
                       (jump L.+.1 rbp r15 rdi rsi)))
                   (define L.add-and-multiply.11
                     ((new-frames ((nfv.43 nfv.44)))
                      (locals
                       (a.69
                        g.75
                        sum.78
                        h.76
                        i.77
                        tmp-ra.42
                        c.71
                        nfv.44
                        d.72
                        b.70
                        f.74
                        e.73
                        nfv.43))
                      (undead-out
                       ((rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.42 rbp)
                        (rsi rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.42 a.69 rbp)
                        (rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.42 a.69 b.70 rbp)
                        (rcx r8 r9 fv0 fv1 fv2 tmp-ra.42 a.69 b.70 c.71 rbp)
                        (r8 r9 fv0 fv1 fv2 tmp-ra.42 a.69 b.70 c.71 d.72 rbp)
                        (r9 fv0 fv1 fv2 tmp-ra.42 a.69 b.70 c.71 d.72 e.73 rbp)
                        (fv0 fv1 fv2 tmp-ra.42 a.69 b.70 c.71 d.72 e.73 f.74 rbp)
                        (fv1 fv2 tmp-ra.42 a.69 b.70 c.71 d.72 e.73 f.74 g.75 rbp)
                        (fv2 tmp-ra.42 a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76 rbp)
                        (tmp-ra.42 i.77 a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76 rbp)
                        ((rax i.77 tmp-ra.42 rbp)
                         ((b.70 c.71 d.72 e.73 f.74 g.75 h.76 rdi rbp)
                          (c.71 d.72 e.73 f.74 g.75 h.76 rsi rdi rbp)
                          (d.72 e.73 f.74 g.75 h.76 rdx rsi rdi rbp)
                          (e.73 f.74 g.75 h.76 rcx rdx rsi rdi rbp)
                          (f.74 g.75 h.76 r8 rcx rdx rsi rdi rbp)
                          (g.75 h.76 r9 r8 rcx rdx rsi rdi rbp)
                          (h.76 nfv.43 r9 r8 rcx rdx rsi rdi rbp)
                          (nfv.44 nfv.43 r9 r8 rcx rdx rsi rdi rbp)
                          (nfv.44 nfv.43 r9 r8 rcx rdx rsi rdi r15 rbp)
                          (rbp r15 rdi rsi rdx rcx r8 r9 nfv.43 nfv.44)))
                        (sum.78 i.77 tmp-ra.42 rbp)
                        (i.77 tmp-ra.42 rdi rbp)
                        (tmp-ra.42 rsi rdi rbp)
                        (rsi rdi r15 rbp)
                        (rbp r15 rdi rsi)))
                      (call-undead (tmp-ra.42 i.77)))
                     (begin
                       (set! tmp-ra.42 r15)
                       (set! a.69 rdi)
                       (set! b.70 rsi)
                       (set! c.71 rdx)
                       (set! d.72 rcx)
                       (set! e.73 r8)
                       (set! f.74 r9)
                       (set! g.75 fv0)
                       (set! h.76 fv1)
                       (set! i.77 fv2)
                       (return-point L.rp.18
                                     (begin
                                       (set! rdi a.69)
                                       (set! rsi b.70)
                                       (set! rdx c.71)
                                       (set! rcx d.72)
                                       (set! r8 e.73)
                                       (set! r9 f.74)
                                       (set! nfv.43 g.75)
                                       (set! nfv.44 h.76)
                                       (set! r15 L.rp.18)
                                       (jump L.add.10 rbp r15 rdi rsi rdx rcx r8 r9 nfv.43 nfv.44)))
                       (set! sum.78 rax)
                       (set! rdi sum.78)
                       (set! rsi i.77)
                       (set! r15 tmp-ra.42)
                       (jump L.*.2 rbp r15 rdi rsi)))
                   (begin
                     (set! tmp-ra.45 r15)
                     (set! rdi 8)
                     (set! rsi 16)
                     (set! rdx 24)
                     (set! rcx 32)
                     (set! r8 40)
                     (set! r9 48)
                     (set! fv0 56)
                     (set! fv1 64)
                     (set! fv2 16)
                     (set! r15 tmp-ra.45)
                     (jump L.add-and-multiply.11 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2))))
  (check-equal? (undead-analysis '(module
                                      ((new-frames ()) (locals (tmp-ra.45)))
                                    (define L.*.2
                                      ((new-frames ())
                                       (locals (tmp.24 tmp.27 tmp.2 tmp.25 tmp-ra.39 tmp.23 tmp.1 tmp.26)))
                                      (begin
                                        (set! tmp-ra.39 r15)
                                        (set! tmp.1 rdi)
                                        (set! tmp.2 rsi)
                                        (if (begin
                                              (if (begin
                                                    (begin
                                                      (set! tmp.24 tmp.2)
                                                      (set! tmp.24 (bitwise-and tmp.24 7)))
                                                    (= tmp.24 0))
                                                  (set! tmp.23 14)
                                                  (set! tmp.23 6))
                                              (!= tmp.23 6))
                                            (if (begin
                                                  (if (begin
                                                        (begin
                                                          (set! tmp.26 tmp.1)
                                                          (set! tmp.26 (bitwise-and tmp.26 7)))
                                                        (= tmp.26 0))
                                                      (set! tmp.25 14)
                                                      (set! tmp.25 6))
                                                  (!= tmp.25 6))
                                                (begin
                                                  (set! tmp.27 tmp.2)
                                                  (set! tmp.27 (arithmetic-shift-right tmp.27 3))
                                                  (set! rax tmp.1)
                                                  (set! rax (* rax tmp.27))
                                                  (jump tmp-ra.39 rbp rax))
                                                (begin (set! rax 318) (jump tmp-ra.39 rbp rax)))
                                            (begin (set! rax 318) (jump tmp-ra.39 rbp rax)))))
                                    (define L.+.1
                                      ((new-frames ())
                                       (locals (tmp.4 tmp.31 tmp-ra.40 tmp.3 tmp.28 tmp.30 tmp.29)))
                                      (begin
                                        (set! tmp-ra.40 r15)
                                        (set! tmp.3 rdi)
                                        (set! tmp.4 rsi)
                                        (if (begin
                                              (if (begin
                                                    (begin
                                                      (set! tmp.29 tmp.4)
                                                      (set! tmp.29 (bitwise-and tmp.29 7)))
                                                    (= tmp.29 0))
                                                  (set! tmp.28 14)
                                                  (set! tmp.28 6))
                                              (!= tmp.28 6))
                                            (if (begin
                                                  (if (begin
                                                        (begin
                                                          (set! tmp.31 tmp.3)
                                                          (set! tmp.31 (bitwise-and tmp.31 7)))
                                                        (= tmp.31 0))
                                                      (set! tmp.30 14)
                                                      (set! tmp.30 6))
                                                  (!= tmp.30 6))
                                                (begin
                                                  (set! rax tmp.3)
                                                  (set! rax (+ rax tmp.4))
                                                  (jump tmp-ra.40 rbp rax))
                                                (begin (set! rax 574) (jump tmp-ra.40 rbp rax)))
                                            (begin (set! rax 574) (jump tmp-ra.40 rbp rax)))))
                                    (define L.add.10
                                      ((new-frames (() () () () () ()))
                                       (locals
                                        (e.65
                                         c.63
                                         tmp.34
                                         tmp.37
                                         g.67
                                         tmp.35
                                         tmp.36
                                         a.61
                                         tmp.32
                                         h.68
                                         f.66
                                         d.64
                                         b.62
                                         tmp-ra.41
                                         tmp.33)))
                                      (begin
                                        (set! tmp-ra.41 r15)
                                        (set! a.61 rdi)
                                        (set! b.62 rsi)
                                        (set! c.63 rdx)
                                        (set! d.64 rcx)
                                        (set! e.65 r8)
                                        (set! f.66 r9)
                                        (set! g.67 fv0)
                                        (set! h.68 fv1)
                                        (return-point L.rp.12
                                                      (begin
                                                        (set! rdi g.67)
                                                        (set! rsi h.68)
                                                        (set! r15 L.rp.12)
                                                        (jump L.+.1 rbp r15 rdi rsi)))
                                        (set! tmp.37 rax)
                                        (return-point L.rp.13
                                                      (begin
                                                        (set! rdi f.66)
                                                        (set! rsi tmp.37)
                                                        (set! r15 L.rp.13)
                                                        (jump L.+.1 rbp r15 rdi rsi)))
                                        (set! tmp.36 rax)
                                        (return-point L.rp.14
                                                      (begin
                                                        (set! rdi e.65)
                                                        (set! rsi tmp.36)
                                                        (set! r15 L.rp.14)
                                                        (jump L.+.1 rbp r15 rdi rsi)))
                                        (set! tmp.35 rax)
                                        (return-point L.rp.15
                                                      (begin
                                                        (set! rdi d.64)
                                                        (set! rsi tmp.35)
                                                        (set! r15 L.rp.15)
                                                        (jump L.+.1 rbp r15 rdi rsi)))
                                        (set! tmp.34 rax)
                                        (return-point L.rp.16
                                                      (begin
                                                        (set! rdi c.63)
                                                        (set! rsi tmp.34)
                                                        (set! r15 L.rp.16)
                                                        (jump L.+.1 rbp r15 rdi rsi)))
                                        (set! tmp.33 rax)
                                        (return-point L.rp.17
                                                      (begin
                                                        (set! rdi b.62)
                                                        (set! rsi tmp.33)
                                                        (set! r15 L.rp.17)
                                                        (jump L.+.1 rbp r15 rdi rsi)))
                                        (set! tmp.32 rax)
                                        (set! rdi a.61)
                                        (set! rsi tmp.32)
                                        (set! r15 tmp-ra.41)
                                        (jump L.+.1 rbp r15 rdi rsi)))
                                    (define L.add-and-multiply.11
                                      ((new-frames ((nfv.43 nfv.44)))
                                       (locals
                                        (a.69
                                         g.75
                                         sum.78
                                         h.76
                                         i.77
                                         tmp-ra.42
                                         c.71
                                         nfv.44
                                         d.72
                                         b.70
                                         f.74
                                         e.73
                                         nfv.43)))
                                      (begin
                                        (set! tmp-ra.42 r15)
                                        (set! a.69 rdi)
                                        (set! b.70 rsi)
                                        (set! c.71 rdx)
                                        (set! d.72 rcx)
                                        (set! e.73 r8)
                                        (set! f.74 r9)
                                        (set! g.75 fv0)
                                        (set! h.76 fv1)
                                        (set! i.77 fv2)
                                        (return-point L.rp.18
                                                      (begin
                                                        (set! rdi a.69)
                                                        (set! rsi b.70)
                                                        (set! rdx c.71)
                                                        (set! rcx d.72)
                                                        (set! r8 e.73)
                                                        (set! r9 f.74)
                                                        (set! nfv.43 g.75)
                                                        (set! nfv.44 h.76)
                                                        (set! r15 L.rp.18)
                                                        (jump L.add.10 rbp r15 rdi rsi rdx rcx r8 r9 nfv.43 nfv.44)))
                                        (set! sum.78 rax)
                                        (set! rdi sum.78)
                                        (set! rsi i.77)
                                        (set! r15 tmp-ra.42)
                                        (jump L.*.2 rbp r15 rdi rsi)))
                                    (begin
                                      (set! tmp-ra.45 r15)
                                      (set! rdi 8)
                                      (set! rsi 16)
                                      (set! rdx 24)
                                      (set! rcx 32)
                                      (set! r8 40)
                                      (set! r9 48)
                                      (set! fv0 56)
                                      (set! fv1 64)
                                      (set! fv2 16)
                                      (set! r15 tmp-ra.45)
                                      (jump L.add-and-multiply.11 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2))))
                '(module
   ((new-frames ())
    (locals (tmp-ra.45))
    (call-undead ())
    (undead-out
     ((tmp-ra.45 rbp)
      (tmp-ra.45 rdi rbp)
      (tmp-ra.45 rsi rdi rbp)
      (tmp-ra.45 rdx rsi rdi rbp)
      (tmp-ra.45 rcx rdx rsi rdi rbp)
      (tmp-ra.45 r8 rcx rdx rsi rdi rbp)
      (tmp-ra.45 r9 r8 rcx rdx rsi rdi rbp)
      (tmp-ra.45 fv0 r9 r8 rcx rdx rsi rdi rbp)
      (tmp-ra.45 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)
      (tmp-ra.45 fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)
      (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi r15 rbp)
      (rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2))))
   (define L.*.2
     ((new-frames ())
      (locals (tmp.24 tmp.27 tmp.2 tmp.25 tmp-ra.39 tmp.23 tmp.1 tmp.26))
      (undead-out
       ((rdi rsi rbp tmp-ra.39)
        (rsi tmp.1 rbp tmp-ra.39)
        (tmp.2 tmp.1 rbp tmp-ra.39)
        ((((((tmp.24 tmp.2 tmp.1 rbp tmp-ra.39)
             (tmp.24 tmp.2 tmp.1 rbp tmp-ra.39))
            (tmp.2 tmp.1 rbp tmp-ra.39))
           (tmp.23 tmp.2 tmp.1 rbp tmp-ra.39)
           (tmp.23 tmp.2 tmp.1 rbp tmp-ra.39))
          (tmp.2 tmp.1 rbp tmp-ra.39))
         ((((((tmp.26 tmp.2 tmp.1 rbp tmp-ra.39)
              (tmp.26 tmp.2 tmp.1 rbp tmp-ra.39))
             (tmp.2 tmp.1 rbp tmp-ra.39))
            (tmp.25 tmp.2 tmp.1 rbp tmp-ra.39)
            (tmp.25 tmp.2 tmp.1 rbp tmp-ra.39))
           (tmp.2 tmp.1 rbp tmp-ra.39))
          ((tmp.27 tmp.1 rbp tmp-ra.39)
           (tmp.1 tmp.27 rbp tmp-ra.39)
           (tmp.27 rax rbp tmp-ra.39)
           (rax rbp tmp-ra.39)
           (rbp rax))
          ((rax rbp tmp-ra.39) (rbp rax)))
         ((rax rbp tmp-ra.39) (rbp rax)))))
      (call-undead ()))
     (begin
       (set! tmp-ra.39 r15)
       (set! tmp.1 rdi)
       (set! tmp.2 rsi)
       (if (begin
             (if (begin
                   (begin
                     (set! tmp.24 tmp.2)
                     (set! tmp.24 (bitwise-and tmp.24 7)))
                   (= tmp.24 0))
               (set! tmp.23 14)
               (set! tmp.23 6))
             (!= tmp.23 6))
         (if (begin
               (if (begin
                     (begin
                       (set! tmp.26 tmp.1)
                       (set! tmp.26 (bitwise-and tmp.26 7)))
                     (= tmp.26 0))
                 (set! tmp.25 14)
                 (set! tmp.25 6))
               (!= tmp.25 6))
           (begin
             (set! tmp.27 tmp.2)
             (set! tmp.27 (arithmetic-shift-right tmp.27 3))
             (set! rax tmp.1)
             (set! rax (* rax tmp.27))
             (jump tmp-ra.39 rbp rax))
           (begin (set! rax 318) (jump tmp-ra.39 rbp rax)))
         (begin (set! rax 318) (jump tmp-ra.39 rbp rax)))))
   (define L.+.1
     ((new-frames ())
      (locals (tmp.4 tmp.31 tmp-ra.40 tmp.3 tmp.28 tmp.30 tmp.29))
      (undead-out
       ((rdi rsi rbp tmp-ra.40)
        (rsi tmp.3 rbp tmp-ra.40)
        (tmp.3 tmp.4 rbp tmp-ra.40)
        ((((((tmp.29 tmp.3 tmp.4 rbp tmp-ra.40)
             (tmp.29 tmp.3 tmp.4 rbp tmp-ra.40))
            (tmp.3 tmp.4 rbp tmp-ra.40))
           (tmp.28 tmp.3 tmp.4 rbp tmp-ra.40)
           (tmp.28 tmp.3 tmp.4 rbp tmp-ra.40))
          (tmp.3 tmp.4 rbp tmp-ra.40))
         ((((((tmp.31 tmp.3 tmp.4 rbp tmp-ra.40)
              (tmp.31 tmp.3 tmp.4 rbp tmp-ra.40))
             (tmp.3 tmp.4 rbp tmp-ra.40))
            (tmp.30 tmp.3 tmp.4 rbp tmp-ra.40)
            (tmp.30 tmp.3 tmp.4 rbp tmp-ra.40))
           (tmp.3 tmp.4 rbp tmp-ra.40))
          ((tmp.4 rax rbp tmp-ra.40) (rax rbp tmp-ra.40) (rbp rax))
          ((rax rbp tmp-ra.40) (rbp rax)))
         ((rax rbp tmp-ra.40) (rbp rax)))))
      (call-undead ()))
     (begin
       (set! tmp-ra.40 r15)
       (set! tmp.3 rdi)
       (set! tmp.4 rsi)
       (if (begin
             (if (begin
                   (begin
                     (set! tmp.29 tmp.4)
                     (set! tmp.29 (bitwise-and tmp.29 7)))
                   (= tmp.29 0))
               (set! tmp.28 14)
               (set! tmp.28 6))
             (!= tmp.28 6))
         (if (begin
               (if (begin
                     (begin
                       (set! tmp.31 tmp.3)
                       (set! tmp.31 (bitwise-and tmp.31 7)))
                     (= tmp.31 0))
                 (set! tmp.30 14)
                 (set! tmp.30 6))
               (!= tmp.30 6))
           (begin
             (set! rax tmp.3)
             (set! rax (+ rax tmp.4))
             (jump tmp-ra.40 rbp rax))
           (begin (set! rax 574) (jump tmp-ra.40 rbp rax)))
         (begin (set! rax 574) (jump tmp-ra.40 rbp rax)))))
   (define L.add.10
     ((new-frames (() () () () () ()))
      (locals
       (e.65
        c.63
        tmp.34
        tmp.37
        g.67
        tmp.35
        tmp.36
        a.61
        tmp.32
        h.68
        f.66
        d.64
        b.62
        tmp-ra.41
        tmp.33))
      (undead-out
       ((rdi rsi rdx rcx r8 r9 fv0 fv1 tmp-ra.41 rbp)
        (rsi rdx rcx r8 r9 fv0 fv1 a.61 tmp-ra.41 rbp)
        (rdx rcx r8 r9 fv0 fv1 b.62 a.61 tmp-ra.41 rbp)
        (rcx r8 r9 fv0 fv1 b.62 a.61 tmp-ra.41 c.63 rbp)
        (r8 r9 fv0 fv1 d.64 b.62 a.61 tmp-ra.41 c.63 rbp)
        (r9 fv0 fv1 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp)
        (fv0 fv1 f.66 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp)
        (fv1 f.66 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 g.67 rbp)
        (f.66 d.64 b.62 a.61 tmp-ra.41 c.63 e.65 g.67 h.68 rbp)
        ((rax e.65 c.63 tmp-ra.41 a.61 b.62 d.64 f.66 rbp)
         ((h.68 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
        (e.65 c.63 tmp-ra.41 a.61 b.62 d.64 f.66 tmp.37 rbp)
        ((rax d.64 b.62 a.61 tmp-ra.41 c.63 e.65 rbp)
         ((tmp.37 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
        (d.64 b.62 a.61 tmp-ra.41 c.63 e.65 tmp.36 rbp)
        ((rax c.63 tmp-ra.41 a.61 b.62 d.64 rbp)
         ((tmp.36 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
        (c.63 tmp-ra.41 a.61 b.62 d.64 tmp.35 rbp)
        ((rax b.62 a.61 tmp-ra.41 c.63 rbp)
         ((tmp.35 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
        (b.62 a.61 tmp-ra.41 c.63 tmp.34 rbp)
        ((rax tmp-ra.41 a.61 b.62 rbp)
         ((tmp.34 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
        (tmp-ra.41 a.61 b.62 tmp.33 rbp)
        ((rax a.61 tmp-ra.41 rbp)
         ((tmp.33 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rbp r15 rdi rsi)))
        (a.61 tmp.32 tmp-ra.41 rbp)
        (tmp.32 tmp-ra.41 rdi rbp)
        (tmp-ra.41 rsi rdi rbp)
        (rsi rdi r15 rbp)
        (rbp r15 rdi rsi)))
      (call-undead (f.66 e.65 d.64 c.63 b.62 tmp-ra.41 a.61)))
     (begin
       (set! tmp-ra.41 r15)
       (set! a.61 rdi)
       (set! b.62 rsi)
       (set! c.63 rdx)
       (set! d.64 rcx)
       (set! e.65 r8)
       (set! f.66 r9)
       (set! g.67 fv0)
       (set! h.68 fv1)
       (return-point L.rp.12
         (begin
           (set! rdi g.67)
           (set! rsi h.68)
           (set! r15 L.rp.12)
           (jump L.+.1 rbp r15 rdi rsi)))
       (set! tmp.37 rax)
       (return-point L.rp.13
         (begin
           (set! rdi f.66)
           (set! rsi tmp.37)
           (set! r15 L.rp.13)
           (jump L.+.1 rbp r15 rdi rsi)))
       (set! tmp.36 rax)
       (return-point L.rp.14
         (begin
           (set! rdi e.65)
           (set! rsi tmp.36)
           (set! r15 L.rp.14)
           (jump L.+.1 rbp r15 rdi rsi)))
       (set! tmp.35 rax)
       (return-point L.rp.15
         (begin
           (set! rdi d.64)
           (set! rsi tmp.35)
           (set! r15 L.rp.15)
           (jump L.+.1 rbp r15 rdi rsi)))
       (set! tmp.34 rax)
       (return-point L.rp.16
         (begin
           (set! rdi c.63)
           (set! rsi tmp.34)
           (set! r15 L.rp.16)
           (jump L.+.1 rbp r15 rdi rsi)))
       (set! tmp.33 rax)
       (return-point L.rp.17
         (begin
           (set! rdi b.62)
           (set! rsi tmp.33)
           (set! r15 L.rp.17)
           (jump L.+.1 rbp r15 rdi rsi)))
       (set! tmp.32 rax)
       (set! rdi a.61)
       (set! rsi tmp.32)
       (set! r15 tmp-ra.41)
       (jump L.+.1 rbp r15 rdi rsi)))
   (define L.add-and-multiply.11
     ((new-frames ((nfv.43 nfv.44)))
      (locals
       (a.69
        g.75
        sum.78
        h.76
        i.77
        tmp-ra.42
        c.71
        nfv.44
        d.72
        b.70
        f.74
        e.73
        nfv.43))
      (undead-out
       ((rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.42 rbp)
        (rsi rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.42 a.69 rbp)
        (rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.42 a.69 b.70 rbp)
        (rcx r8 r9 fv0 fv1 fv2 tmp-ra.42 a.69 b.70 c.71 rbp)
        (r8 r9 fv0 fv1 fv2 tmp-ra.42 a.69 b.70 c.71 d.72 rbp)
        (r9 fv0 fv1 fv2 tmp-ra.42 a.69 b.70 c.71 d.72 e.73 rbp)
        (fv0 fv1 fv2 tmp-ra.42 a.69 b.70 c.71 d.72 e.73 f.74 rbp)
        (fv1 fv2 tmp-ra.42 a.69 b.70 c.71 d.72 e.73 f.74 g.75 rbp)
        (fv2 tmp-ra.42 a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76 rbp)
        (tmp-ra.42 i.77 a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76 rbp)
        ((rax i.77 tmp-ra.42 rbp)
         ((b.70 c.71 d.72 e.73 f.74 g.75 h.76 rdi rbp)
          (c.71 d.72 e.73 f.74 g.75 h.76 rsi rdi rbp)
          (d.72 e.73 f.74 g.75 h.76 rdx rsi rdi rbp)
          (e.73 f.74 g.75 h.76 rcx rdx rsi rdi rbp)
          (f.74 g.75 h.76 r8 rcx rdx rsi rdi rbp)
          (g.75 h.76 r9 r8 rcx rdx rsi rdi rbp)
          (h.76 nfv.43 r9 r8 rcx rdx rsi rdi rbp)
          (nfv.44 nfv.43 r9 r8 rcx rdx rsi rdi rbp)
          (nfv.44 nfv.43 r9 r8 rcx rdx rsi rdi r15 rbp)
          (rbp r15 rdi rsi rdx rcx r8 r9 nfv.43 nfv.44)))
        (sum.78 i.77 tmp-ra.42 rbp)
        (i.77 tmp-ra.42 rdi rbp)
        (tmp-ra.42 rsi rdi rbp)
        (rsi rdi r15 rbp)
        (rbp r15 rdi rsi)))
      (call-undead (tmp-ra.42 i.77)))
     (begin
       (set! tmp-ra.42 r15)
       (set! a.69 rdi)
       (set! b.70 rsi)
       (set! c.71 rdx)
       (set! d.72 rcx)
       (set! e.73 r8)
       (set! f.74 r9)
       (set! g.75 fv0)
       (set! h.76 fv1)
       (set! i.77 fv2)
       (return-point L.rp.18
         (begin
           (set! rdi a.69)
           (set! rsi b.70)
           (set! rdx c.71)
           (set! rcx d.72)
           (set! r8 e.73)
           (set! r9 f.74)
           (set! nfv.43 g.75)
           (set! nfv.44 h.76)
           (set! r15 L.rp.18)
           (jump L.add.10 rbp r15 rdi rsi rdx rcx r8 r9 nfv.43 nfv.44)))
       (set! sum.78 rax)
       (set! rdi sum.78)
       (set! rsi i.77)
       (set! r15 tmp-ra.42)
       (jump L.*.2 rbp r15 rdi rsi)))
   (begin
     (set! tmp-ra.45 r15)
     (set! rdi 8)
     (set! rsi 16)
     (set! rdx 24)
     (set! rcx 32)
     (set! r8 40)
     (set! r9 48)
     (set! fv0 56)
     (set! fv1 64)
     (set! fv2 16)
     (set! r15 tmp-ra.45)
     (jump L.add-and-multiply.11 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2)))))
