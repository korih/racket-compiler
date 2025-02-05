#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/2c-run-time
  rackunit)

(provide
 check-values-lang
 uniquify
 sequentialize-let
 normalize-bind
 select-instructions
 uncover-locals
 ;undead-analysis
 conflict-analysis
 assign-registers
 replace-locations
 assign-homes-opt
 assign-homes
 flatten-begins
 patch-instructions
 implement-fvars
 generate-x64

 compile-m2
 compile-m3)

;; STUBS; delete when you've begun to implement the passes or replaced them with
;; your own stubs.
(define-values (check-values-lang
                uniquify
                sequentialize-let
                normalize-bind
                select-instructions
                uncover-locals
                ;undead-analysis
                conflict-analysis
                assign-registers
                replace-locations
                assign-homes-opt
                assign-homes
                flatten-begins
                patch-instructions
                implement-fvars
                generate-x64

                compile-m2
                compile-m3)
  (;values
   values
   values
   values
   values
   values
   values
   values
   values
   values
   values
   values
   values
   values
   values
   values
   values
   values
   values))

;; TODO: Fill in.
;; You'll want to merge milestone-2 code in

;; asm-lang-v2/locals? -> asm-lang-v2/undead?
(define (undead-analysis p)

  ;; asm-lang-v2/locals? tail -> undead-out set
  (define (compile-tail t)
    (match t
      [_ (define-values (ust _)
           (compile-effects t '()))
         (reverse (cons '() (remove '() ust)))]))

  (define (compile-effects e undead-out)
    (match e
      [`(begin ,effects ...)
       (define-values (rev-ust undead-in)
         (for/foldr ([rev-ust '()]
                     [undead-out undead-out])
           ([effect effects])
           (define-values (ust undead-in)
                    (compile-effects effect undead-out))
           (values (cons ust rev-ust) undead-in)))
       (values (reverse rev-ust) undead-in)]
      [`(set! ,aloc_1 (,binop ,aloc_1 ,triv))
       (let ([undead-in (set-union (set-add (set-remove undead-out aloc_1) aloc_1) (analyze-triv triv))])
         (values undead-in undead-in))]
      [`(set! ,aloc ,triv)
       (let ([undead-in (set-union (set-remove undead-out aloc) (analyze-triv triv))])
         (values undead-in undead-in))]
      [`(halt ,triv)
       (let ([undead-in (set-add undead-out triv)])
         (values undead-in undead-in))]))

  (define (analyze-triv triv)
    (match triv
      [`,triv (if (aloc? triv)
                  (list triv)
                  '())]))

  (define (compile-info i ust)
    (match i
      [`,info #:when (info? info) (info-set info 'undead-out ust)]))

  (match p
    [`(module ,info ,tail) `(module ,(compile-info info (compile-tail tail)) ,tail)]))

;; undead-analysis tests from textbook
#;
(check-equal? (undead-analysis
               '(module ((locals (x.1)))
                  (begin
                    (set! x.1 42)
                    (halt x.1))))
              '(module
                   ((locals (x.1)) (undead-out ((x.1) ())))
                 (begin (set! x.1 42) (halt x.1))))
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

(module+ test
  (require
    rackunit
    rackunit/text-ui
    cpsc411/langs/v3
    cpsc411/langs/v2-reg-alloc
    cpsc411/langs/v2
    cpsc411/test-suite/public/v3
    cpsc411/test-suite/public/v2-reg-alloc)

  ;; You can modify this pass list, e.g., by adding check-assignment, or other
  ;; debugging and validation passes.
  ;; Doing this may provide additional debugging info when running the rest
  ;; suite.
  ;; If you modify, you must modify the corresponding interpreter in the
  ;; interp-ls, at least by interesting #f as the interpreter for the new pass.
  ;; See the documentation for v3-public-test-suite for details on the structure
  ;; of the interpreter list.
  (current-pass-list (list
                      check-values-lang
                      uniquify
                      sequentialize-let
                      normalize-bind
                      select-instructions
                      assign-homes-opt
                      flatten-begins
                      patch-instructions
                      implement-fvars
                      generate-x64
                      wrap-x64-run-time
                      wrap-x64-boilerplate))

  (define interp-ls (list
                     interp-values-lang-v3
                     interp-values-lang-v3
                     interp-values-unique-lang-v3
                     interp-imp-mf-lang-v3
                     interp-imp-cmf-lang-v3
                     interp-asm-lang-v2
                     interp-nested-asm-lang-v2
                     interp-para-asm-lang-v2
                     interp-paren-x64-fvars-v2
                     interp-paren-x64-v2
                     #f #f))

  (run-tests (v3-public-test-sutie (current-pass-list) interp-ls))
  (run-tests (v2-reg-alloc-public-test-suite undead-analysis conflict-analysis assign-registers)))
