#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2
  rackunit)

(provide replace-locations)

;; asm-lang-v2/assignments -> nested-asm-lang-v2
;; interp. replaces the abstract locations with the concrete locations
(define (replace-locations p)
  (-> asm-lang-v2/assignments? nested-asm-lang-v2?)

  ;; acc is (Map-of aloc reg)
  ;; the abstract locations mapped to its physical location
  (define assignments (make-hash))
  
  ;; interp. replaces the abstract locations with the concrete locations
  (define (replace-locations/tail t)
    (match t
      [`(halt ,triv)
       `(halt ,(replace-locations/triv triv))]
      [`(begin ,fx ... ,tail)
       (define compiled-fx (for/list ([e fx])
                             (replace-locations/effect e)))
       (define compiled-tail (replace-locations/tail tail))
       `(begin ,@compiled-fx ,compiled-tail)]))

  ;; interp. replaces the abstract locations with the concrete locations
  (define (replace-locations/effect e)
    (match e
      [`(set! ,x (,binop ,x ,v))
       (define reg (dict-ref assignments x))
       `(set! ,reg (,binop ,reg ,(replace-locations/triv v)))]
      [`(set! ,x ,v)
       `(set! ,(dict-ref assignments x) ,(replace-locations/triv v))]
      [`(begin ,fx ... ,e)
       (define compiled-fx (for/list ([e fx]) (replace-locations/effect e)))
       (define compiled-e (replace-locations/effect e))
       `(begin ,@compiled-fx ,compiled-e)]))

  ;; interp. replaces any abstract locations with the concrete locations
  (define (replace-locations/triv t)
    (match t
      [`,x #:when (aloc? x) (dict-ref assignments x)]
      [`,x x]))

  (match p
    [`(module ,info ,tail)
     (for ([pair (info-ref info 'assignment)])
       (dict-set! assignments (first pair) (second pair)))
     (replace-locations/tail tail)]))

(test-case
 "replace-locations"
 (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                     (begin (set! x.1 (+ x.1 1)) (halt x.1))))
               '(begin (set! rax (+ rax 1)) (halt rax)))
 (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax)))) (begin (set! x.1 0) (halt x.1))))
               '(begin (set! rax 0) (halt rax)))
 (check-equal? (replace-locations '(module ((locals (x.1 y.1 w.1)) (assignment ((x.1 rax) (y.1 rbx) (w.1 r9))))
                                     (begin (set! x.1 0)
                                            (set! y.1 x.1)
                                            (set! w.1 1)
                                            (set! w.1 (+ w.1 y.1))
                                            (halt w.1))))
               '(begin (set! rax 0) (set! rbx rax) (set! r9 1) (set! r9 (+ r9 rbx)) (halt r9))))