#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v5
  rackunit)

(provide assign-registers)

;; Exercise 5
;; asm-lang-v5/conflicts -> asm-lang-v5/assignments
;; perform graph-colouring register allocation, compiling p to
;; Asm-pred-lang v5/assignments by decorating programs with their register
;; assignments
(define/contract (assign-registers p)
  (-> asm-pred-lang-v5/conflicts? asm-pred-lang-v5/assignments?)

  ;; graph (List-of register) -> (List-of (list aloc loc))
  (define (graph-colouring-with-spilling conflict-graph registers)

    ;; fvar-index is Natural
    ;; keeps track of the current index of frame variables for spillovers 
    (define fvar-index 0)

    ;; -> fvar
    ;; EFFECTS: increments fvar-index by 1
    (define (make-fvar-spill)
      (define fvar (make-fvar fvar-index))
      (set! fvar-index (+ fvar-index 1))
      fvar)

    ;; graph -> (List-of (list aloc loc))
    (define (colour-graph graph)
      (cond
        [(null? graph) '()]
        [else
         (define sorted-graph (sort (map car graph)
                                    (lambda (a b)
                                      (< (length (get-neighbors graph a))
                                         (length (get-neighbors graph b))))))
         (define chosen-node (car sorted-graph))
         (define updated-graph (remove-vertex graph chosen-node))
         (define sub-assign (colour-graph updated-graph))
         (define conflicts (get-neighbors graph chosen-node))
         (define used-registers (map cdr sub-assign))
         (define available-registers (filter (lambda (r)
                                               (not (member r used-registers)))
                                             registers))
         (define new-location
           (if (empty? available-registers)
               (make-fvar-spill)
               (car available-registers)))
         (cons (list chosen-node new-location) sub-assign)]))

    (colour-graph conflict-graph))

  (match p
    [`(module ,info ,tail)
     (define assignments (graph-colouring-with-spilling (info-ref info 'conflicts)
                                                        (current-assignable-registers)))
     `(module ,(info-set (info-remove info 'conflicts)
                         'assignment
                         assignments)
        ,tail)]))

(module+ test
  (check-equal? (assign-registers
                 '(module ((locals (x.1))
                           (conflicts ((x.1 ()))))
                    (begin
                      (set! x.1 42)
                      (halt x.1))))
                '(module
                     ((locals (x.1)) (assignment ((x.1 rsp))))
                   (begin (set! x.1 42) (halt x.1))))
  (check-equal? (parameterize ([current-assignable-registers '(r9)])
                  (assign-registers
                   '(module ((locals (x.1))
                             (conflicts ((x.1 ()))))
                      (begin
                        (set! x.1 42)
                        (halt x.1)))))
                '(module
                     ((locals (x.1)) (assignment ((x.1 r9))))
                   (begin (set! x.1 42) (halt x.1))))
  (check-equal? (parameterize ([current-assignable-registers '()])
                  (assign-registers
                   '(module ((locals (x.1))
                             (conflicts ((x.1 ()))))
                      (begin
                        (set! x.1 42)
                        (halt x.1)))))
                '(module
                     ((locals (x.1)) (assignment ((x.1 fv0))))
                   (begin (set! x.1 42) (halt x.1))))
  (check-equal? (assign-registers
                 '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                           (conflicts
                            ((x.3 (z.5 p.1 y.4 v.1 w.2))
                             (w.2 (z.5 p.1 y.4 v.1 x.3))
                             (v.1 (w.2 x.3))
                             (y.4 (t.6 z.5 p.1 w.2 x.3))
                             (p.1 (t.6 z.5 y.4 w.2 x.3))
                             (z.5 (t.6 p.1 y.4 w.2 x.3))
                             (t.6 (z.5 p.1 y.4)))))
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
                      (assignment
                       ((v.1 rsp) (t.6 rsp) (x.3 rsp) (w.2 rsp) (y.4 rsp) (p.1 rsp) (z.5 rsp))))
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
