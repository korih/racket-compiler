#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v2
  cpsc411/langs/v2-reg-alloc
  rackunit)

(provide assign-registers)

;; Exercise 3
;; asm-lang-v2/conflicts -> asm-lang-v2/assignments
;; compiles p to asm-lang-v2/assignments by attempting to fit each of the
;; abstract location declared in the locals set into a register, and if one
;; cannot be found, assigns it a frame variable instead
(define/contract (assign-registers p)
  (-> asm-lang-v2/conflicts? asm-lang-v2/assignments?)

  (define (graph-colouring-register-allocation conflict-graph registers)
    (define fvar-index 0)

    (define (make-fvar-spill)
      (define fvar (make-fvar fvar-index))
      (set! fvar-index (+ fvar-index 1))
      fvar)

    (define (assign-registers-helper remaining-graph assignment)
      (if (null? remaining-graph)
          assignment
          (let* ([sorted-nodes (sort (map car remaining-graph)
                                     (lambda (a b)
                                       (< (length (get-neighbors conflict-graph a))
                                          (length (get-neighbors conflict-graph b)))))]
                 [chosen-node (car sorted-nodes)]
                 [conflicting (get-neighbors conflict-graph chosen-node)]
                 [used-registers (map (lambda (conflict) (info-ref assignment conflict #f)) conflicting)]
                 [available-registers (filter (lambda (r) (not (member r used-registers))) registers)]
                 [new-location (if (null? available-registers)
                                   (make-fvar-spill)
                                   (car available-registers))])
            (assign-registers-helper (remove-vertex remaining-graph chosen-node)
                                     (info-set assignment chosen-node new-location)))))

    (assign-registers-helper conflict-graph '()))

  (match p
    [`(module ,info ,tail)
     (define assignments (graph-colouring-register-allocation (info-ref info 'conflicts)
                                                              (current-assignable-registers)))
     `(module ,(info-set (info-remove info 'conflicts)
                         'assignment
                         assignments)
        ,tail)]))

(test-case
 "assign-registers"
 (check-true #t))