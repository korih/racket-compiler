#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v8)

(provide assign-registers)

;; asm-pred-lang-v8/framed -> asm-pred-lang-v8/spilled
;; perform graph-colouring register allocation, compiling p to
;; Asm-pred-lang-v8/spilled by decorating programs with their register
;; assignments
(define/contract (assign-registers p)
  (-> asm-pred-lang-v8/framed? asm-pred-lang-v8/spilled?)

  ;; func is `(define ,label ,info ,tail)
  ;; interp. a function definition

  ;; spilled-variables is (Box-of (List-of aloc))
  ;; interp. stores a list of abstract locations that could not be assigned to 
  ;; register sand must be spilled to memory
  (define spilled-variables (box '()))

  ;; func -> func
  ; interp. performs register allocation for one function by modifying its
  ;; assignment mapping using graph colouring
  ;; EFFECTS: resets spilled-variables to an empty list
  (define (assign-registers-func func)
    (match func
      [`(define ,label ,info ,tail)
       (set-box! spilled-variables '())
       (define assignments (graph-colouring-with-spilling (info-ref info 'assignment)
                                                          (info-ref info 'conflicts)
                                                          (reverse (current-assignable-registers))))
       (define updated-info (info-set info 'assignment assignments))
       (define updated-locals (remove* (map car (info-ref updated-info 'assignment)) (info-ref updated-info 'locals)))
       (set! updated-info (info-set updated-info 'locals updated-locals))
       `(define ,label ,updated-info ,tail)]))

  ;; (Graph-of loc) (List-of register) -> (List-of (list aloc loc))
  ;; interp. colours the conflict graph using the given registers, reusing 
  ;; existing assignments and allocating registers greedily
  ;; EFFECTS: mutates spilled-variables by adding any unassignable alocs
  (define (graph-colouring-with-spilling assignments conflict-graph registers)
    (define assigned-alocs (map car assignments))
    (define graph^ (for/fold ([new-graph conflict-graph])
                             ([assignment assigned-alocs])
                     (remove-vertex new-graph assignment)))

    ;; (Graph-of loc) -> (List-of (list aloc loc))
    ;; interp. recursively assigns a register to each unassigned aloc using the
    ;; greedy graph colouring algorithm
    ;; EFFECTS: updates spilled-variables with any aloc that could not be assigned
    (define (colour-graph graph)
      (define sorted-graph (sort (filter aloc? (map car graph))
                                 (lambda (a b)
                                   (< (length (get-neighbors graph a))
                                      (length (get-neighbors graph b))))))
      (cond
        [(null? sorted-graph) '()]
        [else
         (define chosen-node (car sorted-graph))
         (define updated-graph (remove-vertex graph chosen-node))
         (define sub-assign (colour-graph updated-graph))
         (define conflicts (get-neighbors graph chosen-node))
         (define used-registers
           (map (lambda (conflict)
                  (if (aloc? conflict)
                      (let ([conflict-used-register-pair (assoc conflict sub-assign)])
                        (if conflict-used-register-pair
                            (first (cdr conflict-used-register-pair))
                            conflict))
                      conflict))
                conflicts))
         (define available-registers
           (filter (lambda (r) (not (member r used-registers))) registers))
         (define new-location
           (cond
             [(null? available-registers) '()]
             [else (car available-registers)]))
         (cond
           [(rloc? chosen-node) sub-assign]
           [(empty? new-location) (begin
                                    (set-box! spilled-variables (cons chosen-node spilled-variables))
                                    sub-assign)]
           [else (cons (list chosen-node new-location) sub-assign)])]))

    (append assignments (colour-graph graph^)))

  (match p
    [`(module ,info ,funcs ... ,tail)
     (set-box! spilled-variables '())
     (define assignments (graph-colouring-with-spilling (info-ref info 'assignment)
                                                        (info-ref info 'conflicts)
                                                        (reverse (current-assignable-registers))))
     (define updated-info (info-set info 'assignment assignments))
     (define updated-locals (remove* (map car (info-ref updated-info 'assignment)) (info-ref updated-info 'locals)))
     (set! updated-info (info-set updated-info 'locals updated-locals))
     `(module ,updated-info ,@(map assign-registers-func funcs) ,tail)]))


