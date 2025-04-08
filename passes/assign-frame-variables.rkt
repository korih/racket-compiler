#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v8)

(provide assign-frame-variables)

;; asm-pred-lang-v8/spilled -> asm-pred-lang-v8/assignments
;; compiles p to Asm-pred-lang-v8/assignments by allocating all abstract
;; locations in the locals set to free frame variables
(define/contract (assign-frame-variables p)
  (-> asm-pred-lang-v8/spilled? asm-pred-lang-v8/assignments?)

  ;; func is `(define ,label ,info ,tail)
  ;; interp. a function definition

  ;; (List-of aloc) (Graph-of loc) (List-of (list aloc rloc)) -> (List-of (list aloc rloc))
  ;; Recursively assigns frame variables to each aloc, avoiding conflicts
  (define (graph-colouring alocs conflicts-graph assignments)
    (cond
      [(empty? alocs) assignments]
      [else
       (define x (first alocs))
       (define conflict-list (get-neighbors conflicts-graph x))
       (define fvar-assignment
         (for/or ([i (in-naturals)])
           (define fv (make-fvar i))
           (if (and (not (member fv conflict-list))
                    (not (ormap (lambda (assignment) (symbol=? fv (cadr assignment)))
                                assignments)))
               fv
               #f)))
       (graph-colouring (rest alocs)
                        conflicts-graph
                        (cons (list x fvar-assignment) assignments))]))


  ;; asm-pred-lang-v8/spilled.info -> asm-pred-lang-v8/assignments.info
  (define (assign-call-variables-info info)
    (define conflicts-graph (info-ref info 'conflicts))
    (define locals (info-ref info 'locals))
    (define existing-assignments (info-ref info 'assignment))

    (define final-assignments
      (if (empty? locals)
          existing-assignments
          (graph-colouring locals conflicts-graph existing-assignments)))

    (info-set info 'assignment final-assignments))

  ;; func -> func
  (define (assign-call-fun f)
    (match f
      [`(define ,name ,info ,tail)
       `(define ,name ,(assign-call-variables-info info) ,tail)]))

  (match p
    [`(module ,info ,funs ... ,tail)
     `(module ,(assign-call-variables-info info) ,@(map assign-call-fun funs) ,tail)]))

