#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v8)

(provide assign-call-undead-variables)

;; asm-lang-v8/conflicts -> asm-lang-v8/pre-framed
;; Compiles Asm-pred-lang-v8/conflicts to Asm-pred-lang-v8/pre-framed by
;; pre-assigning all variables in the call-undead sets to frame variables
(define/contract (assign-call-undead-variables p)
  (-> asm-pred-lang-v8/conflicts? asm-pred-lang-v8/pre-framed?)

  ;; func is `(define ,label ,info ,tail)
  ;; interp. a function definition

  ;; (List-of loc) (Graph-of loc) (List-of (list aloc fvar)) -> (List-of (list aloc fvar))
  ;; interp. recursively assigns each variable in call-undead-set to the first
  ;; compatible frame variable without conflicts
  (define (graph-colouring call-undead-set conflicts-graph assignments)
    (cond
      [(empty? call-undead-set) assignments]
      [else
       (let* ([x (car call-undead-set)]
              [rest (cdr call-undead-set)]
              [assignments^ (graph-colouring rest conflicts-graph assignments)]
              ;; Collect vars assigned to each frame variable
              [frame-assignments (foldl (lambda (pair acc)
                                          (let* ([var (car pair)]
                                                 [fv (cadr pair)]
                                                 [existing (assoc fv acc)])
                                            (if existing
                                                (cons (list fv (cons var (cadr existing))) (remove existing acc))
                                                (cons (list fv (list var)) acc))))
                                        '()
                                        assignments^)]
              [conflict-vars (get-neighbors conflicts-graph x)])

         ;; Recursively find the first valid frame slot
         (define (find-fvar i)
           (let* ([candidate-fv (make-fvar i)]
                  [assigned-vars (let ([entry (assoc candidate-fv frame-assignments)])
                                   (if entry (cadr entry) '()))]
                  [incompatible?
                   (or (member candidate-fv (get-neighbors conflicts-graph x))
                       (ormap (lambda (y)
                                (or (member y (get-neighbors conflicts-graph x))
                                    (member x (get-neighbors conflicts-graph y))))
                              assigned-vars))])
             (if incompatible?
                 (find-fvar (add1 i))
                 candidate-fv)))

         ;; Assign x to the first safe frame var
         (cons (list x (find-fvar 0)) assignments^))]))

  ;; asm-lang-v8/conflicts.info -> asm-lang-v8/pre-framed.info
  (define (assign-call-undead-variables-info info)
    (define assignments (graph-colouring (info-ref info 'call-undead) (info-ref info 'conflicts) '()))
    (define locals
      (let ([local-variables (reverse (info-ref info 'locals))])
        (remove* (map car assignments) local-variables)))
    (info-set (info-set info 'locals locals) 'assignment assignments))

  ;; func -> func
  (define (assign-call-undead-variables-func func)
    (match func
      [`(define ,label ,info ,tail)
       `(define ,label ,(assign-call-undead-variables-info info) ,tail)]))

  (match p
    [`(module ,info ,funcs ... ,tail)
     `(module ,(assign-call-undead-variables-info info) ,@(map assign-call-undead-variables-func funcs) ,tail)]))

