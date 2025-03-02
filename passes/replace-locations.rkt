#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5
  rackunit)

(provide replace-locations)

;; asm-lang-v5/assignments -> nested-asm-lang-v5
;; interp. replaces the abstract locations with the concrete locations
(define (replace-locations p)
  (-> asm-pred-lang-v5/assignments? nested-asm-lang-v5?)

  ;; asm-lang-v5/assignments.tail (dictof asm-lang-v5/assignments.aloc asm-lang-v5/assignments.rloc)
  ;; -> nested-asm-lang-v5.tail
  (define (replace-locations-tail t assignments)
    (match t
      [`(halt ,triv)
       `(halt ,(replace-locations-triv triv assignments))]
      [`(begin ,fx ... ,tail)
       (define compiled-fx (for/list ([e fx])
                             (replace-locations-effect e assignments)))
       (define compiled-tail (replace-locations-tail tail assignments))
       `(begin ,@compiled-fx ,compiled-tail)]
      [`(if ,pred ,t1 ,t2)
       (define pred^ (replace-locations-pred pred assignments))
       (define t1^ (replace-locations-tail t1 assignments))
       (define t2^ (replace-locations-tail t2 assignments))
       `(if ,pred^ ,t1^ ,t2^)]
      [`(jump ,trg ,loc ...)
       `(jump ,(replace-locations-trg trg assignments)
              ,@(for/list ([l loc]) (replace-locations-triv l assignments)))]))

  ;; asm-pred-lang-v5/assignments.trg (dictof asm-lang-v5/assignments.aloc asm-lang-v5/assignments.rloc)
  ;; -> nested-asm-lang-v5.trg
  ;; interp. replaces abstract location if trg is a label
  (define (replace-locations-trg trg assignments)
    (match trg
      [l #:when (label? l) l]
      [loc (replace-locations-loc loc assignments)]))

  ;; asm-pred-lang-v5/assignments.log (dictof asm-lang-v5/assignments.aloc asm-lang-v5/assignments.rloc)
  ;; -> nested-asm-lang-v5.log
  (define (replace-locations-loc loc assignments)
    (match loc
      [a #:when (aloc? a) (dict-ref assignments a)]
      [loc loc]))

  ;; asm-lang-v5/assignments.effect (dictof asm-lang-v5/assignments.aloc asm-lang-v5/assignments.rloc)
  ;; -> nested-asm-lang-v5.effect
  (define (replace-locations-effect e assignments)
    (match e
      [`(set! ,x (,binop ,x ,v))
       (define reg (dict-ref assignments x))
       `(set! ,reg (,binop ,reg ,(replace-locations-triv v assignments)))]
      [`(set! ,x ,v)
       `(set! ,(dict-ref assignments x) ,(replace-locations-triv v assignments))]
      [`(begin ,fx ... ,e)
       (define compiled-fx (for/list ([e fx]) (replace-locations-effect e assignments)))
       (define compiled-e (replace-locations-effect e assignments))
       `(begin ,@compiled-fx ,compiled-e)]
      [`(if ,pred ,e1 ,e2)
       (define pred^ (replace-locations-pred pred assignments))
       (define e1^ (replace-locations-effect e1 assignments))
       (define e2^ (replace-locations-effect e2 assignments))
       `(if ,pred^ ,e1^ ,e2^)]))

  ;; asm-lang-v5/assignments.pred (dictof asm-lang-v5/assignments.aloc asm-lang-v5/assignments.rloc)
  ;; -> nested-asm-lang-v5.pred
  (define (replace-locations-pred p assignments)
    (match p
      [`(true) `(true)]
      [`(false) `(false)]
      [`(begin ,effects ... ,pred)
       (define effects^
         (for/list ([effect effects])
           (replace-locations-effect effect assignments)))
       (define pred^ (replace-locations-pred pred assignments))
       `(begin ,@effects^ ,pred^)]
      [`(not ,pred) `(not ,(replace-locations-pred pred assignments))]
      [`(,relop ,aloc ,triv)
       (define reg (dict-ref assignments aloc))
       `(,relop ,reg ,triv)]
      [`(if ,p1 ,p2 ,p3)
       (define p1^ (replace-locations-pred p1 assignments))
       (define p2^ (replace-locations-pred p2 assignments))
       (define p3^ (replace-locations-pred p3 assignments))
       `(if ,p1^ ,p2^ ,p3^)]))

  ;; asm-lang-v5/assignments.triv (dictof asm-lang-v5/assignments.aloc asm-lang-v5/assignments.rloc)
  ;; -> nested-asm-lang-v5.effect
  (define (replace-locations-triv t assignments)
    (match t
      [`,x #:when (aloc? x) (dict-ref assignments x)]
      [`,x x]))

  ;; asm-lang-v5/assignments -> (dictof asm-lang-v5/assignments.aloc asm-lang-v5/assignments.rloc)
  ;; interp. creates a dictionary of assignments
  (define (make-assignments-dict assignments)
    (for/fold ([acc (hash)])
              ([pair assignments])
      (dict-set acc (first pair) (second pair))))

  ;; asm-pred-lang-v5/assignments.label asm-pred-lang-v5/assignments.info asm-pred-lang-v5/assignments.tail ->
  ;; partial nested-asm-lang-v5/assignments.p
  ;; interp. replaces abstract locations within the scope of the label
  (define (replace-locations-func label info tail)
    (define assignments (make-assignments-dict (info-ref info 'assignment)))
    `(define ,label ,(replace-locations-tail tail assignments)))

  (match p
    [`(module ,info (define ,labels ,infos ,tails) ... ,tail)
     `(module ,@(for/list ([label labels] [info infos] [tail tails])
                  (replace-locations-func label info tail))
        ,(replace-locations-tail tail (make-assignments-dict (info-ref info 'assignment))))]))

(module+ test
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (begin (set! x.1 1) (if (> x.1 0) (set! x.1 2) (set! x.1 3)) (halt x.1))))
                '(module
                     (begin (set! rax 1) (if (> rax 0) (set! rax 2) (set! rax 3)) (halt rax))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (if (true) (true) (false)) (halt 1) (halt 0))))
                '(module (if (if (true) (true) (false)) (halt 1) (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (begin (set! x.1 1) (begin (set! x.1 2) (set! x.1 3)) (> x.1 0)) (halt 1) (halt 0))))
                '(module
                     (if (begin (set! rax 1) (begin (set! rax 2) (set! rax 3)) (> rax 0))
                         (halt 1)
                         (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (begin (set! x.1 1) (set! x.1 1) (> x.1 0)) (halt 1) (halt 0))))
                '(module (if (begin (set! rax 1) (set! rax 1) (> rax 0)) (halt 1) (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (begin (set! x.1 1) (> x.1 0)) (halt 1) (halt 0))))
                '(module (if (begin (set! rax 1) (> rax 0)) (halt 1) (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (not (> x.1 1)) (halt 1) (halt 0))))
                '(module (if (not (> rax 1)) (halt 1) (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (true) (halt 1) (halt 0))))
                '(module (if (true) (halt 1) (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (if (> x.1 1) (halt 1) (halt 0))))
                '(module (if (> rax 1) (halt 1) (halt 0))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (begin (if (true)
                                                 (begin
                                                   (set! x.1 (+ x.1 1))
                                                   (if (> x.1 0) (set! x.1 1) (set! x.1 0))
                                                   (halt x.1))
                                                 (begin
                                                   (set! x.1 (+ x.1 2))
                                                   (if (if (true) (false) (false))
                                                       (set! x.1 2)
                                                       (set! x.1 3))
                                                   (halt x.1))))))
                '(module
                     (begin
                       (if (true)
                           (begin
                             (set! rax (+ rax 1))
                             (if (> rax 0) (set! rax 1) (set! rax 0))
                             (halt rax))
                           (begin
                             (set! rax (+ rax 2))
                             (if (if (true) (false) (false)) (set! rax 2) (set! rax 3))
                             (halt rax))))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax))))
                                      (begin (set! x.1 (+ x.1 1)) (halt x.1))))
                '(module (begin (set! rax (+ rax 1)) (halt rax))))
  (check-equal? (replace-locations '(module ((locals (x.1)) (assignment ((x.1 rax)))) (begin (set! x.1 0) (halt x.1))))
                '(module (begin (set! rax 0) (halt rax))))
  (check-equal? (replace-locations '(module ((locals (x.1 y.1 w.1)) (assignment ((x.1 rax) (y.1 rbx) (w.1 r9))))
                                      (begin (set! x.1 0)
                                             (set! y.1 x.1)
                                             (set! w.1 1)
                                             (set! w.1 (+ w.1 y.1))
                                             (halt w.1))))
                '(module (begin (set! rax 0) (set! rbx rax) (set! r9 1) (set! r9 (+ r9 rbx)) (halt r9))))

  (define label2 (fresh-label))
  (check-equal? (replace-locations `(module ((locals (x.1)) (assignment ((x.1 r8))))
                                      (define ,label2
                                        ((locals (x.3)) (assignment ((x.3 r9))))
                                        (begin (set! x.3 (+ x.3 1)) (halt x.3)))
                                      (begin (set! x.1 0) (jump ,label2 x.1))))
                `(module
  (define ,label2 (begin (set! r9 (+ r9 1)) (halt r9)))
  (begin (set! r8 0) (jump ,label2 r8))))

  (define label1 (fresh-label))  
  (define label3 (fresh-label))
  (check-equal? (replace-locations `(module ((locals (x.2 x.3)) (assignment ((x.2 r7) (x.3 r8))))
                                      (define ,label1
                                        ((locals (x.2)) (assignment ((x.2 r7))))
                                        (begin (set! x.2 0)
                                          (set! x.2 (* x.2 2))
                                          (halt x.2)))
                                      (define ,label3
                                        ((locals (x.3)) (assignment ((x.3 r8))))
                                        (begin 
                                          (set! x.3 -1)
                                          (set! x.3 (+ x.3 2))
                                          (halt x.3)))
                                      (begin (set! x.2 1) (jump ,label1 x.2))))
                `(module (define ,label1 (begin (set! r7 0)
                                           (set! r7 (* r7 2))
                                           (halt r7)))
                   (define ,label3 (begin (set! r8 -1)
                                     (set! r8 (+ r8 2))
                                     (halt r8)))
                   (begin (set! r7 1) (jump ,label1 r7)))))
