#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/2c-run-time
  cpsc411/langs/v4
  rackunit)

(provide expose-basic-blocks)

;; Exercise 10
;; nested-asm-lang-v4 -> block-pred-lang-v4
;; compiles p to Block-pred-lang v4 by eliminating all nested expressions by
;; generating fresh basic blocks and jumps
(define/contract (expose-basic-blocks p)
  (-> nested-asm-lang-v4? block-pred-lang-v4?)

  ;; blocks is (Box (List-of block-pred-lang-v4.b))
  ;; stores new block definitions created
  (define blocks (box '()))

  ;; block-pred-lang-v4.b ->
  ;; adds blk to blocks
  (define (add-block blk)
    (set-box! blocks (cons blk (unbox blocks))))

  ;; nested-asm-lang-v4-tail -> block-pred-lang-v4-tail
  ;; interp. converts the tail program into a list of basic blocks
  (define (expose-basic-blocks-tail tail)
    (match tail
      [`(halt ,triv) `(halt ,triv)]
      [`(begin ,fx ... ,tail)
       (define-values (bpl-fx bpl-tail) (for/foldr ([bpl-fx empty]
                                                    [bpl-tail (expose-basic-blocks-tail tail)])
                                          ([e fx])
                                          (expose-basic-blocks-effect e bpl-fx bpl-tail)))
       (if (empty? bpl-fx)
           bpl-tail
           `(begin ,@bpl-fx ,bpl-tail))]
      [`(if ,pred ,t-tail ,f-tail)
       (let* ([tail1-label (fresh-label)]
              [tail2-label (fresh-label)])
         (add-block `(define ,tail1-label ,(expose-basic-blocks-tail t-tail)))
         (add-block `(define ,tail2-label ,(expose-basic-blocks-tail f-tail)))
         ((expose-basic-blocks-pred pred) tail1-label tail2-label))]))

  ;; nested-asm-lang-v4-effect (list-of block-pred-lang-v4-effect) block-pred-lang-v4-tail -> block-pred-lang-v4-tail
  ;; interp. returns the current tail and the current list of effects for this block
  (define (expose-basic-blocks-effect e bpl-fx bpl-tail)
    (match e
      [`(set! ,loc (,binop ,loc ,triv))
       (values (cons `(set! ,loc (,binop ,loc ,triv)) bpl-fx) bpl-tail)]
      [`(set! ,loc ,triv)
       (values (cons `(set! ,loc ,triv) bpl-fx) bpl-tail)]
      [`(begin ,fx ...)
       (for/fold ([bpl-fx-acc bpl-fx] [bpl-tail-acc bpl-tail])
                 ([e fx])
         (expose-basic-blocks-effect e bpl-fx-acc bpl-tail-acc))]
      [`(if ,pred ,t-e ,f-e)
       (define merge-label (fresh-label))
       (define true-label (fresh-label))
       (define false-label (fresh-label))
       (define-values (bpl-t-e bpl-t-tail)
         (expose-basic-blocks-effect t-e
                                     empty
                                     `(jump ,merge-label)))
       (define-values (bpl-f-e bpl-f-tail)
         (expose-basic-blocks-effect f-e
                                     empty
                                     `(jump ,merge-label)))
       (add-block `(define ,true-label (begin ,@bpl-t-e ,bpl-t-tail)))
       (add-block `(define ,false-label (begin ,@bpl-f-e ,bpl-f-tail)))

       (define merge-tail (if (empty? bpl-fx)
                              bpl-tail
                              `(begin ,@bpl-fx ,bpl-tail)))
       (add-block `(define ,merge-label ,merge-tail))

       (define main-tail ((expose-basic-blocks-pred pred) true-label false-label))
       (values empty main-tail)]))

  ;; nested-asm-lang-v4.pred -> (block-pred-lang-v4.trg block-pred-lang-v4.trg -> block-pred-lang-v4.tail)
  (define (expose-basic-blocks-pred p)
    (match p
      ['(true)
       (lambda (t f) `(if ,p (jump ,t) (jump ,f)))]
      ['(false)
       (lambda (t f) `(if ,p (jump ,t) (jump ,f)))]
      [`(not ,pred)
       (lambda (t f) `(if (not ,pred) (jump ,t) (jump ,f)))]
      [`(begin ,e ... ,pred)
       (define pred-fn (expose-basic-blocks-pred pred))
       (lambda (t f)
         (define-values (fx tail)
           (for/foldr ([fx-acc empty]
                       [tail-acc (pred-fn t f)])
             ([e e])
             (expose-basic-blocks-effect e fx-acc tail-acc)))
         (if (empty? fx)
             tail
             `(begin ,@fx ,tail)))]
      [`(if ,pred1 ,pred2 ,pred3)
       (lambda (t f)
         (let* ([label2 (fresh-label)]
                [label3 (fresh-label)])
           (add-block `(define ,label3 ,((expose-basic-blocks-pred pred3) t f)))
           (add-block `(define ,label2 ,((expose-basic-blocks-pred pred2) t f)))
           ((expose-basic-blocks-pred pred1) label2 label3)))]
      [`(,relop ,loc ,triv)
       (lambda (t f) `(if (,relop ,loc ,triv) (jump ,t) (jump ,f)))]))

  (match p
    [`(module ,tail)
     (add-block `(define ,(fresh-label) ,(expose-basic-blocks-tail tail)))
     `(module ,@(unbox blocks))]))


(module+ test
  (check-equal? (expose-basic-blocks '(module (begin (halt rax))))
                '(module (define L.tmp.1 (halt rax))))
  (check-equal? (expose-basic-blocks '(module (begin (begin (set! rax 1)) (halt rax))))
                '(module (define L.tmp.2 (begin (set! rax 1) (halt rax)))))
  (check-equal? (expose-basic-blocks '(module (begin (begin (begin (begin (set! rax 1)))) (halt rax))))
                '(module (define L.tmp.3 (begin (set! rax 1) (halt rax)))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (if (true) (set! rax 5) (set! rax 6))
                                                (halt rax))))
                '(module
                     (define L.tmp.4 (if (true) (jump L.tmp.6) (jump L.tmp.7)))
                   (define L.tmp.5 (halt rax))
                   (define L.tmp.7 (begin (set! rax 6) (jump L.tmp.5)))
                   (define L.tmp.6 (begin (set! rax 5) (jump L.tmp.5)))))

  (check-equal? (expose-basic-blocks '(module (halt 5)))
                '(module (define L.tmp.8 (halt 5))))
  (check-equal? (expose-basic-blocks '(module (if (true) (halt 5) (halt 6))))
                '(module
                     (define L.tmp.9 (if (true) (jump L.tmp.10) (jump L.tmp.11)))
                   (define L.tmp.11 (halt 6))
                   (define L.tmp.10 (halt 5))))
  (check-equal? (expose-basic-blocks '(module (if (not (true)) (halt 5) (halt 6))))
                '(module
                     (define L.tmp.12 (if (not (true)) (jump L.tmp.13) (jump L.tmp.14)))
                   (define L.tmp.14 (halt 6))
                   (define L.tmp.13 (halt 5))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (set! rax 5)
                                                (set! rax (+ rax rax))
                                                (halt rax))))
                '(module
                     (define L.tmp.15 (begin (set! rax 5) (set! rax (+ rax rax)) (halt rax)))))
  (check-equal? (expose-basic-blocks '(module (if (if (true) (true) (false))
                                                  (halt 5)
                                                  (halt 6))))
                '(module
                     (define L.tmp.16 (if (true) (jump L.tmp.19) (jump L.tmp.20)))
                   (define L.tmp.19 (if (true) (jump L.tmp.17) (jump L.tmp.18)))
                   (define L.tmp.20 (if (false) (jump L.tmp.17) (jump L.tmp.18)))
                   (define L.tmp.18 (halt 6))
                   (define L.tmp.17 (halt 5))))
  (check-equal? (expose-basic-blocks '(module (if (begin
                                                    (set! rax 5)
                                                    (if (true) (true) (false)))
                                                  (halt 5)
                                                  (halt rax))))
                '(module
                     (define L.tmp.21
                       (begin (set! rax 5) (if (true) (jump L.tmp.24) (jump L.tmp.25))))
                   (define L.tmp.24 (if (true) (jump L.tmp.22) (jump L.tmp.23)))
                   (define L.tmp.25 (if (false) (jump L.tmp.22) (jump L.tmp.23)))
                   (define L.tmp.23 (halt rax))
                   (define L.tmp.22 (halt 5))))
  (check-equal? (expose-basic-blocks '(module (if (begin
                                                    (set! rax 5)
                                                    (if (true) (true) (false)))
                                                  (begin
                                                    (set! rax (+ rax rax))
                                                    (halt rax))
                                                  (begin
                                                    (set! rax (* rax rax))
                                                    (halt rax)))))
                '(module
                     (define L.tmp.26
                       (begin (set! rax 5) (if (true) (jump L.tmp.29) (jump L.tmp.30))))
                   (define L.tmp.29 (if (true) (jump L.tmp.27) (jump L.tmp.28)))
                   (define L.tmp.30 (if (false) (jump L.tmp.27) (jump L.tmp.28)))
                   (define L.tmp.28 (begin (set! rax (* rax rax)) (halt rax)))
                   (define L.tmp.27 (begin (set! rax (+ rax rax)) (halt rax)))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (set! rax 5)
                                                (set! rax (+ rax rax))
                                                (if (true) (halt rax) (halt 0)))))
                '(module
                     (define L.tmp.31
                       (begin
                         (set! rax 5)
                         (set! rax (+ rax rax))
                         (if (true) (jump L.tmp.32) (jump L.tmp.33))))
                   (define L.tmp.33 (halt 0))
                   (define L.tmp.32 (halt rax))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (set! rax 5)
                                                (set! rax (+ rax rax))
                                                (if (true)
                                                    (begin
                                                      (set! rax (+ rax 1))
                                                      (halt rax))
                                                    (begin
                                                      (set! rax (+ rax 2))
                                                      (halt rax))))))
                '(module
                     (define L.tmp.34
                       (begin
                         (set! rax 5)
                         (set! rax (+ rax rax))
                         (if (true) (jump L.tmp.35) (jump L.tmp.36))))
                   (define L.tmp.36 (begin (set! rax (+ rax 2)) (halt rax)))
                   (define L.tmp.35 (begin (set! rax (+ rax 1)) (halt rax)))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (set! rax 5)
                                                (set! rax (+ rax rax))
                                                (if (true)
                                                    (begin
                                                      (set! rax (+ rax 1))
                                                      (if (false) (halt 0) (halt rax)))
                                                    (begin
                                                      (set! rax (+ rax 2))
                                                      (if (true) (halt 0) (halt rax)))))))
                '(module
                     (define L.tmp.37
                       (begin
                         (set! rax 5)
                         (set! rax (+ rax rax))
                         (if (true) (jump L.tmp.38) (jump L.tmp.39))))
                   (define L.tmp.39
                     (begin (set! rax (+ rax 2)) (if (true) (jump L.tmp.42) (jump L.tmp.43))))
                   (define L.tmp.43 (halt rax))
                   (define L.tmp.42 (halt 0))
                   (define L.tmp.38
                     (begin (set! rax (+ rax 1)) (if (false) (jump L.tmp.40) (jump L.tmp.41))))
                   (define L.tmp.41 (halt rax))
                   (define L.tmp.40 (halt 0))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (if (true) (halt 1) (halt 2)))))
                '(module
                     (define L.tmp.44 (if (true) (jump L.tmp.45) (jump L.tmp.46)))
                   (define L.tmp.46 (halt 2))
                   (define L.tmp.45 (halt 1))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (if (not (true)) (halt 1) (halt 2)))))
                '(module
                     (define L.tmp.47 (if (not (true)) (jump L.tmp.48) (jump L.tmp.49)))
                   (define L.tmp.49 (halt 2))
                   (define L.tmp.48 (halt 1))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (if (if (not (true)) (true) (false))
                                                    (halt 1)
                                                    (halt 2)))))
                '(module
                     (define L.tmp.50 (if (not (true)) (jump L.tmp.53) (jump L.tmp.54)))
                   (define L.tmp.53 (if (true) (jump L.tmp.51) (jump L.tmp.52)))
                   (define L.tmp.54 (if (false) (jump L.tmp.51) (jump L.tmp.52)))
                   (define L.tmp.52 (halt 2))
                   (define L.tmp.51 (halt 1))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (if (if (true) (true) (false))
                                                    (halt 1)
                                                    (halt 2)))))
                '(module
                     (define L.tmp.55 (if (true) (jump L.tmp.58) (jump L.tmp.59)))
                   (define L.tmp.58 (if (true) (jump L.tmp.56) (jump L.tmp.57)))
                   (define L.tmp.59 (if (false) (jump L.tmp.56) (jump L.tmp.57)))
                   (define L.tmp.57 (halt 2))
                   (define L.tmp.56 (halt 1))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (if (true)
                                                    (set! rax 5)
                                                    (set! rax 2))
                                                (set! rax (+ rax rax))
                                                (set! rax (* rax 2))
                                                (if (false)
                                                    (set! rax (+ rax 1))
                                                    (set! rax (+ rax 2)))
                                                (halt rax))))
                '(module
                     (define L.tmp.60 (if (true) (jump L.tmp.65) (jump L.tmp.66)))
                   (define L.tmp.64
                     (begin
                       (set! rax (+ rax rax))
                       (set! rax (* rax 2))
                       (if (false) (jump L.tmp.62) (jump L.tmp.63))))
                   (define L.tmp.66 (begin (set! rax 2) (jump L.tmp.64)))
                   (define L.tmp.65 (begin (set! rax 5) (jump L.tmp.64)))
                   (define L.tmp.61 (halt rax))
                   (define L.tmp.63 (begin (set! rax (+ rax 2)) (jump L.tmp.61)))
                   (define L.tmp.62 (begin (set! rax (+ rax 1)) (jump L.tmp.61)))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (set! rax 10)
                                                (set! rax (+ rax rax))
                                                (if (> rax 10) (set! rax 2) (set! rax 5))
                                                (if (true) (halt rax) (halt 3)))))
                '(module
                     (define L.tmp.67
                       (begin
                         (set! rax 10)
                         (set! rax (+ rax rax))
                         (if (> rax 10) (jump L.tmp.71) (jump L.tmp.72))))
                   (define L.tmp.70 (if (true) (jump L.tmp.68) (jump L.tmp.69)))
                   (define L.tmp.72 (begin (set! rax 5) (jump L.tmp.70)))
                   (define L.tmp.71 (begin (set! rax 2) (jump L.tmp.70)))
                   (define L.tmp.69 (halt 3))
                   (define L.tmp.68 (halt rax)))))
