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

  ;; nested-asm-lang-v4.tail -> block-pred-lang-v4.tail
  (define (expose-basic-blocks-tail t)
    (match t
      [`(halt ,triv) t]
      [`(begin ,e ... ,tail)
       (define effects (for/fold ([fx-acc empty])
                         ([e e])
                                 (append fx-acc (expose-basic-blocks-effect e))))
       (define new-tail (expose-basic-blocks-tail tail))
       (if (empty? effects)
         new-tail
         `(begin ,@effects ,new-tail))]
      [`(if ,pred ,tail1 ,tail2)
       (let* ([tail1-label (fresh-label)]
              [tail2-label (fresh-label)])
         (add-block `(define ,tail2-label ,(expose-basic-blocks-tail tail2)))
         (add-block `(define ,tail1-label ,(expose-basic-blocks-tail tail1)))
         ((expose-basic-blocks-pred pred) tail1-label tail2-label))]))

  ;; nested-asm-lang-v4.effect -> (listof block-pred-lang-v4.effect)
  (define (expose-basic-blocks-effect e)
    (match e
      [`(set! ,loc ,triv) (list `(set! ,loc ,triv))]
      [`(set! ,loc (,binop ,loc ,triv)) (list `(set! ,loc (,binop ,loc ,triv)))]
      [`(begin ,e ...)
       (for/fold ([fx empty])
         ([e e])
                 (append fx (expose-basic-blocks-effect e)))]
      [`(if ,pred ,e1 ,e2)
       (let* ([merge-label (fresh-label)]
              [lab1 (fresh-label)]
              [lab2 (fresh-label)]
              [updated-e1 (expose-basic-blocks-effect e1)]
              [updated-e2 (expose-basic-blocks-effect e2)])
         (add-block `(define ,lab1 (begin ,updated-e1 (jump ,merge-label))))
         (add-block `(define ,lab2 (begin ,updated-e2 (jump ,merge-label))))
         ((expose-basic-blocks-pred pred) lab1 lab2))]))

  ;; nested-asm-lang-v4.pred -> (block-pred-lang-v4.trg block-pred-lang-v4.trg -> block-pred-lang-v4.tail)
  (define (expose-basic-blocks-pred p)
    (match p
      ['(true)
       (lambda (t f) `(if ,p (jump ,t) (jump ,f)))]
      ['(false)
       (lambda (t f) `(if ,p (jump ,t) (jump ,f)))]
      [`(not ,pred)
       (lambda (t f) ((expose-basic-blocks-pred pred) f t))]
      [`(begin ,e ... ,pred)
       (lambda (t f)
         `(begin ,@(map expose-basic-blocks-effect e)
                 ,((expose-basic-blocks-pred pred) t f)))]
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
                     (define L.__main.4 (if (true) (jump L.tmp.1) (jump L.tmp.2)))
                   (define L.tmp.1 (begin (set! rax 5) (jump L.tmp.3)))
                   (define L.tmp.2 (begin (set! rax 6) (jump L.tmp.3)))
                   (define L.tmp.3 (halt rax))))

  (check-equal? (expose-basic-blocks '(module (halt 5)))
                '(module (define L.tmp.1 (halt 5))))
  (check-equal? (expose-basic-blocks '(module (if (true) (halt 5) (halt 6))))
                '(module
                     (define L.tmp.2 (if (true) (jump L.tmp.3) (jump L.tmp.4)))
                   (define L.tmp.3 (halt 5))
                   (define L.tmp.4 (halt 6))))
  (check-equal? (expose-basic-blocks '(module (if (not (true)) (halt 5) (halt 6))))
                '(module
                     (define L.tmp.5 (if (true) (jump L.tmp.7) (jump L.tmp.6)))
                   (define L.tmp.6 (halt 5))
                   (define L.tmp.7 (halt 6))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (set! rax 5)
                                                (set! rax (+ rax rax))
                                                (halt rax))))
                '(module
                     (define L.tmp.8 (begin (set! rax 5) (set! rax (+ rax rax)) (halt rax)))))
  (check-equal? (expose-basic-blocks '(module (if (if (true) (true) (false))
                                                  (halt 5)
                                                  (halt 6))))
                '(module
                     (define L.tmp.9 (if (true) (jump L.tmp.12) (jump L.tmp.13)))
                   (define L.tmp.12 (if (true) (jump L.tmp.10) (jump L.tmp.11)))
                   (define L.tmp.13 (if (false) (jump L.tmp.10) (jump L.tmp.11)))
                   (define L.tmp.10 (halt 5))
                   (define L.tmp.11 (halt 6))))
  (check-equal? (expose-basic-blocks '(module (if (begin
                                                    (set! rax 5)
                                                    (if (true) (true) (false)))
                                                  (halt 5)
                                                  (halt rax))))
                '(module
                     (define L.tmp.14
                       (begin (set! rax 5) (if (true) (jump L.tmp.17) (jump L.tmp.18))))
                   (define L.tmp.17 (if (true) (jump L.tmp.15) (jump L.tmp.16)))
                   (define L.tmp.18 (if (false) (jump L.tmp.15) (jump L.tmp.16)))
                   (define L.tmp.15 (halt 5))
                   (define L.tmp.16 (halt rax))))
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
                     (define L.tmp.19
                       (begin (set! rax 5) (if (true) (jump L.tmp.22) (jump L.tmp.23))))
                   (define L.tmp.22 (if (true) (jump L.tmp.20) (jump L.tmp.21)))
                   (define L.tmp.23 (if (false) (jump L.tmp.20) (jump L.tmp.21)))
                   (define L.tmp.20 (begin (set! rax (+ rax rax)) (halt rax)))
                   (define L.tmp.21 (begin (set! rax (* rax rax)) (halt rax)))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (set! rax 5)
                                                (set! rax (+ rax rax))
                                                (if (true) (halt rax) (halt 0)))))
                '(module
                     (define L.tmp.24
                       (begin
                         (set! rax 5)
                         (set! rax (+ rax rax))
                         (if (true) (jump L.tmp.25) (jump L.tmp.26))))
                   (define L.tmp.25 (halt rax))
                   (define L.tmp.26 (halt 0))))
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
                     (define L.tmp.27
                       (begin
                         (set! rax 5)
                         (set! rax (+ rax rax))
                         (if (true) (jump L.tmp.28) (jump L.tmp.29))))
                   (define L.tmp.28 (begin (set! rax (+ rax 1)) (halt rax)))
                   (define L.tmp.29 (begin (set! rax (+ rax 2)) (halt rax)))))
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
                     (define L.tmp.30
                       (begin
                         (set! rax 5)
                         (set! rax (+ rax rax))
                         (if (true) (jump L.tmp.31) (jump L.tmp.32))))
                   (define L.tmp.31
                     (begin (set! rax (+ rax 1)) (if (false) (jump L.tmp.35) (jump L.tmp.36))))
                   (define L.tmp.35 (halt 0))
                   (define L.tmp.36 (halt rax))
                   (define L.tmp.32
                     (begin (set! rax (+ rax 2)) (if (true) (jump L.tmp.33) (jump L.tmp.34))))
                   (define L.tmp.33 (halt 0))
                   (define L.tmp.34 (halt rax))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (if (true) (halt 1) (halt 2)))))
                '(module
                     (define L.tmp.37 (begin (if (true) (jump L.tmp.38) (jump L.tmp.39))))
                   (define L.tmp.38 (halt 1))
                   (define L.tmp.39 (halt 2))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (if (not (true)) (halt 1) (halt 2)))))
                '(module
                     (define L.tmp.40 (begin (if (true) (jump L.tmp.42) (jump L.tmp.41))))
                   (define L.tmp.41 (halt 1))
                   (define L.tmp.42 (halt 2))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (if (if (not (true)) (true) (false))
                                                    (halt 1)
                                                    (halt 2)))))
                '(module
                     (define L.tmp.43 (begin (if (true) (jump L.tmp.47) (jump L.tmp.46))))
                   (define L.tmp.46 (if (true) (jump L.tmp.44) (jump L.tmp.45)))
                   (define L.tmp.47 (if (false) (jump L.tmp.44) (jump L.tmp.45)))
                   (define L.tmp.44 (halt 1))
                   (define L.tmp.45 (halt 2))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (if (if (true) (true) (false))
                                                    (halt 1)
                                                    (halt 2)))))
                '(module
                     (define L.tmp.48 (begin (if (true) (jump L.tmp.51) (jump L.tmp.52))))
                   (define L.tmp.51 (if (true) (jump L.tmp.49) (jump L.tmp.50)))
                   (define L.tmp.52 (if (false) (jump L.tmp.49) (jump L.tmp.50)))
                   (define L.tmp.49 (halt 1))
                   (define L.tmp.50 (halt 2))))
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
                     (define L.__main.7 (if (true) (jump L.tmp.4) (jump L.tmp.5)))
                   (define L.tmp.4 (begin (set! rax 5) (jump L.tmp.6)))
                   (define L.tmp.5 (begin (set! rax 2) (jump L.tmp.6)))
                   (define L.tmp.6
                     (begin
                       (set! rax (+ rax rax))
                       (set! rax (* rax 2))
                       (if (false) (jump L.tmp.1) (jump L.tmp.2))))
                   (define L.tmp.1 (begin (set! rax (+ rax 1)) (jump L.tmp.3)))
                   (define L.tmp.2 (begin (set! rax (+ rax 2)) (jump L.tmp.3)))
                   (define L.tmp.3 (halt rax))))
  (check-equal? (expose-basic-blocks '(module (begin
                                                (set! rax 10)
                                                (set! rax (+ rax rax))
                                                (if (> rax 10) (set! rax 2) (set! rax 5))
                                                (if (true) (halt rax) (halt 3)))))
                '(module
                     (define L.tmp.2
                       (begin
                         (set! rax 10)
                         (set! rax (+ rax rax))
                         (if (> rax 10) (jump L.tmp.3) (jump L.tmp.4))))
                   (define L.tmp.3 (halt rax))
                   (define L.tmp.4 (halt 3))
                   (define L.tmp.5 (begin (set! rax 2) (jump L.tmp.5)))
                   (define L.tmp.6 (begin (set! rax 5) (jump L.tmp.5)))
                   (define L.tmp.7 (if (true) (jump L.__nested.1) (jump L.__nested.2))))))
