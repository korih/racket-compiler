#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4
  rackunit)

(provide flatten-begins)

;; block-asm-lang-v4 -> para-asm-lang-v4
;; interp. flatten begin statements in the program
(define/contract (flatten-begins p)
  (-> block-asm-lang-v4? para-asm-lang-v4?)

  ;; interp. flatten begin statements in the program into a list of effect
  ;; statements
  (define (flatten-begins/effect e)
    (match e
      [`(set! ,x (,binop ,x ,v)) (list `(set! ,x (,binop ,x ,v)))]
      [`(set! ,x ,v) (list `(set! ,x ,v))]
      [`(begin ,fx ... ,e)
       (define compiled-fx (for/foldr ([fx-acc empty])
                             ([e fx])
                             (append (flatten-begins/effect e) fx-acc)))
       (append compiled-fx (flatten-begins/effect e))]))

  (match p
    [`(halt ,triv) `(begin (halt ,triv))]
    [`(begin ,fx ... ,tail)
     (define compiled-fx (for/foldr ([fx-acc empty])
                           ([e fx])
                           (append (flatten-begins/effect e) fx-acc)))
     (make-begin compiled-fx (flatten-begins tail))]))

(test-case
 "flatten-begins"
 (let ([x (fresh-label)])
     (check-equal? (flatten-begins `(module (define ,x (halt 1)))) '(begin (halt 1))))
 (let ([x (fresh-label)])
   (check-equal? (flatten-begins `(module (define ,x (begin (set! rbx 1) (halt rbx))))
                                '(begin (with-label ,x (set! rbx 1)) (halt rbx)))))
 (let ([x (fresh-label)])
   (check-equal? (flatten-begins `(module (define ,x (begin (set! rax 0) (begin (set! rbx 1) (halt rbx))))))
                 `(begin (with-label ,x (set! rax 0)) (set! rbx 1) (halt rbx))))
 (let ([x (fresh-label)])
   (check-equal? (flatten-begins `(module (define ,x (begin (begin (set! rax 0)) (halt rax))))
                                 `(begin (with-label ,x (set! rax 0)) (halt rax)))))
 (let ([x (fresh-label)]
       [y (fresh-label)])
   (check-equal? (flatten-begins `(module (define ,x (jump ,y)) (define ,y (halt 0))))
                 `(begin (with-label ,x (jump ,y)) (with-label ,y (halt 0)))))
 (let ([x (fresh-label)]
       [y (fresh-label)]
       [z (fresh-label)])
   (check-equal? (flatten-begins `(module (define ,x (if (< r1 0)) (jump ,y) (jump ,z))
                                    (define ,y (halt r0))
                                    (define ,z (set! r1 (+ r1 -1)) (jump ,x))))
                 `(begin (with-label ,x (compare r1 0) (jump-if < ,y))
                    (with-label ,y (halt r0))
                    (with-label ,z (set! r1 (+ r1 -1)) (jump ,x)))))
 (let ([x (fresh-label)]
       [y (fresh-label)]
       [z (fresh-label)])
   (check-equal? (flatten-begins `(module (define ,x (set! r1 0) (jump ,y))
                                    (define ,y (set! r2 0) (jump ,z))
                                    (define ,z (halt r0)))
                 `(begin (with-label ,x (set! r1 0) (jump ,y))
                         (with-label ,y (set! r2 0) (jump ,z))
                         (with-label ,z (halt r0)))))))
