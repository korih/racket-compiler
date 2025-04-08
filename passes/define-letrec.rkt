#lang racket

(require
  cpsc411/langs/v9)

(provide define->letrec)

;; exprs-unsafe-lang-v9 -> just-exprs-lang-v9
;; compiles p to Just-exprs-lang v9 by  transforming all top-level bindings into
;; local bindings
(define/contract (define->letrec p)
  (-> exprs-unsafe-lang-v9? just-exprs-lang-v9?)

  ;; func is `(define ,label (lambda (,alocs ...) ,value))
  ;; interp. a function definition
  ;;
  ;; exprs-unsafe-lang-v9 -> just-exprs-lang-v9
  ;; destructure the funciton definition to for letrec bindings
  (define (transform-funcs->let f)
    (match f
      [`(define ,label (lambda (,alocs ...) ,value))
       `(,label (lambda ,alocs ,value))]))

  ;; exprs-unsafe-lang-v9 -> just-exprs-lang-v9
  ;; transform the function definitions into letrec bindings in value
  ;; if there are no funciton definitions don't do a letrec
  (define (transform-funcs->letrec funcs v)
    (cond
      [(empty? funcs) v]
      [else
       `(letrec ,funcs ,v)]))

  (match p
    [`(module ,funcs ... ,value)
     (define funcs^ (for/fold ([acc '()])
                              ([f funcs])
                      (cons (transform-funcs->let f) acc)))
     `(module ,(transform-funcs->letrec (reverse funcs^) value))]))

