#lang racket

(require
  cpsc411/langs/v9)

(provide define->letrec)

;; exprs-unsafe-lang-v9 -> just-exprs-lang-v9
;; compiles p to Just-exprs-lang v9 by transforming all top-level bindings into
;; local bindings
(define/contract (define->letrec p)
  (-> exprs-unsafe-lang-v9? just-exprs-lang-v9?)

  ;; func is `(define ,label (lambda (,alocs ...) ,value))
  ;; interp. a function definition
  
  ;; exprs-unsafe-lang-v9 -> just-exprs-lang-v9
  ;; interp. transforms a top-level function definition into a letrec binding
  (define (func->letrec-binding f)
    (match f
      [`(define ,label (lambda (,alocs ...) ,value))
       `(,label (lambda ,alocs ,value))]))

  (match p
    [`(module ,funcs ... ,value)
     (define bindings (map func->letrec-binding funcs))
     `(module ,(if (empty? bindings)
                   value
                   `(letrec ,bindings ,value)))]))

