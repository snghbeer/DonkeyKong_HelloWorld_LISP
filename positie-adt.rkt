#lang racket

(#%require "Graphics.rkt")
(#%provide maak-positie-adt)

(define (maak-positie-adt x y)

    (define (set-positie! tile x y)
      ((tile 'set-x!) x)
      ((tile 'set-y!) y))

    (define (dispatch m)
      (cond ((eq? m 'set-pos!) set-positie!)))
    dispatch)