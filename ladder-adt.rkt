#lang racket

(#%require "teken-adt.rkt")
(#%provide maak-ladder-adt)

(define w-ladder1 30)
(define h-ladder1 60)

(define (maak-ladder-adt x y)
  (let* ((pos-x x)
         (pos-y y)
         (width w-ladder1)
         (height h-ladder1)
         (x-radius (+ pos-x width))
         (y-radius (+ pos-y height)))

    (define (teken-ladder! teken-adt)
      ((teken-adt 'ladder!) pos-x pos-y))

    (define (kan-klimmen? obj-x obj-radius-x obj-y obj-radius-y)
      (and (or (<= pos-x obj-x x-radius)
               (<= pos-x obj-radius-x x-radius))
           (or (<= pos-y obj-y y-radius)
               (<= pos-y obj-radius-y y-radius))))

    (define (dispatch m)
      (cond ((eq? m 'x) pos-x)
            ((eq? m 'y) pos-y)
            ((eq? m 'r-x) x-radius)
            ((eq? m 'r-y) y-radius)
            ((eq? m 'teken-ladder!) teken-ladder!)
            ((eq? m 'kan-klimmen?) kan-klimmen?)))
    dispatch))