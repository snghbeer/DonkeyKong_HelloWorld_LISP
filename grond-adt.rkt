#lang racket

(#%require "teken-adt.rkt")
(#%provide maak-grond-adt)


(define grond-w 900)
(define grond-h 17)

(define (maak-grond-adt x y)
  (let* ((pos-x x)
         (pos-y y)
         (radius-x (+ pos-x grond-w))
         (radius-y (+ pos-y grond-h)))

    (define (teken-grond! teken-adt)
      ((teken-adt 'teken-grond!) pos-x pos-y))

    (define (op-platform? obj-x obj-y obj-rx obj-ry)
      (and  (<= pos-y obj-ry radius-y)
            (or (<= pos-x obj-x radius-x)
                (<= pos-x obj-rx radius-x))))

    (define (grond-dispatch m)
      (cond ((eq? m 'x) pos-x)
            ((eq? m 'y) pos-y)
            ((eq? m 'rx) radius-x)
            ((eq? m 'ry) radius-y)
            ((eq? m 'op-platform?) op-platform?)
            ((eq? m 'teken-grond!) teken-grond!)))
    grond-dispatch))