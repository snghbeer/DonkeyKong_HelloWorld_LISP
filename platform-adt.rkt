#lang racket

(#%require "teken-adt.rkt")
(#%provide maak-platform-adt platform-w platform-h)


(define platform-w 524)
(define platform-h 17)

(define (maak-platform-adt x y)
  (let* ((pos-x x)
         (pos-y y)
         (radius-x (+ pos-x platform-w))
         (radius-y (+ pos-y platform-h)))

    (define (teken-platform! teken-adt)
      ((teken-adt 'teken-platform!) pos-x pos-y))

    (define (op-platform? obj-x obj-y obj-rx obj-ry)
      (and  (<= pos-y obj-ry radius-y)
            (or (<= pos-x obj-x radius-x)
                (<= pos-x obj-rx radius-x))))

    (define (dispatch m)
      (cond ((eq? m 'x) pos-x)
            ((eq? m 'y) pos-y)
            ((eq? m 'rx) radius-x)
            ((eq? m 'ry) radius-y)
            ((eq? m 'op-platform?) op-platform?)
            ((eq? m 'teken-platform!) teken-platform!)))
    dispatch))
