#lang racket

(#%require "teken-adt.rkt")
(#%require "startposities-config.rkt")
(#%provide maak-pauline-adt)

(define (maak-pauline-adt)
  (let* ((x-pos 485)
         (y-pos 18)
         (width 40)
         (height 70)
         (x-radius (+ x-pos width))
         (y-radius (+ y-pos height))
         (x-plat x-pauline-floor)
         (y-plat y-pauline-floor)
         (plat-rx (+ x-plat pauline-floor-w))
         (plat-ry (+ y-plat pauline-floor-h))
         (op-platform #t))

    (define (teken! teken-adt)
      ((teken-adt 'pauline) x-pos y-pos))

    (define (teken-platform! teken-adt)
      ((teken-adt 'teken-pauline-platform) x-plat y-plat))
    
    (define (op-platform? obj-x obj-y obj-rx obj-ry)
      (and  (<= y-plat obj-ry plat-ry)
            (or (<= x-plat obj-x plat-rx)
                (<= x-plat obj-rx plat-rx))))

    (define (dispatch m)
      (cond ((eq? m 'teken!) teken!)
            ((eq? m 'teken-platform!) teken-platform!)
            ((eq? m 'op-platform?) op-platform?)
            ((eq? m 'x) x-pos)
            ((eq? m 'y) y-pos)
            ((eq? m 'rx) x-radius)
            ((eq? m 'ry) y-radius)))
    dispatch))