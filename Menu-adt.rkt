#lang racket

(#%provide maak-menu-adt)
(#%require "teken-adt.rkt")

;;Menu-adt
(define (maak-menu-adt teken-adt)
  (let ((pos-x -15)
        (pos-y 0)
        (getekend #f))

    (define (teken! teken-adt)
      (when (eq? getekend #f)
        ((teken-adt 'menu) pos-x pos-y)
        (set! getekend #t)))

    (define (verwijder! teken-adt)
      (when (eq? getekend #t)
        (teken-adt 'verwijder-menu!)
        (set! getekend #f)))

    (define (menu-dispatch m)
      (cond ((eq? m 'verwijder!) verwijder!)
            ((eq? m 'teken!) teken!)))
    menu-dispatch))