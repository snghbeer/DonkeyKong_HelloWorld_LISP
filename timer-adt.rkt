#lang racket

(#%require "teken-adt.rkt")
(#%require "startposities-config.rkt")
(#%provide maak-timer-adt)

(define (maak-timer-adt)
  (let* ((tijd 120)
         (tijd-in-string (number->string tijd)))

    (define (teken! teken-adt)
      ((teken-adt 'schrijf-tijd!) tijd-in-string x-tijd y-tijd))

    (define (verlaag-tijd! teken-adt)
      (set! tijd (- tijd 1))
      (set! tijd-in-string (number->string tijd))
      (teken! teken-adt))

    (define (initialiseer! teken-adt)
      (set! tijd 120)
      (set! tijd-in-string (number->string tijd))
      (teken! teken-adt))

    (define (timer-dispatch m)
      (cond ((eq? m 'teken!) teken!)
            ((eq? m 'verlaag-tijd!) verlaag-tijd!)
            ((eq? m 'tijd) tijd)
            ((eq? m 'initialiseer!) initialiseer!)))
    timer-dispatch))