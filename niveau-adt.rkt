#lang racket

(#%require "teken-adt.rkt")
(#%provide maak-niveau-adt)

(define (maak-niveau-adt)
  (let* ((current-niveau 1)
         (current-niveau-string (number->string current-niveau)))

    (define (teken! teken-adt)
      ((teken-adt 'schrijf-niveau!) current-niveau-string 420 10))

    (define (verhoog-niveau! teken-adt)
      (when (< current-niveau 2)
        (set! current-niveau (+ current-niveau 1))
        (set! current-niveau-string (number->string current-niveau))
        (teken! teken-adt)))

    (define (reset-niveau! teken-adt)
      (set! current-niveau 1)
      (set! current-niveau-string (number->string current-niveau))
      (teken! teken-adt))

    (define (niveau-dispatch m)
      (cond ((eq? m 'teken!) teken!)
            ((eq? m 'verhoog!) verhoog-niveau!)
            ((eq? m 'reset) reset-niveau!)
            ((eq? m 'current-niveau) current-niveau)))
    niveau-dispatch))