#lang racket

(#%provide maak-leven-adt)
(#%require "startposities-config.rkt")
(#%require "teken-adt.rkt")

(define (maak-leven-adt teken-adt)
  (let* ((leven-counter 3)
         (leven-string (number->string leven-counter)))

    (define (teken-hart!)
      ((teken-adt 'teken-leven!) hart-x hart-y))
    
    (define (teken-aantal-leven!)
     ((teken-adt 'set-leven!) leven-string hart-text-x hart-text-y))

    (define (decrementeer-leven!)
      (when (not (= leven-counter 0))
        (set! leven-counter (- leven-counter 1))
        (set! leven-string (number->string leven-counter))
        (teken-aantal-leven!)))

    (define (initialiseer-leven!)
      (set! leven-counter 3)
      (set! leven-string (number->string leven-counter))
      (teken-aantal-leven!))
    
    (define (game-over?)
      (= leven-counter 0))

    (define (dispatch m)
      (cond ((eq? m 'game-over?) (game-over?))
            ((eq? m 'decrementeer) (decrementeer-leven!))
            ((eq? m 'teken-aantal-leven!) (teken-aantal-leven!))
            ((eq? m 'teken-hart!) (teken-hart!))
            ((eq? m 'initialiseer-leven!) (initialiseer-leven!))))
    dispatch))
