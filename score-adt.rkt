#lang racket

(#%require "teken-adt.rkt")
(#%provide maak-score-adt)

(define (maak-score-adt)
  (let* ((score 0)
         (score-string (number->string score))
         (beste-score 0)
         (beste-score-string (number->string beste-score)))

;    (define (sla-beste-score-op! score)
;      (define data-geheugen (read (open-input-file "highscores.txt")))
;      (define een-nieuwe-score data-geheugen)
;      (define naar-schrijven (open-output-file "highscores.txt" #:exists 'update))
;      (write score naar-schrijven)
;      (close-output-port naar-schrijven)
;      een-nieuwe-score)

    (define (teken-score! teken-adt)
      ((teken-adt 'schrijf-scores!) score-string beste-score-string 15))

    (define (nieuwe-beste-score teken-adt)
      (when (> score beste-score)
        (set! beste-score score)
        (set! beste-score-string (number->string beste-score))
        (teken-score! teken-adt)))

    (define (tel-score! teken-adt)
      (set! score (+ score 100))
      (set! score-string (number->string score))
      (teken-score! teken-adt))

    (define (reset-score teken-adt)
      (set! score 0)
      (set! score-string (number->string score))
      (teken-score! teken-adt))

    (define (score-dispatch m)
      (cond ((eq? m 'reset) reset-score)
            ((eq? m 'tel) tel-score!)
            ((eq? m 'nieuwe-beste-score) nieuwe-beste-score)
            ((eq? m 'teken-score!) teken-score!)))
    score-dispatch))
    