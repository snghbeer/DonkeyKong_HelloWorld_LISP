#lang racket

(#%provide maak-mario-adt)
(#%require "teken-adt.rkt")
(#%require "ladder-adt.rkt")
(#%require "startposities-config.rkt")


;;Linker -en rechtermuur
(define rechtermuur 730)
(define linkermuur 20)

;;Mario-adt
(define (maak-mario-adt)
  (let* ((mario-x mario-begin-x)
         (mario-y mario-begin-y)
         (w 25)
         (h 40)
         (x-radius (+ mario-x w))
         (y-radius (+ mario-y h))
         (1-stap 17)
         (spring-status #f)
         (springhoogte 45)
         (val-px 4))

    (define (teken-mario! teken-adt)
      ((teken-adt 'teken-mario!) mario-x mario-y))

    (define (initialiseer-postie! teken-adt)
      (set! mario-x mario-begin-x)
      (set! mario-y mario-begin-y)
      (set! x-radius (+ mario-x w))
      (set! y-radius (+ mario-y h))
      (teken-mario! teken-adt))

    (define (beweeg-naar-links teken-adt)
      (when (<= linkermuur mario-x)
        (set! mario-x (- mario-x 1-stap))
        (set! x-radius (+ mario-x w))
        (teken-mario! teken-adt)))
    
    (define (beweeg-naar-rechts teken-adt)
      (when (>= rechtermuur mario-x)
        (set! mario-x (+ mario-x 1-stap))
        (set! x-radius (+ mario-x w))
        (teken-mario! teken-adt)))
    
    (define (naar-boven teken-adt)
      (set! mario-y (- mario-y 1-stap))
      (set! y-radius (+ mario-y h))
      (teken-mario! teken-adt))

    (define (naar-beneden teken-adt)
      (set! mario-y (+ mario-y 1-stap))
      (set! y-radius (+ mario-y h))
      (teken-mario! teken-adt))
 
    (define (jump-down teken-adt) ;procedure om mario-tile te laten zakken
      (set! mario-y (+ mario-y springhoogte))
      (set! y-radius (+ mario-y h))
      (teken-mario! teken-adt)
      (set-status!))

    (define (jump-up teken-adt) ;Springprocedure
      (when (eq? spring-status #f)
        (set! mario-y (- mario-y springhoogte))
        (set! y-radius (+ mario-y h))
        (teken-mario! teken-adt)
        (set-status!)))

    (define (valt-stilaan teken-adt)
      (set! mario-y (+ mario-y val-px))
      (set! y-radius (+ mario-y h))
      (teken-mario! teken-adt))

    (define (set-status!)
      (cond ((eq? spring-status #t) (set! spring-status #f))
            (else (set! spring-status #t))))

    (define (raakt-pauline? pauline-x pauline-y pauline-rx pauline-ry)
      (and (or (<= pauline-y mario-y pauline-ry)
               (<= pauline-y y-radius pauline-ry))
           (or (<= pauline-x mario-x pauline-rx)
               (<= pauline-x x-radius pauline-rx))))
      
    (define (mario-dispatch m)
      (cond ((eq? m 'teken!) teken-mario!)
            ((eq? m 'x) mario-x)
            ((eq? m 'y) mario-y)
            ((eq? m 'rx) x-radius)
            ((eq? m 'ry) y-radius)
            ((eq? m 'links) beweeg-naar-links)
            ((eq? m 'rechts) beweeg-naar-rechts)
            ((eq? m 'boven) naar-boven)
            ((eq? m 'beneden) naar-beneden)
            ((eq? m 'valt-stilaan) valt-stilaan)
            ((eq? m 'reset) initialiseer-postie!)
            ((eq? m 'j-up) jump-up)
            ((eq? m 'j-down) jump-down)
            ((eq? m 'status) spring-status)
            ((eq? m 'raakt-pauline?) raakt-pauline?)))
    mario-dispatch))