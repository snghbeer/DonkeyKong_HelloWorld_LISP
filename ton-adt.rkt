#lang racket

(#%require "teken-adt.rkt")
(#%require "startposities-config.rkt")
(#%provide maak-ton-adt)

(define width 25)
(define height 27)

;;Ton-adt
(define (maak-ton-adt)
  (let* ((pos-x ton-begin-x)
         (pos-y ton-begin-y)
         (radius-voor-punten (- pos-y 40))
         (1-stap 2)
         (ton-versneld? #f)
         (x-radius (+ pos-x width))
         (y-radius (+ pos-y height))
         (collisie-status #f)
         (richting 'l))

    (define (verander-richting! platform-x platform-rx)
      (cond ((not (> pos-x platform-x)) (set! richting 'r))
            ((> pos-x platform-rx) (set! richting 'l))))

    (define (verwijder-richting!)
      (set! richting "geen-richting"))

    (define (beweeg-tonnen!)
      (cond ((eq? richting 'l) (go-left))
            ((eq? richting 'r) (go-right))))
    
    (define (teken! teken-adt) ;;Procedure om tonnen te tekenen
      ((teken-adt 'teken-ton!) ton-dispatch))

    (define (verwijder! teken-adt)
      ((teken-adt 'verwijder-ton!) ton-dispatch))

    (define (verhoog-ton-snelheid!)
      (when (eq? ton-versneld? #f)
        (set! 1-stap (+ 1-stap (/ 3 2)))
        (set! ton-versneld? #t)))

    (define (go-left)
      (set! pos-x (- pos-x 1-stap))
      (set! x-radius (+ pos-x width)))

    (define (go-right)
      (set! pos-x (+ pos-x 1-stap))
      (set! x-radius (+ pos-x width)))

    (define (go-down)
      (set! pos-y (+ pos-y 1-stap))
      (set! y-radius (+ pos-y height))
      (set! radius-voor-punten (- pos-y 40)))

    (define (collisie? obj-x obj-y obj-rx obj-ry)
      (and (or (<= obj-x pos-x obj-rx)
               (<= obj-x x-radius obj-rx))
           (or (<= obj-y pos-y obj-ry)
               (<= obj-y y-radius obj-ry))))
    
    (define (obj-boven-ton? obj-x obj-rx obj-ry)
      (and (<= radius-voor-punten obj-ry pos-y)
           (or (= pos-x obj-x)
               (= pos-x obj-rx)
               (= x-radius obj-x)
               (= x-radius obj-rx))))
      
    (define (ton-dispatch m)
      (cond ((eq? m 'teken!) teken!)
            ((eq? m 'verwijder!) verwijder!)
            ((eq? m 'links) (go-left))
            ((eq? m 'rechts) (go-right))
            ((eq? m 'down) (go-down))
            ((eq? m 'beweeg!) (beweeg-tonnen!))
            ((eq? m 'obj-boven-ton) obj-boven-ton?)
            ((eq? m 'x) pos-x)
            ((eq? m 'y) pos-y)
            ((eq? m 'rx) x-radius)
            ((eq? m 'ry) y-radius)
            ((eq? m 'collisie?) collisie?)
            ((eq? m 'verander-richting!) verander-richting!)
            ((eq? m 'verwijder-richting!) verwijder-richting!)
            ((eq? m 'verhoog-ton-snelheid!) (verhoog-ton-snelheid!))))
    ton-dispatch))
