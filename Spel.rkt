#lang racket

(#%require "teken-adt.rkt")
(#%require "mario-adt.rkt")
(#%require "menu-adt.rkt")
(#%require "ton-adt.rkt")
(#%require "ladder-adt.rkt")
(#%require "pauline-adt.rkt")
(#%require "platform-adt.rkt")
(#%require "hart-adt.rkt")
(#%require "startposities-config.rkt")
(#%require "niveau-adt.rkt")
(#%require "spook-adt.rkt")
(#%require "score-adt.rkt")
(#%require "grond-adt.rkt")
(#%require "bonus-adt.rkt")
(#%require "timer-adt.rkt")

(define px-horizontaal 800)
(define px-verticaal 600)
(define titel "Donkey Kong ")


;;Gebruikte adt's
(define teken-adt (maak-teken-adt px-horizontaal px-verticaal titel))
(define mario-adt (maak-mario-adt))
(define menu-adt (maak-menu-adt teken-adt))
(define hart-adt (maak-leven-adt teken-adt))
(define pauline-adt (maak-pauline-adt))
(define niveau-adt (maak-niveau-adt))
(define score-adt (maak-score-adt))
(define timer-adt (maak-timer-adt))

;;Het spel-adt
(define (spel-adt)

  (let ((spel-tijd 0)
        (spel-interval 1000)
        (mario-tijd 0)
        (spring-tijd 500)
        (game-status #f)
        (gewonnen #f)
        (ton-teller 0)
        (ton-interval 170))

    (define (reset-tellers!)
      (set! spel-tijd 0)
      (set! ton-teller 0))

    (define (start)

      (define (spel-lus-functie delta-tijd)
        (when game-status
          (set! spel-tijd (+ spel-tijd delta-tijd))
          (set! ton-teller (modulo (+ ton-teller 1) ton-interval))
          (niveau-2)
          (gewonnen?)
          (pauline-bereikt?)
          (verloren?)
          (when (>= spel-tijd spel-interval)
            (verlaag-tijd!)
            (set! spel-tijd 0)
            (begin-opnieuw!))
          (when (= ton-teller 0) ;;Gaat tonnen-adt's genereren
            (set! tonnen (cons (maak-ton-adt) tonnen)))
          (teken-de-tonnen!) ;met de gegenerende ton-adt's wordt er msg ernaar gestuurd om ze te tekenen 
          (beweeg-de-tonnen!)
          (verander-richting-spoken!)
          (mario-niet-op-platformen)
          (mario-scoort!)
          (mario-raapt-bonus?)
          (when (mario-adt 'status)
            (set! mario-tijd (+ mario-tijd delta-tijd))
            (when (and (mario-adt 'status)
                       (>= mario-tijd spring-tijd))
              ((mario-adt 'j-down) teken-adt)
              (set! mario-tijd 0)))
          (verliest-leven)))
     
      ((teken-adt 'spel-lus) spel-lus-functie))
    
    (define (key-dispatch key)
      (cond ((eq? key 'left) ((mario-adt 'links) teken-adt))
            ((eq? key 'right) ((mario-adt 'rechts) teken-adt))
            ((eq? key 'up) (when (mag-ladder-klimmen)
                             ((mario-adt 'boven) teken-adt)))
            ((eq? key 'down) (when (and (mag-ladder-klimmen)
                                        (eq? (ormap (lambda (platform)
                                                      ((platform 'op-platform?) (mario-adt 'x) (mario-adt 'y)
                                                                                (mario-adt 'rx) (mario-adt 'ry)))
                                                    platformen) #f)
                                        (eq? ((grond 'op-platform?)(mario-adt 'x) (mario-adt 'y)
                                                                   (mario-adt 'rx) (mario-adt 'ry)) #f))
                               ((mario-adt 'beneden) teken-adt)))
            ((eq? key '#\space) ((mario-adt 'j-up) teken-adt))
            ((eq? key '#\return) ((menu-adt 'verwijder!) teken-adt)
                                 (set! game-status #t))
            ((eq? key 'escape) (exit))))
    ((teken-adt 'key) key-dispatch) ;;Gaat ervoor zorgen dat key bindingen werken
  
    ;;Mario
    ((mario-adt 'teken!) teken-adt)
   
    (define (mario-niet-op-platformen)
      (when (and (eq? (ormap (lambda (platform)
                               ((platform 'op-platform?) (mario-adt 'x) (mario-adt 'y)
                                                         (mario-adt 'rx) (mario-adt 'ry)))
                             platformen) #f)
                 (eq? ((grond 'op-platform?)(mario-adt 'x) (mario-adt 'y)
                                            (mario-adt 'rx) (mario-adt 'ry)) #f)
                 (eq? (mag-ladder-klimmen) #f)
                 (eq? ((pauline-adt 'op-platform?)(mario-adt 'x) (mario-adt 'y)
                                                  (mario-adt 'rx) (mario-adt 'ry)) #f)
                 (eq? (mario-adt 'status) #f))
        ((mario-adt 'valt-stilaan) teken-adt)))

    ;;Hier wordt er bepaald of mario boven een bepaald object is of niet.
    (define (mario-boven-ton)
      (ormap (lambda (ton-adt)
               ((ton-adt 'obj-boven-ton) (mario-adt 'x)
                                         (mario-adt 'rx)
                                         (mario-adt 'ry))) tonnen))

    (define (mario-boven-spook)
      (ormap (lambda (spook-adt)
               ((spook-adt 'obj-boven-ton) (mario-adt 'x)
                                           (mario-adt 'rx)
                                           (mario-adt 'ry))) spoken))

    (define (mario-scoort!)
      (when (or (and (mario-boven-ton) (not (mag-ladder-klimmen)))
                (and (mario-boven-spook) (not (mag-ladder-klimmen)))
                (heeft-opgerapen?))
        ((score-adt 'tel) teken-adt)
        ((score-adt 'nieuwe-beste-score) teken-adt)))
             
    ;;Procedures om objecten te verplaatsen (links & rechts)

    ;;Het object moet een lijst zijn dat een adt-constructor bevat
    (define (naar-links! object)
      (for-each (lambda (adt)
                  (adt 'links)) object))

    (define (naar-rechts! object)
      (for-each (lambda (adt)
                  (adt 'rechts)) object))

    ;;Tonnen
    (define tonnen '())

    (define (initialiseer-tonnen!)
      (set! tonnen '()))
    
    (define (ton-naar-links!)
      (naar-links! tonnen))
    (define (ton-naar-rechts!)
      (naar-rechts! tonnen))    
    (define (val!)
      (for-each (lambda (ton-adt)
                  ((ton-adt 'down) teken-adt)) tonnen))

    (define (ton-op-platform? ton-adt)
      (ormap (lambda (platform-adt)
               ((platform-adt 'op-platform?) (ton-adt 'x) (ton-adt 'y)
                                             (ton-adt 'rx) (ton-adt 'ry))) platformen))
    (define (ton-op-grond? ton-adt)
      ((grond 'op-platform?) (ton-adt 'x) (ton-adt 'y)
                             (ton-adt 'rx) (ton-adt 'ry)))
    
    (define (collisie-met-object? adt)
      (ormap (lambda (ton-adt)
               ((ton-adt 'collisie?) (adt 'x)(adt 'y)
                                     (adt 'rx) (adt 'ry))) tonnen))

    (define (collisie-ton)
      (collisie-met-object? mario-adt))
    
    (define (verander-richting-ton! platform-adt)
      (map (lambda (ton-adt)
             ((ton-adt 'verander-richting!) (platform-adt 'x) (platform-adt 'rx)))
           tonnen))

    (define (teken-de-tonnen!)
      (for-each (lambda (ton-adt)
                  ((ton-adt 'teken!) teken-adt)) tonnen))
    
    (define (beweeg-de-tonnen!)
      (for-each (lambda (ton-adt)
                  (cond ((ton-op-platform? ton-adt) (ton-adt 'beweeg!)
                                                    (ormap (lambda (platform-adt)
                                                             (verander-richting-ton! platform-adt))
                                                           platformen))
                        ((ton-op-grond? ton-adt) (ton-adt 'links) (ton-adt 'verwijder-richting!))
                        (else (ton-adt 'down)))) tonnen))

    (define (verwijder-tonnen!)
      (for-each (lambda (ton-adt)
                  ((ton-adt 'verwijder!) teken-adt)) tonnen))

    ;;Spoken
    (define spoken (list (maak-spook-adt spook-x spook-y)
                         (maak-spook-adt spook2-x spook2-y)))
    (define (teken-spoken!)
      (for-each (lambda (spook-adt)
                  ((spook-adt 'teken!) teken-adt)) spoken))

    (define (beweeg-spoken)
      (for-each (lambda (spook-adt)
                  (spook-adt 'beweeg!)) spoken))

    (define (verander-richting-spoken!)
      (for-each (lambda (spook-adt)
                  (spook-adt 'verander-richting!)) spoken))

    (define (collisie-spook)
      (ormap (lambda (spook-adt)
               ((spook-adt 'collisie?) (mario-adt 'x) (mario-adt 'y)
                                       (mario-adt 'rx) (mario-adt 'ry))) spoken))

    (define (verwijder-spoken!)
      (for-each (lambda (spook-adt)
                  ((spook-adt 'verwijder!) teken-adt)) spoken))
              
    ;;Mario's leven
    (hart-adt 'teken-hart!)
    (hart-adt 'teken-aantal-leven!)

    (define (verliest-leven)
      (when (or (collisie-ton) (collisie-spook))
        (hart-adt 'decrementeer)
        ((mario-adt 'reset) teken-adt)))
                          
    ;;Niveau
    ((niveau-adt 'teken!) teken-adt)
    ;;Score
    ((score-adt 'teken-score!) teken-adt)
    ;;Timer
    ((timer-adt 'teken!) teken-adt)

    (define (verlaag-tijd!)
      ((timer-adt 'verlaag-tijd!) teken-adt))

    (define (begin-opnieuw!)
      (when (= (timer-adt 'tijd) 0)
        (herinitialiseer-alles!)))
    
    ;;;CONSTANTEN

    ;;Menu
    ((menu-adt 'teken!) teken-adt)

    ;;Bonusvoorwerpen
    (define bonusvoorwerpen (list (maak-bonus-adt x-munt1 y-munt1 x-padd y-padd)
                                  (maak-bonus-adt x-munt2 y-munt2 x-padd2 y-padd2)))

    (for-each (lambda (bonus-adt)
                ((bonus-adt 'teken-munt!) teken-adt)) bonusvoorwerpen)
    (for-each (lambda (bonus-adt)
                ((bonus-adt 'teken-paddenstoel!) teken-adt)) bonusvoorwerpen)

    (define (mario-raapt-bonus?)
      (for-each (lambda (bonus-adt)
                  ((bonus-adt 'collisie) (mario-adt 'x) (mario-adt 'y)
                                         (mario-adt 'rx) (mario-adt 'ry) teken-adt)) bonusvoorwerpen))

    (define (heeft-opgerapen?)
      (ormap (lambda (bonus-adt)
               (bonus-adt 'heeft-iets-opgerapen?)) bonusvoorwerpen))
                                         
    ;Vlag
    ((teken-adt 'teken-vlag!) x-vlag y-vlag)
    
    ;;Ladders
    (define ladders (list (maak-ladder-adt x-ladder1 y-ladder1)
                          (maak-ladder-adt x-ladder2 y-ladder2)
                          (maak-ladder-adt x-ladder3 y-ladder3)
                          (maak-ladder-adt x-ladder4 y-ladder4)
                          (maak-ladder-adt x-ladder5 y-ladder5)))
    (for-each (lambda (ladder)
                ((ladder 'teken-ladder!) teken-adt)) ladders)
    (define (mag-ladder-klimmen)
      (ormap (lambda (ladder)
               ((ladder 'kan-klimmen?) (mario-adt 'x) (mario-adt 'rx)
                                       (mario-adt 'y) (mario-adt 'ry))) ladders))

    
    ;;Hier worden de platformen getekend
    (define platformen
      (list (maak-platform-adt x-1st-plat y-1st-plat)
            (maak-platform-adt x-2nd-plat y-2nd-plat)
            (maak-platform-adt x-3rd-plat y-3rd-plat)
            (maak-platform-adt x-4th-plat y-4th-plat)))
    (define grond (maak-grond-adt x-grond y-grond))
    ((grond 'teken-grond!) teken-adt)
         
    (for-each (lambda (platform)
                ((platform 'teken-platform!) teken-adt)) platformen)       

    ;;Pauline & Donkey-Kong
    ((pauline-adt 'teken!) teken-adt)
    ((pauline-adt 'teken-platform!) teken-adt)
    (teken-adt 'dk)

    (define (reset-spel-status!)
      (set! game-status #f))
    
    ;;De speler wint wanneer Mario Pauline bereikt heeft.
    (define (pauline-bereikt?)
      (when ((mario-adt 'raakt-pauline?) (pauline-adt 'x)
                                         (pauline-adt 'y)
                                         (pauline-adt 'rx)
                                         (pauline-adt 'ry))
        (set! gewonnen #t)))

    (define (herinitialiseer-alles!) ;;Wat er gedaan moet worden bij een herinitialisatie
      ((mario-adt 'reset) teken-adt)
      (reset-win-status!)
      (reset-spel-status!)
      ((timer-adt 'initialiseer!) teken-adt)
      (verwijder-tonnen!)
      (teken-adt 'verwijder-tonnen-tiles!)
      (verwijder-spoken!)
      (teken-adt 'verwijder-spoken-tiles!)
      (initialiseer-tonnen!)
      (reset-tellers!)
      ((niveau-adt 'reset) teken-adt)
      (hart-adt 'initialiseer-leven!)
      ((score-adt 'reset) teken-adt)
      (start-spel-opnieuw))
    
    (define (gewonnen?)
      (cond ((and gewonnen (= (niveau-adt 'current-niveau) 1))
             ((mario-adt 'reset) teken-adt)
             (reset-win-status!)
              ((timer-adt 'initialiseer!) teken-adt)
             (verwijder-tonnen!)
             (teken-adt 'verwijder-tonnen-tiles!)
             (initialiseer-tonnen!)
             (reset-tellers!)
             ((niveau-adt 'verhoog!) teken-adt)
             (hart-adt 'initialiseer-leven!)
             (start-spel-opnieuw))
            ((and gewonnen (= (niveau-adt 'current-niveau) 2))
             (herinitialiseer-alles!))))

    (define (reset-win-status!)
      (set! gewonnen #f))

    (define (verloren?)
      (when (hart-adt 'game-over?)
        (herinitialiseer-alles!)))

    (define (start-spel-opnieuw)
      ((spel-adt) 'start))
    
    
    ;;;ENKEL BIJ NIVEAU 2

    ;;Spoken worden getekend
    ;;Snelheid van de tonnen geïncrementeerd
    (define (niveau-2)
      (when (= (niveau-adt 'current-niveau) 2)
        (teken-spoken!)
        (beweeg-spoken)
        (for-each (lambda (ton-adt)
                    (ton-adt 'verhoog-ton-snelheid!)) tonnen)))


    ;;Het spel-dispatch
    (define (spel-dispatch m)
      (cond ((eq? m 'start) (start))))
    spel-dispatch))


;;Gaat het spel oproepen
(define spel (spel-adt))
(spel 'start)