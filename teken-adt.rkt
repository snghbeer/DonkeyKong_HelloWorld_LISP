#lang racket

(#%require "Graphics.rkt")
(#%require "positie-adt.rkt")
(#%require "startposities-config.rkt")
(#%provide maak-teken-adt)

;;Teken-spelbord

(define (maak-teken-adt px-horizontaal px-verticaal titel)
  
  (define window
    (make-window px-horizontaal px-verticaal titel))
  ((window 'set-background!) "black")

  ;;Tekent de constanten
  (define (teken-constanten! tile layer x y)
    (let ((positie (maak-positie-adt x y)))
      ((layer 'add-drawable) tile)
      ((positie 'set-pos!) tile x y)))

  ;;Procedure om tiles te verwijderen
  (define (verwijder! adt laag tiles)
    (let ((tile (assoc adt tiles)))
      ((laag 'remove-drawable) (cdr tile))))

  ;;LAGEN
  (define laag-voor-niveau (window 'make-layer))
  (define laag-voor-tijd (window 'make-layer))
  (define laag-voor-scores  (window 'make-layer))
  (define laag-voor-leven (window 'make-layer))
  (define hart-laag (window 'make-layer))
  (define constant-laag (window 'make-layer))
  (define spoken-laag (window 'make-layer))
  (define ton-laag (window 'make-layer))
  (define mario-laag (window 'make-layer))
  (define menu-laag (window 'make-layer))
  
  
  ;;;CONSTANTEN
  (define munt-tiles '())
  (define padd-tiles '())
 
  (define (teken-munt! bonus-adt)
    (let ((x (bonus-adt 'x-munt))
          (y (bonus-adt 'y-munt))
          (tile (assoc bonus-adt munt-tiles)))
      (if tile
          (begin (((cdr tile) 'set-x!) x)
                 (((cdr tile) 'set-y!) y))
          (begin (let ((munt-tile (make-bitmap-tile "sprites/coin.png" "sprites/coin_mask.png")))
                   ((constant-laag 'add-drawable) munt-tile)
                   (set! munt-tiles (cons (cons bonus-adt munt-tile) munt-tiles))
                   ((munt-tile 'set-x!) x)
                   ((munt-tile 'set-y!) y))))))

  (define (teken-paddenstoel! bonus-adt)
    (let ((x (bonus-adt 'x-paddenstoel))
          (y (bonus-adt 'y-paddenstoel))
          (tile (assoc bonus-adt padd-tiles)))
      (if tile
          (begin (((cdr tile) 'set-x!) x)
                 (((cdr tile) 'set-y!) y))
          (begin (let ((paddenstoel-tile (make-bitmap-tile "sprites/mushroom.png" "sprites/mushroom_mask.png")))
                   ((constant-laag 'add-drawable) paddenstoel-tile)
                   (set! padd-tiles (cons (cons bonus-adt paddenstoel-tile) padd-tiles))
                   ((paddenstoel-tile 'set-x!) x)
                   ((paddenstoel-tile 'set-y!) y))))))

  (define (verwijder-munt! bonus-adt)
    (verwijder! bonus-adt constant-laag munt-tiles))
  (define (verwijder-paddenstoel! bonus-adt)
    (verwijder! bonus-adt constant-laag padd-tiles))
  

  (define (teken-vlag! x y)
    (let ((vlag-tile (make-bitmap-tile "sprites/flag.png" "sprites/flag_mask.png")))
      (teken-constanten! vlag-tile constant-laag x y)))

  ;;Tekent de grond en de platformen

  ;;Tekent de platformen
  (define (teken-platform! x y)
    (let ((platform-tile (make-bitmap-tile "sprites/Floor.png")))
      (teken-constanten! platform-tile constant-laag x y)))

  ;;Tekent de grond & pauline floor
  (define (teken-grond! x y)
    (let ((ground-tile (make-bitmap-tile "sprites/grond1.png")))
      (teken-constanten! ground-tile constant-laag x y)))

  (define (teken-pauline-platform x y)
    (let ((pauline-floor-tile (make-bitmap-tile "sprites/Floor3.png")))
      (teken-constanten! pauline-floor-tile constant-laag x y)))

  ;;Tekent Pauline
  (define (teken-pauline! x y)
    (let ((pauline-tile (make-bitmap-tile "sprites/Pauline.png" "sprites/pauline_mask.png")))
      (teken-constanten! pauline-tile constant-laag x y)))

  ;;Tekent Donkey Kong & zijn tonnen achter hem
  (define (teken-dk!)
    (let ((x-dk 500)
          (y-dk 90)
          (x-barell 610)
          (y-barell 90)
          (dk-tile (make-bitmap-tile "sprites/Kong2.png" "sprites/Kong2_mask.png"))
          (barell-tile (make-bitmap-tile "sprites/barells.png" "sprites/barells_mask.png")))
          
      (teken-constanten! dk-tile constant-laag x-dk y-dk)
      (teken-constanten! barell-tile constant-laag x-barell y-barell)))

  ;;Tekent Mario's leven
  (define lege-tile (make-tile 200 200 "sprites/black-tile.png"))
  (define (teken-leven x y)
    (let ((life-tile (make-bitmap-tile "sprites/heart.png" "sprites/heart_mask.png")))
      (teken-constanten! life-tile hart-laag x y)))
   ((laag-voor-leven 'add-drawable) lege-tile)

  (define (set-leven! aantal x y)
    (lege-tile 'clear)
    ((lege-tile 'draw-text) aantal 24 x y "white"))

  ;;Teksten voor score & niveau
  (define tile-voor-scores (make-bitmap-tile "sprites/blacktile.png"))
  (define tile-voor-niveau (make-bitmap-tile "sprites/blacktile.png"))
  (define tile-voor-tijd (make-bitmap-tile "sprites/blacktile.png"))
 
  ((laag-voor-scores 'add-drawable) tile-voor-scores)
  ((laag-voor-niveau 'add-drawable) tile-voor-niveau)
  ((laag-voor-tijd 'add-drawable) tile-voor-tijd)

  (define (schrijf-scores! score beste-score y)
    (tile-voor-scores 'clear)
    ((tile-voor-scores 'draw-text) "Score: " 14 140 y "white")
    ((tile-voor-scores 'draw-text) score 14 200 y "white")
    ((tile-voor-scores 'draw-text) "Beste score: " 14 250 y "white")
    ((tile-voor-scores 'draw-text) beste-score 14 355 y "white"))
  
  (define (schrijf-niveau! niveau x y)
    (tile-voor-niveau 'clear)
    ((tile-voor-niveau 'set-x!) 300)
    ((tile-voor-niveau 'draw-text) "Level: " 18 350 y "white")
    ((tile-voor-niveau 'draw-text) niveau 18 x y "white"))

  (define (schrijf-tijd! tijd x y)
    (tile-voor-tijd 'clear)
    ((tile-voor-tijd 'set-x!) 300)
    ((tile-voor-tijd 'set-y!) 50)
    ((tile-voor-tijd 'draw-text) "Tijd: " 18 350 y "white")
    ((tile-voor-tijd 'draw-text) tijd 18 x y "white"))
    
  ;;Tekent het ladder
  (define (teken-ladder! x y)
    (let ((ladder-tile (make-bitmap-tile "sprites/ladder.png" "sprites/ladder_mask.png")))
      (teken-constanten! ladder-tile constant-laag x y)))

  ;;Tekent de tonnen
  (define tonnen-tiles '())

  (define (teken-ton! ton-adt)
    (let ((x (ton-adt 'x))
          (y (ton-adt 'y))
          (tile (assoc ton-adt tonnen-tiles)))
      (if tile
          (begin (((cdr tile) 'set-x!) x)
                 (((cdr tile) 'set-y!) y))
          (begin (let ((ton-tile (make-bitmap-tile "sprites/barell.png" "sprites/barell_mask.png")))
                   ((ton-laag 'add-drawable) ton-tile)
                   (set! tonnen-tiles (cons (cons ton-adt ton-tile) tonnen-tiles))
                   ((ton-tile 'set-x!) x)
                   ((ton-tile 'set-y!) y))))))

  (define (verwijder-ton! ton-adt)
    (verwijder! ton-adt ton-laag tonnen-tiles))  

  (define (verwijder-tonnen-tiles!)
    (set! tonnen-tiles '()))
           
  ;;Spoken
  (define spoken-tiles '())

  (define (teken-een-spook spook-adt)
    (let ((x (spook-adt 'x))
          (y (spook-adt 'y))
          (tile (assoc spook-adt spoken-tiles)))
      (if tile
          (begin (((cdr tile) 'set-x!) x)
                 (((cdr tile) 'set-y!) y))
          (begin (let ((spook-tile (make-bitmap-tile "sprites/ghost.png" "sprites/ghost_mask.png")))
                   ((spoken-laag 'add-drawable) spook-tile)
                   (set! spoken-tiles (cons (cons spook-adt spook-tile) spoken-tiles))
                   ((spook-tile 'set-x!) x)
                   ((spook-tile 'set-y!) y))))))

    (define (verwijder-spook! spook-adt)
      (verwijder! spook-adt spoken-laag spoken-tiles))
  
  (define (verwijder-spoken-tiles!)
    (set! spoken-tiles '()))
  
  ;Tekent Mario
  (define mario-tile (make-bitmap-tile "sprites/marioRight.png" "sprites/marioRight_mask.png")) ;tekent mario
  ((mario-laag 'add-drawable) mario-tile)
  
  (define (teken-mario! x y)
    (let ((positie (maak-positie-adt x y)))
      ((positie 'set-pos!) mario-tile x y)))

  ;;Hier wordt het menu getekend of verwijderd
  (define menu-tiles '())
            
  (define (teken-menu! x y)
    (set! menu-tiles (cons (make-bitmap-tile "sprites/Donkey_kong_menu.png") menu-tiles))
    (teken-constanten! (car menu-tiles) menu-laag x y))
  
  (define (verwijder-menu!)
    ((menu-laag 'remove-drawable) (car menu-tiles)))
      
  ;;Spel lus functies
  (define (set-spel-lus-functie functie)
    ((window 'set-update-callback!) functie))

  (define (set-key-callback-functie functie)
    ((window 'set-key-callback!) functie))

  ;Dispatch-functie
  (define (teken-dispatch m)
    (cond ((eq? m 'teken-mario!) teken-mario!)
          ((eq? m 'teken-grond!) teken-grond!)
          ((eq? m 'teken-platform!) teken-platform!)
          ((eq? m 'teken-pauline-platform) teken-pauline-platform)
          ((eq? m 'ladder!) teken-ladder!)
          ((eq? m 'pauline) teken-pauline!)
          ((eq? m 'dk) (teken-dk!))
          ((eq? m 'teken-ton!) teken-ton!)
          ((eq? m 'verwijder-ton!) verwijder-ton!)
          ((eq? m 'verwijder-tonnen-tiles!) (verwijder-tonnen-tiles!))
          ((eq? m 'teken-spook!) teken-een-spook)
          ((eq? m 'verwijder-spook!) verwijder-spook!)
          ((eq? m 'verwijder-spoken-tiles!) (verwijder-spoken-tiles!))
          ((eq? m 'teken-leven!) teken-leven)
          ((eq? m 'set-leven!) set-leven!)
          ((eq? m 'teken-paddenstoel!) teken-paddenstoel!)
          ((eq? m 'verwijder-paddenstoel!) verwijder-paddenstoel!)
          ((eq? m 'teken-munt!) teken-munt!)
          ((eq? m 'verwijder-munt!) verwijder-munt!)
          ((eq? m 'teken-vlag!) teken-vlag!)
          ((eq? m 'schrijf-niveau!) schrijf-niveau!)
          ((eq? m 'schrijf-scores!) schrijf-scores!)
          ((eq? m 'schrijf-tijd!) schrijf-tijd!)
          ((eq? m 'menu) teken-menu!)
          ((eq? m 'verwijder-menu!) (verwijder-menu!))
          ((eq? m 'spel-lus) set-spel-lus-functie)
          ((eq? m 'key) set-key-callback-functie)))
  teken-dispatch)