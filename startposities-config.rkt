#lang racket

(#%provide x-1st-plat y-1st-plat x-2nd-plat y-2nd-plat x-tijd y-tijd
           x-3rd-plat y-3rd-plat x-4th-plat y-4th-plat
           x-ladder1 y-ladder1 x-ladder2 y-ladder2
           x-ladder3 y-ladder3 x-ladder4 y-ladder4 x-grond
           y-grond x-pauline-floor y-pauline-floor pauline-floor-w
           pauline-floor-h x-ladder5 y-ladder5 hart-x hart-y 
           ton-begin-x ton-begin-y hart-text-x hart-text-y
           spook-x spook-y spook2-x spook2-y spoken-eind-limiet
           spoken-begin-limiet mario-begin-x mario-begin-y x-vlag y-vlag
           x-munt1 y-munt1 x-munt2 y-munt2 x-padd y-padd x-padd2 y-padd2)

;;Configuratie van platformen
(define x-grond 0)
(define y-grond 575)
(define x-pauline-floor 410)
(define y-pauline-floor 70)
(define pauline-floor-w 113)
(define pauline-floor-h 18)

(define x-1st-plat 100)
(define y-1st-plat 470)
(define x-2nd-plat 140)
(define y-2nd-plat 370)
(define x-3rd-plat 100)
(define y-3rd-plat 270)
(define x-4th-plat 140)
(define y-4th-plat 170)

;;Configuratie van ladders
(define x-ladder1 580)
(define y-ladder1 477)
(define x-ladder2 160)
(define y-ladder2 375)
(define x-ladder3 570)
(define y-ladder3 275)
(define x-ladder4 160)
(define y-ladder4 175)
(define x-ladder5 420)
(define y-ladder5 75)

(define mario-begin-x 30)
(define mario-begin-y 535)

;;leven van mario
(define hart-x 20)
(define hart-y 10)
(define hart-text-x 60)
(define hart-text-y 14)

;;Ton startposities
(define ton-begin-x 600)
(define ton-begin-y 145)


;;Spoken
(define spook-x 230)
(define spook-y 445)
(define spook2-x 230)
(define spook2-y 245)

(define spoken-eind-limiet 430)
(define spoken-begin-limiet 230)

;;Vlag
(define x-vlag -5)
(define y-vlag 463)

;;Bonusvoorerpen
(define x-munt1 630)
(define y-munt1 350)
(define x-munt2 470)
(define y-munt2 150)
(define x-padd 110)
(define y-padd 250)
(define x-padd2 110)
(define y-padd2 450)

;;Timer
(define x-tijd 400)
(define y-tijd 0)
  