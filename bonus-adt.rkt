#lang racket

(#%require "teken-adt.rkt")
(#%provide maak-bonus-adt)

(define w 35)
(define h 20)

(define (maak-bonus-adt x-m y-m x-p y-p)
  (let* ((x-munt x-m)
         (y-munt y-m)
         (x-paddenstoel x-p)
         (y-paddenstoel y-p)
         (rx-munt (+ x-munt w))
         (ry-munt (+ y-munt h))
         (rx-paddenstoel (+ x-paddenstoel w))
         (ry-paddenstoel (+ y-paddenstoel h))
         (heeft-opgerapen? #f)
         (munt-getekend #f)
         (padd-getekend #f))

    (define (teken-munt! teken-adt)
      ((teken-adt 'teken-munt!) bonus-dispatch)
      (set! munt-getekend #t))

    (define (teken-paddenstoel! teken-adt)
      ((teken-adt 'teken-paddenstoel!) bonus-dispatch)
      (set! padd-getekend #t))

    (define (verwijder-munt! teken-adt)
      ((teken-adt 'verwijder-munt!) bonus-dispatch)
      (set! munt-getekend #f))

    (define (verwijder-paddenstoel! teken-adt)
      ((teken-adt 'verwijder-paddenstoel!) bonus-dispatch)
      (set! padd-getekend #f))

    (define (collisie obj-x obj-y obj-rx obj-ry teken-adt)
      (cond ((and (or (<= obj-x x-munt obj-rx) ;;collisie met munt
                      (<= obj-x rx-munt obj-rx))
                  (or (<= obj-y y-munt obj-ry)
                      (<= obj-y ry-munt obj-ry))
                  (eq? #t munt-getekend))
             (set! heeft-opgerapen? #t)
             (verwijder-munt! teken-adt))
            ((and (or (<= obj-x x-paddenstoel obj-rx) ;;collisie met paddenstoel
                      (<= obj-x rx-paddenstoel obj-rx))
                  (or (<= obj-y y-paddenstoel obj-ry)
                      (<= obj-y ry-paddenstoel obj-ry))
                  (eq? #t padd-getekend))
             (set! heeft-opgerapen? #t)
             (verwijder-paddenstoel! teken-adt))
            (else (set! heeft-opgerapen? #f))))

    (define (heeft-iets-opgerapen?)
      (and (eq? heeft-opgerapen? #t)))

    (define (bonus-dispatch m)
      (cond ((eq? m 'teken-munt!) teken-munt!)
            ((eq? m 'verwijder-munt!) verwijder-munt!)
            ((eq? m 'teken-paddenstoel!) teken-paddenstoel!)
            ((eq? m 'verwijder-padd!) verwijder-paddenstoel!)
            ((eq? m 'x-munt) x-munt)
            ((eq? m 'y-munt) y-munt)
            ((eq? m 'rx-munt) rx-munt)
            ((eq? m 'ry-munt) ry-munt)
            ((eq? m 'x-paddenstoel) x-paddenstoel)
            ((eq? m 'y-paddenstoel) y-paddenstoel)
            ((eq? m 'rx-paddenstoel) rx-paddenstoel)
            ((eq? m 'ry-paddenstoel) ry-paddenstoel)
            ((eq? m 'collisie) collisie)
            ((eq? m 'heeft-iets-opgerapen?) (heeft-iets-opgerapen?))))
    bonus-dispatch))
    