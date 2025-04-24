#lang racket
(define promenna 3.14)
(define seznam '(1 2 3))
(define scitani (+ 8 7))
(define odcitani (- 8 7))
(define nasobeni (* 8 7))
(define deleni (/ 8 7))

; Je číslem?
(number? scitani)
; Plocha kruhu
(define (plocha-kruhu r) (* 3.14 r r))
; Plocha prstence
(define (plocha-prstence vnejsi vnitrni) (- (plocha-kruhu vnejsi) (plocha-kruhu vnitrni)))
; Výsledky
(plocha-kruhu 5)
(round (plocha-prstence 8 5))

; Délka přepony
(define (prepona a b)
  (sqrt (+ (expt a 2) (expt b 2)))
)

(prepona 3 4)

; Je pravoúhly?
(define (pravouhly? a b c)
  (= (prepona a b) c)
)

(pravouhly? 3 4 5)

; Převod teploty
(define (C->F c)
  (+
   (* c 9/5)
   32
  )
)

(C->F 20)

; Výpočet (a+b)^3
(define (ab3 a b)
  (+
   (expt a 3)
   (* 3 (expt a 2) b)
   (* 3 (expt b 2) a)
   (expt b 3)
  )
)

(ab3 2 3)

; Skoro celé číslo
(define (skorocele? x)
  (<
   (abs (- (round x) x))
   0.001
  )
)

(skorocele? 7.99)
(skorocele? 7.9999)