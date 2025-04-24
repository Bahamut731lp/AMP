#lang racket
; První prvek seznamu
(car '(1 2 3))
(first '(1 2 3))

; Zbytek seznamu
(cdr '(1 2 3))
(rest '(1 2 3))

; Spojení seznamu
(cons 1 '(2 3))
(cons 1 '())

; Vytvoření seznamu z argumentů
(list (* 5 5) (first '(1 2 3)))

; Podmínky
(define (sign n)
  (cond
    [(< 0 n) 1]
    [(> 0 n) -1]
    [else 0]
    )
 )

; Vrací druhý prvek ze seznamu L
(define (druhy L)
  (first (rest L))
)

(define (posledni L)
  (first (reverse L))
)

(define (bezkonce L)
  (reverse (rest (reverse L)))
)

; Seznam bez druhého prvku
(define (bezdruheho L)
  (cons (first L) (rest (rest L)))
)

; Prohoď pořadí bez použití reverse
(define (prohod L)
  (list (first (rest L)) (first L))
)

; Modulo 2 a 3
(define (modulo23 n)
  (and (= (modulo n 2) 0) (= (modulo n 3) 0))
)

; Omezit podle intervalu
(define (omezit minima x maxima)
  (cond
    [(< x minima) minima]
    [(> x maxima) maxima]
    [else x]
  )
)

(define (suda? L)
  (cond
    [(empty? L) #t]
    [else (and (even? (first L)) (suda? (rest L)))]
  )
)

(define (secti L)
  (cond
    [(= (length L) 1) (first L)]
    [else (+ (first L) (secti (rest L)))]
  )
)

(druhy '(1 2 3 4))
(posledni '(1 2 3 4))
(bezkonce '(1 2 3 4))
(bezdruheho '(1 2 3 4))

(prohod '(1 2))
  
(modulo23 12)
(modulo23 15)

(omezit 0 19 100)
(omezit 0 -5 100)
(omezit 0 250 100)

(suda? '(1 2 3))
(suda? '(2 4 6))

(secti '(1 2 3))
(secti '(25 31 6 10))