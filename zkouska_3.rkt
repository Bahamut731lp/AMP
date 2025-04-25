#lang racket

; Úloha 1
(define (f a b c)
  (/ (+ a (* b c)) (sqrt (+ (expt a 2) (expt b 2) (expt c 2))))
  )

(f 6 18 9)

; Úloha 2
(define (jenLicha? L)
  (foldl (lambda (value result) (and value result)) #t (map (lambda (z) (odd? z)) L))
)

(jenLicha? '(5 8 3 9 1))
(jenLicha? '(17 5 21 9))

; Úloha 3
(define-struct linka (cislo zastavky))

(define (jedouZastavkou Nazev Linky)
  (local (
          [define (jedeZastavkou? Nazev Zastavky)
            (cond
              [(empty? Zastavky) #f]
              [(string=? Nazev (first Zastavky)) #t]
              [else (jedeZastavkou? Nazev (rest Zastavky))]
              )
            ]
          [define (prevedNaCisla mezivysledek)
            (map (lambda (v) (first v)) mezivysledek)
            ]
          [define (vyfiltrujFalse meziVysledek)
            (filter (lambda (c) (second c)) meziVysledek)
            ]
          [define (vytvorParCisloJedeZastavkou meziVysledek)
            (map (lambda (l) (list (linka-cislo l) (jedeZastavkou? Nazev (linka-zastavky l)))) meziVysledek)
            ]
          )
(prevedNaCisla (vyfiltrujFalse (vytvorParCisloJedeZastavkou Linky))
  )))

(jedouZastavkou "Babylon" (list
                           (make-linka 12 '("Fugnerova" "Šaldovo Náměstí" "Malé náměstí"))
                           (make-linka 17 '("Fugnerova" "Babylon" "Zimní stadion"))
                           (make-linka 25 '("Fugnerova" "Šaldovo Náměstí" "Tržní náměstí"))
                           (make-linka 31 '("Fugnerova" "Babylon" "Garáže ČSAD"))
                           )
                )