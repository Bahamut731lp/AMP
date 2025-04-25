#lang racket

; Vytvořte ve Scheme funkci f s argumenty a a b, která vyhodnocuje vzorec
(define (f a b)
  (/ (+ (/ a b) (/ b a)) (* a b))
)

(f 1 2)

; Vytvořte ve Scheme funkci (vyber n L), která ze seznamu L vybere n-tý prvek. Pokud je n větší než délka L, vrátí prázdný seznam.
(define (vyber N L)
  (cond
    [(< N 1) '()]
    [(> N (length L)) '()]
    [(zero? (- N 1)) (first L)]
    [else (vyber (- N 1) (rest L))]
    )
  )

(display "\n")
(define seznam '(10 20 30 40 50)) 
(vyber 0 seznam)
(vyber 1 seznam)
(vyber 3 seznam)
(vyber 5 seznam)
(vyber 8 seznam)

; Mějme následující definici datové struktury pro oddělení (define-struct oddeleni (nazev zamestnanci))
; kde nazev daného oddělení je řetězec a zamestnanci je seznam řetězců, obsahující jména zaměstnanců pracujících v daném oddělení.
; Vytvořte funkci (hledej-oddeleni jmeno seznam-oddeleni), která vrátí název oddělení, ve kterém pracuje zaměstnanec daného jména.
; Pokud zaměstnanec nepracuje v žádném oddělení ze seznamu, vrátí funkce prázdný seznam.

(define-struct oddeleni (nazev zamestnanci))
(define podnik (list (make-oddeleni "Prodej" '("Anna Bílá" "Lída Žlutá" "Petr Modrý")) (make-oddeleni "Vývoj" '("Josef Zelený" "Lenka Růžová" "Alena Černá"))))

(define (hledej-oddeleni jmeno seznam-oddeleni)
  (local (
      [define (najdi-v-oddeleni jmeno seznam-zamestnancu)
        (cond
          [(empty? seznam-zamestnancu) #f]
          [(equal? jmeno (first seznam-zamestnancu)) #t]
          [else (najdi-v-oddeleni jmeno (rest seznam-zamestnancu))]
         )
      ]
  )
    (cond
      [(empty? seznam-oddeleni) '()]
      [(najdi-v-oddeleni jmeno (oddeleni-zamestnanci (first seznam-oddeleni))) (oddeleni-nazev (first seznam-oddeleni))]
      [else (hledej-oddeleni jmeno (rest seznam-oddeleni))]
    )
  )
)

(display "\n")
(hledej-oddeleni "Petr Modrý" podnik)
(hledej-oddeleni "Lenka Zelená" podnik)
(hledej-oddeleni "Lenka Růžová" podnik)

; Příklald fold left
(foldl (lambda (value result) (+ value result)) 0 '(1 2 3 4 5))
