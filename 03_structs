#lang racket

(define-struct clovek (jmeno vek plat))
(define eva1 (make-clovek "Eva" 17 10000))

(define (novy-clovek jmeno vek plat)
  (cond
    [(not (string? jmeno)) (error "novy-clovek" "jmeno musí být řetězec")]
    [(not (number? vek)) (error "novy-clovek" "vek musí být číslo")]
    [(not (number? plat)) (error "novy-clovek" "plat musí být číslo")]
    [else (make-clovek jmeno vek plat)]
    ))

; vytvořte datovou strukturu zbozi obsahující položky nazev, cena, kusu. Vytvořte konstantu sklad, která obsahuje několik záznamů se zbožím
(define-struct zbozi (nazev cena kusu) #:transparent)
(define sklad (list (make-zbozi "Test 1" 100 10) (make-zbozi "Test 2" 150 10) (make-zbozi "Test 3" 200 10)))

(define (najdi-cenu jmeno seznam)
  (cond
    [(empty? seznam) 0]
    [(string=? jmeno (zbozi-nazev (first seznam))) (zbozi-cena (first seznam))]
    [else (najdi-cenu jmeno (rest seznam))]
    ) 
  )

(najdi-cenu "Test 1" sklad)
(najdi-cenu "Test X" sklad)

; vytvořte funkci (celkova-hodnota seznam), která dostane seznam datových struktur typu zbozi a
; vydá jeho celkovou cenu (počty kusů krát jednotková cena pro všechna zboží ze seznamu)
(define (celkova-hodnota seznam)
  (cond
    [(empty? seznam) 0]
    [else (
           + (
              *
              (zbozi-kusu (first seznam))
              (zbozi-cena (first seznam))
              )
             (celkova-hodnota (rest seznam))
             )
          ]
    )
  )

(celkova-hodnota sklad)

; vytvořte funkci (cena-objednavky objednavka sklad) která pro objednávku
; ve tvaru ((jméno1 počet_kusů1) (jméno2 počet_kusů2) ... ) spočítá celkovou cenu objednaného zboží
(define (cena-objednavky objednavka sklad)
  (local [
          ; Pomocná rekurzivní funkce hledá zboží ve skladu
          (define (najdi-zbozi name sklad)
            (cond
              [(empty? sklad) #f]
              [(string=? name (zbozi-nazev (first sklad))) (first sklad)]
              [else (najdi-zbozi name (rest sklad))]))

          ; Spočítá cenu jednoho řádku objednávky
          (define (cena-riadku polozka)
            (let* ((name (first polozka))
                   (pocet (second polozka))
                   (zbozi (najdi-zbozi name sklad))) ; <- sklad se předává sem
              (if zbozi
                  (* (zbozi-cena zbozi) pocet)
                  0)))
          ]
    ; Sečtení cen všech řádků
    (apply + (map cena-riadku objednavka))))



(define objednavka '(("Test 1" 2) ("Test 2" 3)))
(cena-objednavky objednavka sklad)

; vytvořte funkci (vydej sklad název kusů), která ze skladu odebere příslušný počet kusů zboží s daným názvem
(define (vydej sklad nazev kusy)
  (map (lambda (z)
         (if (string=? (zbozi-nazev z) nazev)
             (make-zbozi (zbozi-nazev z) (zbozi-nazev z) (- (zbozi-kusu z) kusy))
             z))
       sklad))

(vydej sklad "Test 1" 5)