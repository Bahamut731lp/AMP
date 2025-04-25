#lang racket

; Úloha 1
(define (f a b c)
  (/ (sqrt (+
            (expt a 2)
            (expt b 2)
            ))
     (sqrt (+
            (expt b 2)
            (expt c 2)
            ))
     )
  )

(f 9 3 1)

; Úloha 2
(define (odKonce N L)
  (local (
          [define (najdiOdKonce N L delka index)
            (cond
              [(empty? L) 0]
              [(= (first L) N) (- delka index)]
              [else (najdiOdKonce N (rest L) delka (+ index 1))]
              )
            ]
          )
    (najdiOdKonce N L (length L) 0) ; Nula, protože jedeme od konce. Délka pole je v příkladu 7, takže já najdu prvek na pozici 3 (od 1), což je 5 míst od konce.
    )
  )

(odKonce 3 '(5 8 3 9 2 3 1))

; Úloha 3
(define-struct obchod (nazev cenik))
(define-struct zbozi (nazev cena))

(define test (list 
              (make-obchod "Billa" 
                           (list (make-zbozi "Mouka hladká" 12)
                                 (make-zbozi "Mattoni" 15)
                                 (make-zbozi "Chléb Horal" 27))) 
              (make-obchod "Lidl"
                           (list (make-zbozi "Rohlík" 3)
                                 (make-zbozi "Mouka hladká" 10)))
              (make-obchod "Tesco"
                           (list (make-zbozi "Mattoni" 15)
                                 (make-zbozi "Chléb Horal" 25)))))


(define (prumernaCena Nazev Obchody)
  (local (
          [define (najdiCenuProduktu Jmeno Cenik)
            (cond
              [(empty? Cenik) 0]
              [(string=? Jmeno (zbozi-nazev (first Cenik))) (zbozi-cena (first Cenik))]
              [else (najdiCenuProduktu Jmeno (rest Cenik))]
              )
            ]
          [define (isNonZero? x)
            (not (zero? x))
            ]
          [define (sum seznam)
            (if (empty? seznam) 0 (+ (first seznam) (sum (rest seznam))))
            ]
          [define (avg seznam)
            (/ (sum seznam) (max (length seznam) 1))
            ]
          )+
   
    (avg (filter isNonZero? (map (lambda (z) (najdiCenuProduktu Nazev (obchod-cenik z))) Obchody)))
    )
  )

(prumernaCena "Mouka hladká" test)