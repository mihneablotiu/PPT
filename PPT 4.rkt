#lang racket

(provide (all-defined-out))

;; Dacă ne interesează doar al n-lea TPP din arbore, este
;; convenabil să determinăm secvența de transformări care
;; conduce la acest TPP, așa cum am procedat până acum.
;;
;; În schimb, dacă ne interesează primele n TPP (sau în
;; general o secvență mai lungă de TPP) ar fi de preferat
;; crearea unui flux infinit care să le conțină pe toate
;; în ordine.
;;
;; Observăm că această ordine corespunde unei parcurgeri
;; BFS a arborelui infinit. Acesta este un BFS mai simplu
;; decât BFS-ul uzual
;; (https://en.wikipedia.org/wiki/Breadth-first_search),
;; întrucât succesorii unui TPP sunt automat triplete noi,
;; deci nu este necesar să verificăm dacă un nod a mai
;; fost sau nu vizitat.
;; 
;; Schema acestui BFS simplificat este:
;;  1. inițializăm coada de noduri care trebuie vizitate cu
;;     rădăcina arborelui (tripletul (3,4,5))
;;  2. adăugăm primul nod din coadă în rezultat
;;  3. adăugăm cei 3 succesori ai săi în coada de noduri
;;     care trebuie vizitate
;;  4. revenim la pasul 2 (întrucât construim un flux
;;     infinit, nu există condiție de oprire, și toate
;;     structurile sunt fluxuri: atât coada cât și
;;     rezultatul funcției BFS)

;; Vom refolosi matricile T1, T2, T3:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Aduceți aici (nu sunt necesare modificări) implementările
; funcțiilor dot-product și multiply din etapa 1 sau 2.
; Cele două funcții nu sunt re-punctate de checker, însă 
; sunt necesare generării succesorilor unui nod.
(define (dot-product X Y)
  (foldl (λ (x y result) (+ result (* x y))) 0 X Y))

(define (multiply M V)
  (map (λ (x) (dot-product x V)) M)) 

; TODO
; Definiți fluxul infinit de TPP folosind algoritmul descris
; (parcurgerea BFS a arborelui infinit).
; Funcție utilă: stream-append
; Folosiți cel puțin o formă de let.

; Ca să definim fluxul infinit de TPP, am urmat algoritmul
; descris mai sus. Am inițializat coada de noduri cu rădăcina
; arborelui (tripletul (3 4 5)). Dacă coada este goală, ar trebui să întoarcem
; streamul gol, dar acest lucru nu se va întâmpla niciodată pentru că
; stream-urile sunt infinite. Din acest motiv, construim mai departe streamul
; cu primul element din coadă și adăugăm în aceasta, cele 3
; posibilități în care putem merge din tripletul actual folosind
; funcția expand

(define ppt-stream-in-tree-order
  (let ppt-search ([states (stream-cons + empty-stream)])
        (let ([state (stream-first states)]
              [states (stream-rest states)])
          (stream-cons state
                       (ppt-search (stream-append states (expand state)))))))

(define (expand state)
  (append (list (multiply T1 state)) (list (multiply T2 state)) (list (multiply T3 state))))


;; Un alt mod de a genera TPP se folosește de perechi (g, h)
;; care indeplinesc condițiile:
;;    g, h impare
;;    g < h
;;    g, h prime între ele
;;
;; Nu întâmplător am ales aceste notații, teoria este aceeași
;; cu cea din spatele cvartetelor (g, e, f, h), pe care le
;; putem exprima și ca (g, (h-g)/2, (h+g)/2, h).
;;
;; Pentru a obține un TPP dintr-o pereche (g, h) se aplică
;; aceleași formule (dar le vom exprima în funcție de g și h):
;;    a = gh
;;    b = 2ef = (h - g)(h + g) / 2
;;      = (h^2 - g^2) / 2
;;    c = e^2 + f^2 = (h - g)^2 / 4 + (h + g)^2 / 4
;;      = (h^2 + g^2) / 2
;;
;; Acest mod de generare ne furnizează TPP în altă ordine
;; decât cea dată de parcurgerea în lățime a arborelui TPP.
;;
;; Noua ordine se obține parcurgând pe coloane diagrama:
;;                        h      
;;         3     5     7     9     11   .  .  .
;;    1  (1,3) (1,5) (1,7) (1,9) (1,11) .  .  .
;;    3        (3,5) (3,7)   -   (3,11) .  .  .
;;    5              (5,7) (5,9) (5,11) .  .  .
;; g  7                    (7,9) (7,11) .  .  .
;;    9                          (9,11) .  .  .
;;    .                                 .  .  .
;;    .                                    .  .
;;    .                                       .
;; (lipsește perechea (3,9), 3 și 9 nefiind prime între ele)
;;
;; Folosind această indexare, primele 6 TPP sunt:
;;    (3,4,5)                           - din perechea (1,3)
;;    (5,12,13), (15,8,17)              - din (1,5), (3,5)
;;    (7,24,25), (21,20,29), (35,12,37) - din (1,7), (3,7), (5,7)
;;
;; Ne propunem să definim fluxul infinit de TPP în ordinea de
;; mai sus. Acesta se bazează pe fluxul corespunzător de 
;; perechi (g, h), pe care îl generăm astfel:
;;  - pornim cu 2 fluxuri infinite:
;;    * G = 1, 3, 5, 7 ...
;;    * H = 3, 5, 7, 9 ... (întrucât g < h)
;;  - fluxul ordonat pe coloane va conține:
;;    * perechea compusă din cele mai mici numere din G și H
;;      (ex: (1,3))
;;    * apoi interclasarea (conform ordinii "pe coloane") între:
;;      - perechile compuse dintre minimul din G și restul din H
;;        (ex: (1,5), (1,7), (1,9) ...)
;;      - fluxul ordonat generat de restul lui G și restul lui H
;;        (ex: (3,5), (3,7), (5,7) ...)
;; Aceasta este abordarea generală, în urma căreia generăm toate
;; perechile, inclusiv pe cele de numere care nu sunt prime  
;; între ele. Perechile neconforme trebuie înlăturate ulterior
;; (utilizând funcția de bibliotecă gcd).


; TODO
; Definiți o funcție care primește 2 fluxuri numerice infinite
; G și H, și generează fluxul de perechi de câte un element 
; din G și unul din H ordonate conform metodei de mai sus.
; Condițiile ca g și h să fie impare, prime între ele, respectiv
; menținerea restricției g < h (cât timp urmați algoritmul) nu
; trebuie impuse în implementarea funcției pairs.
; Ele vor fi asigurate de definirea fluxurilor de mai jos prin:
;  - apelarea lui pairs exclusiv pe fluxurile
;    G = 1, 3, 5, 7 ... și H = 3, 5, 7, 9 ...
;  - eliminarea perechilor de numere neprime între ele (care 
;    există în rezultatul funcției pairs, dar nu vor mai exista
;    în fluxul gh-pairs-stream)


; funcție ajutătoare care construiește un stream de count elemente
; format din perechi unde elementul din H este constant (minimul de
; la acel moment și primele count elemente din G)
(define (next-pairs G min-H count)
  (stream-take
   (stream-map (λ(x) (cons x min-H)) G)
   count))

; fiecare pas din funcția de generare este format de generarea unei coloane
; pentru început avem count = 1 deoarece ne dorim generarea unui singur element
; pe coloană apoi o să crească în funcție de numărul coloanei la care am ajuns.
; Stream-ul nostru este construit din alăturarea primelor count perechi unde
; elementul din H este constant, iar elementul din G crește de la valoarea minimă
; până la cea imediat alăturată lui H și apelul recursiv cu restul elementelor din
; G și din H.
(define (pairs G H)
  (let next-step ([count 1] [initial-G G] [G G] [H H])
    (stream-filter (λ(x) (not (stream? x)))
                   (stream-cons
                        empty-stream
                        (stream-append
                         (next-pairs initial-G (stream-first H) count)
                         (next-step (+ count 1) initial-G (stream-rest G) (stream-rest H)))))))


; TODO
; Definiți fluxul de perechi (g, h) pe care se bazează noua
; indexare a TPP.
; Nu folosiți recursivitate explicită (decât pentru a genera
; fluxurile de pornire - G și H).

; echivalent cu map de la liste, deoarece stream-map nu funcționează
; decât pentru un singur stream.
(define (stream-zip-with f s1 s2)
  (if (stream-empty? s1) s2
      (stream-cons (f (stream-first s1) (stream-first s2))
                   (stream-zip-with f (stream-rest s1) (stream-rest s2)))))

; am pornit de la fluxul numerelor naturale definit implicit
(define (naturals-from n)
  (stream-cons n (naturals-from (+ n 1))))

(define G (stream-zip-with + (naturals-from 0) (stream-rest (naturals-from 0))))
(define H (stream-zip-with + (naturals-from 1) (stream-rest (naturals-from 1))))

(define gh-pairs-stream
   (stream-filter (λ(x) (= (gcd (car x) (cdr x)) 1)) (pairs G H)))


; TODO
; Definiți fluxul de TPP corespunzător fluxului anterior de
; perechi (g, h).

; transformam din stream-ul de perechi in stream-ul de triplete
; pentru inceput transformam din stream-ul de perechi intr-un
; stream de liste de cate doua elemente pentru a putea folosi
; apoi functionala apply care primeste o functie cu doi parametrii
; si intoarce o lista cu 3 elemente din cele 2 initiale
(define ppt-stream-in-pair-order
  (stream-map (curry apply (λ (g h) (list (* g h) (/ (- (expt h 2) (expt g 2)) 2) (/ (+ (expt g 2) (expt h 2)) 2)) )) (stream-map (λ(x) (list (car x) (cdr x))) gh-pairs-stream)))

