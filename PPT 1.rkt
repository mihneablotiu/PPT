#lang racket

(provide (all-defined-out))

;; Un triplet pitagoreic primitiv (TPP) este format din 
;; 3 numere naturale nenule a, b, c cu proprietățile:
;;    a^2 + b^2 = c^2
;;    a, b, c prime între ele
;;
;; TPP pot fi generate sub formă de arbore (infinit) cu
;; rădăcina (3,4,5), pe baza a 3 transformări matriciale:
;;
;;      |-1 2 2|        |1 2 2|        |1 -2 2|
;; T1 = |-2 1 2|   T2 = |2 1 2|   T3 = |2 -1 2|
;;      |-2 2 3|        |2 2 3|        |2 -2 3|
;;
;;                         (3,4,5)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)      (21,20,29)     (5,12,13)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37) ..........................................
;;
;; unde:
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.
;;
;; În această reprezentare, TPP sunt indexate "de sus în jos",
;; respectiv "de la stânga la dreapta", rezultând ordinea:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) (35,12,37) ... etc.

;; Reprezentăm matricile T1, T2, T3 ca liste de liste:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Implementați o funcție care calculează produsul scalar
; a doi vectori X și Y (reprezentați ca liste).
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
; Utilizați recursivitate pe stivă.
(define (dot-product X Y)
  (if (null? X)
      0
      (+ (* (car X) (car Y)) (dot-product (cdr X) (cdr Y)))))


; TODO
; Implementați o funcție care calculează produsul dintre
; o matrice M și un vector V (puneți V "pe verticală").
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
; Utilizați recursivitate pe coadă.
(define (multiply-helper res M V)
  (if (null? M)
      res
      (multiply-helper (append res (list (dot-product (car M) V)) ) (cdr M) V)))

(define (multiply M V)
  (multiply-helper '() M V))


; TODO
; Implementați o funcție care primește un număr n și
; întoarce o listă numerică (unde elementele au valoarea
; 1, 2 sau 3), reprezentând secvența de transformări prin
; care se obține, plecând de la (3,4,5), al n-lea TPP
; din arbore.
; Ex: (get-transformations 8) întoarce '(2 1), adică
; al 8-lea TPP din arbore se obține din T1·T2·(3,4,5).
; Sunteți încurajați să folosiți funcții ajutătoare
; (de exemplu pentru determinarea nivelului din arbore 
; pe care se află n, sau a indexului minim/maxim de pe 
; nivelul respectiv, etc.)
(define (power n p)
  (if (zero? p)
      1
      (* n (power n (sub1 p)))))
      
(define (get-row acc row n)
  (if (>= acc n)
      row
      (get-row (+ acc (power 3 row))(+ row 1) n)))

(define (get-row-helper n)
  (get-row 0 0 n))

(define (get-right-index acc row)
  (if (= row 0)
      (+ acc 1)
      (get-right-index (+ acc (power 3 row)) (- row 1))))

(define (get-right-index-helper row)
  (get-right-index 0 (sub1 row)))

(define (get-left-index-helper row)
  (+ 1 (- (get-right-index-helper row) (power 3 (sub1 row)))))


(define (get-transformations-helper res n left-index right-index delta)
  (if (equal? (length res) (- (get-row-helper n) 1))
      (reverse res)
      (cond
        [(< n (+ delta left-index)) (get-transformations-helper (cons 1 res) n left-index (+ delta left-index) (/ (-  (+ delta left-index) left-index) 3))]
        [(and (> n (+ delta left-index)) (< n (- right-index delta))) (get-transformations-helper (cons 2 res) n (+ delta left-index) (- right-index delta) (/ (-  (- right-index delta) (+ delta left-index)) 3))]
        [else (get-transformations-helper (cons 3 res) n (+ (* 2 delta) left-index) right-index (/ (- right-index (+ (* 2 delta) left-index)) 3))]
        )
      ))

(define (get-transformations n)
  (define left-index (get-left-index-helper (get-row-helper n)))
  (define right-index (get-right-index-helper (get-row-helper n)))
  (define delta (/ (- right-index left-index) 3))
  (get-transformations-helper '() n left-index right-index delta))


; TODO
; Implementați o funcție care primește o listă Ts de 
; tipul celei întoarsă de get-transformations, respectiv 
; un triplet de start ppt și întoarce tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Utilizați recursivitate pe coadă.
(define (apply-matrix-transformations Ts ppt)
  (if (null? Ts)
      ppt
      (cond
        [(equal? (car Ts) 1) (apply-matrix-transformations (cdr Ts) (multiply T1 ppt))]
        [(equal? (car Ts) 2) (apply-matrix-transformations (cdr Ts) (multiply T2 ppt))]
        [else (apply-matrix-transformations (cdr Ts) (multiply T3 ppt))])))


; TODO
; Implementați o funcție care calculează al n-lea TPP
; din arbore, folosind funcțiile anterioare.
(define (get-nth-ppt-from-matrix-transformations n)
  (define ppt '(3 4 5))
  (apply-matrix-transformations (get-transformations n) ppt))
