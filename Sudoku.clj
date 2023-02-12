(ns user)
; Funckija izrabotena isto kako i shto e navedeno
(defn transform [matrix]
  (cond
    (empty? matrix) ()
    (seq? (first matrix)) (cons (transform (first matrix)) (transform (rest matrix)) )
    (= 0 (first matrix)) (cons #{1 2 3 4 5 6 7 8 9} (transform (rest matrix)))
    :else (cons (first matrix) (transform (rest matrix)))
  )
)
;;;;;;;;;;;;;;;;;;;
; go zima n tiot element od listata
(defn get [n lst]
  (cond
    (= 0 n) (first lst)
    :else (get (- n 1) (rest lst))
  )
)
;;;;;;;;;;;;;;;;;;;;;;
; moja implementacija na Funckcijata 'and' so samo dva argumenti
(defn myand [a b]
  (and a b)
)
;;;;;;;;;;;;;;;;;;
;Ovaa funckija proveruva dali moze da se postavi brojot k na pozicija (i,j) vo matrixot
;Prvo del od and proveruva dali ima ist broj kako k vo 9 kockata
; Vtoriot del od and proveruva dali psotoi vo i row ist broj kako k
;Tretiot del od and proveruiva dali postoi vo j column ist broj kako k
(defn check [k i j matrix]
 (and
       (reduce myand (for [i1 (range (* 3 (Math/floor (quot i 3))) (+ (* 3 (Math/floor (quot i 3))) 3) ) ]
         (reduce myand (for [j1 (range (* 3 (Math/floor (quot j 3))) (+ (* 3 (Math/floor (quot j 3))) 3) ) ]
          (cond
             (seq? (get (int j1) (get (int i1) matrix))) true
             :else (not (= k (get (int j1) (get (int i1) matrix)) ))
          )
         ))
        ))
  (reduce myand (for [x (range 9)]
   (not (= k (get j (get x matrix))))
  ))
  (reduce myand (for [x (range 9)]
   (not (= k (get x (get i matrix))))
  ))
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;In the i'th row and jth column put k in the matrix
(defn construct [k i j matrix]
  (cond
    (> i 0) (cons (first matrix) (construct k (- i 1) j (rest matrix)))

    (and (= i 0) ) (cons (construct k -1 j  (first matrix) ) (rest matrix))
    (and (= i -1) (> j 0)) (cons (first matrix) (construct k i (- j 1) (rest matrix) ) )
    (= j 0) (cons k (rest matrix))
    ;:else (construct k 0 j (first matrix))
    ;(= j 0) matrix
    ;(not (seq? matrix)) matrix
    ;(= 0 j) (cons i matrix)
    ;(= 0 i) (cons (construct k i j (cons (first matrix) () ))  )
    ;(and (> i 0) (seq? (first matrix))) (cons (first matrix) (construct k (- i 1) j (rest matrix)) )
    ;:else ()
    ;:else ()
    ;:else (cons (first matrix) (construct k i (- j 1) (rest matrix)))
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Funckcija povikana od SolveHelp
;Gi zema broevite od k i proveruva dali moze da se stavat vo mnatrixot
;Ako moze povikuva SolveHelp so toj broj
(defn solveRec [i j k matrix func]
  (cond
    (empty? k) false
    (and (check (first k) i j matrix) (and (def temp (func i (+ 1 j) (construct (first k) i j matrix) )) temp) ) temp
    ;:else false
    :else (solveRec i j (rest k) matrix func)
  )

)

;Go naogja slednata prazna pozicija i povikuva SolveRec so nego
(defn solveHelp [i j matrix]
  ;(println "----" i j matrix)
 (cond
   (= i 9) matrix
   (> j 8) (solveHelp (+ i 1) 0 matrix)
   (coll? (get j (get i matrix))) (solveRec i j (get j (get i matrix)) matrix solveHelp)
   (< j 9) (solveHelp i (+ j 1) matrix)
 )
)
;Povikuvaj ova funkcija so matrix koj treba da se reshi toest sudoku puzzle
(defn solve [matrix]
  (solveHelp 0 0 (transform matrix))
)


(transform '(
  (0 2 5 0 0 1 0 0 0)
  (1 0 4 2 5 0 0 0 0)
  (0 0 6 0 0 4 2 1 0)

  (0 5 0 0 0 0 3 2 0)
  (6 0 0 0 2 0 0 0 9)
  (0 8 7 0 0 0 0 6 0)

  (0 9 1 5 0 0 6 0 0)
  (0 0 0 0 7 8 1 0 3)
  (0 0 0 6 0 0 5 9 0)
))


(check 3 4 5 '(
  (0 2 5  0 0 1  0 0 0)
  (1 0 4  2 5 0  0 0 0)
  (0 0 6  0 0 4  2 1 0)

  (0 5 0  0 0 0  3 2 0)
  (6 0 0  0 2 0  0 0 9)
  (0 8 7  0 0 0  0 6 0)

  (0 9 1  5 0 0  6 0 0)
  (0 0 0  0 7 8  1 0 3)
  (0 0 0  6 0 0  5 9 0)
))


(construct 11 0 0 '(
  (0 2 5  0 0 1  0 0 0)
  (1 0 4  2 5 0  0 0 0)
  (0 0 6  0 0 4  2 1 0)

  (0 5 0  0 0 0  3 2 0)
  (6 0 0  0 2 0  0 0 9)
  (0 8 7  0 0 0  0 6 0)

  (0 9 1  5 0 0  6 0 0)
  (0 0 0  0 7 8  1 0 3)
  (0 0 0  6 0 0  5 9 0)
))

(solve '(
  (0 2 5  0 0 1  0 0 0)
  (1 0 4  2 5 0  0 0 0)
  (0 0 6  0 0 4  2 1 0)

  (0 5 0  0 0 0  3 2 0)
  (6 0 0  0 2 0  0 0 9)
  (0 8 7  0 0 0  0 6 0)

  (0 9 1  5 0 0  6 0 0)
  (0 0 0  0 7 8  1 0 3)
 (0 0 0  6 0 0  5 9 0)
))



(solve '(
  (7 0 2  4 0 0  0 0 5)
  (0 1 0  0 7 0  0 0 0)
  (0 0 0  0 9 2  8 0 7)

  (3 6 0  0 0 0  5 4 0)
  (0 2 9  3 0 4  1 7 0)
  (0 4 1  0 0 0  0 2 6)

  (6 0 3  9 4 0  0 0 0)
  (0 0 0  0 8 0  0 6 0)
  (2 0 0  0 0 1  9 0 4)
))

(solve '(
  (9 0 1  0 5 6  0 0 4)
  (0 4 0  0 9 0  0 0 0)
  (5 0 2  4 0 0  9 0 7)

  (1 8 0  0 0 0  4 0 0)
  (6 9 0  5 0 4  0 7 1)
  (0 0 4  0 0 0  0 5 9)

  (8 0 9  0 0 3  1 0 5)
  (0 0 0  0 1 0  0 2 0)
  (3 0 0  2 4 0  6 0 8)
))

(solve '(
  (0 0 0  0 3 0  0 1 4)
  (4 1 3  8 9 6  0 0 0)
  (0 0 0  0 4 0  0 0 0)

  (6 0 9  0 0 0  2 0 8)
  (2 7 0  9 6 0  0 0 0)
  (0 0 0  3 0 2  9 0 0)

  (9 8 0  4 0 0  0 0 3)
  (7 0 0  0 0 0  4 0 0)
  (0 4 5  0 2 9  1 0 0)
))
(solve '(
  (8 0 0  0 6 0  0 0 0)
  (0 2 0  0 0 0  9 0 0)
  (9 0 0  0 1 7  0 0 8)

  (0 5 0  0 7 4  0 1 0)
  (0 0 0  2 0 0  0 0 4)
  (7 0 0  6 0 0  0 0 0)

  (2 0 0  0 9 8  0 0 1)
  (0 0 3  0 0 0  0 5 0)
  (0 0 0  4 0 0  0 0 0)
))
