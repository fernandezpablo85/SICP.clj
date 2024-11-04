(ns sicp.c2.rationals)

(defn gcd [a b]
  (if (== 0 b) a (recur b (mod a b))))

(defn make-rat [a b]
  ;; Exercise 2.1
  (when (< b 0) (make-rat (* -1 a) (* -1 b)))

  (let [gcd' (gcd a b)]
    [(/ a gcd') (/ b gcd')]))

(defn numer [x] (first x))

(defn denom [x] (second x))

(defn add-rat [x y]
  (make-rat
   (+ (* (numer x) (denom y))
      (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat
   (-
    (* (numer x) (denom y))
    (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(defn mult-rat [x y]
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat
   (* (numer x) (denom y))
   (* (numer y) (denom x))))

(defn equal-rat [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 2.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Segments on a plane

(defn make-segment [a b] [a b])

(defn start-segment [s] (first s))
(defn end-segment [s] (second s))

(defn make-point [x y] [x y])
(defn x [p] (first p))
(defn y [p] (second p))

(defn midpoint-segment [s]
  (let [x1 (x (start-segment s))
        y1 (y (start-segment s))
        x2 (x (end-segment s))
        y2 (y (end-segment s))]
    (make-point (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))))

(midpoint-segment (make-segment (make-point 0 0) (make-point 8 8)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 2.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Rectangles on a plane

;; V1 point-base-height
(defn make-rect [v b h]
  [v b h])

(defn vertex [r] (first r))
(defn base-length [r] (second r))
(defn height [r] (nth r 2))

(defn perimeter [r]
  (+
   (* 2 (base-length r))
   (* 2 (height r))))

(defn surface [r]
  (* (base-length r) (height r)))

(def r1 (make-rect (make-point 0 0) 2 3))
(perimeter r1)
(surface r1)

;; V2 diagonal-points
(defn make-rect' [p1 p2]
  (let [x1 (x p1)
        y1 (y p1)
        x2 (x p2)
        y2 (y p2)]
    {:p1 p1 :p2 p2, :height (Math/abs (- y2 y1)) :width (Math/abs (- x2 x1))}))

(make-rect' (make-point 0 0) (make-point 3 4))