(ns sicp.c2.custom-cons)

;; no data structures, only procedures.

(defn cons' [a b]
  (fn [n]
    (cond
      (== 0 n) a
      (== 1 n) b
      :else (throw (IllegalAccessError. "wrong argument")))))

(defn car [x] (x 0))
(defn cdr [x] (x 1))

(car (cons' 1 2))
(cdr (cons' 1 2))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 2.4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Nested functions cons/car/cdr

(defn cons'' [x y]
  (fn [m] (m x y)))

(defn car'' [z]
  (z (fn [a _] a)))

(defn cdr'' [z]
  (z (fn [_ b] b)))

(car'' (cons'' 1 2))
(cdr'' (cons'' 1 2))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 2.5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Single number cons/car/cdr

(defn n-cons [x y]
  (* (int (Math/pow 2 x)) (int (Math/pow 3 y))))

(defn count-factors [n f]
  (loop [c 0 n n]
    (if (not= (mod n f) 0) c (recur (inc c) (/ n f)))))

(defn n-car [z]
  (count-factors z 2))

(defn n-cdr [z]
  (count-factors z 3))

(n-car (n-cons 3 6))
(n-cdr (n-cons 2 9))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 2.6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Church numerals

(def zero
  (fn [_] (fn [x] x)))

(def one
  (fn [f] (fn [x] (f x))))

(def two
  (fn [f] (fn [x] (f (f x)))))

(defn church->int [n]
  ((n inc) 0))

(defn add-1 [n]
  (fn [f] (fn [x] (f ((n f) x)))))

(defn sum [a b]
  (fn [f] (fn [x] ((a f) ((b f) x)))))

(church->int (sum two two))