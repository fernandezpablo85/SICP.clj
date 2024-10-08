(ns sicp.c1.naming)

;; define
(def size 5)

;; use
(* size 20)

;; and so on
(def pi 3.14159)

(def radius 10)

(def circunference (* 2 pi radius))

;; procedures

(defn square [x] (* x x))

(square 10)

(defn sum-of-squares [x y] (+ (square x) (square y)))

(sum-of-squares 4 3)

;; conditionals

(defn abs-1 [x]
  (cond
    (> x 0) x
    :else (* -1 x)))

(defn abs-2 [x]
  (if (< x 0) (* -1 x) x))

;; exercises

;;  1.3
(defn sum-of-2-largest-sq [a b c]
  (if
   (< a b)
    (if (< c a) (sum-of-squares a b) (sum-of-squares c b))
    (if (< c b) (sum-of-squares a b) (sum-of-squares c a))))

(sum-of-2-largest-sq 3 3 2)

;; 1.4

(defn a-plus-abs-b [a b]
  ((if (> b 0) + -) a b))

(a-plus-abs-b 10 -2)