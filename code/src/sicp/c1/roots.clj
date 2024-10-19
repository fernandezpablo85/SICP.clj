(ns sicp.c1.roots
  (:require [clojure.math :refer [log]]))

(defn is-close? [a b]
  (< (abs (- a b)) 0.001))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Half-Interval Method ;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn search [f a b]
  (let [midpoint (/ (+ a b) 2)]
    (if (is-close? a b)
      midpoint
      (let [y (f midpoint)]
        (if (> y 0) (search f a midpoint) (search f midpoint b))))))

(defn half-interval-method [f a b]
  (let [ya (f a) yb (f b)]
    (cond
      (and (neg? ya) (pos? yb)) (search f a b)
      (and (pos? ya) (neg? yb)) (search f b a)
      :else (throw (Exception. "invalid params")))))

(float (half-interval-method #(- (* 2 %) 10) 0 120))
(float (half-interval-method #(Math/sin %) 2 4))
(float (half-interval-method #(- (Math/pow % 3) (* 2 %) 3) 1 2))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Fixed Point ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn fixed-point [f guess]
  (let [yguess (f guess)]
    (if (is-close? yguess guess)
      guess
      (fixed-point f yguess))))

(float (fixed-point #(Math/cos %) 1))
(float (fixed-point #(+ (Math/sin %) (Math/cos %)) 1))

;; sqrt 
(defn broken-sqrt [x]
  (fixed-point #(/ x %) 1.0))

;; (broken-sqrt 16) – does not converge, oscillates between y1 and y3

;; average (x / y) with y => (y + (x / y)) / 2
(defn fixed-sqrt [x]
  (fixed-point #(/ (+ % (/ x %)) 2) 1.0))

(fixed-sqrt 16)

;; golden ratio apprx
(defn golden-ratio []
  (fixed-point #(+ 1 (/ 1 %)) 7))

(float (golden-ratio))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 1.36 ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn x-square-est [x]
  (println x)
  (/ (log 1000) (log x)))

(defn x-square-est-avg [x]
  (println x)
  (/ (+ x (/ (log 1000) (log x))) 2))


(fixed-point x-square-est-avg 5)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 1.37 ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cont-frac
  ([n d k] (cont-frac n d k 0))
  ([n d k i]
   (if (= i k)
     (/ (n i) (d i))
     (/ (n i) (+ (d i) (cont-frac n d k (inc i)))))))

(defn cont-frac-iter
  ([n d k] (cont-frac n d k 0))
  ([n d k i]
   (loop [i i
          total (/ (n i) (d i))]
     (if (= i k)
       total
       (recur (inc i) (/ (n i) (+ (d i) total)))))))

(defn one [_] 1.0)
(/ 1 (cont-frac one one 11))
(/ 1 (cont-frac-iter one one 11))