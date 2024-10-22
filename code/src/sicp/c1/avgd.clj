(ns sicp.c1.avgd
  (:require [sicp.c1.roots :as r]))

(defn average [x y]
  (/ (+ x y) 2))

(defn avg-damp [f]
  #(average % (f %)))

;; sqrt(x) = y
;; y^2 = x
;; y * y = x
;; y = x / y
(defn sqrt [x]
  (r/fixed-point (avg-damp #(/ x %)) 1.0))

;; cube-root(x) = y
;; y^3 = x
;; y * y * y = x
;; y = x / (y * y) 
(defn cube-root [x]
  (r/fixed-point (avg-damp #(/ x (* % %))) 1.0))

(cube-root 8)