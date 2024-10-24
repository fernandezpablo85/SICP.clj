(ns sicp.c1.newton-dx
  (:require [sicp.c1.avgd :as avg])
  (:require [sicp.c1.roots :as r]))

;; Newton’s method
(def dx 0.00001)

(defn deriv [f]
  #(/ (- (f (+ % dx)) (f %)) dx))

(defn cube [x] (* x x x))

(cube 4)
((deriv cube) 4)

(defn newtons-transform [f]
  #(- % (/ (f %) ((deriv f) %))))

(defn newtons-method [f guess]
  (r/fixed-point (newtons-transform f) guess))

(defn sqrt [x]
  (newtons-method #(- (* % %) x) 1.0))

(sqrt 16)

;; Abstractions and first-class procedures
(defn fixed-point-of-transform [f t guess]
  (r/fixed-point (t f) guess))

(defn sqrt' [x]
  (fixed-point-of-transform #(/ x %) avg/avg-damp 1.0))

(defn sqrt'' [x]
  (fixed-point-of-transform #(- (* % %) x) newtons-transform 1.0))

(sqrt   16)
(sqrt'  16)
(sqrt'' 16)