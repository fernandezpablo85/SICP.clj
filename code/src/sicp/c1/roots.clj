(ns sicp.c1.roots)

(defn abs [n] (max n (- n)))

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

;; (broken-sqrt 16) – does not converge



