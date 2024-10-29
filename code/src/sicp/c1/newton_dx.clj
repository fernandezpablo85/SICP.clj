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


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 1.40 ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 

(defn cubic [a b c]
  (fn [x]
    (+ (* x x x) (* a x x) (* b x) c)))

(defn zero-cubic []
  (newtons-method (cubic -4 -5 24) 1.0))

(zero-cubic)
((cubic -4 -5 24) (zero-cubic))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 1.41 ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 

(defn double [f]
  #(f (f %)))

(((double (double double)) inc) 5)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 1.42 ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 

(defn compose [f g]
  #(f (g %)))

((compose #(* % %) inc) 6)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 1.43 ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 

(defn repeated
  ([f n] (repeated f n identity))
  ([f n total]
   (if (== 0 n)
     total
     (repeated f (dec n) (compose f total)))))

;; loop-recur alternative
(defn repeated' [f n]
  (loop [i n
         total identity]
    (if (== 0 i)
      total
      (recur (dec i) (compose f total)))))

((repeated inc 4) 2)
((repeated' inc 4) 2)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 1.44 ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 

(defn smooth [f]
  (fn [x]
    (/ (+ (f x) (f (- x dx)) (f (+ x dx))) 3)))

((smooth #(* % % %)) 2)

(defn n-smooth [f n]
  ((repeated smooth n) f))

((n-smooth #(* % % %) 5) 2)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 1.45 ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 

(defn abs [x]
  (if (> x 0) x (* -1 x)))

(defn is-close? [a b]
  (< (abs (- a b)) 0.00001))

(defn fixed-point [f guess]
  (let [yguess (f guess)]
    (println "guess")
    (if (is-close? yguess guess)
      guess
      (fixed-point f yguess))))

(defn times [n t]
  (if (== t 1) n (* n (times n (dec t)))))

(defn log2 [x]
  (Math/floor (/ (Math/log x) (Math/log 2))))

;; you must avg-damp log2(n) times 
(defn root [n x]
  (fixed-point
   ((repeated' avg/avg-damp (log2 n)) #(/ x (times % (dec n))))
   1.0))
