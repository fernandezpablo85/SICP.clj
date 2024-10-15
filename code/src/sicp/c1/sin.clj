(ns sicp.c1.sin
  (:require [clojure.test :refer [deftest are testing run-tests]]))

(defn abs [x]
  (if (< x 0) (* -1 x) x))

(defn cube [x] (* x x x))

(defn ident [s]
  (- (* 3 s) (* 4 (cube s))))

(defn sin [x]
  (if (< (abs x) 0.1)
    x
    (ident (sin (/ x 3.0)))))


;; tests

(deftest sin-test

  (testing "ours-vs-java-math"
    (are [n] (< (abs (- (Math/sin n) (sin n))) 0.01)
      1
      2
      3
      4
      5
      6
      7)))

(run-tests 'sicp.c1.sin)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 1.15 ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;   Q: how many times is `ident` called for 12.5?
;;
;;   A: 12.5 * (1 / 3)^n = 0.1
;;   A: (1 / 3)^n = 0.1 / 12.5
;;   A: n * log (1 / 3) = log(0.1 / 12.5)
;;   A: n = log(0.1 / 12.5) / log (1 / 3)
;;   A: n = 4.394920562153781
;;   A: It will be called 5 times (ceil previous step)

;;   Q: What's the order of growth as a function of `a`?
;;
;;   A: log(0.1 / a) / log (1 / 3)
;;   A: (log(0.1) - log(a)) / log (1 / 3)
;;   A: log(a)
;;   Both space and number of steps, since function is not tail-recursive

