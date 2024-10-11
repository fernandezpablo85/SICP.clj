(ns sicp.c1.pascal
  (:require [clojure.test :refer [deftest testing are run-tests]]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 1.11 ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;   n if n < 3
;;   f(n−1) + 2f(n−2) + 3f(n−3) if n >= 3

;; recursive
(defn f [n]
  (if (< n 3)
    n
    (+
     (* 1 (f (- n 1)))
     (* 2 (f (- n 2)))
     (* 3 (f (- n 3))))))

;; iterative 😮‍💨
(defn g [n]
  (letfn [(compute [a b c]
            (+ c (* 2 b) (* 3 a)))
          (iter [n a b c]
            (if (< n 3)
              c
              (recur (dec n) b c (compute a b c))))]
    (iter n 0 1 2)))

;; tests
(deftest f-and-g-test

  (testing "iterative and recursive"
    (are [expected n] (= expected (f n) (g n))
      4 3
      11 4
      25 5
      59 6
      142 7
      335 8
      796 9
      1892 10)))

(run-tests 'sicp.c1.pascal)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 1.12 ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 1.13 ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;