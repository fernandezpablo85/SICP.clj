(ns sicp.c1.high-order
  (:require [clojure.test :refer [are deftest testing run-tests is]]))

;; summation
(defn sum [a b]
  (if (> a b)
    0
    (+ a (sum (inc a) b))))

;; summation of cubes
(defn cube [a] (* a a a))
(defn sum-cubes [a b]
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (inc a) b))))

;; summation of stuff that adds up to 1/8 π
(defn pi-sum [a b]
  (if (> a b)
    0
    (+ (/ 1 (* a (+ 2 a))) (pi-sum (+ 4 a) b))))

(deftest summation-tests
  (testing "sums"
    (is (= (sum 10 50) 1230)))

  (testing "cubes"
    (is (= (sum-cubes 10 20) 42075)))

  (letfn [(is-close
            ([a b] (is-close a b 0.01))
            ([a b delta] (< (Math/abs (- a b)) delta)))]
    (testing "sum pi"
      (is (is-close (* 8 (float (pi-sum 1 5100))) 3.14)))))

(run-tests 'sicp.c1.high-order)