(ns sicp.c1.primes
  (:require [clojure.test :refer [deftest testing are run-tests]]))

(defn square [x] (* x x))

(defn divisible? [x y] (== (mod x y) 0))

(defn smallest-divisor [x]
  (letfn [(iter [current]
            (cond
              (> (square current) x) x
              (divisible? x current) current
              :else (recur (inc current))))]
    (iter 2)))

(defn prime? [x] (== x (smallest-divisor x)))

;; Fermat's Algorithm
;; 
;; If n is a prime number and a is any positive integer less 
;; than n, then a raised to the nth power is congruent 
;; to a modulo n:
;;
;; n <- prime number
;; a <- any integer < n
;; x <- a^n 
;; (mod x n) == (mod a n)

(defn exp-mod
  "computes x^y mod m"
  [x y m]

  (cond
    (== 0 y) 1
    (odd? y) (mod (*  x (exp-mod x (dec y) m)) m)
    :else (mod (square (exp-mod x (/ y 2) m)) m)))

;; (exp-mod 2 20 17) == 16

(defn fermat-test [n]
  (let [a (+ 1 (rand-int (dec n)))]
    (== (exp-mod a n n) (mod a n))))

(defn fast-prime?
  ([n] (fast-prime? n 10))
  ([n times]
   (every? (fn [_] (fermat-test n)) (range times))))

(deftest prime-test
  (testing "check both implementations"
    (are [n] (= (fast-prime? n) (prime? n))
      4
      3
      11
      25
      5
      59
      6
      142
      7
      335
      8
      796
      9
      1892
      10)))

(run-tests 'sicp.c1.primes)


