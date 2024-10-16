(ns sicp.c1.primes
  (:require [clojure.test :refer [deftest testing are run-tests]]))

(defn square [x] (* x x))

(defn divisible? [x y] (== (mod x y) 0))

(defn smallest-divisor [x]
  (letfn [(next [n] (if (== 2 n) 3 (+ 2 n)))
          (iter [current]
            (cond
              (> (square current) x) x
              (divisible? x current) current
              :else (recur (next current))))]
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
      ;; 561 Carmichael Number 😲
      10)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 1.21 ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert (== (smallest-divisor 199) 199))
(assert (== (smallest-divisor 1999) 1999))
(assert (== (smallest-divisor 19999) 7))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 1.22 ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn runtime [] (long (/ (System/nanoTime) 1000)))


(defn report-prime [n elapsed]
  (println " *** ")
  (println n)
  (println elapsed)
  :true)

(defn start-prime-test [n start]
  (if (fast-prime? n)
    (report-prime n (- (runtime) start))))

(defn timed-prime-test [n]
  (start-prime-test n (runtime)))

(defn search-for-primes [start n]
  (if (== 0 n)
    (println "done")
    (if (timed-prime-test start)
      (search-for-primes (inc start) (dec n))
      (search-for-primes (inc start) n))))

;; (search-for-primes 10000 10)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 1.27 ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-congruent? [n]
  (map #(== (exp-mod % n n) %) (range 1 n)))

(every? identity (is-congruent? 561))
(every? identity (is-congruent? 1105))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 1.28 ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn exp-mod-root
  "computes x^y mod m and checks for non-trivial square roots"
  [x y m]

  (cond
    (== 0 y) 1
    (odd? y) (mod (*  x (exp-mod-root x (dec y) m)) m)
    :else
    (if (and (== (mod (square x) m) 1) (not= (mod x m) 1) (not= (mod x m) (- m 1)))
      0
      (mod (square (exp-mod-root x (/ y 2) m)) m))))

(defn miller-rabin?
  ([n] (miller-rabin? n 10))
  ([n times]
   (letfn [(rand-a [] (+ (rand-int (- n 1)) 1))
           (looks-prime? [] (== (exp-mod-root (rand-a) (- n 1) n) 1))]
     (every? identity (map (fn [_] (looks-prime?)) (range times))))))

(deftest carmichael-numbers-test
  (testing "checks that miller rabin catches charmicael composites"
    (are [n] (false? (miller-rabin? n 50))
      561
      1105
      1729
      2465
      2821
      6601
      8911
      10585
      15841
      29341)))

(run-tests 'sicp.c1.primes)
