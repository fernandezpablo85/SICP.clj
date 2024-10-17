(ns sicp.c1.exp
  (:require [clojure.test :refer [deftest testing are run-tests]]))

(defn square [x] (*' x x))

(defn rec-exp [b n]
  (if (== n 0) 1 (*' b (rec-exp b (dec n)))))

(defn iter-exp [b n]
  (letfn [(iter [n acc]
            (if (== n 0)
              acc
              (recur (dec n) (*' acc b))))]
    (iter n 1)))

(defn fast-exp [b n]
  (cond
    (== 0 n) 1
    (odd? n) (*' b (fast-exp b (dec n)))
    :else (square (fast-exp b (/ n 2)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 1.16 ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;   Design a procedure that does exponentiation in log(n) 
;;   operations and it's iterative (non-recursive).

(defn fast-exp-iter [b n]
  (letfn [(iter [b n acc]
            (cond
              (== 0 n) acc
              (odd? n) (recur b (dec n) (*' acc b))
              :else (recur (square b) (/ n 2) acc)))]
    (iter b n 1)))

;; Tests
(deftest exponentiations
  (testing "all impls"
    (are [expected n] (== expected (rec-exp 2 n) (iter-exp 2 n) (fast-exp 2 n) (fast-exp-iter 2 n))
      256 8
      128 7)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 1.17 & 1.18 ;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;   Create an procedure log(n) like 1.16 but for multiplication.
;;   1.17 - recursive
;;   1.18 - recursive-iterative

(defn twice [x] (* x 2))
(defn halve [x] (/ x 2))
(defn multiply [a b]
  (cond (== b 0) 0
        (odd? b) (+ a (multiply a (dec b)))
        :else (multiply (twice a) (halve b))))

;; Tests
(deftest multiplications
  (testing "test multiply"
    (are [a b] (== (multiply a b) (* a b))
      256 8
      128 7)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 1.19 ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;   Compute fib(n) in log(n) time using p-q substitution.

(defn slow-fib [n]
  (cond
    (== 0 n) 0
    (== 1 n) 1
    :else (+ (slow-fib (- n 1)) (slow-fib (- n 2)))))

(defn log-fib-iter [a b p q count]
  (cond
    (== 0 count)
    b
    (even? count)
    (let [p' (+ (square q) (square p))
          q' (+ (* 2 p q) (square q))]
      (log-fib-iter a b p' q' (/ count 2)))
    :else
    (log-fib-iter (+ (* b q) (* a q) (* a p))
                  (+ (* b p) (* a q))
                  p
                  q
                  (dec count))))

(defn log-fib [n]
  (log-fib-iter 1 0 0 1 n))

;; Tests
(deftest fibs
  (testing "test log-fib against oracle"
    (are [n] (== (slow-fib n) (log-fib n))
      1 2 3 4 5 6 7 8 9 10 11 12)))

(run-tests 'sicp.c1.exp)

;; slow-fib takes about ~1.5s on fib(35)
(time (slow-fib 35))
;; log-fib takes about 50µs on fib(35)
(time (log-fib 35))