(ns sicp.c1.high-order
  (:require [clojure.test :refer [deftest testing run-tests is are]]))

;; summation
(defn summation [a b]
  (if (> a b)
    0
    (+ a (summation (inc a) b))))

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

(defn is-close
  ([a b] (is-close a b 0.01))
  ([a b delta] (< (Math/abs (- a b)) delta)))

;; High Order Procedures
(defn sum [f next a b]
  (if (> a b)
    0
    (+ (f a) (sum f next (next a) b))))

(defn h-summation [a b] (sum identity inc a b))
(defn h-sum-cubes [a b] (sum cube inc a b))

(defn f-pi [a]
  (/ 1 (* a (+ 2 a))))
(defn h-pi-sum [a b] (sum f-pi (partial + 4) a b))

;; Integral
(defn integral [f a b dx]
  (letfn [(add-dx [x] (+ x dx))]
    (* (sum f add-dx (+ a (/ dx 2)) b) dx)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Exercise 1.29 ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Simpson's Rule for integration

(defn simpsons-rule
  ([f a b n] (simpsons-rule f a b n 0))
  ([f a b n k]
   (let [h (/ (- b a) n)
         y (fn [x k] (* x (f (+ a (* k h)))))]
     (cond
       (== k 0) (* (/ h 3) (+ (y 1 k) (simpsons-rule f a b n (inc k))))
       (== k n) (y 1 k)
       (even? k) (+ (y 2 k) (simpsons-rule f a b n (inc k)))
       :else (+ (y 4 k) (simpsons-rule f a b n (inc k)))))))


(defn iter-sum
  ([f next a b] (iter-sum f next a b 0))
  ([f next a b total]
   (if (> a b)
     total
     (recur f next (next a) b (+ (f a) total)))))

(iter-sum identity inc 1 10)

(deftest all-tests
  (testing "sums"
    (is (= (summation 10 50) 1230)))

  (testing "cubes"
    (is (= (sum-cubes 10 20) 42075)))

  (testing "sum pi"
    (is (is-close (* 8 (float (pi-sum 1 5100))) 3.14)))

  (testing "h-sums"
    (are [a b] (= (summation a b) (h-summation a b))
      3 4
      4 11
      5 25
      6 59))

  (testing "h-cubes"
    (are [a b] (= (sum-cubes a b) (h-sum-cubes a b))
      3 4
      4 11
      5 25
      6 59))

  (testing "h-sum-pi"
    (are [a b] (= (pi-sum a b) (h-pi-sum a b))
      3 4
      4 11
      5 25
      6 59))

  (testing "integral-cube"
    (is (is-close (integral cube 0 1 0.001) 0.25)))

  (testing "simpsons-cube"
    (is (is-close (float (simpsons-rule cube 0 1 20)) 0.25)))

  (testing "iter-sum vs sum"
    (is (= (iter-sum cube inc 1 10) (sum cube inc 1 10)))))


(run-tests 'sicp.c1.high-order)

