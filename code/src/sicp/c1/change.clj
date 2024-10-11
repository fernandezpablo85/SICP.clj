(ns sicp.c1.change
  (:require [clojure.test :refer [deftest testing is run-tests]]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; Change Example ;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn change [amount coins]
  (cond
    (zero? amount) 1
    (empty? coins) 0
    (neg? amount) 0
    :else (let [[coin & rest] coins]
            (+
             (change amount rest)
             (change (- amount coin) coins)))))

;; tests
(deftest change-test

  (testing "SICP Example"
    (is (= (change 100 [1 5 10 25 50]) 292)))

  (testing "Impossible"
    (is (= (change 3 [2 4]) 0))

    (testing "Zero"
      (is (= (change 0 [1 2 4]) 1)))))

(run-tests 'sicp.c1.change)
