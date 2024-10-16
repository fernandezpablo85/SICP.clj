(ns sicp.c1.gcd)

;; Example: Euclid's GCD
(defn gcd [x y]
  (if (== 0 y) x (recur y (mod x y))))
