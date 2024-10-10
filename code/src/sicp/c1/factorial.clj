(ns sicp.c1.factorial)

;; linear-recursive
(defn factorial [n]
  (if (== 1 n)
    1
    (* n (factorial (- n 1)))))

(factorial 7)

;; linear-iterative

(defn fact [n]
  (letfn [(iter [total counter]
            (if (> counter n)
              total
              (recur (* total counter) (inc counter))))]
    (iter 1 1)))

(fact 7)