(ns sicp.c1.ackermann)

(defn a [x y]
  (cond (== 0 y) 0
        (== 0 x) (* 2 y)
        (== 1 y) 2
        :else (recur (dec x) (a x (dec y)))))


(a 1 10)
(a 2 4)
(a 3 3)