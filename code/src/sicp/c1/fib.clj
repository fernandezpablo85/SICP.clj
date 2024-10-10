(ns sicp.c1.fib)

;; golden ratio
(def φ (/ (+ 1 (Math/sqrt 5.0)) 2))

;; golden ratio equivalence
(== (Math/pow φ 2)
    (+ φ 1))

;; fib(n) is the closest integer to φ^n / sqrt(5)
(defn ratio-fib [n]
  (/ (Math/pow φ n) (Math/sqrt 5.0)))

;; tree-recursive fib
(defn fib [n]
  (cond (== 0 n) 0
        (== 1 n) 1
        :else (+ (fib (- n 1)) (fib (- n 2)))))


;; iterative fib
(defn ifib [n]
  (letfn [(iter-fib [a b count]
            (if (== count n)
              b
              (iter-fib (+ a b) a (inc count))))]
    (iter-fib 1 0 0)))


(map fib (range 0 9))
(map ifib (range 0 9))
(map ratio-fib (range 0 9))

;;  slow and fast fibs
(time (fib 35))
(time (ifib 45))
