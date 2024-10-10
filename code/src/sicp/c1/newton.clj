(ns sicp.c1.newton)

(def err 0.000001)

(defn square [x] (* x x))
(defn cube [x] (* x x x))

(defn avg [x y]
  (/ (+ x y) 2.0))

;; square root
(defn improve [guess x]
  (avg guess (/ x guess)))

(defn good-enough? [prev-guess current-guess]
  (< (Math/abs (- current-guess prev-guess)) err))

(defn sqrt-iter [guess x]
  (if (good-enough? guess (improve guess x))
    guess
    (recur (improve guess x) x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

(square (sqrt 16))

;; cubic root
(defn improve-cube [guess x]
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(defn cube-iter [guess x]
  (if (good-enough? guess (improve-cube guess x))
    guess
    (recur (improve-cube guess x) x)))

(defn cube-root [x]
  (cube-iter 1.0 x))

(cube (cube-root 9))

;; square root – lexical scoped
(defn lex-sqrt [x]

  (letfn [(improve [guess]
            (avg guess (/ x guess)))

          (good-enough? [prev-guess current-guess]
            (< (Math/abs (- current-guess prev-guess)) err))

          (sqrt-iter [guess]
            (let [next-guess (improve guess)]
              (if (good-enough? guess next-guess)
                guess
                (recur next-guess))))]

    (sqrt-iter 1.0)))

(lex-sqrt 16)

