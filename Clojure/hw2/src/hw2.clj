;factorial
(defmulti factorial int)
(defmethod factorial 0 [_] 1)
(defmethod factorial 1 [_] 1)
(defmethod factorial :default [n]
  (* n (factorial (- n 1))))

;(println (factorial 6))

;my-or
(defmacro my-or
  ([] nil)
  ([x] x)
  ([x & next]
    (list 'if x x (cons 'my-or next)))
  )

;(println (my-or false false false))

;my-let
(defmacro my-let
  [bindings & body]
    `(apply
      (fn ~(vec (take-nth 2 bindings)) ~@body)
      ~(vec (take-nth 2 (rest bindings)))
      )
  )

;(my-let [a 7 v 5] (print a))

(defn transferMoney [from to amount]
  (dosync
    (alter to + amount)
    (alter from - amount))
  )
