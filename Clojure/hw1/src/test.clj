;task1
(println "task1")
(defn helper [param]
  (println param)
  )

(defn call-twice [func, param]
  (func param)
  (func param)
  )

(call-twice helper "123")

;task2
(println "\ntask2")
(defn readFile [filename]
  (let [data (slurp filename)]
    (print data)
    data
    )
  )

(readFile "test.txt")

;task3
(println "\ntask3")
(def cube-anonymous
  (fn [param]
    (* (* param param) param)
    )

  )

(println (cube-anonymous 5))

;task4
(println "\ntask4")
(defn concatReverse [seq1 seq2]
  (concat (reverse seq1) (reverse seq2))
  )

(println (concatReverse '(1 2 3) '(2 3 4)))

;task5
(println "\ntask5")
(defn in [seq elm]
  (some #(= elm %) seq)
  )

(println (in '(1 34 56) 1))

;task6
(println "\ntask6")
(defn differenPairs [seq1 seq2]
  (doseq [a seq1  b seq2]
    (when (not= a b)
      (println a b)
      )
    )
  )

(differenPairs '(1 2 3) '(1 4))

;task7
(println "\ntask7")
(defn repeatN [elem n]
  (seq (repeat n elem))
  )

(println (repeatN 5 4))
