(ns day1
  (:require [util]))

(defn part1 [input]
  (first
   (for [a input
         b input
         :while (not= a b)
         :when (= 2020 (+ a b))]
     (* a b))))

(defn part2 [input]
  (first
   (for [a input
         b input
         c input
         :while (not= a b c)
         :when (= 2020 (+ a b c))]
     (* a b c))))

(comment
  (def input (util/read-lines "input1.txt"))
  (def parsed (map #(Integer/parseInt %) input))

  (part1 parsed)
  (part2 parsed)
  )
