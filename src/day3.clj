(ns day3
  (:require [util]))

(defn traverse [input right down]
  (map nth (map cycle (take-nth down input)) (map #(* right %) (range))))

(defn tree-count [input right down]
  (->> (traverse input right down)
       (filter #(= \# %))
       count))

(defn part2 [input]
  (*
   (tree-count input 1 1)
   (tree-count input 3 1)
   (tree-count input 5 1)
   (tree-count input 7 1)
   (tree-count input 1 2)))

(defn part1 [input]
  (tree-count input 3 1))

(comment
  (def input (util/read-lines "input3.txt"))

  (part1 input)
  (part2 input)

  )

