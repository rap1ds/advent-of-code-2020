(ns day6
  (:require [util]
            [clojure.set :as set]))

(defn mapmap [f coll]
  (map (fn [x] (map f x)) coll))

(defn parse [input]
  (->> input
       (partition-by empty?)
       (filter (comp seq first))
       (mapmap (comp set seq))))


(defn part1 [parsed]
  (->> parsed
       (map #(apply set/union %))
       (map count)
       (apply +)))

(defn part2 [parsed]
  (->> parsed
       (map #(apply set/intersection %))
       (map count)
       (apply +)))

(comment
  (do
    (def input (util/read-lines "input6.txt"))
    (def parsed (parse input))
    )

  (part1 parsed)
  )
