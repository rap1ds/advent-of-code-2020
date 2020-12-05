(ns day5
  (:require [util]
            [clojure.set :as set]))

(def rows (range 128))
(def cols (range 8))

(defn split [v]
  (split-at (/ (count v) 2) v))

(defn rows-half [rows dir]
  (let [f (case dir
            \F first
            \B second)]
    (f (split rows))))

(defn cols-half [cols dir]
  (let [f (case dir
            \L first
            \R second)]
    (f (split cols))))

(defn row [dir-seq]
  (first (reduce rows-half rows dir-seq)))

(defn col [dir-seq]
  (first (reduce cols-half cols dir-seq)))

(defn parse [seat-spec]
  (let [[_ row-dir-seq col-dir-seq]
        (re-matches #"([FB]{7})([LR]{3})" seat-spec)]
    [row-dir-seq col-dir-seq]))

(defn seat [seat-spec]
  (let [[row-dir-seq col-dir-seq] (parse seat-spec)
        r (row row-dir-seq)
        c (col col-dir-seq)]
    [r c (+ (* 8 r) c)]))

(comment
  ;; Tests
  (seat "BFFFBBFRRR")
  (seat "FFFBBBFRRR")
  (seat "BBFFBBFRLL"))

(defn part1 [input]
  (->> (map seat input)
       (map last)
       (apply max)))

(defn part2 [input]
  (let [seat-ids (->> (map seat input)
                      (map last)
                      sort)
        min-id (apply min seat-ids)
        max-id (apply max seat-ids)]
    (set/difference
     (set (range min-id (inc max-id)))
     (set seat-ids))))

(comment
  (def input (util/read-lines "input5.txt"))

  (part1 input)
  (part2 input))
