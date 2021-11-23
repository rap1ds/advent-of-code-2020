(ns day23
  (:require [util]))

(defn parse [lines]
  (map #(read-string (str %)) (first lines)))

(def example (parse ["389125467"]))

(def input (parse (util/read-lines "input23.txt")))

#_(defn index [i]
    (rem i 9))

#_(defn get-by-index [cups i]
    (get cups (index i)))

#_(defn value->index [cups]
    (into {}
          (map-indexed (fn [i v] [v i]))
          cups))

#_(defn find-by-value [cups val]
    (get (value->index cups) val))

#_(defn pick-three [{:keys [curr-i cups] :as state}]

    )

(defn dest [label cups]
  (some (set [2 5 4 6 7]) [0])

  )

(defn dest [cups]
  (let [lowest 1
        highest 9
        curr (take 1 cups)
        pick (set (take 3 (drop 1 cups)))
        #_#_rest (filter (set (concat curr pick)) cups)
        #_#_~jj#_rev-rest (reverse (sort rest))]
    (loop [candidate (dec (first curr))]
      (cond
        (< candidate lowest) (recur highest)
        (contains? pick candidate) (recur (dec candidate))
        :else candidate

        )
      )
    #_(first (concat (drop-while #(> % (first curr)) rev-rest)
                     (take-while #(> % (first curr)) rev-rest)))))

(defn move [cups]
  (let [curr (take 1 cups)
        pick (take 3 (drop 1 cups))
        rest (take 5 (drop 4 cups))
        rev-rest (reverse (sort rest))
        dest (dest cups)
        #_#_dest (first (concat (drop-while #(> % (first curr)) rev-rest)
                                (take-while #(> % (first curr)) rev-rest)))
        next (concat
              (take-while #(not= % dest) rest)
              (take 1 (drop-while #(not= % dest) rest))
              pick
              (drop 1 (drop-while #(not= % dest) rest))
              curr
              #_#_#_#_(take-while #(= % dest) rest) pick (drop-while #(= % dest) rest) curr)
        #_#_next (concat (take-while #(= % dest) rest) pick (drop-while #(= % dest) rest) curr)]
    {:cups cups
     :curr curr
     :pick pick
     :rest rest
     :dest dest
     :next next}
    next))

(comment
  ;; part 1
  (= '(3 4 9 5 2 7 8 6)
     (take 8 (drop 1 (drop-while #(not= 1 %) (cycle (last (take (inc 100) (iterate move input))))))))


  ;; part 2

  (take 1 (iterate move (concat input (range 10 (inc 1000000)))))


  )
