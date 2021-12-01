(ns day23
  (:require [util]))

(defn parse [lines]
  (map #(read-string (str %)) (first lines)))

(def example (parse ["389125467"]))

(def input (parse (util/read-lines "input23.txt")))

(defn dest [cups lowest highest]
  (let [curr (take 1 cups)
        pick (set (take 3 (drop 1 cups)))]
    (loop [candidate (dec (first curr))]
      (cond
        (< candidate lowest) (recur highest)
        (contains? pick candidate) (recur (dec candidate))
        :else candidate))))

(defn move [lowest highest cups]
  (let [curr (take 1 cups)
        pick (take 3 (drop 1 cups))
        rest (drop 4 cups)
        dest (dest cups lowest highest)
        next (concat
              (take-while #(not= % dest) rest)
              (take 1 (drop-while #(not= % dest) rest))
              pick
              (drop 1 (drop-while #(not= % dest) rest))
              curr)]
    {:cups cups
     :curr curr
     :pick pick
     :rest rest
     :dest dest
     :next next}
    next))

;; part 2
(def part2-input (concat input (range 10 (inc 1000000))))

(def part2-example (concat example (range 10 (inc 1000000))))

(comment
  ;; part 1
  (= '(3 4 9 5 2 7 8 6)
     (take 8 (drop 1 (drop-while #(not= 1 %) (cycle (last (take (inc 100) (iterate (partial move 1 9) input))))))))

  (let [moves 10]
    (take 100 (drop 1 (drop-while #(not= 1 %) (cycle (last (take (inc moves) (iterate (partial move 1 1000000) part2-input))))))))

  (take 10 (drop-while #(not= 1 %) (cycle (last (take 1000 (iterate (partial move 1 1000000) part2-input))))))


  )

(defn linked-list-map [cups]
  (into {}
        (map vec)
        (partition 2 1 (concat cups (take 1 cups)))))

(linked-list-map
 input

 )

(defn remove-after [m at]
  (let [next (m at)
        new-next (m next)]
    [next
     (-> m
         (dissoc next)
         (assoc at new-next))]))

(defn add-after [m at v]
  (let [next (m at)]
    (-> m
        (assoc at v)
        (assoc v next))))

(defn move***** [lowest highest cups]
  (let [curr (take 1 cups)
        pick (take 3 (drop 1 cups))
        rest (drop 4 cups)
        dest (dest cups lowest highest)
        next (concat
              (take-while #(not= % dest) rest)
              (take 1 (drop-while #(not= % dest) rest))
              pick
              (drop 1 (drop-while #(not= % dest) rest))
              curr)]
    {:cups cups
     :curr curr
     :pick pick
     :rest rest
     :dest dest
     :next next}
    next))

(defn dest* [curr cups lowest highest]
  (loop [candidate (dec curr)]
    (cond
      (< candidate lowest) (recur highest)
      (contains? cups candidate) candidate
      :else (recur (dec candidate)))))

(defn move* [lowest highest [curr cups]]
  (let [[pick1 cups] (remove-after cups curr)
        [pick2 cups] (remove-after cups curr)
        [pick3 cups] (remove-after cups curr)
        dest (dest* curr cups lowest highest)
        cups (add-after cups dest pick1)
        cups (add-after cups pick1 pick2)
        cups (add-after cups pick2 pick3)]
    [(cups curr) cups]))

(defn cups-vec [cups start]
  (loop [next (cups start)
         v [start]]
    (if (= next start)
      v
      (recur (cups next) (conj v next)))))

(comment
  (= example (cups-vec (linked-list-map example) (first example)))


  (map
   #(cups-vec
     (second %)
     (first example))
   (let [m (linked-list-map example)]
     (take (inc 10) (iterate (partial move* 1 9) [3 m]))))

  (time
   (def part2-example-result
     (let [input part2-example
           curr (first input)
           m (linked-list-map input)
           result (second (last (take (inc 10000000) (iterate (partial move* 1 1000000) [curr m]))))]
       result)))

  (let [a (get part2-example-result 1)
        b (get part2-example-result a)]
    (* a b))

  ;; "Elapsed time: 52066.46674 msecs"
  (time
   (def part2-result
     (let [input part2-input
           curr (first input)
           m (linked-list-map input)
           result (second (last (take (inc 10000000) (iterate (partial move* 1 1000000) [curr m]))))]
       result)))

  (let [a (get part2-result 1)
        b (get part2-result a)]
    (* a b))

  )
