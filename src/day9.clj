(ns day9)

(def test-input
  [35
   20
   15
   25
   47
   40
   62
   55
   65
   95
   102
   117
   150
   182
   127
   219
   299
   277
   309
   576])

(def preample-len 5)

(def sum 6)
(def nums [1 2 3 4 5])

(defn sum-pairs [sum nums]
  (into #{}
        (for [a nums
              b nums
              :when (and (not= a b)
                         (= sum (+ a b)))]
          #{a b})))

(defn part1 [preample-len input]
  (->> input
       (partition (inc preample-len) 1)
       (map (fn [nums] [nums (sum-pairs (last nums) (butlast nums))]))
       (filter #(empty? (second %)))
       first
       first
       last))

(defn cont-sum [target nums]
  (let [[_ _ result]
        (reduce (fn  [[sum nums :as acc] x]
                  (cond
                    (= sum target) (reduced [sum nums acc])

                    (< sum target) [(+ sum x) (conj nums x)]

                    :else (reduced nil)))
                [0 []]
                nums)]
    (second result)))

(defn part2 [input target]
  (->> (partition-all (count input) 1 input)
       (map #(cont-sum target %))
       (remove nil?)
       first
       sort
       ((juxt first last))
       (apply +)))

(comment
  ;; Test
  (part1 5 test-input)

  (do
    (def input (util/read-bigints "input9.txt"))
    (part1 25 input))

  (do
    (def input (util/read-bigints "input9.txt"))
    (part2 input (part1 25 input)))

  )
