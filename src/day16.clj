(ns day16
  (:require util
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-range [range]
  (let [[start end] (str/split range #"-")]
    [(Integer/parseInt start) (Integer/parseInt end)]))

(defn parse-rule [rule]
  (let [[rule-name ranges] (str/split rule #": ")
        [first-range second-range] (str/split ranges #" or ")]
    [rule-name (parse-range first-range) (parse-range second-range)]))

(defn parse-ticket [ticket]
  (map #(Integer/parseInt %) (str/split ticket #",")))

(defn rules []
  (map parse-rule (util/read-lines "input16_rules.txt")))

(defn expand [[start end]]
  (set (range start (inc end))))

(defn valid-numbers-by-rules [rules]
  (map (fn [[n r1 r2]]
         [n (set/union (expand r1) (expand r2))])
       rules))

(defn valid-numbers [rules]
  (into #{}
        (mapcat second)
        (valid-numbers-by-rules rules)))

(defn analyze [cols rules]
  (map (fn [col]
         (->> rules
              (filter (fn [r] (empty? (set/difference col (second r)))))
              (map first)))
       cols))

(defn col-names [col-name-candidates]
  (->> col-name-candidates
       (map set)
       (map-indexed vector)
       (sort-by (comp count second))
       (reduce
        (fn [{:keys [resolved result]} v]
          (let [[id cands] v
                col-name (first (set/difference cands resolved))]
            {:resolved (conj resolved col-name)
             :result (conj result [id col-name])}))
        {})
       :result
       (sort-by first)))

(comment
  (util/read-lines "input16_rules.txt")
  (util/read-lines "input16_your.txt")
  (util/read-lines "input16_nearby.txt")

  ;; part 1
  (let [valid (valid-numbers (rules))]
    (->> (util/read-lines "input16_nearby.txt")
         (map (comp set parse-ticket))
         (map #(set/difference % valid))
         (mapcat seq)
         (reduce +)))

  (let [valid (valid-numbers (rules))
        your (vec (parse-ticket (first (util/read-lines "input16_your.txt"))))
        cols (->> (util/read-lines "input16_nearby.txt")
                  (map parse-ticket)
                  (filter #(empty? (set/difference (set %) valid)))
                  (apply map vector) ;; transpose
                  (map set))

        rules (valid-numbers-by-rules (rules))]
    (->> (analyze cols rules)
         col-names
         (filter (fn [[_id name]] (str/starts-with? name "departure")))
         (map first)
         (map (fn [id]
                (get your id)))
         (reduce *)))

  (analyze
   [#{3 15 5}
    #{9 1 14}
    #{18 5 9}]
   [["class" #{0 1 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19}]
    ["row" #{0 1 2 3 4 5 8 9 10 11 12 13 14 15 16 17 18 19}]
    ["seat" #{0 1 2 3 4 5 6 7 8 9 10 11 12 13 16 17 18 19}]]
   )

  (valid-numbers-by-rules (rules))

  )
