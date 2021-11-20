(ns day21
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [util :as util]))

(defn parse-line [line]
  (let [[list allergs] (str/split line #"\(contains ")]
    [(set (map keyword (str/split list #" ")))
     (when allergs
       (set (map keyword (str/split (apply str (drop-last allergs)) #", "))))]))

(defn parse [lines]
  (map parse-line lines))

(def example
  (parse
   ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
    "trh fvjkl sbzzf mxmxvkd (contains dairy)"
    "sqjhc fvjkl (contains soy)"
    "sqjhc mxmxvkd sbzzf (contains fish)"]))

(defn group-by-allergens [parsed]
  (reduce
   (fn [allrg-map [ingredients allrgs]]
     (reduce
      (fn [allrg-map allrg]
        (update allrg-map allrg conj ingredients)
        )
      allrg-map
      allrgs))
   {}
   parsed))

(defn remove-known [by-allrgs allrg ing]
  (into {}
        (map (fn [[k v]] [k (map (fn [ing-list] (disj ing-list ing)) v)]))
        (dissoc by-allrgs allrg)))

(defn remove-knowns [by-allrgs resolved]
  (reduce (fn [by-allrgs [allrg ing]]
            (remove-known by-allrgs allrg ing))
          by-allrgs
          resolved))

(defn resolve [by-allrgs]
  (into {}
        (->> by-allrgs
             (map (fn [[allrg ing-list]] [allrg (apply set/intersection ing-list)]))
             (filter #(= (count (second %)) 1))
             (map (fn [[allrg ing]] [allrg (first ing)])))))

(let [by-allrgs (group-by-allergens example)
      resolved (resolve by-allrgs)]
  (resolve (remove-knowns by-allrgs resolved))
  )



(defn resolve-all [parsed]
  (loop [known {}
         by-allrgs (group-by-allergens parsed)]
    (let [resolved (resolve by-allrgs)]
      (if (seq resolved)
        (recur (merge known resolved) (remove-knowns by-allrgs resolved))
        known))))

(defn part1 [parsed-input]
  (let [known (set (vals (resolve-all parsed-input)))]
    (->> parsed-input
         (map first)
         (map #(set/difference % known))
         (apply concat)
         count)))

(comment
  (part1 (parse (util/read-lines "input21.txt")))
  )

(defn part2 [parsed-input]
  (->> (vals (sort-by first (resolve-all parsed-input)))
       (map name)
       (str/join ",")))

(comment
  (part2 example)

  (part2 (parse (util/read-lines "input21.txt")))

  )
