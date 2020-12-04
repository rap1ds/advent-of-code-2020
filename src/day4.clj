(ns day4
  (:require [util]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [input]
  (->> input
       (partition-by empty?)
       (remove (comp empty? first))
       (map (fn [x] (mapcat #(str/split % #" ") x)))
       (map (fn [x] (into {} (map #(let [[k v] (str/split % #":")]
                                     [(keyword k) v]) x))))))

(def required-fields #{:byr
                       :iyr
                       :eyr
                       :hgt
                       :hcl
                       :ecl
                       :pid})

(defn part1 [passports]
  (filter #(set/subset? required-fields (set (keys %))) passports)
  )

(comment
  (def input (util/read-lines "input4.txt"))
  (def passports (parse input))
  )

