(ns day7
  (:require [clojure.string :as str]
            [util]))

(defn parse-children [children]
  (if (= "no other bags." children)
    {}
    (->> (str/split children #", ")
         (map #(let [[_ num color] (re-matches #"(\d+) (.*) bags?\.?" %)]
                 [color (Integer/parseInt num)]))
         (into {}))))

(defn parse-rule [rule]
  (let [[parent children] (str/split rule #" bags contain ")]
    [parent (parse-children children)]))

(defn parse-rules [rules]
  (reduce
   (fn [rules [parent children]]
     (-> rules
         (assoc-in [:parent->children parent] children)
         (update :child->parents (fn [child->parents]
                                   (reduce
                                    (fn [child->parents [child weight]]
                                      (assoc-in child->parents [child parent] weight))
                                    child->parents
                                    children)))))
   {}
   (map parse-rule rules)))

(defn can-contain [color rules]
  (let [direct (keys (get-in rules [:child->parents color]))
        in-direct (mapcat #(can-contain % rules) direct)]
    (into #{} (concat direct in-direct))))

(defn num-bags [color rules]
  (let [children (get-in rules [:parent->children color])]
    (apply + (map (fn [[c v]] (+ v (* v (num-bags c rules)))) children))))

(comment
  (def test-rules
    (str/split-lines
     "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."))

  (def parsed-test-rules (parse-rules test-rules))
  (can-contain "shiny gold" parsed-test-rules)

  (count (can-contain "shiny gold" (parse-rules (util/read-lines "input7.txt"))))
  (num-bags "shiny gold" (parse-rules (util/read-lines "input7.txt"))))
