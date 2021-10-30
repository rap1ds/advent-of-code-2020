(ns day17
  (:require [clojure.set :as set]
            util))

(defn parse-row
  ([row y z] (parse-row row y z nil))
  ([row y z w]
   (let [base (if w
                [y z w]
                [y z])]
     (into []
           (comp
            (map-indexed (fn [x c] [(concat [x] base) c]))
            (remove #(= \. (second %)))
            (map first))
           row))))

(defn parse-plane
  ([lines z] (parse-plane lines z nil))
  ([lines z w]
   (set
    (apply concat
           (map-indexed
            (fn [y row]
              (parse-row row y z w))
            lines)))))

(defn neighbours [[x y z w]]
  (set
   (for [x* [(dec x) x (inc x)]
         y* [(dec y) y (inc y)]
         z* [(dec z) z (inc z)]
         w* (if w [(dec w) w (inc w)] [nil])
         :when (not= [x* y* z* w*] [x y z w])]
     (if w*
       [x* y* z* w*]
       [x* y* z*]))))

(defn new-state-active? [coord active-cubes]
  (let [active? (contains? active-cubes coord)
        active-neighbours (count (set/intersection active-cubes (neighbours coord)))]
    (cond
      (and active? (or (= 2 active-neighbours)
                       (= 3 active-neighbours)))
      true
      active?
      false

      (and (not active?) (= 3 active-neighbours))
      true

      (not active?)
      false)))

(defn turn [active-cubes]
  (let [all-neighbours (apply set/union (map neighbours active-cubes))]
    (into #{}
          (filter (fn [coord] (new-state-active? coord active-cubes)))
          all-neighbours)))

(comment
  (parse-row ".##" 0 0)

  (parse-plane
   [".#."
    "..#"
    "###"]
   0)

  (new-state-active? [0 0 0] (parse-plane [".#." "..#" "###"] 0))

  (-> (parse-plane [".#." "..#" "###"] 0)
      turn
      turn
      turn
      turn
      turn
      turn
      count)
  ;; #=> 112

  ;; part 1
  (-> (parse-plane (util/read-lines "input17.txt") 0)
      turn
      turn
      turn
      turn
      turn
      turn
      count)

  ;; example part 2
  ;;
  (-> (parse-plane [".#." "..#" "###"] 0 0)
      turn
      turn
      turn
      turn
      turn
      turn
      count)

  ;; part 2
  (-> (parse-plane (util/read-lines "input17.txt") 0 0)
      turn
      turn
      turn
      turn
      turn
      turn
      count)

  )
