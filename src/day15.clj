(ns day15)

(defn turn [{:keys [last seen idx] :as ctx}]
  (let [l last #_(last nums)
        s (get seen l)
        new (if s
              (- idx s)
              0)]
    (assoc ctx
           #_#_:nums (conj nums new)
           :last new
           :seen (assoc seen l idx)
           :idx (inc idx))))

(defn pour [nums]
  (into {}
        (map-indexed (fn [i item] [item (inc i)]))
        nums))

(defn init [start-nums]
  {:nums start-nums
   :last (last start-nums)
   :seen (pour (butlast start-nums))
   :idx (count start-nums)})

(defn play [turns start-nums]
  (let [i (init start-nums)]
    (->> (take (inc (- turns (:idx i))) (iterate turn i))
         last
         :last
         #_#_:nums
         last)))

(comment

  (turn
   (turn
    (init [0 3 6])))

  ;; example
  (play 100 [0 3 6])

  (play 2020 [1 3 2])
  (play 2020 [2 1 3])
  (play 2020 [1 2 3])
  (play 2020 [2 3 1])
  (play 2020 [3 2 1])
  (play 2020 [3 1 2])

  ;; input - part 1
  (play 2020 [2 20 0 4 1 17])

  ;; input - part 2
  ;; slow, but not too slow...
  (play 30000000 [2 20 0 4 1 17])

  )
