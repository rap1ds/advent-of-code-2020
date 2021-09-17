(ns day10
  (:require [util]))

(def small-example [16 10 15 5 1 11 7 19 6 12 4])

(def large-example [28 33 18 42 31 14 46 20 48 47
                    24 23 49 45 19 38 39 11 1 32
                    25 35 8 17 7 9 4 2 34 10 3])

(defn adapter [output-joltage]
  {:input (set [(- output-joltage 3)
                (- output-joltage 2)
                (- output-joltage 1)])
   :output output-joltage})

(defn adapters [outputs]
  (set (map adapter outputs)))

(defn input->outputs [outputs]
  (reduce
   (fn [m output]
     (-> m
         (update (max 0 (dec (dec (dec output)))) (fnil conj #{}) output)
         (update (max 0 (dec (dec output))) (fnil conj #{}) output)
         (update (max 0 (dec output)) (fnil conj #{}) output)))
   {}
   outputs))

(input->outputs small-example)

(def counter (atom 0))

(defn log-progress! []
  (swap! counter inc)

  (when (zero? (mod @counter 1000))
    (println "Progress " @counter)))

(defn build-chain
  ([adapters] (build-chain (set adapters) (input->outputs adapters) [] 0))
  ([adapters input->outputs chain joltage]
   (let [outputs (get input->outputs joltage)
         new-chain (conj chain joltage)]
     (log-progress!)
     (cond
       (empty? adapters) new-chain

       (seq outputs) (->> outputs
                          (map #(build-chain (disj adapters %) input->outputs new-chain %))
                          (filter some?)
                          first)

       :else nil))))

(defn built-in-joltage [input]
  (+ 3 (apply max input)))

(defn diffs [input]
  (->> (conj (build-chain input) (built-in-joltage input))
       (partition 2 1)
       (map (fn [[a b]] (- b a)))
       frequencies))

(defn part1 [input]
  (let [diff-freqs (diffs input)
        diff1 (get diff-freqs 1 1)
        diff3 (get diff-freqs 3 1)]
    (* diff1 diff3)))

(comment
  (reset! counter 0)

  (diffs small-example)
  (diffs large-example)

  (time
   (part1 small-example))
  (time
   (part1 large-example))

  (time
   (do
     (part1 (util/read-longs "input10.txt"))))

  (count small-example)
  (count large-example)
  (count (util/read-longs "input10.txt"))

  )


(comment
  (def adapters (input->outputs large-example))
  (def input-joltage 0)
  (part1 (input->outputs large-example) 0)
  )



















small-example

(defn built-in-joltage [input]
  (+ 3 (apply max input)))

(built-in-joltage small-example)




























;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def small-example (sort [16 10 15 5 1 11 7 19 6 12 4]))

(def large-example (sort [28 33 18 42 31 14 46 20 48 47
                          24 23 49 45 19 38 39 11 1 32
                          25 35 8 17 7 9 4 2 34 10 3]))

(defn add-start-end [joltages]
  (concat [0] joltages [(+ 3 (apply max joltages))]))

(defn diffs [input]
  (->> input
       add-start-end
       sort
       (partition 2 1)
       (map (fn [[a b]] (- b a)))))

(defn part1 [input]
  (->> input
       diffs
       (frequencies)
       vals
       (apply *)))

(defn to-remove [input]
  (->> input
       add-start-end
       (partition 3 1)
       (filter (fn [[l _ r]] (< (- r l) 3)))
       (map second)))

(->> small-example
     add-start-end
     (partition 3 1)
     (filter (fn [[l x r]] (< (- r l) 3)))
     (map second))

(to-remove (apply sorted-set small-example))

(defn part2 [inp]
  (if-let [rs (seq (to-remove inp))]
    (apply + 1 (map (fn [r] (part2 (disj inp r))) rs))
    1))

(comment
  (println "---------------")
  (time
   (part2 (apply sorted-set large-example)))

  )

(count (partition-all 3 1 small-example))

(defn can-remove? [fst trd]
  (and fst
       trd
       (<= (- trd fst) 3)))

(defn solve2 [[fst snd trd & rest]]
  (cond
    (not trd) [fst [snd]]

    (can-remove? fst trd)
    [fst
     (solve2 (into [snd trd] rest))
     (solve2 (into [trd] rest))]

    :else
    [fst
     (solve2 (into [snd trd] rest))]))

(solve2 (add-start-end small-example))

(defn branches [[node left right]]
  (* (cond
       right 2
       left 1
       :else)
     ()
     )

  #_(if node
    (+ (if left (branches left) 0)
       (if right (+ 1 (branches right)) 0))
    0))

(branches [1])
(branches [1 [2]])
(branches [1 [2 [3]]])
(branches [1 [2] [3]])
(branches [1 [2 [3] [4]]])
(branches [2 [3] [4]])

(branches (solve2 (add-start-end small-example)))


(defn solve3 [inp]
  (let [[fst & rest1] inp
        [_ & rest2]   rest1
        [trd & rest3] rest2
        [frt] rest3]
    (println inp)
    (if (not fst)
      []
      (if (can-remove? fst trd)
        [fst

         (vec (distinct
               (concat
                [(solve3 rest1)]

                ;; snd remoed
                [(solve3 rest2)]

                ;; snd & trd removed
                (when (can-remove? fst frt)
                  [(solve3 rest3)])

                )))]
        [fst [(solve3 rest1)]]))))

(def ana (atom 0))

(declare solve3++)

(defn solve3+ [inp]
  (swap! ana inc)
  (let [[fst _ trd frt] inp]
    (if (not fst)
      []
      [fst
       (cond-> []
         true (conj (solve3+ (rest inp)))
         (can-remove? fst trd) (conj (solve3++ (rest (rest inp))))
         (can-remove? fst frt) (conj (solve3++ (rest (rest (rest inp))))))])))

(solve3+ [])
(solve3+ [1])
(solve3+ [1 2])
(solve3+ [1 2 3])
(=
 (solve3 [1 2 3 4])
 (solve3+ [1 2 3 4]))

(def solve3++ (memoize solve3+))

(time
 (do
   (reset! ana 0)
   (solve3++ (add-start-end sorted-input))
   :done))

(declare leaf-count)

(def leaf-count+ (memoize leaf-count))

(defn leaf-count [tree]
  (if (empty? (second tree))
    1
    (apply + (map leaf-count+ (second tree)))))

(leaf-count (solve3++ (add-start-end sorted-input)))
(leaf-count (solve3++ (add-start-end sorted-input)))
(time
 (leaf-count (solve3++ (add-start-end large-example))))

(time
 (leaf-count (solve3++ (add-start-end sorted-input))))

;; 43406276662336


(comment
  (def sorted-input (sort (util/read-longs "input10.txt")))
  (part1 (util/read-longs "input10.txt"))

  )
