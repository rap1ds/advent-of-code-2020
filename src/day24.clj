(ns day24
  (:require [util]))

(def dirs
  {:nw [1 1]
   :sw [1 -1]
   :se [-1 -1]
   :ne [-1 1]
   :w [2 0]
   :e [-2 0]})

(defn parse-line [line]
  (loop [acc []
         chars (seq line)]
    (let [one-char (take 1 chars)
          two-char (take 2 chars)]
      (cond
        (empty? chars) acc
        (= two-char [\n \w])
        (recur (conj acc (dirs :nw)) (drop 2 chars))
        (= two-char [\s \w])
        (recur (conj acc (dirs :sw)) (drop 2 chars))
        (= two-char [\s \e])
        (recur (conj acc (dirs :se)) (drop 2 chars))
        (= two-char [\n \e])
        (recur (conj acc (dirs :ne)) (drop 2 chars))
        (= one-char [\w])
        (recur (conj acc (dirs :w)) (drop 1 chars))
        (= one-char [\e])
        (recur (conj acc (dirs :e)) (drop 1 chars))

        :else (assert "not found")))))

(defn sum [xs]
  (apply map + xs))

(defn parse [lines]
  (map parse-line lines))

(def flip
  {:white :black
   :black :white})

(defn flip-all [input]
  (->> input
       (map sum)
       (reduce
        (fn [acc coord]
          (update acc coord (fnil flip :white)))
        {})))

(defn count-blacks [tiles]
  (count (filter #(= :black (second %)) tiles
                 )))

(defn part1 [input]
  (count-blacks (flip-all input)))

(defn neighbours [coord]
  (set (map #(sum [% coord]) (vals dirs))))

(defn flip-candidates [tiles]
  (let [black-tiles (keys (filter #(= :black (val %)) tiles))
        neighb (map neighbours black-tiles)]
    (set (apply concat black-tiles neighb))))

(defn move-one [tiles coord]
  (let [val (get tiles coord :white)
        black-nbs (:black (frequencies (map #(get tiles % :white) (neighbours coord))) 0)
        flip? (or
               (and (= :black val)
                    (or
                     (= 0 black-nbs)
                     (< 2 black-nbs)))
               (and (= :white val)
                    (= 2 black-nbs)))]
    (if flip?
      (flip val)
      val)))

(defn move [tiles]
  (reduce
   (fn [tiles* coord]
     (assoc tiles* coord (move-one tiles coord)))
   tiles
   (flip-candidates tiles)))

(comment

  (part1 (parse (util/read-lines "input24_example.txt")))

  (part1 (parse (util/read-lines "input24.txt")))

  (count-blacks
   (last (take (inc 100) (iterate move (flip-all (parse (util/read-lines "input24_example.txt")))))))

  (time
   (count-blacks
    (last (take (inc 100) (iterate move (flip-all (parse (util/read-lines "input24.txt"))))))))

  )
