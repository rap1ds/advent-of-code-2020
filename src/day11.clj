(ns day11
  (:require [clojure.string :as str]))

(def sample
  "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(defn expand [n]
  [(dec n) n (inc n)])

(defn wh [matrix]
  (let [h (count matrix)
        w (count (first matrix))]
    [w h]))

(defn adjacent-points [matrix [x y]]
  (let [h (count matrix)
        w (count (first matrix))]
    (for [x* (expand x)
          y* (expand y)
          :when (not= [x y] [x* y*])
          :when (<= 0 x* (dec w))
          :when (<= 0 y* (dec h))]
      [x* y*])))

(defn row [matrix y]
  (nth matrix y))

(defn column [row x]
  (nth row x))

(defn val [matrix [x y]]
  (-> matrix
      (row y)
      (column x)))

(def matrix
  [[\L \L \.]
   [\L \L \.]
   [\. \L \.]])

(def matrix
  [[\L \L \.]
   [\L \L \.]
   [\. \. \.]])

(defn update-seat [val adjacent]
  (cond
    (and
     (= \L val)
     (not (contains? (set adjacent) \#)))
    \#

    (and (= \# val)
         (<= 4 (get (frequencies adjacent) \# 0)))
    \L

    :else val))

(defn map-matrix [f matrix]
  (mapv (fn [row y]
          (mapv (fn [val x]
                  (f val [x y])) row (range)))
        matrix
        (range)))

(def up [0 -1])
(def up-right [1 -1])
(def right [1 0])
(def down-right [1 1])
(def down [0 1])
(def down-left [-1 1])
(def left [-1 0])
(def up-left [-1 -1])

(def dirs [up
           up-right
           right
           down-right
           down
           down-left
           left
           up-left])

(defn inside? [[x y] [w h]]
  (and (<= 0 x (dec w))
       (<= 0 y (dec h))))

(defn next-point-at-dir [[dir-x dir-y] [x y] [w h]]
  (let [new-x (+ x dir-x)
        new-y (+ y dir-y)]
    (when (inside? [new-x new-y] [w h])
      [new-x new-y])))

(defn dir-occupied? [matrix [x y] dir]
  (let [next-point (next-point-at-dir dir [x y] (wh matrix))
        v (when next-point (val matrix next-point))]
    (cond
      (nil? next-point) 0

      (= v \#) 1

      (= v \L) 0

      :else
      (dir-occupied? matrix next-point dir))))

(defn adjacent-vals [matrix [x y]]
  (map #(val matrix %) (adjacent-points matrix [x y])))

(defn p [matrix]
  (println "")
  (println "---")
  (println (to-s matrix))
  (println "---")
  (println ""))

#_(println
   (update-seat
    (val matrix [1 1])
    (adjacent-vals matrix [1 1])))

(defn to-s [matrix]
  (->> matrix
       (map #(apply str %))
       (str/join "\n")))

(to-s matrix)

(p matrix)

(defn simulate-one [matrix]
  #_(println "----------------- Simulate ------------------")
  (map-matrix (fn [val [x y]]
                #_(println val [x y] (adjacent-vals matrix [x y]) (update-seat val (adjacent-vals matrix [x y])))
                (update-seat val (adjacent-vals matrix [x y])))
              matrix))


(defn update-seat [val adjacent]
  (cond
    (and
     (= \L val)
     (not (contains? (set adjacent) \#)))
    \#

    (and (= \# val)
         (<= 4 (get (frequencies adjacent) \# 0)))
    \L

    :else val))

(defn simulate-one2 [matrix]
  (map-matrix (fn [val [x y]]
                (if (= \. val)
                  \.
                  (let [score (apply + (map (fn [dir] (dir-occupied? matrix [x y] dir)) dirs))]
                    (cond
                      (and (= \L val)
                           (= 0 score))
                      \#

                      (and (= \# val)
                           (<= 5 score))
                      \L

                      :else val))))
              matrix))


(defn simulate [matrix]
  (iterate simulate-one matrix))

(defn simulate2 [matrix]
  (iterate simulate-one2 matrix))

(defn stable [matrix]
  (->> matrix
       simulate
       (partition 2 1)
       (filter (fn [[a b]] (= a b )))
       ffirst))

(defn stable2 [matrix]
  (->> matrix
       simulate2
       (partition 2 1)
       (filter (fn [[a b]] (= a b )))
       ffirst))


(defn parse-map [lines]
  (mapv vec (str/split-lines lines)))

(defn score [matrix]
  (get (frequencies (flatten matrix)) \# 0))

(comment
  (parse-map sample)


  (time
   (score (stable (parse-map sample))))

  (time
   (score (stable2 (parse-map sample))))


  ;; EI TOIMI KAY LAPI SAMPLE YKS KERRALLAAN
  (p (nth (simulate2 (parse-map sample)) 3))


  (time
   (score (stable (parse-map (util/read "input11.txt")))))

  (time
   (score (stable2 (parse-map (util/read "input11.txt")))))



  (->> (parse-map sample)
       simulate
       #_(partition 2 1)
       ((fn [c] (nth c 5)))
       #_(filter (fn [[a b]] (= a b )))
       #_first
       p)

  (first (filter (fn [[a b]] (= a b)) (partition 2 1 (simulate (parse-map sample)))))

  (first (filter (fn [[a b]] (= a b)) (partition 2 1 (take 6 (simulate (parse-map sample))))))

  (partition 2 1 (take 2 (simulate (parse-map sample))))

  (partition 2 1 (take 6 (simulate (parse-map sample))))

  )
