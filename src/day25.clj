(ns day25
  (:require [util])
  )

(def initial-subject 7)
(def initial-value 1)
(def div 20201227)

(defn perform [subject value]
  (rem (* value subject) div))

(defn perform-n [subject value n]
  (last (take (inc n) (iterate (partial perform subject) value))))

(defn find-loop-size [public-key]
  (ffirst (filter #(= public-key (second %)) (map-indexed (fn [size pk] [size pk]) (iterate (partial perform initial-subject) initial-value)))))

(defn encryption-key [pk1 pk2]
  (let [ls1 (find-loop-size pk1)
        ls2 (find-loop-size pk2)
        enc-ks (set
                [(perform-n pk2 initial-value ls1)
                 (perform-n pk1 initial-value ls2)])]
    (assert (= 1 (count enc-ks)) "same enc key")
    (first enc-ks)))

(def input (map read-string (util/read-lines "input25.txt")))

(comment
  ;; example
  (encryption-key 5764801 17807724)

  ;; part1
  ;; "Elapsed time: 5890.16211 msecs"
  (time
   (apply encryption-key input))

  )
