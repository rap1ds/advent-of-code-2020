(ns day2
  (:require [util]))

(defn parse-line [line]
  (let [[_ low high c pwd] (re-matches #"(\d+)-(\d+) (\w): (\w+)" line)]
    {:low (Integer/parseInt low)
     :high (Integer/parseInt high)
     :char (.charAt c 0)
     :pwd pwd}))

(defn valid1? [{:keys [low high char pwd]}]
  (<= low
      (count (filter #(= char %) pwd))
      high))

(defn part1 [parsed]
  (count (filter valid1? parsed)))

(defn xor [a b]
  (or (and a (not b))
      (and b (not a))))

(comment
  ;; test xor
  (xor true true)
  (xor true false)
  (xor false false)
  (xor false true))

(defn valid2? [{:keys [low high char pwd] :as line}]
  (xor (= char (.charAt pwd (dec low)))
       (= char (.charAt pwd (dec high)))))

(defn part2 [parsed]
  (count (filter valid2? parsed)))

(comment
  (def input (util/read-lines "input2.txt"))
  (def parsed (map parse-line input))

  (part1 parsed)
  (part2 parsed)
 )
