(ns day13
  (:require [clojure.string :as str]
            util))

(defn parse-timetable [timetable]
  (->> timetable
       (re-seq #"(\d+|x)")
       (map-indexed (fn [i [_ v]]
                      (when-not (= "x" v)
                        [(Integer/parseInt v) i])))
       (filter some?)))

(def test-earliest 939)
(def test-timetable (parse-timetable "7,13,x,x,59,x,31,19"))

(defn earliest-bus-per-id [earliest-t bus-id]
  (let [earliest-mult (inc (quot earliest-t bus-id))
        earliest-bus (* bus-id earliest-mult)]
    [bus-id earliest-bus (mod earliest-bus earliest-t)]))

(defn earliest-bus [earliest-t timetable]
  (->> timetable
       (map (partial earliest-bus-per-id earliest-t))
       (sort-by second)
       first))

(defn result [[bus-id _ wait-t]]
  (* bus-id wait-t))

(comment
  (let [[earliest-str timetable-str] (util/read-lines "input13.txt")
        earliest-t (Integer/parseInt earliest-str)
        timetable (->> timetable-str
                       parse-timetable
                       (map first))]
    [earliest-t timetable]
    (result (earliest-bus earliest-t timetable)))
  )

;; part 2

(defn equations-str [timetable]
  (->> timetable
       (map (fn [char [bus-id n]]
              (str bus-id char (when-not (zero? n) (str " - " n)) " = t"))
            "abcdefghijklmnopqrstu")))

(let [[earliest-str timetable-str] (util/read-lines "input13.txt")
      _earliest-t (Integer/parseInt earliest-str)
      timetable (parse-timetable timetable-str)]
  timetable
  (equations-str timetable))


;; "41a = t"
;; "37b - 35 = t"
;; "557c - 41 = t"
;; "29d - 43 = t"
;; "13e - 54 = t"
;; "17f - 58 = t"
;; "23g - 64 = t"
;; "419h - 72 = t"
;; "19i - 91 = t"

(defn solves? [mult const t]
  (zero? (mod (+ t const) mult)))

(defn solves-all? [eqs t]
  (every? (fn [[mult const]]
            (solves? mult const t)) eqs))

(defn solve-one [[mult const] incr]
  (first
   (for [a (map #(* incr %) (drop 1 (range)))
         :when (solves? mult const a)]
     a)))

(defn solve-all [eqs]
  (loop [eqs eqs
         incr 1]
    (if (seq eqs)
      (recur (rest eqs) (solve-one (first eqs) incr))
      incr)))

(comment

  1428

  ;; 17x = t
  ;; 13y = t + 2
  ;; 19z = t + 3

  (time
   (solve-all
    [[17 0]
     [13 2]
     [19 3]]))

  (time
   (solve-all
    [[41 0]
     [419 -72]
     #_[37 -35]
     #_[557 -41]
     #_[29 -43]
     #_[19 -91]
     #_[13 -54]
     #_[17 -58]
     #_[23 -64]
     #_[19 -91]]))

  )

(time
 (doall
  (for [c (range 100000000)
        :let [t (- (* 557 c) 41)]
        :when (solves-all? [[41 0]
                            [37 -35]
                            [557 -41]
                            [29 -43]
                            [13 -54]
                            [17 -58]
                            [23 -64]
                            #_[419 -72]
                            #_[19 -91]] t)]
    t)))

(comment
  (* 41
     37
     557
     29
     13
     17
     23)
  )

(let [x4 1]
  (/ x4 17)
  )

(+
(/ (* 1 37) 13) (+ 11 (* 13 2))
(/ 2 13))

(comment
  (- 17 13)

  ;; 17x +0y +0z -t = 0
  ;; 0x +13y +0z -t = 2
  ;; 0x +0y +19z -t = 3

  ;; t = 17x
  ;; t + 2 = 13y -> t = 13y - 2
  ;; t + 3 = 19z -> t = 19z - 3

  ;; 17x = t
  ;; 13y = t + 2
  ;; 19z = t + 3

  ;; =>

  ;; 13y = 17x + 2
  ;; 19z = 17x + 3



  ;; 4199x = 247t
  ;; 4199y = 323t + 646
  ;; 4199z = 221t + 663


  )


(comment
  (def bus2 (map #(+ (* 2 %) 0) (range)))
  (def bus5 (map #(+ (* 3 %) 1) (range)))
  (def bus3 (map #(+ (* 3 %) 2) (range)))

  (defn bus? [t id]
    (= (mod t id) 0))

  (for [t (range 500)]
    [t
     (if (bus? t 5) 'D '-)
     (if (bus? t 11) 'D '-)
     (if (bus? t 7) 'D '-)])

  (* 5 7 11)

  ;; ratkaisu 2, 5, 3
  ;; kaikki samassa: 30 välein
  ;; toistuu: 4,34,64 (4 + 30N välein)
  ;; a, b samassa 4 + (2 * 5)N
  ;; b, c samassa 5 + (5 * 3)N

  ;; ratkaisu 5, 7, 11
  ;; kaikki samassa: 385 välein
  ;; toistuu: 20 + 385n välein
  ;; a, b samassa 20 + (5 * 7)N
  ;; b, c samassa paikassa 21 + (7 * 11)N


  ;; ratkaisu 5, 11, 7
  ;; kaikki samassa: 385 välein
  ;; toistuu: 285 + 385n välein
  ;; a, b samassa 10 + (5 * 11)N
  ;; b, c samassa paikassa 55 + (11 * 7)N

  (mod (* 2 11) (* 2 7))


  ;; 1/2 parin erotus 7 - 5 => 2
  ;;
  ;; b ja c toistuu: 21, 98
  ;;
  (let [a 5
        b 7
        diff (- b a) ;; abs?
        ]
    (quot a diff))

  )
