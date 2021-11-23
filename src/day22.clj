(ns day22
  (:require [util]))

(defn parse-deck [lines]
  (map read-string lines))

(def p1-deck (parse-deck (util/read-lines "input22-p1.txt")))

(def p2-deck (parse-deck (util/read-lines "input22-p2.txt")))

(def example-p1 [9 2 6 3 1])

(def example-p2 [5 8 4 7 10])

(defn play [p1 p2]
  (cond
    (empty? p1) [:player2-won :p1-empty p2]
    (empty? p2) [:player1-won :p2-empty p1]
    :else (let [[p1-card & p1-deck] p1
                [p2-card & p2-deck] p2]
            (assert (not= p1-card p2-card) "Cards equal")

            (if (< p1-card p2-card)
              ;; p2 won
              (recur p1-deck (conj (vec p2-deck) p2-card p1-card))
              (recur (conj (vec p1-deck) p1-card p2-card) p2-deck)))))

(defn play-rec [p1 p2]
  (loop [decks-seen #{}
         p1 p1
         p2 p2]
    (cond
      (empty? p1) [:player2-won :p1-empty p2]
      (empty? p2) [:player1-won :p2-empty p1]
      (contains? decks-seen #{[p1 p2]}) [:player1-won :recursion p1]
      :else (let [[p1-card & p1-deck] p1
                  [p2-card & p2-deck] p2
                  new-decks (conj decks-seen #{[p1 p2]})]
              (assert (not= p1-card p2-card) "Cards equal")

              (if (and (<= p1-card (count p1-deck))
                       (<= p2-card (count p2-deck)))
                (let [sub-game-result (play-rec (take p1-card p1-deck)
                                                (take p2-card p2-deck))]
                  (case (first sub-game-result)
                    :player1-won
                    (recur new-decks (conj (vec p1-deck) p1-card p2-card) p2-deck)
                    :player2-won
                    (recur new-decks p1-deck (conj (vec p2-deck) p2-card p1-card))))

                (if (< p1-card p2-card)
                  ;; p2 won
                  (recur new-decks p1-deck (conj (vec p2-deck) p2-card p1-card))
                  (recur new-decks (conj (vec p1-deck) p1-card p2-card) p2-deck)))))))

(defn score [result]
  (let [[_winner _reason deck] result]
    (reduce + (map-indexed (fn [i card]
                             (* (inc i) card)) (reverse deck)))))

(comment
  ;; example
  (score (play example-p1 example-p2))

  ;; part 1
  (score (play p1-deck p2-deck))

  ;; part 2
  (score (play-rec example-p1 example-p2))

  (score (play-rec p1-deck p2-deck))

  )
