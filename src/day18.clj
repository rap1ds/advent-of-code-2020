(ns day18
  (:require util
            [clojure.test :refer [deftest is]]))

(defn parse-line [line]
  (->> line
       (map (fn [c]
              ({\1 1
                \2 2
                \3 3
                \4 4
                \5 5
                \6 6
                \7 7
                \8 8
                \9 9
                \0 0
                \+ '+
                \* '*
                \( 'open
                \) 'close}
               c)))
       (remove nil?)))

(defn parens* [line]
  (loop [line line
         group []]
    (let [fst (first line)
          rst (rest line)]
      (cond
        (not fst) [group rst]

        (= 'close fst) [group rst]

        (= 'open fst) (let [[subgroup rst] (parens* rst)]
                        (recur rst (conj group subgroup)))
        :else
        (recur rst (conj group fst))))))

(defn parens [line]
  (first (parens* line)))

(defn do-op [a op b]
  (condp = op
    '+ (+ a b)
    '* (* a b)))

(defn precedence [x]
  (cond
    (not (coll? x)) x

    (#{1} (count x)) [(precedence (first x))]

    (#{3} (count x))
    (let [[a op b] x]
      [(precedence a) op (precedence b)])

    :else
    (loop [x x
           acc []]
      (cond
        (not (coll? x)) x

        (#{1} (count x)) (conj acc (precedence (first x)))


        :else
        (let [[a op b & rest] x]
          (condp = op
            '+ (recur (conj rest [a op b]) acc)
            '* (recur (conj rest b) (conj acc (precedence a) op))))))))

(defn evaal [x]
  (if (coll? x)
    (loop [[a op b & rst] x]
      (if op
        (recur (conj rst (do-op (evaal a) op (evaal b))))
        (evaal a)))
    x))

(deftest precedence-test
  (is (= 3 (precedence 3)))
  (is (= [3] (precedence [3])))
  (is (= [3 '+ 1] (precedence [3 '+ 1])))
  (is (= [[[3 '+ 2] '+ 1]] (precedence [3 '+ 2 '+ 1])))
  (is (= [3 '* 2] (precedence [3 '* 2])))
  (is (= [3 '* 2 '* 1] (precedence [3 '* 2 '* 1])))
  (is (= [[3 '+ 2]] (precedence [[3 '+ 2]])))
  (is (= [[[[3 '+ 2] '+ 1]]] (precedence [[3 '+ 2 '+ 1]])))

  ;; examples
  (is (= 231 (evaal (precedence [1 '+ 2 '* 3 '+ 4 '* 5 '+ 6]))))
  (is (= 51 (evaal (precedence [1 '+ [2 '* 3] '+ [4 '* [5 '+ 6]]]))))
  (is (= 46 (evaal (precedence [2 '* 3 '+ [4 '* 5]]))))
  (is (= 1445 (evaal (precedence [5 '+ [8 '* 3 '+ 9 '+ 3 '* 4 '* 3]]))))
  (is (= 669060 (evaal (precedence [5 '* 9 '* [7 '* 3 '* 3 '+ 9 '* 3 '+ [8 '+ 6 '* 4]]]))))
  (is (= 23340 (evaal (precedence [[[2 '+ 4 '* 9] '* [6 '+ 9 '* 8 '+ 6] '+ 6] '+ 2 '+ 4 '* 2]))))
  )

(comment
  ;; example
  (evaal [1 '+ 2 '* 3 '+ 4 '* 5 '+ 6])
  ;; #=> 71

  ;; part 1
  (transduce
   (comp
    (map parse-line)
    (map parens)
    (map evaal))
   +
   (util/read-lines "input18.txt"))

  ;; #=> 45840336521334
  ;;

  ;; part 2
  (transduce
   (comp
    (map parse-line)
    (map parens)
    (map precedence)
    (map evaal))
   +
   (util/read-lines "input18.txt"))
  )
