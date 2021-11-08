(ns day19
  (:require util
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests]]))

;; parsing

(defn parse-rule [line]
  (let [[num body] (str/split line #": ")
        body (case body
               "\"a\"" \a
               "\"b\"" \b
               (let [parts (str/split body #" \| ")]
                 (set (map (fn [part]
                             (map #(Integer/parseInt %) (str/split part #" ")))
                           parts))))]
    [(Integer/parseInt num) body]))

(defn parse-rules [lines]
  (into {} (map parse-rule) lines))

(defn parse-message [line]
  (map identity line))

(defn parse-messages [lines]
  (map parse-message lines))

(defn rules []
  (parse-rules
   (util/read-lines "input19_rules.txt")))

(defn messages []
  (parse-messages
   (util/read-lines "input19_messages.txt")

   ))

;; part 1

(defn denormalize-rule [rule-num rules]
  (let [body (get rules rule-num)]
    (set
     (if (#{\a \b} body)
       (list body)
       (map (fn [group]
              (map (fn [ref-rule-num]
                     (denormalize-rule ref-rule-num rules)
                     )
                   group))
            body)))))

(declare valid-rule-ref?)

(defn valid-rule-coll? [rule-coll rules message]
  (let [[first-rule-ref & rest-rule-refs] rule-coll]
    (if (empty? rule-coll)
      message
      (if-let [rest-message (valid-rule-ref? first-rule-ref rules message)]
        (recur rest-rule-refs rules rest-message)
        nil))))

(deftest valid-rule-coll-test
  (is (= '(\a) (valid-rule-coll? '() '(\a))))
  (is (= nil (valid-rule-coll? '(#{\a}) '())))
  (is (= '() (valid-rule-coll? '() '())))
  (is (= nil (valid-rule-coll? '(#{\b}) '(\a))))
  (is (= '() (valid-rule-coll? '(#{\a} #{\a}) '(\a \a))))
  (is (= nil (valid-rule-coll? '(#{\a} #{\b}) '(\a \a))))
  (is (= nil (valid-rule-coll? '(#{\a} #{\a} #{\b}) '(\a \a))))
  (is (= '(\a) (valid-rule-coll? '(#{\a}) '(\a \a)))))

(defn valid-char-rule? [rule message]
  (if (or (= \a rule (first message))
          (= \b rule (first message)))
    (rest message)
    nil))

(defn valid-rule-set? [rule-set rules message]
  (if (empty? rule-set)
    (if (empty? message)
      message
      nil)
    (some #(valid-rule-coll? % rules message) rule-set)))

(deftest valid-rule-set-test
  (is (= '() (valid-rule-set? #{} {} '())))
  (is (= '() (valid-rule-set? #{\a} {} '(\a))))
  (is (= nil (valid-rule-set? #{} {} '(\a))))
  (is (= nil (valid-rule-set? #{\a} {} '())))
  (is (= '() (valid-rule-set? #{'(#{\a}) '(#{\b})} {} '(\a))))
  (is (= '() (valid-rule-set? #{'(#{\a}) '(#{\b})} {} '(\b))))
  (is (= '(\a) (valid-rule-set? #{'(#{\a}) '(#{\b})} {} '(\a \a))))
  (is (= '(\b) (valid-rule-set? #{'(#{\a}) '(#{\b})} {} '(\b \b))))
  (is (= '() (valid-rule-set? #{'(#{\a} #{\a}) '(#{\b} #{\b})} {} '(\a \a))))
  (is (= '() (valid-rule-set? #{'(#{\a} #{\a}) '(#{\b} #{\b})} {} '(\b \b))))
  (is (= nil (valid-rule-set? #{'(#{\a} #{\a}) '(#{\b} #{\b})} {} '(\a \b))))
  (is (= nil (valid-rule-set? #{'(#{\a} #{\a}) '(#{\b} #{\b})} {} '(\a \b))))
  (is (= nil (valid-rule-set? #{'(#{\a} #{\a}) '(#{\b} #{\b})} {} '(\a))))
  (is (= nil (valid-rule-set? #{'(#{\a} #{\a}) '(#{\b} #{\b})} {} '(\b)))))

(defn valid-rule? [rule rules message]
  (cond
    (char? rule) (valid-char-rule? rule message)
    (set? rule) (valid-rule-set? rule rules message)
    :else (throw (ex-info "Don't know how to handle rule" {:rule rule :message message}))))

(deftest valid-rule-test
  (is (= '() (valid-rule? \a {} '(\a))))
  (is (= nil (valid-rule? \b {} '(\a))))
  (is (= '() (valid-rule? \b {} '(\b))))
  (is (= nil (valid-rule? \a {} '(\b))))
  (is (= '(\a) (valid-rule? \a {} '(\a \a))))

  (let [rules {}]
    (is (= '() (valid-rule? '(#{1} #{1}) '(\a \a))))
    (is (= nil (valid-rule? '(#{1} #{2}) '(\a \a))))
    (is (= nil (valid-rule? '(#{1} #{3} #{\1}) '(\a \a))))
    (is (= '(\a) (valid-rule? '(#{1}) '(\a \a))))))

(defn valid-rule-ref? [rule-ref rules message]
  (let [rule (get rules rule-ref)]
    (assert rule "Couldn't find rule")

    (valid-rule? rule rules message)))

(defn valid? [rule-ref rules message]
  (boolean
   (when-let [res (valid-rule-ref? rule-ref rules message)]
     (empty? res))))

(deftest valid-test
  (is (= true (valid? #{} '())))
  (is (= true (valid? #{\a} '(\a))))
  (is (= false (valid? #{} '(\a))))
  (is (= false (valid? #{\a} '())))
  (is (= true (valid? #{'(#{\a}) '(#{\b})} '(\a))))
  (is (= true (valid? #{'(#{\a}) '(#{\b})} '(\b))))
  (is (= false (valid? #{'(#{\a}) '(#{\b})} '(\a \a))))
  (is (= false (valid? #{'(#{\a}) '(#{\b})} '(\b \b))))
  (is (= false (valid? #{'(#{\a}) '(#{\b})} '(\a \a))))
  (is (= true (valid? #{'(#{\a} #{\a}) '(#{\b} #{\b})} '(\a \a))))
  (is (= true (valid? #{'(#{\a} #{\a}) '(#{\b} #{\b})} '(\b \b))))
  (is (= false (valid? #{'(#{\a} #{\a}) '(#{\b} #{\b})} '(\a \b))))
  (is (= false (valid? #{'(#{\a} #{\a}) '(#{\b} #{\b})} '(\a \b))))
  (is (= false (valid? #{'(#{\a} #{\a}) '(#{\b} #{\b})} '(\a))))
  (is (= false (valid? #{'(#{\a} #{\a}) '(#{\b} #{\b})} '(\b)))))

(declare rule->regexp)
(declare rule-ref->regexp)

(defn rule8->regexp [rule rules]
  (str (rule-ref->regexp 42 rules) "+"))

(defn rule11->regexp [rule rules]
  (for [i (range 20)]
    (concat (repeat i 42) (repeat i 31)))

  )

(defn rule-ref->regexp [rule-ref rules]
  (let [rule (get rules rule-ref)]
    (cond
      (= 8 rule) (rule8->regexp rule rules)
      (= 11 rule) (rule11->regexp rule rules)
      :else (rule->regexp rule rules))))

(defn rule-coll->regexp [rule-coll rules]
  (str/join (map #(str "(" (rule-ref->regexp % rules) ")") rule-coll)))

(defn rule-set->regexp [rule-set rules]
  (str/join "|" (map #(rule-coll->regexp % rules) rule-set)))

(defn rule->regexp [rule rules]
  (cond
    (char? rule) (str rule)
    (set? rule) (rule-set->regexp rule rules)))

(defn regexp [rule-ref rules]
  (java.util.regex.Pattern/compile (rule-ref->regexp rule-ref rules)))

(comment
  (rule-ref->regexp 0 {0 \a})
  (rule-ref->regexp 0 {0 #{[1 2]}
                       1 \a
                       2 \b})
  (rule-ref->regexp 0 {0 #{[1 2] [3 4]}
                       1 \a
                       2 \b
                       3 #{[1] [2]}
                       4 #{[1 2] [2 3]}})

  (regexp 0 {0 #{[1 2] [3 4]}
             1 \a
             2 \b
             3 #{[1] [2]}
             4 #{[1 2] [2 3]}})

  )

(deftest example1-test
  (is
   (valid?
    0 (parse-rules
       ["0: 4 1 5"
        "1: 2 3 | 3 2"
        "2: 4 4 | 5 5"
        "3: 4 5 | 5 4"
        "4: \"a\""
        "5: \"b\""])
    (map identity "ababbb"))

   ))

(deftest example1-regexp-test
  (is
   (re-matches
    (regexp
     0 (parse-rules
        ["0: 4 1 5"
         "1: 2 3 | 3 2"
         "2: 4 4 | 5 5"
         "3: 4 5 | 5 4"
         "4: \"a\""
         "5: \"b\""]))
    "ababbb")))

(deftest part1
  (is (= 168
         (let [rules* (rules)]
           (->> (messages)
                (filter (partial valid? 0 rules*))
                count)))))

(deftest part1-regexp
  (is (= 168
         (let [re (regexp 0 (rules))]
           (->> (messages)
                (map str/join)
                (filter #(re-matches re %))
                count)))))

(declare count-rule-set)

(defn count-rule-coll [rule-coll]
  (apply + (map count-rule-set rule-coll)))

(defn count-rule-set [rule-set]
  (let [cnt-set (set (map (fn [rule]
                            (cond
                              (char? rule) 1
                              (coll? rule) (count-rule-coll rule)
                              :else (throw (ex-info "failure" {:rule rule}))))
                          rule-set))]
    (assert (= 1 (count cnt-set)) cnt-set)
    (first cnt-set)))

(comment
  (run-tests)

  (rule-ref->regexp 42 (rules))

  (rule-ref->regexp 31 (rules))

  (re-matches #"(((a)(b))*)" "abababababab")

  (count-rule-set (denormalize-rule 42 (rules)))

  (count-rule-set (denormalize-rule 31 (rules)))

  (denormalize-rule 42 (rules))

  (count-rule-coll
   '(#{\a}
     #{(#{(#{\b}
           #{(#{(#{\b}
                 #{(#{(#{\a} #{\b}) (#{\a} #{\a})} #{\a})
                   (#{(#{\b} #{\b}) (#{\a} #{\a})} #{\b})})
                (#{\a}
                 #{(#{(#{\b} #{(#{\a}) (#{\b})}) (#{\a} #{\a})} #{\a})
                   (#{(#{\b} #{\a}) (#{\a} #{\a})} #{\b})})}
              #{\a})
             (#{(#{\b} #{(#{\b} #{(#{\a} #{\a})}) (#{\a} #{(#{\a} #{\a})})})
                (#{\a} #{(#{\b} #{(#{\b} #{(#{\a}) (#{\b})}) (#{\a} #{\a})})})}
              #{\b})})
          (#{\a}
           #{(#{\b}
              #{(#{\b}
                 #{(#{(#{\a} #{\b}) (#{\a} #{\a})} #{\a})
                   (#{(#{\b} #{(#{\a}) (#{\b})}) (#{\a} #{\a})} #{\b})})
                (#{\a}
                 #{(#{\b} #{(#{\b} #{\b}) (#{\a} #{(#{\a}) (#{\b})})})
                   (#{\a} #{(#{\b} #{\a}) (#{\b} #{\b})})})})
             (#{\a}
              #{(#{(#{\a} #{(#{\b} #{(#{\a}) (#{\b})}) (#{\a} #{\a})})
                   (#{\b} #{(#{\b} #{\b}) (#{\a} #{\b})})}
                 #{\a})
                (#{(#{(#{\a} #{\a})} #{\a})
                   (#{(#{\b} #{\a}) (#{\a} #{\a})} #{\b})}
                 #{\b})})})}
        #{\a})
       (#{(#{(#{\a}
              #{(#{(#{(#{\b} #{\a}) (#{\a} #{(#{\a}) (#{\b})})} #{\a})
                   (#{(#{\a} #{\b}) (#{\a} #{\a})} #{\b})}
                 #{\b})
                (#{(#{\a} #{(#{\b} #{\a}) (#{\a} #{(#{\a}) (#{\b})})})
                   (#{\b} #{(#{\b} #{(#{\a}) (#{\b})}) (#{\a} #{\a})})}
                 #{\a})})
             (#{\b}
              #{(#{(#{(#{\b} #{\a}) (#{(#{\a}) (#{\b})} #{\b})} #{\b})
                   (#{(#{\b} #{(#{\a}) (#{\b})}) (#{\a} #{\a})} #{\a})}
                 #{\a})
                (#{(#{\b} #{(#{\a} #{\a})})
                   (#{\a} #{(#{\b} #{\b}) (#{\a} #{(#{\a}) (#{\b})})})}
                 #{\b})})}
           #{\b})
          (#{(#{(#{(#{\b} #{(#{\a} #{\a})}) (#{\a} #{(#{\a} #{\a})})} #{\a})
                (#{(#{\b} #{(#{\a} #{\b}) (#{\a} #{\a})})
                   (#{\a} #{(#{\b} #{\b}) (#{\a} #{\b})})}
                 #{\b})}
              #{\b})
             (#{(#{\a}
                 #{(#{\a} #{(#{\b} #{\a}) (#{(#{\a}) (#{\b})} #{\b})})
                   (#{\b} #{(#{\b} #{\a}) (#{\a} #{(#{\a}) (#{\b})})})})
                (#{\b}
                 #{(#{(#{\a} #{\b})} #{\a})
                   (#{(#{\b} #{\a}) (#{\a} #{\a})} #{\b})})}
              #{\a})}
           #{\a})}
        #{\b})}))

  (def examples
    [
     "bbabbbbaabaabba"
     "babbbbaabbbbbabbbbbbaabaaabaaa"
     "aaabbbbbbaaaabaababaabababbabaaabbababababaaa"
     "bbbbbbbaaaabbbbaaabbabaaa"
     "bbbababbbbaaaaaaaabbababaaababaabab"
     "ababaaaaaabaaab"
     "ababaaaaabbbaba"
     "baabbaaaabbaaaababbaababb"
     "abbbbabbbbaaaababbbbbbaaaababb"
     "aaaaabbaabaaaaababaa"
     "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa"
     "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
     ])

  (count "bbabbbbaabaabba")

  (subs "bbabbbbaabaabba" 0 14)

  (map count examples)

  (->> "bbabbbbaabaabba"
       (partition 5)
       (map #(apply str %)))

  (str
   (rule-ref->regexp 42 (rules)))

  (re-matches (regexp 42 (rules)) "bbabbbba")

  (->> (for [i (range (count "bbabbbbaabaabba"))]
         (subs "bbabbbbaabaabba" 0 (inc i)))
       (map #(re-matches (regexp 42 (rules)) %)))

  (re-matches (regexp 31 (rules)) "abaabba")

  (def example-rules
    (parse-rules
     ["42: 9 14 | 10 1"
      "9: 14 27 | 1 26"
      "10: 23 14 | 28 1"
      "1: \"a\""
      "11: 42 31"
      "5: 1 14 | 15 1"
      "19: 14 1 | 14 14"
      "12: 24 14 | 19 1"
      "16: 15 1 | 14 14"
      "31: 14 17 | 1 13"
      "6: 14 14 | 1 14"
      "2: 1 24 | 14 4"
      "0: 8 11"
      "13: 14 3 | 1 12"
      "15: 1 | 14"
      "17: 14 2 | 1 7"
      "23: 25 1 | 22 14"
      "28: 16 1"
      "4: 1 1"
      "20: 14 14 | 1 15"
      "3: 5 14 | 16 1"
      "27: 1 6 | 14 18"
      "14: \"b\""
      "21: 14 1 | 1 14"
      "25: 1 1 | 1 14"
      "22: 14 14"
      "8: 42"
      "26: 14 22 | 1 20"
      "18: 15 15"
      "7: 14 5 | 1 21"
      "24: 14 1"]))

  (count-rule-set (denormalize-rule 42 example-rules))
  (count-rule-set (denormalize-rule 31 example-rules))

  (loop []
    (valid?
     0
     (map identity "ababbb"))
    )


  (map #(apply str %) (partition 2 "abababab"))

  (map (fn [msg]
         (loop [msg msg
                rule 42
                rules (rules)
                acc []]
           (if (seq msg)
             (let [rest (valid-rule-ref? rule rules msg)]
               (if rest
                 (recur rest rule rules (conj acc rule))
                 (if (= 42 rule)
                   (recur msg 31 rules acc)
                   acc)
                 )) acc)))
       (partition 8
                  #_"aaaaaaaabbbbbbbbababaabb"
                  "aaaaababaabbabaabbbabbab"
                  #_"bbbaababbaaaabababbbbaaa"
                  #_"ababbaaababaaaabbabaabab"
                  )
       )

  (->>
   (let [rules (rules)]
     (map (fn [msg]
            (map (fn [msg-part]
                   (cond
                     (valid-rule-ref? 42 rules msg-part) 42
                     (valid-rule-ref? 31 rules msg-part) 31
                     :else nil))
                 (partition-all 8 msg
                                #_"aaaaaaaabbbbbbbbababaabb"
                                #_"aaaaababaabbabaabbbabbab"
                                #_"bbbaababbaaaabababbbbaaa"
                                #_"ababbaaababaaaabbabaabab"
                                )
                 ))
          (util/read-lines "input19_messages.txt")
          )

     )
   (map #(split-with (partial = 42) %))
   (map #(map count %))
   (map #(let [[c42 c31] %
               valid? (and (<= 2 c42)
                           (<= 1 c31)
                           (<= (inc c31) c42))]
           [valid? c42 c31]))
   (filter first)
   count
   )
  ;; 277



  )
