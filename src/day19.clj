(ns day19
  (:require util
            [clojure.string :as str]))

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

(defn parse-messages [lines]
  (map #(map identity %) lines))

(defn rules []
  (parse-rules
   (util/read-lines "input19_rules.txt")))

(defn messages []
  (parse-messages
   (util/read-lines "input19_messages.txt")))

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


(declare valid-rule-set?)

(defn valid-rule-coll? [rule message]
  (let [[first-rule & rest-rules] rule]
    (if (empty? rule)
      message
      (if-let [rest-message (valid-rule-set? first-rule message)]
        (recur rest-rules rest-message)
        nil))))

(defn valid-rule? [rule message]
  (cond
    (= \a rule (first message)) (rest message)
    (= \b rule (first message)) (rest message)
    (coll? rule) (valid-rule-coll? rule message)
    :else nil))

(defn valid-rule-set? [rule-set message]
  (some #(valid-rule? % message) rule-set))

(defn valid? [rule-set message]
  (boolean
   (when-let [res (valid-rule-set? rule-set message)]
     (empty? res))))

(comment
  ;; examples
  (valid?
   (denormalize-rule
    0 (parse-rules
       ["0: 4 1 5"
        "1: 2 3 | 3 2"
        "2: 4 4 | 5 5"
        "3: 4 5 | 5 4"
        "4: \"a\""
        "5: \"b\""]))
   (map identity "ababbb"))

  ;; part 1
  (= 168
     (let [rule-0 (denormalize-rule
                   0
                   (rules)
                   )]
       (->> (messages)
            (filter (partial valid? rule-0))
            count)))

  ;; part 2
  ;;
  (let [rule-0 (denormalize-rule
                0
                (rules)


                )]
    )

  ;; stack oveflow!
  (denormalize-rule 0
                    (-> (rules)
                        (assoc 82 #{(list 42) (list 42 8)})
                        (assoc 11 #{(list 42 31) (list 42 11 31)})
                        ))


  )
