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

(deftest valid-rule-coll-test
  (is (= '(\a) (valid-rule-coll? '() '(\a))))
  (is (= nil (valid-rule-coll? '(#{\a}) '())))
  (is (= '() (valid-rule-coll? '() '())))
  (is (= nil (valid-rule-coll? '(#{\b}) '(\a))))
  (is (= '() (valid-rule-coll? '(#{\a} #{\a}) '(\a \a))))
  (is (= nil (valid-rule-coll? '(#{\a} #{\b}) '(\a \a))))
  (is (= nil (valid-rule-coll? '(#{\a} #{\a} #{\b}) '(\a \a))))
  (is (= '(\a) (valid-rule-coll? '(#{\a}) '(\a \a)))))

(defn valid-rule? [rule message]
  (cond
    (= \a rule (first message)) (rest message)
    (= \b rule (first message)) (rest message)
    (coll? rule) (valid-rule-coll? rule message)
    :else nil))

(deftest valid-rule-test
  (is (= '() (valid-rule? \a '(\a))))
  (is (= nil (valid-rule? \b '(\a))))
  (is (= '() (valid-rule? \b '(\b))))
  (is (= nil (valid-rule? \a '(\b))))
  (is (= '(\a) (valid-rule? \a '(\a \a))))

  (is (= '() (valid-rule? '(#{\a} #{\a}) '(\a \a))))
  (is (= nil (valid-rule? '(#{\a} #{\b}) '(\a \a))))
  (is (= nil (valid-rule? '(#{\a} #{\a} #{\b}) '(\a \a))))
  (is (= '(\a) (valid-rule? '(#{\a}) '(\a \a)))))

(defn valid-rule-set? [rule-set message]
  (if (empty? rule-set)
    (if (empty? message)
      message
      nil)
    (some #(valid-rule? % message) rule-set)))

(deftest valid-rule-set-test
  (is (= '() (valid-rule-set? #{} '())))
  (is (= '() (valid-rule-set? #{\a} '(\a))))
  (is (= nil (valid-rule-set? #{} '(\a))))
  (is (= nil (valid-rule-set? #{\a} '())))
  (is (= '() (valid-rule-set? #{'(#{\a}) '(#{\b})} '(\a))))
  (is (= '() (valid-rule-set? #{'(#{\a}) '(#{\b})} '(\b))))
  (is (= '(\a) (valid-rule-set? #{'(#{\a}) '(#{\b})} '(\a \a))))
  (is (= '(\b) (valid-rule-set? #{'(#{\a}) '(#{\b})} '(\b \b))))
  (is (= '() (valid-rule-set? #{'(#{\a} #{\a}) '(#{\b} #{\b})} '(\a \a))))
  (is (= '() (valid-rule-set? #{'(#{\a} #{\a}) '(#{\b} #{\b})} '(\b \b))))
  (is (= nil (valid-rule-set? #{'(#{\a} #{\a}) '(#{\b} #{\b})} '(\a \b))))
  (is (= nil (valid-rule-set? #{'(#{\a} #{\a}) '(#{\b} #{\b})} '(\a \b))))
  (is (= nil (valid-rule-set? #{'(#{\a} #{\a}) '(#{\b} #{\b})} '(\a))))
  (is (= nil (valid-rule-set? #{'(#{\a} #{\a}) '(#{\b} #{\b})} '(\b)))))

(defn valid? [rule-set message]
  (boolean
   (when-let [res (valid-rule-set? rule-set message)]
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

(deftest example1-test
  (is
   (valid?
    (denormalize-rule
     0 (parse-rules
        ["0: 4 1 5"
         "1: 2 3 | 3 2"
         "2: 4 4 | 5 5"
         "3: 4 5 | 5 4"
         "4: \"a\""
         "5: \"b\""]))
    (map identity "ababbb"))))

(deftest part1
  (is (= 168
         (let [rule-0 (denormalize-rule
                       0
                       (rules)
                       )]
           (->> (messages)
                (filter (partial valid? rule-0))
                count)))))

(comment
  (run-tests)


  )
