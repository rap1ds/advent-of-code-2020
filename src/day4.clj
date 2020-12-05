(ns day4
  (:require [util]
            [clojure.string :as str]
            [clojure.set :as set]))

;; byr (Birth Year) - four digits; at least 1920 and at most 2002.
;; iyr (Issue Year) - four digits; at least 2010 and at most 2020.
;; eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
;; hgt (Height) - a number followed by either cm or in:
;;   If cm, the number must be at least 150 and at most 193.
;;   If in, the number must be at least 59 and at most 76.
;; hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
;; ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
;; pid (Passport ID) - a nine-digit number, including leading zeroes.
;; cid (Country ID) - ignored, missing or not.

(defmulti parse-field first)

(defmethod parse-field :byr [[k v]]
  [k (Integer/parseInt v)])

(defmethod parse-field :iyr [[k v]]
  [k (Integer/parseInt v)])

(defmethod parse-field :eyr [[k v]]
  [k (Integer/parseInt v)])

(defmethod parse-field :hgt [[k v]]
  (if-let [[_ magnitude unit] (re-matches #"(\d+)(cm|in)" v)]
    [k [(Integer/parseInt magnitude) (keyword unit)]]
    [k v]))

(defmethod parse-field :ecl [[k v]]
  [k (keyword v)])

(defmethod parse-field :default [[k v]]
  [k v])

(defmulti validate-field first)

(defmethod validate-field :byr [[k v]]
  [k (and (int? v) (<= 1920 v 2002))])

(defmethod validate-field :iyr [[k v]]
  [k (and (int? v) (<= 2010 v 2020))])

(defmethod validate-field :eyr [[k v]]
  [k (and (int? v) (<= 2020 v 2030))])

(defmethod validate-field :hgt [[k v]]
  [k (when-let [[magnitude unit] (seq v)]
       (case unit
         :cm (<= 150 magnitude 193)
         :in (<= 59 magnitude 76)
         false))])

(defmethod validate-field :ecl [[k v]]
  [k (contains? #{:amb :blu :brn :gry :grn :hzl :oth} v)])

(defmethod validate-field :hcl [[k v]]
  [k (re-matches #"#[a-f0-9]{6}" v)])

(defmethod validate-field :pid [[k v]]
  [k (re-matches #"\d{9}" v)])

(defmethod validate-field :cid [[k v]]
  [k true])

(defn parse-passport-fields [pp]
  (into {}
        (map parse-field)
        pp))

(defn validate-passport-fields [pp]
  (into {}
        (map validate-field)
        pp))

(defn valid-passport? [pp]
  (every? second pp))

(defn parse [input]
  (->> input
       (partition-by empty?)
       (remove (comp empty? first))
       (map (fn [x] (mapcat #(str/split % #" ") x)))
       (map (fn [x] (map #(let [[k v] (str/split % #":")]
                            [(keyword k) v]) x)))
       (map parse-passport-fields)))

(def required-fields #{:byr
                       :iyr
                       :eyr
                       :hgt
                       :hcl
                       :ecl
                       :pid})

(defn part1 [passports]
  (count (filter #(set/subset? required-fields (set (keys %))) passports)))

(defn part2 [passports]
  (->> passports
       (filter #(set/subset? required-fields (set (keys %))))
       (map validate-passport-fields)
       (filter valid-passport?)
       count))


(comment
  (do
    (def input (util/read-lines "input4.txt"))
    (def passports (parse input)))

  (part1 passports)
  (part2 passports)

  ;; Tests

  (def invalid-passports (parse (str/split-lines
                                 "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")))

  (part2 invalid-passports)

  (def valid-passports (parse (str/split-lines "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")))

  (part2 valid-passports)

  )



