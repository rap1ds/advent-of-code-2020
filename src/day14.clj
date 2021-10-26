(ns day14
  (:require [clojure.string :as str]
            util))

(defn from-bits [str-bits]
  (BigInteger. str-bits 2))

(defn parse-line [line]
  (let [[op val] (str/split line #" = ")]
    (case op
      "mask" [:mask val]
      (let [[_ pos] (re-matches #"mem\[(\d+)\]" op)]
        [:mem (biginteger pos) (biginteger val)]))))

(defn parse-input []
  (map parse-line (util/read-lines "input14.txt")))

;; part 1 handlers

(defn parse-mask [mask-str]
  (let [and-mask (from-bits (str/replace mask-str #"X" "1"))
        or-mask (from-bits (str/replace mask-str #"X" "0"))]
    [and-mask or-mask]))

(defn apply-mask [mask val]
  (let [[and-mask or-mask] mask]
    (-> val
        (.and and-mask)
        (.or or-mask))))

(defn handle-mask [acc instr]
  (assoc acc :mask (parse-mask (second instr))))

(defn handle-mem [acc instr]
  (let [[_ pos val] instr]
    (assoc-in acc [:mem pos] (apply-mask (:mask acc) val))))

;; part 2 handlers
;;

(defn floats [mask-str]
  (->> mask-str
       reverse
       (map-indexed (fn [idx x]
                      [idx (= \X x)]))
       (filter second)
       (map first)))

(defn tree [x]
  (if-not (seq x)
    []
    (let [a [(first x) 0]
          b [(first x) 1]
          children (tree (rest x))]
      [[a children]
       [b children]])))

(defn parse-mask2 [mask-str]
  (let [or-mask (from-bits (str/replace mask-str #"X" "0"))
        float-poss (floats mask-str)]
    [or-mask (tree float-poss)]))

(declare apply-floats)

(defn apply-float [[[pos bit] children] val]
  (apply-floats children (if (= 0 bit)
                           (.clearBit val pos)
                           (.setBit val pos))))

(defn apply-floats [floats val]
  (if-not (seq floats)
    val
    (map (fn [f] (apply-float f val)) floats)))

(defn apply-mask2 [mask val]
  (let [[or-mask floats] mask
        or-applied (.or val or-mask)
        floats-applied (flatten (apply-floats floats or-applied))]
    floats-applied))

(defn handle-mask2 [acc instr]
  (assoc acc :mask (parse-mask2 (second instr))))

(defn handle-mem2 [acc instr]
  (let [[_ pos val] instr]
    (update
     acc
     :mem
     #(reduce (fn [m p] (assoc m p val))
              %
              (apply-mask2 (:mask acc) pos)))))

;; common

(defn apply-instructions [instructions handlers]
  (:mem
   (reduce
    (fn [acc instr]
      (let [handler-fn (get handlers (first instr))]
        (handler-fn acc instr)))
    {}
    instructions)))

(defn sum [mem]
  (->> mem
       (map val)
       (reduce + 0)))

(defn run [instructions handlers]
  (-> instructions
      (apply-instructions handlers)
      sum))

(comment
  ;; part 1 real input
  (run
    (parse-input)
    {:mask handle-mask
     :mem handle-mem})

  ;; part 2 example
  (run
    (map parse-line ["mask = 000000000000000000000000000000X1001X"
                     "mem[42] = 100"
                     "mask = 00000000000000000000000000000000X0XX"
                     "mem[26] = 1"])
    {:mask handle-mask2
     :mem handle-mem2})

  ;; part 2 real input
  (run
    (parse-input)
    {:mask handle-mask2
     :mem handle-mem2})

  )
