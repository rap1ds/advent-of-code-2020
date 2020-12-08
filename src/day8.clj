(ns day8
  (:require [clojure.string :as str]
            [util]))

(def instr "nop +2")

(defn parse-instr [pos line]
  (let [[_ op sign val] (re-matches #"([a-z]{3}) ([\+\-])(\d+)" line)]
    {:pos pos
     :op (keyword op)
     :arg ((case sign
             "+" +
             "-" -) (Integer/parseInt val))}))

(defn parse-instrs [lines]
  (vec (map-indexed parse-instr lines)))

(def test-input
  (str/split-lines
   "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"))

(comment
  (parse-instrs test-input)
  )

(defmulti exec-instr (fn [_ctx instr] (:op instr)))

(defmethod exec-instr :nop [{:keys [fix-ops-seen fix] :as ctx} instr]
  (if (and (= (first fix) :nop)
           (= (second fix) fix-ops-seen))
    (exec-instr (-> ctx
                    (update :fix-ops-seen inc)
                    (assoc :fixed instr))
                (assoc instr :op :jmp))
    (cond-> ctx
        true (update :pos inc)
        (= (first fix) :nop) (update :fix-ops-seen inc))))

(defmethod exec-instr :acc [ctx {:keys [arg]}]
  (-> ctx
      (update :pos inc)
      (update :acc + arg)))

(defmethod exec-instr :jmp [{:keys [fix-ops-seen fix] :as ctx} {:keys [arg] :as instr}]
  (if (and (= (first fix) :jmp)
           (= (second fix) fix-ops-seen))
    (exec-instr (-> ctx
                    (update :fix-ops-seen inc)
                    (assoc :fixed instr))
                (assoc instr :op :nop))
    (cond-> ctx
      true (update :pos + arg)
      (= (first fix) :jmp) (update :fix-ops-seen inc))))

(comment
  (exec-instr {:pos 0} {:op :nop})
  (exec-instr {:pos 0
               :acc 0} {:op :acc :arg 7})
  (exec-instr {:pos 0} {:op :jmp :arg -5})

  )

(defn exec [instrs fix]
  (loop [ctx {:acc 0
              :pos 0
              :fix-ops-seen 0
              :fix fix
              :stack []
              :seen #{}
              :instrs instrs}]
    (let [{:keys [pos]} ctx
          instr (nth instrs pos ::out-of-bounds)]
      (cond
        (= pos (count instrs)) (assoc ctx :exit-reason :success)
        (= instr ::out-of-bounds) (assoc ctx :exit-reason :out-of-bounds)
        (contains? (:seen ctx) pos) (assoc ctx :exit-reason :seen)
        :else (recur
               (-> ctx
                   (update :stack conj [(select-keys ctx [:acc :pos]) instr])
                   (exec-instr instr)
                   (update :seen conj pos)))))))

(defn exec-with-fix [instrs]
  (->> (for [fix-op [:jmp :nop]
             fix-op-count (range (count instrs))]
         (exec instrs [fix-op fix-op-count]))
       (filter #(= :success (:exit-reason %)))
       first))

(comment
  (def input (util/read-lines "input8.txt"))
  (def parsed (parse-instrs input))

  (exec parsed nil)

  (exec (parse-instrs test-input) nil)

  (exec-with-fix (parse-instrs test-input))

  (exec-with-fix parsed)

  )
