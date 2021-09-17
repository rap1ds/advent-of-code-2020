(ns day12
  (:require [clojure.set :as set]
            [util]))

(def north [0 -1])
(def east [1 0])
(def south [0 1])
(def west [-1 0])

(def start [[0 0] east])

(defn scale [[x y] v]
  [(* x v) (* y v)])

(defn translate [[x y] [x' y']]
  [(+ x x') (+ y y')])

(defn neg [[x y]]
  [(- x) (- y)])

(def left {north west
           west south
           south east
           east north})

(def right (clojure.set/map-invert left))

(defn turn [dir turn-dir val]
  (if (zero? val)
    dir
    (recur (turn-dir dir) turn-dir (- val 90))))

(defn turn-right [])

(defn new-state [[[x y] dir] [act val]]
  (case act
    N [(translate [x y] (scale north val)) dir]
    S [(translate [x y] (scale south val)) dir]
    E [(translate [x y] (scale east val)) dir]
    W [(translate [x y] (scale west val)) dir]
    F [(translate [x y] (scale dir val)) dir]
    L [[x y] (turn dir left val)]
    R [[x y] (turn dir right val)]))

(def demo-input "F10
N3
F7
R90
F11")

(defn parse [input]
  (map (fn [[_ i val]]
         [(symbol i) (Integer/parseInt val)])
       (re-seq #"(\w)(\d+)" input)))

(defn run-instructions [instructions]
  (reduce new-state start instructions))

(defn dist [[x y]]
  (+ (Math/abs x)
     (Math/abs y)))

(defn state->point [[[_x _y :as point] _dir]]
  point)

(comment
  ;; part 1
  (-> demo-input
      parse
      run-instructions
      state->point
      dist)

  (-> (util/read "input12.txt")
      parse
      run-instructions
      state->point
      dist)

  )

;; Part 2

(defn rotate-right [[x y]]
  [(- y) x])

(defn rotate-left [[x y]]
  [y (- x)])

(defn rotate-rel [[x y] [rel-x rel-y] rotate-dir val]
  (if (zero? val)
    [x y]
    (recur (-> [x y]
               (translate (neg [rel-x rel-y]))
               rotate-dir
               (translate [rel-x rel-y]))
           [rel-x rel-y]
           rotate-dir
           (- val 90))))

(defn new-state2 [[[x y] [wx wy]] [act val]]
  (case act
    N [[x y] (translate [wx wy] (scale north val))]
    S [[x y] (translate [wx wy] (scale south val))]
    E [[x y] (translate [wx wy] (scale east val))]
    W [[x y] (translate [wx wy] (scale west val))]
    F (let [tr (scale (translate [wx wy] (neg [x y])) val)]
        [(translate [x y] tr) (translate [wx wy] tr)])
    L [[x y] (rotate-rel [wx wy] [x y] rotate-left val)]
    R [[x y] (rotate-rel [wx wy] [x y] rotate-right val)]))

(def start2 [[0 0] [10 -1]])

(defn run-instructions2 [instructions]
  (reduce new-state2 start2 instructions))

(comment
  (-> demo-input
      parse
      run-instructions2
      state->point
      dist)

  (-> (util/read "input12.txt")
      parse
      run-instructions2
      state->point
      dist)

  )
