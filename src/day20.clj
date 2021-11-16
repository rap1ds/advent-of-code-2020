(ns day20
  (:require util))

(defn- parse-tile [[title & rest]]
  (let [i (read-string (second (re-matches #"Tile (\d+):" title)))]
    [i (map seq rest)]))

(defn- parse [lines]
  (->> lines
       (partition-by empty?)
       (remove (comp empty? first))
       (map parse-tile)
       (into {})))

(defn- fingerprint [border]
  #{border (reverse border)})

(defn- borders [tile]
  (let [top (first tile)
        bottom (last tile)
        left (map first tile)
        right (map last tile)]
    [top right bottom left]))

(def example (parse (util/read-lines "input20_examples.txt")))

(def input (parse (util/read-lines "input20.txt")))

(defn- context [tiles]
  (-> (reduce
       (fn [ctx [id tile]]
         (let [[top right bottom left] (borders tile)]
           (-> ctx
               (assoc-in [:tiles id] tile)
               (update-in [:rels (fingerprint top)] conj [id :top top])
               (update-in [:rels (fingerprint bottom)] conj [id :bottom bottom])
               (update-in [:rels (fingerprint left)] conj [id :left left])
               (update-in [:rels (fingerprint right)] conj [id :right right]))))
       {:tiles {}
        :rels {}}
       tiles)
      (update :rels (fn [rels]
                      (reduce
                       (fn [rels [[rel1-id rel1-dir] [rel2-id rel2-dir :as rel2]]]
                         [[rel1-id rel1-dir] [rel2-id rel2-dir]]
                         (cond-> rels
                           true (assoc-in [rel1-id rel1-dir] rel2-id)
                           rel2 (assoc-in [rel2-id rel2-dir] rel1-id)))
                       {}
                       (vals rels))))))

(defn- map-keys [f m]
  (into {}
        (map (fn [[k v]] [(f k) v]))
        m))

(defn- tile-flip-v [tile]
  (mapv (comp vec reverse) tile))

(defn- flip-v [ctx tile-id]
  (-> ctx
      (update-in [:tiles tile-id] tile-flip-v)
      (update-in [:rels tile-id] #(map-keys {:left :right
                                             :right :left
                                             :top :top
                                             :bottom :bottom} %))))

(defn- tile-flip-h [tile]
  ((comp vec reverse) tile))

(defn- flip-h [ctx tile-id]
  (-> ctx
      (update-in [:tiles tile-id] tile-flip-h)
      (update-in [:rels tile-id] #(map-keys {:left :left
                                             :right :right
                                             :top :bottom
                                             :bottom :top} %))))

(defn- tile-transpose [tile]
  (apply mapv vector tile))

(defn- transpose [ctx tile-id]
  (-> ctx
      (update-in [:tiles tile-id] tile-transpose)
      (update-in [:rels tile-id] #(map-keys {:top :left
                                             :right :bottom
                                             :left :top
                                             :bottom :right} %))))

(defn- tile-rotate-r [tile]
  (-> tile
      tile-transpose
      tile-flip-v))

(defn- rotate-r [ctx tile-id]
  (-> ctx
      (transpose tile-id)
      (flip-v tile-id)))

(defn- rotate-l [ctx tile-id]
  (-> ctx
      (flip-v tile-id)
      (transpose tile-id)))

(defn- corners [ctx]
  (keys
   (filter (fn [[_ dirs]]
             (= 2 (count (remove nil? (vals dirs)))))
           (:rels ctx))))

(defn- top-left-corner [ctx]
  (let [corner-rels (select-keys (:rels ctx) (corners ctx))
        top-lefts (filter (fn [[_ {:keys [left top]}]] (and (nil? left) (nil? top))) corner-rels)]
    (assert (= 1 (count top-lefts)) "Top left")
    (key (first top-lefts))))

(defn- align-right [ctx left right]
  (let [curr-dir (ffirst (filter (fn [[_ id]] (= id left)) (get-in ctx [:rels right])))
        new-ctx (case curr-dir
                  :left ctx
                  :top (rotate-l ctx right) ;; FLIP?
                  :right (flip-v ctx right)
                  :bottom (rotate-r ctx right))
        [_ left-right-border] (borders (get-in new-ctx [:tiles left]))
        [_ _ _ right-left-border] (borders (get-in new-ctx [:tiles right]))]
    (cond
      (= left-right-border right-left-border) new-ctx
      (= left-right-border (reverse right-left-border)) (flip-h new-ctx right)
      :else (throw (ex-info "borders dont match" {})))))

(defn- move-right [ctx tile-id]
  (get-in ctx [:rels tile-id :right]))

(defn- move-down [ctx tile-id]
  (get-in ctx [:rels tile-id :bottom]))

(defn- align-bottom [ctx top bottom]
  (let [curr-dir (ffirst (filter (fn [[_ id]] (= id top)) (get-in ctx [:rels bottom])))
        new-ctx (case curr-dir
                  :top ctx
                  :right (rotate-l ctx bottom)
                  :bottom (flip-h ctx bottom)
                  :left (rotate-r ctx bottom))
        [_ _ top-bottom-border] (borders (get-in new-ctx [:tiles top]))
        [bottom-top-border] (borders (get-in new-ctx [:tiles bottom]))]
    (cond
      (= top-bottom-border bottom-top-border) new-ctx
      (= top-bottom-border (reverse bottom-top-border)) (flip-v new-ctx bottom)
      :else (throw (ex-info "borders dont match" {})))))

(defn- arrange-row [ctx left]
  (println "left: " left)
  (if-let [right (get-in ctx [:rels left :right])]
    (do
      (println "right:" right)
      (recur (align-right ctx left right) right))
    ctx))

(defn- arrange-col [ctx top]
  (if-let [bottom (get-in ctx [:rels top :bottom])]
    (recur (align-bottom ctx top bottom) bottom)
    ctx))

(defn- arrange-square [ctx top-left]
  (let [right (get-in ctx [:rels top-left :right])
        bottom (get-in ctx [:rels top-left :bottom])]
    (if (and (nil? right) (nil? bottom))
      ctx
      (let [new-ctx (-> ctx
                        (arrange-row top-left)
                        (arrange-col top-left))
            diag1 (get-in new-ctx [:rels right :bottom])
            diag2 (get-in new-ctx [:rels bottom :right])]

        (assert (= diag1 diag2) (str "Diagonals " diag1 ", " diag2))
        (recur (align-bottom new-ctx right diag1) diag1)))))

(defn- arrange [ctx]
  (arrange-square ctx (top-left-corner ctx)))

(defn- arranged-tile-ids [arranged-ctx]
  (let [cols (take-while some? (iterate (partial move-down arranged-ctx) (top-left-corner arranged-ctx)))]
    (mapv (fn [id] (vec (take-while some? (iterate (partial move-right arranged-ctx) id)))) cols)))

(defn- remove-borders [tile]
  (->> tile
       (drop 1)
       butlast
       (mapv (fn [row] (->> row
                            (drop 1)
                            butlast
                            vec)))))

(defn- combine [tiles]
  (mapv vec (apply concat (mapv (fn [tile-row] (vec (apply mapv concat tile-row))) tiles))))

(defn- image [ctx]
  (let [tile-ids (arranged-tile-ids ctx)]
    (mapv (fn [row]
            (mapv (fn [tile-id] (remove-borders (get-in ctx [:tiles tile-id]))) row)) tile-ids)))

(def sea-monster
  ["                  # "
   "#    ##    ##    ###"
   " #  #  #  #  #  #    "])

(def sea-monster-points
  (->> sea-monster
       (map-indexed (fn [y row]
                      (map-indexed (fn [x item]
                                     [[y x] item]) row)))
       (apply concat)
       (filter (fn [[_ v]] (= \# v)))
       (map first)))

(defn- sea-monster? [image [y x]]
  (= #{\#}
     (set (map (fn [[monster-y monster-x]]
                 (get-in image [(+ y monster-y) (+ x monster-x)])) sea-monster-points))))

(defn- scan [image]
  (let [rows (count image)
        cols (count (first image))]
    (->> (for [y (range rows)
               x (range cols)]
           [[y x] (sea-monster? image [y x])])
         (filter second)
         (map first))))

(defn- scan-all [image]
  (for [rotated (take 4 (iterate tile-rotate-r image))
        fliped (take 2 (iterate tile-flip-v rotated))]
    (scan fliped)))

(defn- sea-monster-count [image]
  (count (first (filter seq (scan-all image)))))

(defn- sea-monster-spaces [image]
  (* (count sea-monster-points)
     (sea-monster-count image)))

(defn- hashes [image]
  (reduce + (map (fn [row] (get (frequencies row) \#)) image)))

(defn- water-roughness [image]
  (- (hashes image)
     (sea-monster-spaces image)))

(comment
  ;; part 1
  (apply * (corners (context input)))

  ;; part 2
  (water-roughness (combine (image (arrange (context input))))))
