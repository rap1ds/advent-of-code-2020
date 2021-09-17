(ns util
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read [file]
  (slurp (io/resource file)))

(defn read-lines [file]
  (-> file
      read
      (str/split-lines)))

(defn read-bigints [file]
  (map bigint (read-lines file)))

(defn read-longs [file]
  (map #(Long/parseLong %) (read-lines file)))
