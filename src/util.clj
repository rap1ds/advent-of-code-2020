(ns util
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read [file]
  (slurp (io/resource file)))

(defn read-lines [file]
  (-> file
      read
      (str/split-lines)))
