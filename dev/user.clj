(ns user
  (:require [portal.api :as p]
            [sc.api :as sc]))

(def p (p/open)) ; Open a new inspector

(add-tap #'p/submit) ; Add portal as a tap> target

(tap> "Hello Portal!") ; Start tapping out values
