(ns retabled.shared
  (:require [clojure.string :as s]))

(defn get-value-from-change [e]
  (.. e -target -value))

(defn idify [s]
  (-> s s/lower-case (s/replace #" " "-") (s/replace #"[^0-9 \- a-z]" "")))
