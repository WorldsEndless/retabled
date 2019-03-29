(ns retabled.shared
  (:require [clojure.string :as s]))

(declare reviewify)

(defn get-value-from-change [e]
  (.. e -target -value))

(defn idify [s]
  (-> s s/lower-case (s/replace #" " "-") (s/replace #"[^0-9 \- a-z]" "")))

(defn reviewify-map
  [m]
  (cond-> m
    true (assoc :disabled true)
    (= (:type m) :select) (dissoc :type)
    (:content m) (update :content reviewify)))


(defn reviewify
  "Adjust a submission-default map for review rather than editing"
  [default-vector]
  (letfn [(m [x]
            (cond
              (map? x) (reviewify-map x)
              (vector? x) (reviewify x)
              :default x))]
    (map m default-vector)))
