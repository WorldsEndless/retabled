(ns retabled.filter
  (:require [retabled.shared :as shared]
            #?(:cljs [reagent.core :refer [atom]])
            [clojure.string :as str]))

(def FILTER-MAP (atom {}))

(defn generate-filter-fn
  "Produce the function which compares a filter-map to a map and deems it good or not. If a :key in `filter-map` doesn't exist when filtering `filterable-map`, the filterable map will fail this function."
  [filter-map]
  (fn [filterable-map]
    (every? some?
            (for [[k f] filter-map
                  :let [field-filter-fn (cond  
                                          (fn? f) f
                                          (string? f) (partial re-find (re-pattern (str f))) ;; TODO right now ints treated as strings. Update this?
                                          (int? f) #(= f %)
                                          :else (throw (ex-info "Invalid filter-fn given to generate-filter-fn" {:received {k f}})))]]
              (when-let [filterable-value (k filterable-map)]
                (field-filter-fn (str filterable-value)))))))

(defn filter-by-map
  "Filter a collection of maps by a filter-map, where the filter map specifies the columns and the value to filter them by.

  Strings will be made into basic regexp."
  [filter-map map-coll]
  (let [filter-fn (generate-filter-fn filter-map)
        results (filter filter-fn map-coll)]
    results))

(defn gen-filter
  "Generate an input meant to filter a column. `filter-address` is the key of this filter in `FILTER-MAP` and
  may be a function, keyword, etc, as specified by `(:valfn col-map)`"
  [col-map]
  (let [id (shared/idify (:headline col-map))
filter-address (:valfn col-map)]
    [:input.filter {:id (str id "_filter")
                    :value (or (@FILTER-MAP filter-address) "")
                    :on-change #(swap! FILTER-MAP assoc filter-address (shared/get-value-from-change %))}]))

(defn ^{:private true} filtering
  "Filter entries according to `FILTER-MAP`"
  [entries]
  (cond->> entries
    (not-empty @FILTER-MAP) (filter-by-map @FILTER-MAP)))
