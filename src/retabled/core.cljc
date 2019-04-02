(ns retabled.core
  (:require [retabled.shared :as shared]
            #?(:cljs [reagent.core :refer [atom]])
            [clojure.string :as str]))

(defn generate-filter-fn
  "Produce the function which compares a filter-map to a map and deems it good or not. If a :key in `filter-map` doesn't exist when filtering `filterable-map`, the filterable map will fail this function."
  [filter-map]
  (fn [filterable-map]
    (every? some?
            (for [[k f] filter-map
                  :let [field-filter-fn (cond 
                                          (fn? f) f
                                          (string? f) (partial re-find (re-pattern (str f)))
                                          (int? f) #(= f %)
                                          :else (throw (ex-info "Invalid filter-fn given to generate-filter-fn" {:received {k f}})))]]
              (when-let [filterable-value (k filterable-map)]
                (field-filter-fn filterable-value))))))


(defn filter-by-map
  "Filter a collection of maps by a filter-map, where the filter map specifies the columns and the value to filter them by.

  Strings will be made into basic regexp."
  [filter-map map-coll]
  (let [filter-fn (generate-filter-fn filter-map)
        results  (filter filter-fn map-coll)]
    results))

#_(do (def map-coll [{:alpha "abcd"
                    :num "213"}
                   {:alpha "cnsnnts"
                    :num "11"}
                   {:alpha "oyoe"}])

    (def filter-map {:alpha "cn"})
    ["Filtered map is: "
     (filter-by-map filter-map map-coll)])

(def col-map-help
  "Possible values of col-maps within `:cols` of control-map "
  [{:valfn (fn [entry] "Retrieves the value of this cell from `entry`")
    :headline "The string to display in table header"
    :css-class-fn (fn [entry] "Produces class (str or vector) to be applied to field/column")
    :page-num (fn [] "Function to get the current page num, from an atom or reframe, etc")
    :page-amount (fn [] "Function to get the number of entries per page")
    }])


(def control-map-help
  "The possible values of a control-map for a table, which should be a
  sequence of maps where each map corresponds to one column of the table."
  {:row-class-fn (fn [entry] "Provides the class (str or vector) of a :tr, given entry")
   :cols col-map-help
   
   })


(defn atom?
  "ducktype an atom as something dereferable"
  [a]
  (try (do (deref a) true)
       (catch #?(:clj Exception :cljs js/Error) _ false)))

(defn generate-theads
  "generate the table headers"
  [controls]
  [:thead
   (into [:tr]
         (for [c (:cols controls) :let [h (:headline c)]]
           [:th h]))])

(defn generate-rows
  "Generate all the rows of the table from `entries`, according to `controls`"
  [controls entries]
  (let [{:keys [valfn row-class-fn cols]
         :or {row-class-fn (constantly "row")}} controls ]
    (into [:tbody]
          (for [e entries :let [tr [:tr {:class (row-class-fn e)}]]]
            (into tr
                  (for [c cols :let [{:keys [valfn css-class-fn]
                                      :or {css-class-fn (constantly "field")}} c]]
                    [:td {:class (css-class-fn e)}(valfn e)]))))))


(defn curate-entries [controls entries]
  (let [{:keys [page-num page-amount]} controls]
    (-> entries
        ;; filtering
        ;; sorting
        ;; paging
        )))

(defn table
  "Generate a table from `entries` according to headers and getter-fns in `controls`"
  [controls entries]
  (let [entries (curate-entries controls entries) ]
    [:table
     (generate-theads controls)
     (generate-rows controls entries)
     ]))

