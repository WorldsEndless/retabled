(ns retabled.core
  (:require [retabled.shared :as shared]
            #?(:cljs [reagent.core :refer [atom]])
            [clojure.string :as str]))

(def col-map-help
  "Possible values of col-maps within `:cols` of control-map "
  [{:valfn (fn [entry] "Retrieves the value of this cell from `entry`")
    :headline "The string to display in table header"
    :css-class-fn (fn [entry] "Produces class (str or vector) to be applied to field/column")
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


(defn table
  "Generate a table from `entries` according to headers and getter-fns in `controls`"
  [controls entries]
  [:table
   (generate-theads controls)
   (generate-rows controls entries)
   ])

