(ns retabled.core
  (:require [retabled.shared :as shared]
            #?(:cljs [reagent.core :refer [atom]])
            [clojure.string :as str]))

(def col-map-help
  "Possible values of col-maps within `:cols` of control-map "
  [{:valfn (fn [entry] "Retrieves the value of this cell from `entry`")
    :displayfn (fn [valfn-value] "Produces the display from result `(valfn entry)`. 
                                  Default `identity`" )
    :headline "The string to display in table header"
    :css-class-fn (fn [entry] "Produces class (str or vector) to be applied to field/column")
    :page-num (fn [] "Function to get the current page num, from an atom or reframe, etc")
    :page-amount (fn [] "Function to get the number of entries per page")
    :filter "If truthy displays a filter-bar that will perform on-change filtering on this column."
    }])


(def control-map-help
  "The possible values of a control-map for a table, which should be a
  sequence of maps where each map corresponds to one column of the table."
  {:row-class-fn (fn [entry] "Provides the class (str or vector) of a :tr, given entry")
   :cols col-map-help
   
   })

(def FILTER-MAP (atom {}))
(def SORT (atom {:selected nil
                 :direction <}))

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
        results (filter filter-fn map-coll)]
    results))

#_(do (def map-coll [{:alpha "abcd"
                    :num "213"}
                   {:alpha "cnsnnts"
                    :num "11"}
                   {:alpha "oyoe"}])

    (def filter-map {:alpha "cn"})
    ["Filtered map is: "
     (filter-by-map filter-map map-coll)])

(defn gen-filter
  "Generate an input meant to filter a column"
  [col-map]
  (let [id (shared/idify (:headline col-map))]
    [:input.filter {:id (str id "_filter")
                    :on-change #(swap! FILTER-MAP assoc (:valfn col-map) (shared/get-value-from-change %))}]))

(defn sort-click
  "Select sort field; if sort field unchanged, sort direction"
  [valfn]
  (let [currently-selected (:selected @SORT)
        swap-dir #(if (= <  %) > <)]
    (if (not= currently-selected valfn)
      (swap! SORT assoc :selected valfn)
      (swap! SORT update :direction swap-dir))))

(defn gen-sort
  "Render the title as a link that toggles sorting on this column"
  [c headline]
  (let [sorting-this? (= (:valfn c) (:selected @SORT))
        sc (condp = (:direction @SORT)
             < "ascending"
             > "descending")
        classes (when sorting-this? ["sorting-by-this" sc])]
    [:a.sortable {:class classes :href "#" :on-click #(sort-click (:valfn c))} headline]))

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
         (for [c (:cols controls) :let [h  (cond->> (:headline c)
                                            (:sort c) (gen-sort c))
                                        fi (when (:filter c) (gen-filter c))]]
           [:th fi h]))])

(defn generate-rows
  "Generate all the rows of the table from `entries`, according to `controls`"
  [controls entries]
  (let [{:keys [row-class-fn cols]
         :or {row-class-fn (constantly "row")}} controls ]
    (into [:tbody]
          (for [e entries :let [tr [:tr {:class (row-class-fn e)}]]]
            (into tr
                  (for [c cols :let [{:keys [valfn css-class-fn displayfn]
                                      :or {css-class-fn (constantly "field")
                                           displayfn identity}} c]]
                    [:td {:class (css-class-fn e)}(-> e valfn displayfn)]))))))

(defn ^{:private true} filtering
  "Filter entries according to `FILTER-MAP`"
  [entries]
  (cond->> entries
    (not-empty @FILTER-MAP) (filter-by-map @FILTER-MAP)))

(defn ^{:private true} sorting
  "Sort given entries"
  [entries]
  (let [f (:selected @SORT)
        dir (:direction @SORT)]
    (if (and f dir)
      (sort-by f dir entries)
      entries)))

(defn curate-entries [controls entries]
  (let [{:keys [page-num page-amount]} controls]
    (->> entries
         filtering
         sorting
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

