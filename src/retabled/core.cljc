(ns retabled.core
  (:require [retabled.shared :as shared]
            #?(:cljs [reagent.core :refer [atom]])
            [clojure.string :as str]))

(def col-map-help
  "Possible values of col-maps within `:columns` of control-map "
  [{:valfn (fn [entry] "Retrieves the value of this cell from `entry`")
    :sortfn (fn [entry] "Given an entry, how to sort it by this field. Defaults to `valfn`.")
    :displayfn (fn [valfn-value] "Produces the display from result `(valfn entry)`. 
                                  Default `identity`" )
    :headline "The string to display in table header"
    :css-class-fn (fn [entry] "Produces class (str or vector) to be applied to field/column")
    :filter "If truthy displays a filter-bar that will perform on-change filtering on this column."
    :click-to-filter "If truthy allows a user to filter by a clicking a value in the table"
    }])


(def control-map-help
  "The possible values of a control-map for a table, which should be a
  sequence of maps where each map corresponds to one column of the table."
  {:row-class-fn (fn [entry] "Provides the class (str or vector) of a :tr, given entry")
   :columns col-map-help
   :controls-left (fn [content] "fn of `content` to place before the paging controls (if any)")
   :controls-right (fn [content] "fn of `content` to place after the paging controls (if any)")
   :default-styling? "If truthy, apply default styles such as direction indicators on sorts"
   :paging {:simple "If truthy, use a local atom and default setters and getters without bothering with anything else defined in `:paging`. "
            :get-current-screen (fn [] "Get the current screen num (0-based), from an atom or reframe, etc")
            :set-current-screen (fn [n] "Set current screen num. Default 0.")
            :get-final-screen (fn [] "Get the final screen num (0-based), from an atom or reframe, etc")
            :set-final-screen (fn [n] "Set final screen num.")
            :get-amount (fn [] "Get the number of entries visible per screen")
            :set-amount (fn [n] "Set number of entries visible per screen. Default 5.")
            :r-content [:div.icon "prev-page"]
            :rr-content [:div.icon "first page"]
            :f-content [:div.icon "next page"]
            :ff-content [:div.icon "final page"]
            :left-bar-content [:div.whatever "Stuff before the controls"]
            :right-bar-content [:div.whatever "Stuff after the controls"]}})

(def FILTER-MAP (atom {}))
(def PAGING (atom {:per-screen 5
                   :current-screen 0
                   :final-screen 0}))

(def default-sort
  "function to return a default sort map-atom"
  {:selected nil
   :direction <})


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

(defn click-to-filter
  [col-map val]
  (when (:click-to-filter col-map)
    #(swap! FILTER-MAP assoc (:valfn col-map) val)))

(defn sort-click
  "Select sort field; if sort field unchanged, sort direction"
  [valfn SORT]
  (let [currently-selected (:selected @SORT)
        swap-dir #(if (= <  %) > <)]
    (if (not= currently-selected valfn)
      (swap! SORT assoc :selected valfn)
      (swap! SORT update :direction swap-dir))))

(defn gen-sort
  "Render the title as a link that toggles sorting on this column"
  [c SORT headline]
  (let [sortfn (or (:sortfn c) (:valfn c))
        sorting-this? (= sortfn (:selected @SORT))
        sc (condp = (:direction @SORT)
             < "ascending"
             > "descending")
        classes (when sorting-this? ["sorting-by-this" sc])]
    [:a.sortable {:class classes :on-click #(sort-click sortfn SORT)} headline]))

(defn atom?
  "ducktype an atom as something dereferable"
  [a]
  (try (do (deref a) true)
       (catch #?(:clj Exception :cljs js/Error) _ false)))

(defn ^{:private true} render-header-fields
  [controls SORT]
  (into [:tr.table-headers.row]
        (for [c (:columns controls)
              :let [h  (cond->> (:headline c)
                         (:sort c) (gen-sort c SORT))
                    fi (when (:filter c) (gen-filter c))]]
          [:th fi h])))

(defn ^{:private true} render-screen-controls
  "Render the controls to edit this screen for results"
  [{:as paging-controls
    :keys [get-current-screen
           get-amount
           set-amount
           set-current-screen
           set-final-screen
           get-final-screen
           r-content
           rr-content
           f-content
           ff-content
           left-bar-content
           right-bar-content
           num-columns]
    :or {num-columns 100}}]
  (let [current-screen-for-display (inc (get-current-screen))
        prevfn #(max (dec (get-current-screen)) 0)
        nextfn #(min (inc (get-current-screen)) (get-final-screen))]
    [:tr.row.screen-controls-row
     [:td.cell.screen-controls {:colSpan num-columns}
      left-bar-content
      [:div.control.first [:a.control-label {:on-click #(set-current-screen 0)} rr-content]]
      [:div.control.prev [:a.control-label {:on-click #(set-current-screen (prevfn))} r-content]]
      [:div.control.current-screen [:span.screen-num current-screen-for-display]]
      [:div.control.next [:a.control-label {:on-click #(set-current-screen (nextfn))} f-content]]
      [:div.control.final [:a.control-label {:on-click #(set-current-screen (get-final-screen))} ff-content]]
      right-bar-content]]))

(defn generate-theads
  "generate the table headers"
  [controls paging-controls SORT]
  [:thead
   (when (:paging controls)
     (render-screen-controls paging-controls))
   (render-header-fields controls SORT)])

(defn generate-rows
  "Generate all the rows of the table from `entries`, according to `controls`"
  [controls entries]
  (let [{:keys [row-class-fn columns]
         :or {row-class-fn (constantly "row")}} controls ]
    (into [:tbody]
          (for [e entries :let [tr ^{:key e} [:tr {:class (row-class-fn e)}]]]
            (into tr
                  (for [c columns :let [{:keys [valfn css-class-fn displayfn]
                                      :or {css-class-fn (constantly "field")
                                           displayfn identity}} c]]
                   ^{:key c} [:td.cell {:class (css-class-fn e) :on-click (click-to-filter c (-> e valfn displayfn))} (-> e valfn displayfn)]))))))

(defn ^{:private true} filtering
  "Filter entries according to `FILTER-MAP`"
  [entries]
  (cond->> entries
    (not-empty @FILTER-MAP) (filter-by-map @FILTER-MAP)))

(defn ^{:private true} sorting
  "Sort given entries"
  [SORT entries]
  (let [f (:selected @SORT)
        dir (:direction @SORT)]
    (if (and f dir)
      (sort-by f dir entries)
      entries)))

(def DEFAULT-PAGE-ATOM (atom {:current-screen 0
                              :final-screen 0
                              :per-screen 5}))

(defn default-paging
  "Set up a local atom and define paging functions with reference to it"
  []
  (let [paging {:get-current-screen #(:current-screen @DEFAULT-PAGE-ATOM)
                :set-current-screen #(swap! DEFAULT-PAGE-ATOM assoc :current-screen %)                
                :get-amount #(:per-screen @DEFAULT-PAGE-ATOM)
                :set-amount #(swap! DEFAULT-PAGE-ATOM assoc :per-screen %)
                :get-final-screen #(:final-screen @DEFAULT-PAGE-ATOM)
                :set-final-screen #(swap! DEFAULT-PAGE-ATOM assoc :final-screen %)
                :r-content "‹"
                :rr-content "«"
                :f-content "›"
                :ff-content "»"
                }]
    paging))

(defn ^{:private true} paging
  "Limit view of entries to a given screen.
  If `paging-controls` is falsy, do not filter."
  [paging-controls entries]
  (if-not paging-controls entries
          (let [{:keys [get-current-screen
                        get-amount
                        set-amount
                        set-current-screen
                        set-final-screen
                        get-final-screen]} paging-controls
                amt (get-amount)
                parted-entries (if (> (get-amount) (count entries))
                                 (list entries)
                                 (partition amt amt nil entries))
                max-screens (dec (count parted-entries))]
            (set-final-screen max-screens)
            (nth parted-entries (get-current-screen)))))

(defn curate-entries [paging-controls entries SORT]
  (when (not-empty entries)
    (->> entries
         (paging paging-controls)
         filtering
         (sorting SORT))))

(defn table
  "Generate a table from `entries` according to headers and getter-fns in `controls`"
  [controls entries]
  (let [SORT (atom default-sort)]
    (fn interior-table [controls entries]
      (let [paging-controls (cond (get-in controls [:paging :simple])
                                  (default-paging)

                                  (get-in controls [:paging])
                                  (merge (default-paging)
                                         (:paging controls))

                                  :no-paging
                                  nil)
            entries (curate-entries paging-controls entries SORT)]      
        [:table.table
         [generate-theads controls paging-controls SORT]
         [generate-rows controls entries]]))))

