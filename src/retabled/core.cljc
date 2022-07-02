(ns retabled.core
  (:require [retabled.filter :as filter]
	    [retabled.sort :as sort]
            #?(:cljs [reagent.core :refer [atom]])
            [clojure.string :as str]))

(def col-map-help
  "Possible values of col-maps within `:columns` of control-map "
  [{:valfn (fn [entry] "Retrieves the value of this cell from `entry`")
    :sortfn (fn [entry] "Given an entry, how to sort it by this field. Defaults to `valfn`.")
    :displayfn (fn [valfn-value] "Produces the display from result `(valfn entry)`. 
                                  Default `identity`")
    :headline "The string to display in table header"
    :css-class-fn (fn [entry] "Produces class (str or vector) to be applied to field/column")
    :ignore-case? "Whether to ignore the case during filtering"
    :filter "If truthy displays a filter-bar that will perform on-change filtering on this column.
             If set to :click-to-filter, makes values searchable by clicking them"
    :filter-in-url "If false turns off filtering in url for the column, by default true"}])

(def control-map-help
  "The possible values of a control-map for a table, which should be a
  sequence of maps where each map corresponds to one column of the table."
  {:row-class-fn (fn [entry] "Provides the class (str or vector) of a :tr, given entry")
   :columns col-map-help
   :controls-left (fn [content] "fn of `content` to place before the paging controls (if any)")
   :controls-right (fn [content] "fn of `content` to place after the paging controls (if any)")
   :default-styling? "If truthy, apply default styles such as direction indicators on sorts"
   :table-id "Allows for each table to have a unique ID"
   :filter-in-url "If false turns off filtering in url for the table, by default true"
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
            :right-bar-content [:div.whatever "Stuff after the controls"]}
   :table-scroll-bar {:fixed-columns {:first? "If truthy, the first column of the table will be fixed/sticky"
                                      :last? "If truthy, the last column of the table will be fixed/sticky"}}})

(def PAGING (atom {:per-screen 10
                   :current-screen 0
                   :final-screen 0}))

(defn atom?
  "ducktype an atom as something dereferable"
  [a]
  (try (do (deref a) true)
       (catch #?(:clj Exception :cljs js/Error) _ false)))

(defn ^{:private true} render-header-fields
  [controls SORT FILTER table-id]
  (let [style-table-scroll-bar {:style {"position" "sticky"
                                        "top" (if (:paging controls) "2em" "0")
                                        "backgroundColor" "white"
                                        "zIndex" "1"}}]
    (into [:tr.table-headers.row (when (:table-scroll-bar controls)
                                   style-table-scroll-bar)]
          (for [c (:columns controls)
                :let [h  (cond->> (:headline c)
                           (:sort c) (sort/gen-sort c SORT))
                      fi (when (:filter c) (filter/gen-filter c FILTER table-id (:filter-in-url controls)))
                      style-first-column-fixed {:style {"position" "sticky"
                                                        "left" "0"
                                                        "backgroundColor" "white"}}
                      style-last-column-fixed {:style {"position" "sticky"
                                                       "right" "0"
                                                       "backgroundColor" "white"}}
                      first-column-and-first-is-fixed? (fn [column] (and (= column (first (:columns controls))) (get-in controls [:table-scroll-bar :first?])))
                      last-column-and-last-is-fixed? (fn [column] (and (= column (last (:columns controls))) (get-in controls [:table-scroll-bar :last?])))
                      currently-sorted-or-filtered? (fn [column] (or (and (:selected @SORT)(= (:sortfn column) (:selected @SORT)))
                                                                     (= (:valfn column) (:selected @SORT))
                                                                     (> (count (get-in @FILTER [(:valfn column) :value])) 0)))
                      style-th (assoc-in (if (first-column-and-first-is-fixed? c)
                                           style-first-column-fixed
                                           (if (last-column-and-last-is-fixed? c)
                                             style-last-column-fixed))
                                         [:style "backgroundColor"]
                                         (if (currently-sorted-or-filtered? c)
                                           "rgb(240, 240, 240)"
                                           "white"))]]
            [:th style-th fi h]))))

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
    :or {num-columns 100}}
   table-scroll-bar]
  (let [current-screen-for-display (inc (get-current-screen))
        prevfn #(max (dec (get-current-screen)) 0)
        nextfn #(min (inc (get-current-screen)) (get-final-screen))
        first-page? (= (get-current-screen) 0)
        last-page? (= (get-current-screen) (get-final-screen))
        style-table-scroll-bar {:style {"position" "sticky"
                                          "top" "0"
                                          "backgroundColor" "white"
                                        "zIndex" "1"}}
        style-hidden {:style {"color" "transparent"}
                      :class "hidden"}
        style-page-to-go {:style {"width" "3em"
                                  "marginLeft" ".5em"}}
        on-change-page-to-go (fn [evt]
                                       (let [val (int (-> evt .-target .-value))]
                                         (when (and (> val 0)(<= val (+ (get-final-screen) 1)))
                                           (set-current-screen (- val 1)))))]
    [:tr.row.screen-controls-row (when table-scroll-bar
                                   style-table-scroll-bar)
     [:td.cell.screen-controls {:colSpan num-columns}
      left-bar-content
      [:div.control.first [:a.control-label (if first-page?
                                              style-hidden
                                              {:on-click #(set-current-screen 0)})
                           rr-content]]
      [:div.control.prev [:a.control-label (if first-page?
                                             style-hidden
                                             {:on-click #(set-current-screen (prevfn))})
                          r-content]]
      [:div.control.current-screen [:span.screen-num current-screen-for-display]]
      [:div.control.next [:a.control-label (if last-page?
                                             style-hidden
                                             {:on-click #(set-current-screen (nextfn))})
                          f-content]]
      [:div.control.final [:a.control-label (if last-page?
                                              style-hidden
                                              {:on-click #(set-current-screen (get-final-screen))})
                           ff-content]]
      [:span.go-to "Go to"]
      [:input.page-to-go (assoc style-page-to-go :on-change on-change-page-to-go)]
      right-bar-content]]))

(defn generate-theads
  "generate the table headers"
  [controls paging-controls SORT FILTER table-id]
  [:thead
   (when (:paging controls)
     (render-screen-controls paging-controls (:table-scroll-bar controls)))
   (render-header-fields controls SORT FILTER table-id)])

(defn generate-rows
  "Generate all the rows of the table from `entries`, according to `controls`"
  [controls entries FILTER table-id]
  (let [{:keys [row-class-fn columns]
         :or {row-class-fn (constantly "row")}} controls]
    (into [:tbody]
          (for [e entries :let [tr ^{:key e} [:tr {:class (row-class-fn e)}]]]
            (into tr
                  (for [c columns :let [{:keys [valfn css-class-fn displayfn filter]                                         
                                         :or {css-class-fn (constantly "field")
                                              displayfn identity}} c
                                        arg-map (cond-> {:class (css-class-fn e)}
                                                  (= filter :click-to-filter) (assoc :on-click (filter/on-click-filter c table-id (:filter-in-url controls) FILTER (filter/resolve-filter c e)))
                                                  (= filter :click-to-filter) (assoc :class (str (css-class-fn e) " click-to-filter")))
                                                  style-first-column-fixed {"position" "sticky"
                                                       "left" "0"
                                                                  "backgroundColor" "white"}
                                        style-last-column-fixed {"position" "sticky"
                                                         "right" "0"
                                                                 "backgroundColor" "white"}
                                        first-column-and-first-is-fixed? (and (= c (first columns)) (get-in controls [:table-scroll-bar :first?]))
                                        last-column-and-last-is-fixed? (and (= c (last columns)) (get-in controls [:table-scroll-bar :last?]))
                                        currently-sorted-or-filtered? (or (and (:selected @SORT)(= (:sortfn c) (:selected @SORT)))
                                                (= valfn (:selected @SORT))
                                                (> (count (get-in @FILTER [valfn :value])) 0))
                                        cell-style-and-arg-map (assoc-in (if first-column-and-first-is-fixed?
                                (assoc arg-map :style style-first-column-fixed)
                                (if last-column-and-last-is-fixed?
                                  (assoc arg-map :style style-last-column-fixed)
                                  arg-map))
                              [:style "backgroundColor"]
                              (if currently-sorted-or-filtered?
                                "rgb(240, 240, 240)"
                                "white"))]]
                    ^{:key c} [:td.cell cell-style-and-arg-map
                               (-> e valfn displayfn)]))))))

(def DEFAULT-PAGE-ATOM (atom {:current-screen 0
                              :final-screen 0
                              :per-screen 10}))

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
                :ff-content "»"}]
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

(defn curate-entries [paging-controls entries SORT FILTER]
  (when (not-empty entries)
    (->> entries
         (paging paging-controls)
         (filter/filtering FILTER)
         (sort/sorting SORT))))

(def TABLE-NAME-COUNT (clojure.core/atom 0))

(defn check-for-duplicates
  [table-id]
  (if (< 1 #?(:clj 0 :cljs (count (js/Array.from (js/document.querySelectorAll (str "#" table-id))))))
    (throw #?(:clj (Exception. "multiple tables with the same table-id") :cljs (js/Error. "multiple tables with the same table-id")))
    ()))

(defn generate-table-id
  [table-id]
  #?(:cljs (js/console.log "generate-table-id call"))
  (if-not table-id
    (do
      (swap! TABLE-NAME-COUNT inc)
      (str "__retabled-" @TABLE-NAME-COUNT))
    (do
      (try
        (check-for-duplicates table-id)
        (catch #?(:clj Exception :cljs js/Error) err
          #?(:clj (println err) :cljs (js/console.error err))))
      table-id)))

(defn table
  "Generate a table from `entries` according to headers and getter-fns in `controls`"
  [controls entries]
  (let [SORT (atom sort/default-sort)
        FILTER (atom {})
        table-id (generate-table-id (:table-id controls))]
    (fn interior-table [controls entries]
      
      (let [paging-controls (cond (get-in controls [:paging :simple])
                                  (default-paging)

                                  (get-in controls [:paging])
                                  (merge (default-paging)
                                         (:paging controls))

                                  :no-paging
                                  nil)
            style-table {"height" "28em"
                         "width" "fit-content"
                         "maxWidth" "100%"
                         "display" "block"
                         "overflowY" "scroll"
                         "overflowX" "scroll"
                         "marginBottom" "3em"}
            entries (curate-entries paging-controls entries SORT FILTER)]      
        [:table.table (assoc {:id table-id} :style (when (:table-scroll-bar controls)
                        style-table))
         [generate-theads controls paging-controls SORT FILTER table-id]
         [generate-rows controls entries SORT FILTER table-id]]))))                               