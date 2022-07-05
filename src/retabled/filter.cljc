(ns retabled.filter
  (:require [retabled.shared :as shared]
            #?(:cljs [reagent.core :refer [atom]])
            [clojure.string :as str]))

(def FILTER-MAP (atom {}))


(defn map-from-url []
  (let [search-string #?(:clj () :cljs (.getDecodedQuery (goog.Uri. (.-location js/window))))]
    (if-not
     (empty? search-string)
      (-> search-string
          (str/split #"[&=]")
          (->> (apply hash-map)))
      {})))

(def SEARCH-MAP (atom (map-from-url)))

(defn url-from-map []
  (let [remove-nils (remove (comp empty? second) @SEARCH-MAP)]
    (str (str/join "&" (map (fn [[k v]] (str (name k) "=" (:value v))) remove-nils)))))

(defn search-in-url
  []
  (let [current-uri #?(:clj () :cljs (goog.Uri. (.-location js/window)))
        search-string (url-from-map)
        complete-uri (.setQuery current-uri (url-from-map))]
    #?(:cljs (js/window.history.replaceState {}, "", (.toString complete-uri)))))


(defn generate-filter-fn
  "Produce the function which compares a filter-map to a map and deems it good or not. If a :key in `filter-map` doesn't exist when filtering `filterable-map`, the filterable map will fail this function."
  [filter-map]
  (fn [filterable-map]
    (every? some?
            (for [[k fm] filter-map
                  :let [f (:value fm)
                        i? (:ignore-case? fm)
                        re-string (if i? (str "(?i)" f) f)
                        field-filter-fn (cond
                                          (fn? f) f
                                          (string? f) (try
                                                        (partial re-find (re-pattern re-string))
                                                        #?(:clj (catch Exception e (str "caught exception: " (.getMessage e)) (partial re-find (re-pattern "")))
                                                           :cljs (catch :default e (println "caught exception: " e) (partial re-find (re-pattern "")))));; TODO right now ints treated as strings. Update this? 
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
  [col-map FILTER table-id filter-in-url]
  (let [id (shared/idify (:headline col-map))
        filter-address (:valfn col-map)
        search-string (@SEARCH-MAP (str table-id "-" (:headline col-map)))]
    [:input.filter {:id (str id "_filter")
                    :value (or (:value search-string) (get-in @FILTER [filter-address :value]))
                    :on-change (do
                                 (search-in-url)
                                 (if (false? filter-in-url)
                                   #(swap! FILTER assoc filter-address {:value (shared/get-value-from-change %) :ignore-case? (:ignore-case? col-map true)})
                                   (if (false? (:filter-in-url col-map))
                                     #(swap! FILTER assoc filter-address {:value (shared/get-value-from-change %) :ignore-case? (:ignore-case? col-map true)})
                                     (do
                                       (when-not (nil? search-string)
                                         (swap! FILTER assoc filter-address search-string))
                                       #(swap! SEARCH-MAP assoc (str table-id "-" (:headline col-map)) {:value (shared/get-value-from-change %) :ignore-case? (:ignore-case? col-map true)})))))}]))

(defn filtering
  "Filter entries according to `FILTER-MAP`"
  [FILTER entries]
  (cond->> entries
    (not-empty @FILTER) (filter-by-map @FILTER)))

(defn on-click-filter
  "Changes the filter value based on value clicked"
  [col-map table-id filter-in-url FILTER val]
  (let [filter-address (:valfn col-map)
        search-string (@SEARCH-MAP (str table-id "-" (:headline col-map)))]
    (if (false? filter-in-url)
      #(swap! FILTER assoc filter-address val)
      (if (false? (:filter-in-url col-map))
        #(swap! FILTER assoc filter-address val)
        (do
          (when-not (nil? search-string)
            (swap! FILTER assoc filter-address search-string))
          #(swap! SEARCH-MAP assoc (str table-id "-" (:headline col-map)) val))))))

(defn resolve-filter
  [controls entries]
  (let [{:keys [valfn displayfn]} controls]
    (cond
      (and valfn (string? (valfn entries)))
      (valfn entries)

      (and displayfn (string? (displayfn entries)))
      (displayfn entries)

      :else
      (throw (ex-info "Unable to resolve filter" entries)))))
