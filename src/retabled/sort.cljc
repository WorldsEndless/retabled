(ns retabled.sort)

(def default-sort
  "function to return a default sort map-atom"
  {:selected nil
   :direction <})

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

(defn ^{:private true} sorting
  "Sort given entries"
  [SORT entries]
  (let [f (:selected @SORT)
        dir (:direction @SORT)]
    (if (and f dir)
      (sort-by f dir entries)
      entries)))
