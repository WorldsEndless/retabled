(ns retabled.core
  (:require [retabled.multitable :refer [multi-table] :as mt]
            [retabled.shared :as shared]
            #?(:cljs [reagent.core :refer [atom]])
            [clojure.string :as str]))
(declare tinput render-application render-review)

(defn map-structure
  "Produce a map with the same key-structure from the vector"
  [v]
  (into {}
        (for [[k v] (partition 2 v) 
              :let [nv (cond
                         (vector? v) (map-structure v)
                         (map? v) (:value v "")
                         :default "")]]
          [k nv])))

(defn reset-default
  "Reset the given atom to a default state based on a default map, where it will possess each of the (possibly nested) structural elements of the given default, but values only according to an internal :default"
  ([default-schema-vec]
   (reset-default (atom {}) default-schema-vec))
  ([A default-schema-vec]
   (when (reset! A (map-structure default-schema-vec))
     A)))

(defn render-label
  "Create a label to go into a .row"
  [{:keys [for-id label-text subtext]}]
  [:div.col-md-3.label-area
   [:label {:for for-id}
    label-text]
   subtext])

(defn select-box [m]
  (let [{:keys [options id on-change required?]
         :or {id "generic-select"
              options ["No :options provided"]}} m]
    (into [:select.form-control {:id id
                                 :name id
                                 :required required?
                                 :on-change on-change}]
          (for [o options]
            (if (map? o)
              [:option {:value (:value o)}
               (:content o)]
              [:option o])))))

(defn text-area [opt-map]
  (let [{:keys [id input-value placeholder disabled label valpath changefn value char-count required]} opt-map
        {:keys [limit enforce?]} char-count
        
        textarea 
        [:textarea.form-control {:id id 
                                 :name id 
                                 :rows 5
                                 :default-value input-value
                                 :value value
                                 :on-change changefn
                                 :required required
                                 :placeholder placeholder
                                 :disabled disabled}]]
    [:div.form-group
     textarea
     (when char-count
       (let [ccount (count value)
             class (if (< ccount limit)
                     "conforms"
                     "exceeded")]
         [:div.char-limit {:class class}
          (str ccount "/" limit " characters")]))]))

(defn validation-function?
  [f]
  (:validation-function? (meta f)))

(defn to-validation 
  "Given a predicate, wrap it properly to be a validation function for tinput.

  Validation function runs on the input at every change, altering the validity of the element as prescribed. It waits for .checkValidity on the input to explain the error"
  [f & [error-message]]
  (if (validation-function? f) f 
      (with-meta (fn [click]
                   (let [v (.. click -target -value)
                         dom-element (.. click -target)
                         error-message (or error-message "Invalid input")]
                     (if (f v)
                       (.setCustomValidity dom-element "")
                       (.setCustomValidity dom-element error-message)))) {:validation-function? true})))


(defn checkbox
  "Create a checkbox"
  [{:keys [READ UPDATE valpath] :as fn-map}
   {:keys [validation-function disabled] :as input-map}]
  (let [checked? (READ valpath)
        toggle-fn (comp (or validation-function identity)
                        #(UPDATE valpath not))]
    [:input {:class (last valpath)
             :type "checkbox"
             :disabled disabled
             :checked checked?
             :on-change toggle-fn}]))

(defn checkset
  "If a checkbox value is nil, set it; otherwise, return it."
  [{:keys [READ UPDATE  valpath default-value]}]
  (let [v (READ valpath)
        dv (boolean default-value)]
    (if (boolean? v)
      v
      (UPDATE valpath (constantly dv)))))


(defn togglebox
  "Builds a group which, when toggled, displays its `:content`"
  [{:keys [label content valpath READ UPDATE default-value override-inline? open-height disabled]
    :or {open-height "5em"}
    :as opt-map}]
  (let [content-id "togglebox-content"
        checked? (checkset opt-map)
        transition-style {:-webkit-transition "height 0.4s ease-in-out"
                          :transition "height 0.4s ease-in-out"
                          :overflow "hidden"}]
    [:div.togglebox
     [tinput (select-keys opt-map [:READ :UPDATE]) valpath
      {:type :checkbox
       :checked checked?
       :disabled disabled
       :label ""}]
     [:div.toggle-content
      {:class (if checked? "togglebox-show" "togglebox-hidden")
       :style (when-not override-inline?
                (assoc transition-style :height (if checked? open-height "0em")))}
      (render-application content opt-map)]]))


(defn tinput
  "Produce data-bound inputs for a given map, using `:READ` and `:UPDATE` for values and changes. `opt-map` specifies options including display variables."
  [{:keys [READ UPDATE] :as fn-map} valpath & [opt-map]]
  (let [{:keys [id validation-function required? type default-value disabled subtext invalid-feedback char-count hidden class contingent]
         :or {id (str/join " " (map name valpath))
              type "text"}} opt-map
        {:keys [limit enforce?]} char-count
        {:keys [field-key contingent-fn]} contingent
        input-value (READ valpath)
        changefn1 (fn [e] (UPDATE valpath #(shared/get-value-from-change e)))
        validation-function (when-let [vf validation-function]
                              (to-validation vf invalid-feedback))
        changefn (cond
                   validation-function (fn [e] (doto e changefn1 validation-function))
                   enforce? (fn [e]
                              (let [v (shared/get-value-from-change e)]
                                (cond
                                  (= limit (dec (count v))) identity
                                  (< limit (count v)) #(UPDATE valpath (constantly (apply str (take limit v))))
                                  :default (changefn1 e))))
                   :default changefn1)
        input-map (merge {:type type
                          :id id
                          :name id
                          :on-change changefn
                          :default-value default-value}
                         {:value input-value}
                         (when disabled
                           {:disabled disabled})
                         (when required?
                           {:required true}))
        sub (when subtext
              [:small.form-text.text-muted subtext])
        invalid-feedback (when invalid-feedback
                           [:div.invalid-feedback invalid-feedback])
        input (condp = type
                :select (select-box (merge (select-keys opt-map [:options :required?])
                                           {:on-change changefn
                                            :id id}))
                :multi-table (multi-table fn-map opt-map)
                :textarea (text-area (assoc input-map :changefn changefn))
                :togglebox [togglebox (merge (assoc fn-map :valpath valpath) opt-map)]
                :checkbox [checkbox (assoc fn-map :valpath valpath) input-map]
                ;; default
                [:input.form-control input-map])]
    [:div.form-group
     {:class [(str id "_group") (when hidden "hidden")]}
     [:div.row
      [render-label {:for-id id
                     :label-text  (:label opt-map id)
                     :subtext sub}]
      [:div.col.val-area
       input
       invalid-feedback]]]))

#_(defn atom? [x]
  #?(:clj (instance? clojure.lang.IAtom x)
     :cljs (instance? cljs.core.IAtom x)))

(defn atom?
  "ducktype an atom as something dereferable"
  [a]
  (try (do (deref a) true)
       (catch #?(:clj Exception :cljs js/Error) _ false)))

(defmulti render-application
  "Render an (default) editable application, receiving either an atom or a CRUD-map.

  The map requires analogous fns which will receive pathv and (if applicable) new-val
     `{:READ get-in
       :UPDATE update-in}`"
  (fn [_fm atom-or-map & [pathv]]
    (cond
      (map? atom-or-map) :map
      (atom? atom-or-map) :atom
      :default (throw (ex-info "Unsupported arg for atom-or-map" {:atom-or-map atom-or-map})))))

(defmethod render-application :map
  [fm {:keys [READ UPDATE] :as fn-map} & [pathv]]
  (for [[k v] (partition 2 fm) :let [path (conj (vec pathv) k)]]
    (cond
      (sequential? v) (render-application v fn-map path)
      (map? v) ^{:key v} [tinput fn-map path v]
      :default [:h3.error (str "Failed to render (type:" (type v) ") \n\n" fm)])))

(defmethod render-application :atom
  [fm A & [pathv]]
  (let [R (fn [pathv] (get-in @A pathv))
        U (fn [pathv upd-fn] (swap! A update-in pathv upd-fn))
        fn-map {:READ R :UPDATE U}]
    (render-application fm fn-map pathv)))

(defn render-review
  "Parse the application map and render the review based on the ordered `schema` of the application, with values in `application` expected to be as given by `render-application`.
  
  Resulting form will be read-only with no changes possible."
  [schema application]
  (render-application
   (shared/reviewify schema)
   (atom application)))
