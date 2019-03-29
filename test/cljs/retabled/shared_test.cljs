(ns retabled.shared-test
  (:require [reagent.core :as r]
            [reagent.session :as session]
            [clojure.string :as s]
            [cljs-time.core :as timec]
            [cljs-time.format :as timef]
            [ajax.core :refer [GET POST]]
            [accountant.core :as accountant])
  (:import [goog.ui Tooltip]))

(def COURSES-ID "course-details")
(def CONTEXT (str js/context))
(defonce text-size 25)

(def ROWS (r/atom {}))
(def *INSTITUTIONS* (r/atom nil))
(def *LANGUAGES* (r/atom nil))

(defn go-to [url &[query]]
  (accountant/navigate! url query))

(defn go-to-internal [pagekey]
  (session/put! :page pagekey))

(defn get-set
  ([m path] (get-in m path))
  ([m path val] (assoc-in m path val)))

(defn get-value-by-id [id]
  (-> (.getElementById js/document id) .-value))

(defn cx [& s]
  (let [CONTEXT (if (and (not (empty? s))
                         (not (empty? CONTEXT))
                         (not (= \/ (last CONTEXT))))
                  (str CONTEXT \/)
                  CONTEXT)]
    (str CONTEXT (apply str s))))


(defn page-template [page-map]
  (let [{:keys [page-class jumbo-title jumbo-subtitle jumbo-contents contents]} page-map]
    [:div.container {:class page-class}
     (when jumbo-title
       [:div.jumbotron
        [:h1 jumbo-title]
        [:div.row
         [:div.col-md-12
          (when-let [jt jumbo-subtitle]
            [:h2 jt])
          [:div jumbo-contents]]]])
     (when contents contents)]))

(defn idify [s]
  (-> s s/lower-case (s/replace #" " "-") (s/replace #"[^0-9 \- a-z]" "")))



(defn assoc-map-in-vector [vm vi mpath mvalue]
        (let [new-m (get vm vi)]
          (assoc vm vi
                 (assoc-in new-m mpath mvalue))))

(defn get-value-from-change [e]
  (.. e -target -value))

(defn read-text
  "Produce a read-only label/value pair"
   [label val &[argmap]]
  [:div.row {:class [(:row-class argmap)
                     (when (empty? val) "no-val")]}
   [:span.label.col label]
   [:span.val.col (if (empty? val)
                    "Not Given"
                    val)]])

(defn format-email [s] (clojure.string/replace s #"%40" "@"))

(def custom-formatter (timef/formatter "YYYY-MM-dd hh:mm a"))

(defn format-date [d]
  ;;(str d)
  (->> d
       timec/date-time
       timec/from-default-time-zone
      ;timec/from-utc-time-zone
      (timef/unparse custom-formatter)
      str))

(defn attach-popover [el s]
  (Tooltip. el s))

(defn uninitialized-map-atom?
  "Determine whether an atom of a map with string values has all string values empty"
  [map-atom]
  (if (map? @map-atom)
    (every? (comp empty? second) @map-atom)
    (throw (ex-info "Invalid map-atom provided" {:map-atom map-atom}))))

(defn get-application
  "Query the server with the appropriate credentials and retrieve the application, putting it in the provided `APPLICATION-ATOM`"
  [{:keys [user-map application-id receptacle-atom filter-fn force?]
    :or {filter-fn identity}}]
  (let [url (str "/admin/application")
        get-application (fn [] (GET url {:handler (fn [r]
                                                    (reset! receptacle-atom (filter-fn (first r))))
                                         :error-handler (fn [e]
                                                          (.log js/console "-------ERROR getting application!")
                                                          (.log js/console e))
                                         :params {:user user-map
                                                  :application-id application-id}}))]
    (when (or force?
              (not (= application-id (:id @receptacle-atom))))
      (get-application))
    @receptacle-atom))

(defn array-map? 
  [m]
  (= (type m) cljs.core/PersistentArrayMap))

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
  "Reset the given atom to a default state based on a default vector, where it will possess each of the (possibly nested) structural elements of the given default, but values only according to an internal :default"
  [A default-map]
  (when (reset! A (map-structure default-map))
    A))

(defn render-val-map
  "set `A` to be a map designed to hold the values specified in `v`"
  [A v]
  
  )

