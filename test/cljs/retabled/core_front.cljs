(ns retabled.core-front
  (:require [reagent.core :as r]
            [reagent.session :as session]
            [retabled.ajax :refer [load-interceptors!]]
            [retabled.shared-test :refer [page-template] :as shared]
            [retabled.routes :as routes]
            [retabled.core :as ret]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [ajax.core :refer [GET POST]]))

(defn random-job
  "generate a random job"
  []
  (let [jobs ["programmer" "Dog catcher" "pizza deliverer" "pro-gamer"] ]
    (rand-nth jobs)))

(defn gen-table-data
  "Generate `n` entries of random table data. Desc-map should have [key fun] pairs and fun will be evaluated each time to generate the data."
  [n desc-map]
  
  (into []
        (repeatedly n
                    #(into {}
                           (for [[k f] desc-map] [k (f)])))))

(def table-data
  (let [AMOUNT 2
        A (atom 0)]
    (r/atom (gen-table-data AMOUNT
                            {:name #(str "John Doe " (swap! A inc))
                             :job  random-job
                             :id   #(deref A)
                             :guid #(rand-int 1000)}))))

#_(swap! table-data into
         (gen-table-data 3
                         {:name #(str "Jane Doe " (rand-int 5))
                          :job  random-job
                          :id (constantly "nonsense")
                          :guid #(rand-int 1000)}))

(def table-data2
  (let [AMOUNT 8
        A (atom 0)]
    (r/atom (gen-table-data AMOUNT
                            {:name #(str "Jane Dear " (swap! A inc))
                             :job  random-job
                             :id   #(deref A)
                             :guid #(rand-int 1000)}))))


(defn empty-link
  "Make an empty link thing for each entry with the text inside"
  [val]
  [:a {:href     val
       :on-click #(js/alert (str "You clicked on " val))} val])

(defn my-valfn
  "name or job"
  [entry]
  (:name entry (:job entry)))

(defn home-page []
  ;; Because the second table columns has its `:valfn` as `my-valfn`, 
  ;; `retabled.core-front/myvalfn` is the address for the filter. 
  ;; The filter can programaticaly be changed like so:
  ;; (swap! retabled.core/FILTER-MAP assoc retabled.core-front/my-valfn "2")
                                        
  (let [controls {:table-id "table"
                  :paging  nil #_{:rr-content        "First"
                                  :get-amount        (constantly (/ AMOUNT 3))}
                  :columns [{:valfn     identity
                             :headline  "ID"
                             :sortfn    (fn [entry] (let [id (:id entry)]
                                                      (cond
                                                        (> 3 id)  (- 5 id)
                                                        (<= 2 id) id)))
                             :sort      true
                             :filter    true
                             :displayfn #(:id %)}
                            {:valfn     my-valfn
                             :displayfn empty-link
                             :headline  "Name"
                             :sort      true
                             :filter    true}
                            {:valfn    :job
                             :sort     true
                             :filter   :click-to-filter
                             :headline "Job"}
                            {:valfn    identity
                             :sort     true
                             :sortfn   #(:guid %)
                             :headline "GUID"}]}
        controls2 {:filter-in-url false
                   :table-id "table2" 
                   :paging  nil #_{:rr-content        "First"
                                   :left-bar-content  [:h3 {:style {:display      "inline-block"
                                                                    :margin-right "1em"}} "I'm on the left"]
                                   :right-bar-content [:h3 "I'm on the right"]
                                   :get-amount        (constantly (/ AMOUNT 3))}
                   :columns [{:valfn     identity
                              :headline  "ID"
                              :sortfn    (fn [entry] (let [id (:id entry)]
                                                       (cond
                                                         (> 3 id)  (- 5 id)
                                                         (<= 2 id) id)))
                              :sort      true
                              :filter    true
                              :displayfn #(:id %)}
                             {:valfn     my-valfn
                              :displayfn empty-link
                              :headline  "Name"
                              :sort      true
                              :filter    true}
                             {:valfn    :job
                              :sort     true
                              :headline "Job"
                              :filter :click-to-filter}
                             {:valfn    identity
                              :sort     true
                              :sortfn   #(:guid %)
                              :headline "GUID"}]}]
    [:div.content
     [:section.hero
      [:div.hero-body
       [:h1.title "Retabled"]]]
     #_[:div.dat "My data:"
      (prn-str @table-data)]
     [:div.table1
      [:h2.title "Table 1"]
      [ret/table controls @table-data]]
     [:div.table2
        [:h2.title "Table 2"]
        [ret/table controls2 @table-data2]]
     ]))

(def pages
  {:home #'home-page})

(defn page []
  [(pages (session/get :page))])


;; -------------------------
;; Initialize app

(defn mount-components []
  (r/render [#'page] (.getElementById js/document "app")))

(defn init! []
  (load-interceptors!)  
  (accountant/dispatch-current!)
  (mount-components))
