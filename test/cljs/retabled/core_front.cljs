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

(def AMOUNT 15)


(def table-data
  (let [A (atom 0)]
    (into [] (repeatedly AMOUNT (fn []
                                  {:name (str "John Doe " (swap! A inc))
                                   :job  (random-job)
                                   :id   @A
                                   :guid (rand-int 1000)})))))


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
  (let [controls {:paging  {:rr-content        "First"
                            :left-bar-content  [:h3 {:style {:display      "inline-block"
                                                             :margin-right "1em"}} "I'm on the left"]
                            :right-bar-content [:h3 "I'm on the right"]
                            :get-amount        (constantly (/ AMOUNT 3))}
                  :columns [{:valfn     identity
                             :headline  "ID"
                             :sortfn    (fn [entry] (let [id (:id entry) ]
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
                             :filter    true
                             }
                            {:valfn    :job
                             :sort     true
                             :headline "Job"}
                            {:valfn    identity
                             :sort     true
                             :sortfn   #(:guid %)
                             :headline "GUID"}]} ]
    [:div.content
     [:section.hero
      [:div.hero-body
       [:h1.title "Retabled"]]]
     [ret/table controls table-data]
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
