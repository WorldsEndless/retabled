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


(def table-data
  (let [A (atom 0)]
    (into [] (repeatedly 15 (fn []
                              {:name (str "John Doe " (swap! A inc))
                               :job (random-job)})))))


(defn home-page []
  (let [controls {:cols [{:valfn :name
                          :headline "Name"}
                         {:valfn :job
                          :headline "Job"}]} ]
    [page-template {:jumbo-title "Retabled"
                    :contents [:div.dashboard.text-center
                               (ret/table controls table-data)
                               ]}]))

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
