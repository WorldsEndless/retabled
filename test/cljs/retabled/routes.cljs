(ns retabled.routes
  (:require [reagent.core :as r]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]))

;; -------------------------
;; Routes
;(secretary/set-config! :prefix "#")

(secretary/defroute home "/" []
  (session/put! :page :home))

;; -------------------------
;; History
;; must be called after routes have been defined

(accountant/configure-navigation! {:nav-handler (fn [path] (secretary/dispatch! path))
                                   :path-exists? (fn [path] (secretary/locate-route path))})

(accountant/dispatch-current!)
