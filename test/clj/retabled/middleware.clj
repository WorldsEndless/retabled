(ns retabled.middleware
  (:require [retabled.env :refer [defaults]]
            [clojure.tools.logging :as log]
            [retabled.layout :refer [*app-context* error-page]]
            [ring.middleware.webjars :refer [wrap-webjars]]
            [ring.middleware.format :refer [wrap-restful-format]]
            [ring.middleware.flash :refer [wrap-flash]]
            [ring.middleware.defaults :refer [site-defaults wrap-defaults]]))

(defn print-handler [handler & [s]]
  (fn [req]
    (println (or s "---Request is:"))
    (prn req)
    (handler req)))

(defn wrap-internal-error [handler]
  (fn [req]
    (try
      (handler req)
      (catch Throwable t
        (log/error t)
        (error-page {:status 500
                     :title "Error"
                     :message "Please report your error to the admin."})))))

(defn wrap-formats [handler]
  (let [wrapped (wrap-restful-format
                 handler
                 {:formats [:json-kw :transit-json :transit-msgpack :edn]})]
    (fn [request]
      ((if (:websocket? request) handler wrapped) request))))

(defn wrap-base [handler]
  (-> handler
      wrap-webjars
      (wrap-defaults
       (-> site-defaults
           (assoc-in [:security :anti-forgery] false)))
      wrap-flash
      wrap-internal-error))
