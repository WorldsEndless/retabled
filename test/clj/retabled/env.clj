(ns retabled.env
  (:require 
            [clojure.tools.logging :as log]
            [retabled.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init
   (fn []
     (log/info "\n-=[retabled started successfully using the development profile]=-"))
   :stop
   (fn []
     (log/info "\n-=[retabled has shut down successfully]=-"))
   :middleware wrap-dev})
