(ns retabled.core-test
  (:require [retabled.handler :as handler]
            [luminus.http-server :as http]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.tools.logging :as log]
            [mount.core :as mount])
  (:gen-class))

(def cli-options
  [["-p" "--port PORT" "Port number"
    :parse-fn #(Integer/parseInt %)]])

(mount/defstate ^{:on-reload :noop}
  http-server
  :start
  (http/start
   {:port 3000
    :host "127.0.0.1"
    :handler (handler/app)})
  :stop
  (http/stop http-server))

(comment (mount/defstate ^{:on-reload :noop}
           repl-server
           :start
           (let [nrepl-port 7000]
             (repl/start {:port nrepl-port}))
           :stop
           (when repl-server
             (repl/stop repl-server))))


(defn stop-app []
  (doseq [component (:stopped (mount/stop))]
    (log/info component "stopped"))
  (shutdown-agents))

(defn start-app [args]
  (doseq [component (-> args
                        (parse-opts cli-options)
                        mount/start-with-args
                        :started)]
    (log/info component "started"))
  (.addShutdownHook (Runtime/getRuntime) (Thread. stop-app)))

(defn -main [& args]
  (cond
    (some #{"migrate" "rollback"} args)
    (do
      (mount/start)
      (System/exit 0))
    :else
    (start-app args)))

