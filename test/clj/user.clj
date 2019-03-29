(ns user
  (:require [mount.core :as mount]
            [retabled.figwheel :refer [start-fw stop-fw cljs]]
            [garden-gnome.watcher :as garden-gnome]
            retabled.core-test))

(mount/defstate garden
  :start (garden-gnome/start! (garden-gnome/default-config))
  :stop (garden-gnome/stop! garden))

(defn start []
  (mount/start)
  ;(mount/start-without #'retabled.core-test/repl-server)
  )

(defn stop []
  (mount/start)
  ;(mount/stop-except #'retabled.core-test/repl-server)
  )

(defn restart []
  (stop)
  (start))


