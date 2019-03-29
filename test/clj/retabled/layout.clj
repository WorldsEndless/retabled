(ns retabled.layout
  (:require
   [hiccup.core :as hc]
   [hiccup.page :as hp]
   [hiccup.util :refer [with-base-url]]
   [hiccup.element :refer [javascript-tag]]
   [cheshire.core :refer [generate-string]]
   [ring.util.http-response :refer [content-type ok]]))

(declare ^:dynamic *app-context*)
(def style-path "/css/")
(def script-path "/js/")
(def assets-path "/assets/")

(defn context-path [& path]
  (apply str path)
  ;; (if (bound? #'*app-context*)
  ;;    (apply str (into [*app-context*] path))
  ;;    (apply str path))
  )

(defn include-byu-deps []
  (hp/include-css "https://cdn.byu.edu/byu-theme-components/latest/byu-theme-components.min.css")
  (hp/include-js  "https://cdn.byu.edu/byu-theme-components/latest/byu-theme-components.min.js"))

(defn boiler-header [& [userinfo]]
  [:head [:title "Humanities Undergraduate Grant Application"]
   (hp/include-css "//cloud.typography.com/75214/6517752/css/fonts.css")
   [:meta {:name "viewport"
           :content "width=device-width, initial-scale=1.0"}]
   (javascript-tag (str "var USER = " (generate-string userinfo)))
   (javascript-tag (str "var context = ''"))
   ;(javascript-tag (str "var context = '" *app-context* "'"))
   ])

(defn cljs-app-base []
  [:div#app
   [:div.container]])

(defn cljs-app-footer []
  [:div#footer
   [:div.footer]])

(defn boiler-plate []
  [:div#boiler-wrapper
   [:div#navbar]
   ;; styles
   (hp/include-css (context-path assets-path "bootstrap/css/bootstrap.min.css")
                   (context-path assets-path "font-awesome/css/font-awesome.min.css")
                   (context-path style-path "style.css"))])

(defn cljs-includes []
  [:div
   (hp/include-js (context-path script-path "app.js") ;; must precede the goog.require
                  (context-path assets-path "jquery/jquery.min.js")
                  (context-path assets-path "tether/dist/js/tether.min.js")
                  (context-path assets-path "bootstrap/js/bootstrap.min.js"))
   [:script {:type "text/javascript"} "goog.require('retabled.app')"]])

(defn hiccup-render-cljs-base
  "Hiccup rendering (no traditional template)"
  [& [userinfo]]
  (content-type
   (ok
    (hp/html5
     (boiler-header userinfo)
     (boiler-plate)
     (cljs-app-base)
     (cljs-app-footer)
     (cljs-includes))) ;; it makes a big difference to make sure the clojurescript is included last, so the DOM is rendered
   "text/html; charset=utf-8"))

(defn error-page
  "error-details should be a map containing the following keys:
   :status - error status
   :title - error title (optional)
   :message - detailed error message (optional)

   returns a response map with the error page as the body
   and the status specified by the status key"
  [error-details]
  {:status  (:status error-details)
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body (hp/html5
          (boiler-header)
          (boiler-plate)
          [:div.alert.alert-warning
           [:h1 (or (:title error-details) (str "Error " (:status error-details)))]
           [:div.error-details (:message error-details)]])})

(defn receipt-page
  ([]
   (content-type
    (ok
     (hp/html5
      [:div#receipt
       [:h1 "You submission was received"]]))
    "text/html; charset=utf-8"))
  ([application]
      (content-type
    (ok
     (hp/html5
      [:div#receipt
       [:h1 "Your submission was received: "]
       [:div.content (str application) ]]))
    "text/html; charset=utf-8")))

