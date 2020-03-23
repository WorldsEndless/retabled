;; -*- eval: (rainbow-mode) -*-
(ns retabled.styles.main
  "Garden styles following the BYU schemes"
  (:require [garden.def :refer [defstylesheet defstyles defkeyframes]]
            [garden.units :as u :refer [px em rem]]
            [garden.color :as c :refer [hex->hsl hsl->hex]] ;:rename {hex->rgb hr, rgb->hex rh}]
            [garden.selectors :as s :refer [nth-child]]))

;;;;;;;;;;;;
;; STYLES ;;
;;;;;;;;;;;;
(def sort-directions
  [:body
   [:.sorting-by-this.descending:after
    {:content "\"▲\""}]
   [:.sorting-by-this.ascending:after
    {:content "\"▼\""}]])

(defstyles main
  {:vendors ["webkit" "moz" "o" "ms"]}
  [:div.control {:display "inline-block"
                 :padding-right (em 1)}]
  sort-directions
)
