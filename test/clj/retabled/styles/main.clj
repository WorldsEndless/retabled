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
(def thead
  [:thead
   [:.sortable {:text-decoration "underline"}
    [:&.sorting-by-this {:color "#000"}]
    [:&.ascending:after {:content "\"▲\""} ]
    [:&.descending:after {:content "\"▼\""} ]]
   [:.screen-controls-row
    [:.screen-controls
     [:div.control {:display "inline-block"
                    :padding-right (em 1)}]
     ]]])

(defstyles main
  {:vendors ["webkit" "moz" "o" "ms"]}
  thead

  )
