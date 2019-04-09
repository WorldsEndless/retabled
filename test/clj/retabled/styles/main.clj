;; -*- eval: (rainbow-mode) -*-
(ns retabled.styles.main
  "Garden styles following the BYU schemes"
  (:require [garden.def :refer [defstylesheet defstyles defkeyframes]]
            [garden.units :as u :refer [px em rem]]
            [garden.color :as c :refer [hex->hsl hsl->hex]] ;:rename {hex->rgb hr, rgb->hex rh}]
            [garden.selectors :as s :refer [nth-child]]
            [retabled.styles.comments :as comments]
            [retabled.styles.definitions :as defs]))

;;;;;;;;;;;;
;; STYLES ;;
;;;;;;;;;;;;
(def thead
  [:thead
   [:.sortable {:text-decoration "underline"}]
   ])

(defstyles main
  {:vendors ["webkit" "moz" "o" "ms"]}
  thead

  )
