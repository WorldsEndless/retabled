;; -*- eval: (rainbow-mode) -*-
(ns retabled.styles.definitions
    (:require [garden.def :refer [defstylesheet defstyles defkeyframes]]
            [garden.units :as u :refer [px em rem]]
            [garden.color :as c :refer [hex->hsl hsl->hex]] ;:rename {hex->rgb hr, rgb->hex rh}]
            [garden.selectors :as s :refer [nth-child]]))

;;;;;;;;;;;;;;;;;;;;
;; COLOR PALLETTE ;;
;;;;;;;;;;;;;;;;;;;;
(def _ycolors
  {:blue "#002255"
   :blue2 "#001948"
   :blue3 "#002255"
   :blue4 "#003366"
   :blue5 "#114477"
   :blue6 "#336699"
   :blue7 "#628CB6"
   :blue8 "#91B2D2"
   :blue9 "#ABC8E4"
   :blue10 "#D1E4F6"
   :blue11 "#E0EDF9"
   :blue12 "#EFF6FC"
   :blue13 "#F9FCFE"
   :gray "#A7A9AC"
   :offwhite "#FFFFEE"
   :lightestblue "#F9FCFE"
   :midblue "#628CB6"})

(def ycolors (into {} (for [[k v] _ycolors] {k (hex->hsl v)}))) ;; Object version of ycolors
(def nav-color {:text (ycolors :blue9)
                :hover (ycolors :blue7)})

(def nav-height 75)
(def body-text-color (c/rgb 36 36 36))
