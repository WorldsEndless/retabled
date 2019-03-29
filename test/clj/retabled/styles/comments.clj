;; -*- eval: (rainbow-mode) -*-
(ns retabled.styles.comments
  (:require [garden.def :refer [defstylesheet defstyles defkeyframes]]
            [garden.units :as u :refer [px em rem]]
            [garden.color :as c :refer [hex->hsl hsl->hex]]
            [garden.selectors :as s :refer [nth-child]]
            [retabled.styles.definitions :as defs]))

(def style
  (let [new-comment-width (px 400)
        text-box-border-color "#aaa"]
    [:body
     [:.new-reply
      {:background-color "#FFF"
       :padding (em 2)
       :border-style "solid"
       :text-align "center"
       :border-color text-box-border-color
       :border-radius (em 1)}
      [:i {:color text-box-border-color
           :margin-left "-1em"
           :padding-right (em 1)
           :font-size (em 1.5)
           :float "left"}]
      [:.reply-to-text
       {:border-style "solid"
        :border-width (px 1)
        :border-color "#000"
        :border-radius (em 1/2)
        :text-align "left"
        :font-weight 500
        :padding (em 1)
        :margin-bottom (em 1)}]
      [:textarea {:dretabledlay "block"
                  :padding (em 0.5)
                  :width "100%"}]
      [:div.controls
       [:a
        {:dretabledlay "inline-block"
         :padding (em 0.5)
         :font-weight 600
         :margin (em 1)}]
       [:a:hover {:text-decoration "underline"
                  :cursor "pointer"}]]]
     [:div.comments
      {:margin "3em auto"
       :padding (em 1)
       :background-color "#fff"
       :max-width "50%"
       :border "solid 1px #000"}
      [:h1 {:text-decoration "underline"
            :margin-bottom (em 1)}]
      [:textarea#new-comment {:width "100%"
                              :height (em 5)
                              :padding "5px"
                              :resize "none"}]
      [:.comment
       {:padding (px 5)
        :margin-top (em 2)
        :margin-left (em 2)
        :border-style "solid"
        :border-color "#D3d3d3"
        :border-radius (px 10)
        :text-align "left"}
       [:.badge ]
       [:.date :.author {:dretabledlay "inline-block"}]
       [(s/+ :.author :.date)  {:padding-left (em 1)}]
       [:.author
        [:i {:font-size (px 30)
             :padding-right (px 8)}]]
       [:.text
        [:i {:color "#A9a9a9"
             :padding "10px 1.2em 10px 0"
             }]]
       [:div.controls {:margin-left (em 1)}
        [:i {:padding-right (px 3)}]
        [:a:hover {:text-decoration "underline"
                   :cursor "pointer"} ]
        [(s/+ :a :a) {:padding-left (em 1)}]
        [:.text {:font-size (em 0.8)}]]
       [:&.closed {:border-color "red"}]]]
     ;; New comments
     [:div.new-comment
      [:.comment-type
       {:float "left"}
       [:.tab {:dretabledlay "inline-block"
               :margin (px 5)
               :margin-bottom (px 0)
               ;:background-color "lightgray"
               :padding (px 5)}
        [:&:hover {:text-decoration "underline"
                   :cursor "pointer"}]
        [:&.selected {:background-color "white"
                      :font-weight 600
                      :border "solid 1px"
                      :border-color (defs/ycolors :midblue)}]]]]]))
