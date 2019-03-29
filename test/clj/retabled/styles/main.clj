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
(defn gradient [color1 color2]
  "Takes two color hex-strings, returns the 'linear-gradient' command that can be put into a :background element"
  (str "linear-gradient(" color1 ", " color2 ")"))

(defn paper-bg
  "Paper bg https://www.sitepoint.com/pure-css3-paper-curls/"
  [selector-kw]
  [selector-kw 
   {:background-color "#fff"
    :box-shadow "0 0 5px rgba(0, 0, 0, 0.2), inset 0 0 50px rgba(0, 0, 0, 0.1)"}
   [:&:before :&:after
    {:transform "skew(5deg) rotate(5deg)"}]])

(defn nav-plain-blue []
  {:background-image "none"
   :background-color (defs/ycolors :blue)
   :background (gradient (defs/ycolors :blue) (defs/ycolors :blue5))
   :border-style "solid"
   :border-width "2px"
   :border-color (defs/ycolors :blue)
   :color (defs/ycolors :lightestblue)
   :line-height (px defs/nav-height)})

(defn nav-hover-blue []
  (let [original (defs/ycolors :blue)]
    (assoc (nav-plain-blue) :background-color original)))

(defkeyframes f-fadeG
  [:from {:background-color (c/rgb 0 0 0)}]
  [:to {:background-color (c/rgb 255 255 255)}])

(def body
  [:body
   {:background-color (defs/ycolors :lightestblue)
    :color defs/body-text-color
    :font-size (px 16)
    :line-height 1.5}])

(def navbar
  [:div.navbar :nav.navbar (nav-plain-blue)

   [:a.navbar-brand {:color (defs/ycolors :blue10)
                     :margin "0 5%"}    
    [:img {:height (px defs/nav-height)}]
    [:&:hover {:color "inherit"}]]
   
   [:ul.navbar-nav
    [:li.nav-item
     [:.nav-link {:color (defs/nav-color :text)
                  :display "inline-block"}
      [:i.fa-home {:font-size (px 30)}]
      [:i.fa-upload {:font-size (px 24)}]]
     [:a.nav-link:hover {:color (defs/nav-color :hover)}]
     [:.username {:display "inline-block"
                  :padding-left "0.5em"
                  :color (defs/ycolors :lightestblue)}
      [:&:hover {:color (defs/nav-color :hover)}]]]]
   [:div.dropdown
    [:a {:cursor "pointer"}]]
   [:ul.dropdown-menu
    {:margin-top "-40px"}
    [:li {:line-height "1.2em"
          :padding "5px"}]
    [:li.dropdown-header {:text-decoration "underline"}]
    [:a {:cursor "pointer"}]]
   [:h4.modal-title {:color (defs/ycolors :blue)}]])

(def jumbotron
  [:.jumbotron {:background-color (defs/ycolors :blue)
                :background (gradient (defs/ycolors :blue5) (defs/ycolors :blue6))
                :color (defs/ycolors :lightestblue)
                :font-size "1.2em"}
   [:a {:color (defs/ycolors :lightestblue)
        :font-weight 500
        :text-decoration "none"}
    [:&:hover {:text-decoration "underline"}]]])

(def form
  [:div.submission-form
   (paper-bg :form)
   [:form
    {:padding (em 2)}
    [:label {:font-weight 600}]
    [:div.form-group
     [:h1 :h2 {:margin-top (em 2)}]
     [:>:div {:margin-top (em 2)}]
     [:.form-text.text-muted
      [:h6 {:font-size "12px"}]]]
    [:div.multi-table
     {:padding-bottom (em 2)}
     (let [cell-width (em 8) ]
       [:table
        {:margin-bottom (em 1)}
        [:th {:max-width cell-width
              :padding (px 2)
              :text-align "center"}]
        [:td {:text-align "center"}
         [((s/selector :input)
           (s/attr= :type :text))
          {:max-width (em 10)}]
         [((s/selector :input)
           (s/attr= :type :number))
          {:max-width (em 4)
           :height (em 2)}]
         [((s/selector :input)
           (s/attr= :type :checkbox))
          {:color #"fff"
           }]]
        [:.bigtext
         {:width (px 400)
          }]
        [:td.total
         [:label  {:padding "0 0.3em"
                   :width (em 3)
                   :display "inline"}]
         [:div.input-group 
          [:span {:display "inline-block"}]
          [:input {:width (em 5)}]]
         ]])]    
    [:.course-learning-outcomes {:margin-top "3em"}
     [:div.particular-course {:border "solid black 2px"
                              :padding "1em"}]]   
    [:.invalid-feedback {:display "none"
                         :color "#dc3545"}]]])

(def form-validity
  [:.was-validated
    [:.form-control:invalid
     ["~ .invalid-feedback" {:display "block"}]]])

(def review
  (let [multitable-top-margin (em 3)]
    [:.review-page
     (paper-bg :review-application)
     [:.review-application
      {:background-color "#fff"
       :box-shadow "0 0 5px rgba(0, 0, 0, 0.2), inset 0 0 50px rgba(0, 0, 0, 0.1)"
       :padding (px 32)}

      [:.review {:padding (em 2)}]
      [:.further-outcomes {:margin-left (em 2)}]
      [:&:before :&:after
       {:transform "skew(5deg) rotate(5deg)"}]
      [:.jumbotron {:background "none"
                    :color defs/body-text-color
                    :text-align "center"
                    :font-size "1.2em"}
       [:a {:color (defs/ycolors :lightestblue)
            :font-weight 500
            :text-decoration "none"}
        [:&:hover {:text-decoration "underline"}]]
       [:.self-proposal {:color "#006400"
                         :font-style "italic"}]]
      [:.row {:margin-left (px 0)
              :margin-right (px 0)}]
      [:.approval-buttons
       [:a.btn {:margin "3px"}]]
      [:div.content.pairs
       [:span.true :span.false {:font-weight 700}]
       [:span.true {:color "#228b22"}]
       [:span.false {:color "#Cd0000"}]
       [:.row.no-val {:background-color "#eee"}
        [:.val {:font-style "italic"}]]
       [:.row:hover {:background-color "lightgray"}]]
      [:span.label {:font-weight "600"}
       [:&:after {:content "\":\""
                  :padding-right "0.5em"}]]
      [:.review.multitable :.approvals {:margin-top multitable-top-margin}
       [:.multirow {:border "solid 1px black"
                    :padding (px 5)
                    :margin (px 5)}
        [:h2 {:text-decoration "underline"}]]]]]))

(def footer
  [:#footer
   {:margin-top (px 100)}
   [:byu-footer
    [:byu-footer-column
     [:span.label {:font-weight "600"}
      [:&:after {:content "\":\""
                 :padding-right "0.5em"}]]
     [:.logo {:height "50px"}]
     [:.email
      [:.value {:text-decoration "underline"}
       [:&:hover {:text-decoration "none"}]]]]]])

(def entry-page
  (let [dash-item-height (px 90) ]
    [:div.dashboard
     [:div {:margin dash-item-height
            :vertical-align "middle"
            :display "inline-block"
            :text-align "center"}
      [:a
       [:&:hover {:text-decoration "none"}]
       [:.fa {:font-size dash-item-height}]
       [:span {:display "block"
               :height dash-item-height
               :vertical-align "middle"}]]]]))

(def applications-table
  [:table.applications
   [:tr.concluded {:background-color (:blue9 defs/ycolors)}
    [:&:hover {:background-color (:blue6 defs/ycolors)}]
    [:a {:color (:blue7 defs/ycolors)}]]])

(def lightbox
  [:div.global-lightbox {:position "fixed"
                         :overflow "auto"
                         :height "100%"
                         :width "100%"
                         :background "rgba(200,200,200,0.8)"
                         :z-index 999}
   [:div.lightbox-contents {:position "absolute"
                            :top "50%"
                            :left "50%"
                            :margin-right "-50%"
                            :transform "translate(-50%, -50%)"}]])

(def comments-style-
  (let [new-comment-width (px 400)
        text-box-border-color "#aaa"
        public-color "#00BB00"
        reviewer-color "#333333"
        hold-color "#FF3232"]
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
      [:textarea {:display "block"
                  :padding (em 0.5)
                  :width "100%"}]
      [:div.controls
       [:a
        {:display "inline-block"
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
       [:.badge {:color "#000"
                 :float "right"
                 :display "inline-block"
                 :border-style "solid"
                 :border-color "#D3d3d3"
                 :font-size (em 1)
                 :height (em 2)
                 :width (em 2)}
        [:&.reviewer {:border-color reviewer-color
                      :color reviewer-color}]
        [:&.public {:border-color public-color
                    :color public-color}]
        [:&.hold {:color hold-color
                  :border-color hold-color}]]
       [:.date :.author {:display "inline-block"}]
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
       [:&.reviewer {:border-color reviewer-color}]
       [:&.hold {:border-color hold-color}]]]
     ;; New comments
     [:div.new-comment
      [:div.badge {:margin-left (em 0.5)}
       [:&.reviewer {:color reviewer-color}]
       [:&.public {:color public-color}]]
      [:.comment-type
       {:float "left"}
       [:.tab {:display "inline-block"
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


(defstyles main
  {:output-to "resources/public/css/isp.css"
   :vendors ["webkit" "moz" "o" "ms"]}
  body
  entry-page
  navbar
  jumbotron
  applications-table
  form
  form-validity
  review
  footer
  comments-style-
  ;comments/style
  lightbox

  [:a.btn {:cursor "pointer"}]
  [:div.goog-tooltip {:background "#000"
                      :border-style "solid"
                      :border-width "2px"
                      :border-color "gray"
                      :opacity "0.85"
                      :color "#afafaf"
                      :border-radius (px 5)
                      :padding (px 4)}]
  [:div.funding-type {:text-align "center"}
   [:a.btn {:display "block"
            :width "20%"
            :margin "0.5em auto"}]]
  [:div.table {:display "table"
               :margin (px 10)}
   [:> [(nth-child :even) {:background-color (defs/ycolors :blue10)}]]
   [:div.row {:display "table-row"}]
   [:div.headers {:font-weight "bold"
                  :text-decoration "underline"}]
   [:div.cell {:display "table-cell"
               :padding (px 10)
               :width "19%"}
    [:&.content-link {:width "auto"}]]
   [:a {:cursor "pointer"
                                        ;:padding "0 5px"
        }
    [:&.rebuild {:padding "0 5px"}]]]

  [:h2.organization {:font-style "italic"
                     :text-decoration "underline"
                     ;:color (defs/ycolors :blue10)
                     :font-weight 700}]

  ;; Circles for busy
  ;; http://cssload.net/en/spinners
  [:#floatingCirclesG {:position "relative"
                       :width (px 125)
                       :height (px 125)
                       :margin "auto"
                       }
   ^:prefix {:transform "scale(0.6)"}]
  [:.f-circleG {:position "absolute"
                :background-color "rgb(255,255,255)"
                :height (px 22)
                :width (px 22)}
   ^:prefix {:border-radius (px 12)
             :animation-name "f-fadeG"
             :animation-duration "1.2s"
             :animation-iteration-count "infinite"
             :animation-direction "normal"}]
  [:#frotateG-0 {:left (px 0)
                :top (px 51)}
   ^:prefix {:animation-delay "0.45s"}]
  [:#frotateG-1 {:left (px 15)
                :top (px 15)}
   ^:prefix {:animation-delay "0.6s"}]
  [:#frotateG-2 {:left (px 51)
                :top (px 0)}
   ^:prefix {:animation-delay "0.9s"}]
  [:#frotateG-3 {:right (px 15)
                :top (px 15)}
   ^:prefix {:animation-delay "0.9s"}]
  [:#frotateG-4 {:right (px 0)
                :top (px 51)}
   ^:prefix {:animation-delay "1.05s"}]    
  [:#frotateG-5 {:right (px 15)
                :bottom (px 15)}
   ^:prefix {:animation-delay "1.2s"}]
  [:#frotateG-6 {:left (px 51)
                :bottom (px 0)}
   ^:prefix {:animation-delay "1.35s"}]
  [:#frotateG-7 {:left (px 15)
                :bottom (px 15)}
   ^:prefix {:animation-delay "1.5s"}]
  f-fadeG)
