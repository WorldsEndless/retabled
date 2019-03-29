(ns retabled.shared.components.lightbox
  "Lightbox components"
  (:require [reagent.core :as r]))

(defonce LIGHTBOX-CONTENTS (r/atom nil))

(defn clear-lightbox [] (reset! LIGHTBOX-CONTENTS nil))

(defn lightbox
  "A lightbox that fills the screen and shades the background with its contents. The lightbox is planted on the page at start-time, waiting to be filled. This function is not intended to directly render -- it populates the already-planted LIGHTBOX-CONTENTS"
  ([] (when (not-empty @LIGHTBOX-CONTENTS)
        [:div.global-lightbox
         [:div.lightbox-contents
          @LIGHTBOX-CONTENTS]]))
  ([contents]
   (reset! LIGHTBOX-CONTENTS contents)
   nil))




