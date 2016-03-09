(ns masonry-layout.styles
  (:require [garden.def :refer [defstylesheet defstyles]]
            [garden.units :refer [px]]))

(defstyles styles
           [:body
            {:font-size (px 16)
             :line-height 1.5}
            [:.main {:display :flex}
             [:.column {:display :flex
                        :flex-flow "column nowrap"
                        :padding :5px}
              [:.column-item {:margin-bottom :10px
                              :padding :10px
                              :justify-content :space-around
                              :display :flex
                              :flex-flow "column nowrap"
                              :flex-grow 1
                              :background :lightgrey}
               [:.message {:background :white}]
               [:.image {:background :lightskyblue}]]]]])

