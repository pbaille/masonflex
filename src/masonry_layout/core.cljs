(ns masonry-layout.core
  (:require [reagent.core :as r :refer [atom]]))

(enable-console-print!)

(defn- colonize [n-cols heights]
  (reduce
    (fn [acc el]
      (let [i (:idx (first (sort-by :y acc)))]    ; calculate the col index
        (-> acc
            (update-in [i :y] + (:height el))     ;add element height to colon y
            (update-in [i :xs] conj el))))        ;add the element to the chosen col
    (vec (for [n (range n-cols)] {:idx n :y 0 :xs []})) ;init cols
    heights))

(defn- add-diffs [target-height cols]
  (map #(assoc % :diff (- target-height (:y %))) cols))

(defn- distribute-diff [col]
  (let [cnt (count (:xs col))                               ;count items
        q (int (/ (:diff col) cnt))                         ;calculate how much pixel to add to each element
        r (mod (:diff col) cnt)                             ;calculate the reminding pixels
        ;prepare a seq for randomly distribute reminding pixels
        rems (shuffle (take cnt (concat (repeat r 1) (repeat 0))))]
    ;update the col items
    (assoc col
      :xs
      (mapv #(assoc %1 :extra (+ q %2)) (:xs col) rems))))

(defn masonry1 [n-cols heights]
  (let [[x & xs] (reverse (sort-by :y (colonize n-cols heights)))]
    (cons x (map distribute-diff (add-diffs (:y x) xs)))))

#_(masonry1 3 (repeatedly 20 (fn [] {:height (rand-nth (range 40 400))})))

(defn- remove-overflowing-xs [max-height col]
  (assoc col
    :xs
    (loop [ret [] y 0 [x & xs] (:xs col)]
      (if (< y max-height)
        (recur (conj ret x) (+ y (:height x)) xs)
        (butlast ret)))))

(defn masonry2 [n-cols max-height heights]
  (let [[x & xs] (reverse (sort-by :y (colonize n-cols heights)))
        xs (map (partial remove-overflowing-xs max-height) xs)]
    (cons x (map distribute-diff (add-diffs (:y x) xs)))))

#_(masonry2 3 200 (repeatedly 20 (fn [] {:height (rand-nth (range 40 400))})))

(defn masonry-demo2 [n-cols max-height text-height items]
  (let [xs (map (partial remove-overflowing-xs max-height)
                (reverse (sort-by :y (colonize n-cols items))))]
    [:div.main
     (for [c xs]
       ^{:key (gensym)}
       [:div.column
        {:style {:width (str (/ 100 n-cols) "%")}}
        (for [i (:xs c)]
          ^{:key (gensym)}
          [:div.column-item
           {:style {:height (:height i)}}
           [:div.message
            {:style {:height (str text-height "px")}}]
           [:div.image
            {:style {:height (str (:img-height i) "px")}}]])])]))

(defn gen-pseudo-messages [n text-height]
  (repeatedly n (fn [] (let [rand-height (rand-nth (cons 0 (range 50 400 50)))]
                          {:height (+ rand-height text-height)
                           :img-height rand-height}))))

(r/render-component [masonry-demo2 3 1000 60 (gen-pseudo-messages 50 60)]
                    (.getElementById js/document "app"))