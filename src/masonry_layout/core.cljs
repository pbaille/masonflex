(ns masonry-layout.core
  (:require [reagent.core :as r :refer [atom]]
            [cljs.pprint :refer [pprint]]))

(enable-console-print!)

(defn- colonize [n-cols heights]
  (reduce
    (fn [acc el]
      (let [i (:idx (first (sort-by :y acc)))]              ; calculate the col index
        (-> acc
            (update-in [i :y] + (:height el))               ;add element height to colon y
            (update-in [i :xs] conj el))))                  ;add the element to the chosen col
    (vec (for [n (range n-cols)] {:idx n :y 0 :xs []}))     ;init cols
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

(comment
  (masonry1 3 (repeatedly 20 (fn [] {:height (rand-nth (range 40 400))})))
  (masonry2 3 200 (repeatedly 20 (fn [] {:height (rand-nth (range 40 400))}))))

;; -------------------------------------------------------------------------------------------------

(defn demo [{:keys [n-cols max-height text-height items]}]
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

;; message-gens --------------

(defn gen-pseudo-messages [n {:keys [text-height max-height]}]
  (repeatedly n (fn [] (let [rand-height (rand-int max-height)]
                         {:height (+ rand-height text-height)
                          :img-height rand-height}))))

(defn gen-asc-height-messages [n {:keys [text-height max-height increment]}]
  (take n
        (for [h (range 0 10000 increment)]
          {:height (+ h text-height)
           :img-height h})))

(defn gen-n-sizes-messages [n {:keys [n-mult base-height text-height]}]
  (repeatedly n
              (fn []
                (let [h (* (rand-nth (range 1 (inc n-mult))) base-height)]
                  {:height (+ text-height h)
                   :img-height h}))))



(comment
  (r/render-component [demo
                       {:n-cols 3
                        :text-height 50
                        :max-height 1000
                        :items (gen-asc-height-messages 100 {:text-height 50 :max-height 400 :increment 40})}]
                      (.getElementById js/document "app")))

(comment
  (r/render-component [demo
                       {:n-cols 3
                        :text-height 50
                        :max-height 1000
                        :items (gen-pseudo-messages 100 {:text-height 30 :max-height 200})}]
                      (.getElementById js/document "app")))

(comment
  (r/render-component [demo
                       {:n-cols 3
                        :text-height 0
                        :max-height 1000
                        :items (gen-n-sizes-messages 100 {:base-height 100 :text-height 0 :n-mult 4})}]
                      (.getElementById js/document "app")))

;; next ---------------

(defn- colonize* [n-cols max-height heights]
  (reduce
    (fn [acc el]
      (let [i (:idx (first (sort-by :y (:cols acc))))       ; calculate the col index
            current-col-height (get-in acc [:cols i :y])]
        (if (< (+ current-col-height (:height el)) max-height)
          (-> acc
              (update-in [:cols i :y] + (:height el))       ;add element height to colon y
              (update-in [:cols i :xs] conj el))            ;add the element to the chosen col
          (update acc :reminding-blocks conj el))))
    {:reminding-blocks []
     :cols (vec (for [n (range n-cols)] {:idx n :y 0 :xs []}))} ;init cols
    heights))

(defn- add-diffs* [max-height cols]
  (map #(assoc % :diff (- max-height (:y %))) cols))

(defn dissoc-vec-idx [v idx]
  (vec (concat (subvec v 0 idx)
               (when (< (inc idx) (count v))
                 (subvec v (inc idx))))))

(defn- complete-cols [{:keys [cols reminding-blocks] :as colonized}]
  (loop [rems (map-indexed vector reminding-blocks) cs cols]
    (let [first-fitable-block
          (first
            (keep
              (fn [[idx {h :height :as block}]]
                (when-let [col (first (filter #(<= h (:diff %)) cs))] ;; one of the column can take the block
                  {:block-idx idx
                   :block block
                   :target-col col}))
              rems))]
      (if first-fitable-block
        (recur (dissoc-vec-idx (vec rems) (:block-idx first-fitable-block))
               ;; update the targeted col, remove block height from its diff and conj block to its elements
               (map #(if (= (get-in first-fitable-block [:target-col :idx]) (:idx %))
                      (-> %
                          (update :diff - (get-in first-fitable-block [:block :height]))
                          (update :xs conj (:block first-fitable-block)))
                      %)
                    cs))
        cs))))

(defn- split-by-overflow [max-height col]
  (assoc col
    :xs
    (loop [ret [] y 0 [x & xs :as allxs] (:xs col)]
      (if (< (+ y (:height x)) max-height)
        (recur (conj ret x) (+ y (:height x)) xs)
        {:taken ret :reminders allxs}))))


(defn demo2 [{:keys [n-cols max-height text-height items]}]
  (let [colonized (colonize* n-cols max-height items)
        with-diffs (update colonized :cols #(add-diffs* max-height %))
        completed (complete-cols with-diffs)]
    [:div.main
     (for [c completed]
       ^{:key (gensym)}
       [:div.column
        {:style {:width (str (/ 100 n-cols) "%")}}
        (for [i (:xs c)]
          ^{:key (gensym)}
          [:div.column-item
           {:style {:height (:height i)
                    :margin "0 !important"}}
           [:div.message
            {:style {:height (str text-height "px")}}]
           [:div.image
            {:style {:height (str (:img-height i) "px")}}]])])]))

(comment
  (r/render-component [demo2
                       {:n-cols 3
                        :text-height 0
                        :max-height 1000
                        :items (gen-n-sizes-messages 50 {:base-height 100 :text-height 0 :n-mult 6})}]
                      (.getElementById js/document "app")))