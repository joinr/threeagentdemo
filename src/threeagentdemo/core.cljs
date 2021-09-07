(ns ^:figwheel-hooks threeagentdemo.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [threeagent.core :as th]
   [threeagentdemo.util :as u]
   [threeagentdemo.threehelp :as threehelp]))

;; Use reactive atom for storing state
(defonce state (th/atom {:ticks 0
                         :sphere 0
                         :ticking? false}))

;; Form-1 component example
(defn color-box [color size]
  [:box {:dims [size size size]
         :material {:color color}}])

(defn row-of-boxes [count color]
  [:object
   (for [i (range count)]
     [:box {:position [i 0 0]
            :material {:color color}}])])


;;add some sprites...


;; Root component render function
(defn root []
  (let [sin (.sin js/Math (/ (:ticks @state) 100))
        cos (.cos js/Math (/ (:ticks @state) 100))]
    #_[:object {:position [0 0 -10]}
     #_[:text {:text "HELLO" :material (js/THREE.MeshBasicMaterial. #js{:color "white"})
             :font (js/THREE.Font.)}]
     #_[:svg {:source "tux.svg"}]]
    [:object
     [:object {:position [10 20 -1]}
      [:point-light {:intensity 0.5}]]

     [:object {:position [1.0 0 -4.0]
               :rotation [0 sin 0]} ; Rotate on Y axis based on :ticks
      [:ambient-light {:intensity 0.8}]
      [color-box "red" (* cos 10.0)] ; Don't forget to use square brackets!
      ]
     [:object {:rotation [1 sin 1]}
      [:object {:position [(+ -5.0 sin) -2.0 -5.0]}
       [row-of-boxes 10 "green"]]]
     [:object {:position [-10 20 -100]}
      [:sphere {:radius   10
                :scale    [sin sin sin]
                :material {:color "blue"}}]]
     [:object {:position [-5 -2  -5]}
      [u/sprite {:source "abct.png"}]]
     [:object {:position [0 0 -15]
               :scale [40 40 1]}
      [u/sprite {:source "1024px-BlankMap-World-Flattened.svg.png"}]]
     #_[:object {:position [0 0 -1] #_[0 200 -1000]
               :scale    [1 -1 1]}
      [u/svg {:source "World_map_-_low_resolution.svg" #_"Ghostscript_Tiger.svg" #_"World_map_(Miller_cylindrical_projection,_blank).svg"}]]]))

(defn tick! []
  (when-not (:ticking? @state)
    (swap! state assoc :ticking? true)
    (.setInterval js/window #(swap! state update :ticks inc) 17)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(defn main []
  (tick!)
  (->> (threehelp/render root (.-body js/document) {:render-params {:antialias true}})
       (swap! state assoc :scene)))

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  (main)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
