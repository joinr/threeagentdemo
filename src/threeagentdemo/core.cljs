(ns ^:figwheel-hooks threeagentdemo.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [threeagent.core :as th]
   [threeagentdemo.threehelp :as threehelp]))

;; Use reactive atom for storing state
(defonce state (th/atom {:ticks 0
                         :sphere 0
                         :ticking? false}))

;; Form-1 component example
(defn color-box [color size]
  [:box {:dims [size size size]
         :material {:color color}}])

;; Form-2 component example
(defn growing-sphere []
  (let [s (atom 0)]
    #_(.setInterval js/window #(swap! s + 0.5) 500)
    (fn []
      [:sphere {:radius   (-> @state :sphere)
                :material {:color "blue"}}])))

(defn row-of-boxes [count color]
  [:object
   (for [i (range count)]
     [:box {:position [i 0 0]
            :material {:color color}}])])

;; Root component render function
(defn root []
  [:object
   [:object {:position [1.0 0 -4.0]
             :rotation [0 (.sin js/Math (/ (:ticks @state) 100)) 0]} ; Rotate on Y axis based on :ticks
    [:ambient-light {:intensity 0.8}]
    [color-box "red" (* (.cos js/Math (/ (:ticks @state) 100)) 10.0)] ; Don't forget to use square brackets!
    [growing-sphere]]
   [:object {:position [(+ -5.0 (.sin js/Math (/ (:ticks @state) 100))) 0.0 -5.0]}
    [row-of-boxes 10 "green"]]])


;; Initialize and begin rendering threeagent scene
#_
(defonce scene (th/render root (.-body js/document)))

#_
(defn mount [el]
  (rdom/render [hello-world] el))
#_
(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

(defn tick! []
  (when-not (:ticking? @state)
    (swap! state assoc :ticking? true)
    (.setInterval js/window #(swap! state update :ticks inc) 17)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(defn main []
  (tick!)
  (->> (threehelp/render root (.-body js/document) {:antialias true})
       (swap! state assoc :scene)))

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  (main)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
