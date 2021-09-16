(ns ^:figwheel-hooks threeagentdemo.core
  (:require
   [cljs-bean.core :refer [bean]]
   [goog.dom :as gdom]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [threeagent.core :as th]
   [threeagentdemo.font :as font]
   [threeagentdemo.state :as s :refer [state]]
   [threeagentdemo.util :as u]
   [threeagentdemo.layout :as layout]
   [threeagentdemo.threehelp :as threehelp]
   [cljs.core.async :refer [chan put! >! <!]])
  (:require-macros [cljs.core.async :refer [go]]))

(def ^:dynamic *bbox-color* "black")
(def contents (th/atom []))

;;state now lives in threeagentdemo.state/state
;;but we refer to it locally, so api hasn't changed.

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
(defn id [id object]
  (with-meta object
    {:on-added   (fn [nd] (swap! state #(assoc-in % [:nodes id] nd)))
     :on-removed (fn [nd] (swap! state #(update % :nodes dissoc id)))}))

(defn get-node [id]
  (-> state deref :nodes (get id)))

(defprotocol IBounds
  (bbox [nd]))

(defn three-bbox
  [nd]
  (let [box (js/THREE.Box3.)
        _   (.setFromObject box nd)]
    box))

(extend-protocol IBounds
  cljs.core/Keyword
  (bbox [nd]
    (when-let [obj (get-node nd)]
      (three-bbox obj)))
  js/THREE.Object3D
  (bbox [nd] (three-bbox nd))
  js/THREE.Box3
  (bbox [this] this))

;;some conveniences...
(extend-protocol ISeqable
  js/THREE.Vector2
  (-seq [this]
    [(.-x this) (.-y this)])
  js/THREE.Vector3
  (-seq [this]
    [(.-x this) (.-y this) (.-z this)])
  js/THREE.Vector4
  (-seq [this]
    [(.-x this) (.-y this) (.-z this) (.-w this)]))

(defn local-bounds [obj]
  (let [res (js/THREE.Vector3.)]
    (.getSize (bbox obj) res)
    res))

(defn world-bounds [obj]
  {:min (.-min (bbox obj))
   :max (.-max (bbox obj))})

(defn wrapped-print [obj ks]
  (into {:type (type obj)} (select-keys (bean obj) ks)))

(defn as-three [obj]
  (cond  (instance? js/THREE.Object3D obj) obj
         (vector? obj)
         (if (fn? (first obj))
           (apply (first obj) (rest obj))
           (threeagent.impl.component/render-component (first obj) (second obj)))
         :else (throw (ex-info "cannot coerce to THREE node" {:in obj}))))

(defn extents [bnds]
  (let [{:keys [min max]} bnds]
    {:left   (.-x min)
     :right  (.-x max)
     :bottom (.-y min)
     :top    (.-y max)
     :front  (.-z min)
     :back   (.-z max)}))

(defn translate [[x y z] & objs]
  (into [:object {:position [x y z]}]
         objs))

;;if we can get the world bounds of l and r, we can apply translations to move them together.
(defn beside
  "Places r beside l in the plane, affects no vertical transfom."
  [l r]
  (let [lt            (as-three l)
        lbounds       (world-bounds lt)
        lext          (extents lbounds)
        rt            (as-three r)
        rbounds       (world-bounds rt)
        rext          (extents rbounds)]
    [:group
     [:instance {:object lt}] ;;just inject the objects for now.
     [translate [(- (:left rext) (:right lext)) 0 0]
      [:instance {:object rt}]]]))

(extend-protocol IPrintWithWriter
  js/THREE.Vector3
  (-pr-writer [new-obj writer _]
    (write-all writer "#js " (wrapped-print new-obj [:x :y :z])))
  js/THREE.Vector2
  (-pr-writer [new-obj writer _]
    (write-all writer "#js " (wrapped-print new-obj [:x :y])))
  js/THREE.Vector4
  (-pr-writer [new-obj writer _]
    (write-all writer "#js " (wrapped-print new-obj [:x :y :z :w]))))

(defn container [width height children]
  (let [xs (vec (for [c children]
                  (let [nd (as-three c)
                        lbounds (local-bounds nd)]
                    {:node nd
                     :Width (.-x lbounds)
                     :Height (.-y lbounds)})))
        grouped (layout/layout-boxes {:Width width :Height height
                                      :JustifyContent :JUSTIFY_COUNT #_:JUSTIFY_CENTER
                                      :FlexDirection  :FLEX_DIRECTION_ROW
                                      :FlexWrap       :WRAP_WRAP
                                      :AlignContent   :ALIGN_FLEX_END}
                                      :DIRECTION_LTR  xs)
        halfwidth  (/ width -2.0)
        halfheight (/ height -2.0)]
    (into [:group {:position [halfwidth halfheight 0]}]
         (for [{:keys [data bounds]} (grouped :children)]
           (let [left (.-left bounds)
                 top  (.-top bounds)
                 threenode (-> data :node)]
             [:object {:position [left top 0]}
              [:instance {:object threenode}]])))))

;; Root component render function
(defn scene []
  (let [ticks @(th/cursor state [:ticks])
        sin (.sin js/Math (/ ticks 100))
        cos (.cos js/Math (/ ticks 100))]
    [:object
     [:ambient-light {:intensity 0.8}]
     #_[:object {:position [10 20 -1]}
      [:point-light {:intensity 0.5}]]
     [:object {:position [0 0 -10]
               :scale    [0.5 0.5 0.5] #_[1 #_0.1 1 #_0.1 1]}
      [:plane {:width 10 :height 10 :material {:color "lightgreen"}}]
      [translate [0.5 0.5 0]
       [container 10 10
        @contents
        #_(concat (repeat @contents [:sprite {:source "abct.png" }])
                #_(take @contents
                        (cycle
                         [[:box {:width 1 :height 1 :material {:color "red"}}]
                          [:box {:width 1 :height 1 :material {:color "blue"}}]
                          [:box {:width 1 :height 1 :material {:color "green"}}]])))]]]
     [translate [0 -3 -5]
      [beside
       [:box {:position [-1 -3 0] :dims [2 2 2] :material {:color "red"}}]
       [:box {:position [1  -3 0] :dims [2 2 2] :material {:color "blue"}}]]]
     (when-let [font  @(th/cursor state [:font])]
       [:object {:position [-2 -2 -5]}
        (id :main-text
            [:text {:text "HELLO WORLD"
                    :material (js/THREE.MeshPhongMaterial. #js{:color "blue"})
                    :font font
                    :height 0.1
                    :size 0.5}])
        #_[:svg {:source "tux.svg"}]])
     #_
       [:object {:position [1.0 0 -4.0]
                 :rotation [0 sin 0]} ; Rotate on Y axis based on :ticks
        [:ambient-light {:intensity 0.8}]
        [color-box "red" (* cos 10.0)] ; Don't forget to use square brackets!
        ]
     #_
       [:object {:rotation [1 sin 1]}
        [:object {:position [(+ -5.0 sin) -2.0 -5.0]}
         [row-of-boxes 10 "green"]]]
     #_
       [:object {:position [-5 -2  -5]}
        (id :sprite
            [u/sprite {:source "abct.png" }])]
     
       [:object {:position [0 0 -15]
                 :scale [40 40 1]}
        (id :world [u/sprite {:source "1024px-BlankMap-World-Flattened.svg.png"}])]
     #_
       [:object {:position [-10 3 -10]}
        [:sphere {:radius   1
                  :scale    [cos cos cos]
                  :material {:color "blue"}}]]
       #_[:object {:position [0 0 -1] #_[0 200 -1000]
               :scale    [1 -1 1]}
          [u/svg {:source "World_map_-_low_resolution.svg" #_"Ghostscript_Tiger.svg" #_"World_map_(Miller_cylindrical_projection,_blank).svg"}]]]))

(defn three-canvas [name f on-before-render]
  (r/create-class
   {:display-name (str name)
    :reagent-render (fn [] [:canvas])
    :component-did-mount
    (fn [this]
      (->> (threehelp/render f (rdom/dom-node this) {:render-params {:antialias true :resize true}
                                                     :on-before-render on-before-render})
           (swap! state assoc :scene)))
    :component-did-update
    (fn [this]
      (->> (threehelp/render f (rdom/dom-node this) {:render-params {:antialias true :resize true}
                                                     :on-before-render on-before-render})
           (swap! state assoc :scene)))
    #_#_:component-will-update
    (fn [this]
      (->> (threehelp/render f this {:render-params {:antialias true}})
           (swap! state assoc :scene)))}))


(defn on-tick [s]
  (let [ticks (s :ticks)]
    (when (zero? (mod ticks 34))
      (swap! contents
             (fn [xs]
               (if (> (count xs) 30)
                 []
                 (->> (conj  xs [:sprite {:source (rand-nth ["abct.png" "sbct.png" "ibct.png"])}])
                      (sort-by (comp :source second))
                      vec)))))
    (update s :ticks inc)))

(defn app [ratom]
  [:div.header {:style {:display "flex" :flex-direction "column" :width "100%" :height "100%"}}
   [:div.header  {:style {:display "flex" :width "100%" :height  "auto"  :class "fullSize" :overflow "hidden"
                          :justify-content "space-between"
                          :font-size "xxx-large"}}
    [:p {:style {:margin "0 auto" :text-align "center" }}
     "Origin"]
    [:p {:id "c-day" :style {:margin "0 auto" :text-align "center" }}
     "C-Day: "]
    [:p {:style {:margin "0 auto" :text-align "center" }}
     "Transit"]]
   [:div  {:style {:display "flex" :width "100%" :height  "auto"  :class "fullSize" :overflow "hidden"
                   :justify-content "space-between"}}
    #_[:p "Canvas"]
    [three-canvas "root-scene" scene (fn [dt] (swap! ratom on-tick))]]
   [:div {:id "chart-root" :style {:display "flex"}}
    [:div {:style {:flex "1" :width "45%"}}
     [:p "Vega Chart"]
     #_[v/vega-chart "flow-plot" v/line-equipment-spec]]
    [:div {:style {:flex "0.1" :width "1%"}}]
    [:div {:style {:flex "1" :width "45%"}}
     [:p "Vega Chart"]
     #_[v/vega-chart "ltn-plot" v/ltn-spec]]]])

;;eventually migrate this to on-before-render, seems more
;;idiomatic.
#_(defn tick! []
  (when-not (:ticking? @state)
    (swap! state assoc :ticking? true)
    (.setInterval js/window #(swap! state on-tick) 17)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(defn main []
  (go (do
        (when-not (@state :font)
          (<! (font/init!)))
        #_(tick!)
        (rdom/render [app state] (.getElementById js/document "app")
                     #_
                     (-> (th/render app
                                    (.getElementById js/document "app")
                                    #_{:on-before-render tick!})
                         (setup-scene)))))
  #_
  (tick!)
  #_
  (rdom/render [app state] (.getElementById js/document "app"))
  #_(->> (threehelp/render root #_(.-body js/document) {:render-params {:antialias true}})
       (swap! state assoc :scene)))

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  (main)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
