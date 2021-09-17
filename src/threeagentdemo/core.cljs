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
   [threeagentdemo.testdata :as td]
   [cljs.core.async :refer [chan put! >! <!]])
  (:require-macros [cljs.core.async :refer [go]]))

(def ^:dynamic *bbox-color* "black")

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
                     :Width  (.-x lbounds)
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

;;this is hugely important.
(def matcache (atom {}))
(defn get-mat [color]
  (if-let [meshmat (get @matcache color)]
    meshmat
    (let [res (js/THREE.MeshPhongMaterial. #js{:color "black"})
          _   (swap! matcache assoc color res)]
      res)))

(defn northcom [font items]
  [:object {:position [-10 0 -10]
            :scale    [0.5 0.5 0.5]}
   [:object {:position [-3 6 0]}
    [:text {:text "NORTHCOM"
            :material (get-mat "black")
            :font font
            :height 0.1
            :size 0.5}]]
   [:box {:width 10.5 :height 6 :material {:color "lightgreen"}
          :position [0 4 -1]}]
   [translate [0.5 2.5 0]
    [container 10 6
     items]]])

(defn eucom [font items]
  [:object {:position [-2 0 -10]
            :scale    [0.5 0.5 0.5]}
   [:object {:position [-3 6 0]}
    [:text {:text "EUCOM"
            :material (get-mat "black")
            :font font
            :height 0.1
            :size 0.5}]]
   [:box {:width 10.5 :height 12 :material {:color "lightgreen"}
          :position [0 1 -1]}]
   [translate [0.5 0.5 0]
    [container 10 10
     items]]])

(defn centcom [font items]
  [:object {:position [4 0 -10]
            :scale    [0.5 0.5 0.5]}
   [:object {:position [-3 6 0]}
    [:text {:text "CENTCOM"
            :material (get-mat "black")
            :font font
            :height 0.1
            :size 0.5}]]
   [:box {:width 10.5 :height 6 :material {:color "lightgreen"}
          :position [0 4 -1]}]
   [translate [0.5 0.5 0]
    [container 10 10
     items]]])

(defn pacom [font items]
  [:object {:position [10 0 -10]
            :scale    [0.5 0.5 0.5]}
   [:object {:position [-3 6 0]}
    [:text {:text "PACOM"
            :material (get-mat "black")
            :font font
            :height 0.1
            :size 0.5}]]
   [:box {:width 10.5 :height 6 :material {:color "lightgreen"}
          :position [0 4 -1]}]
   [translate [0.5 0.5 0]
    [container 10 10 items]]])


(defn racetrack [title width & [contents]]
  (let [n width]
    [:object {:position [0 0 -8]
              :scale    [.8 .8 .8]}
     [:plane {:width (+ width 0.1) :height 5.1 :material {:color "black"}
              :position [0 2 -1.02]}]
     [:plane {:width width :height 5 :material {:color "white"}
              :position [0 2 -1.01]}]
     [:plane {:width width :height 1 :material {:color "blue"}
            :position [0 3 -1]}]
     [:plane {:width width :height 1 :material {:color "lightgreen"}
            :position [0 2 -1]}]
     [:plane {:width width :height 1 :material {:color "yellow"}
            :position [0 1 -1]}]
     [:plane {:width width :height 1 :material {:color "lightyellow"}
            :position [0 0 -1]}]
     [:text {:position [-1 4.25 0.1]
             :scale    [ 1 1 0.1]
             :text title
             :material (get-mat "black")
             :font     (@state :font)
             :height   0.1
             :size     0.5}]
     contents]))

(defn entity-sprite [id]
  (when-let [ent (-> @state :entities id)]
    [:sprite {:source (ent :icon)}]))

(defn entities-at-home
  ([type s]
   (let [ids  (-> s :types (get type))
         ents (s :entities)]
     (mapv ents ids)))
  ([type] (entities-at-home type @state)))

(defn icons-at-home
  ([ents]
   (println [:recomputing-icons])
   (when (seq ents)
     (let [idx  (atom 0)
           offset (fn [] (let [res @idx]
                           (swap! idx inc)
                           res))]
       (->> (for [ent ents]
              [:sprite {:source (ent :icon)
                        :position [(offset) (* (ent :readiness) 7) 0]}])
            (into [:group]))))))

;; Root component render function
(defn scene []
  (let [ticks @(th/cursor state [:ticks])
        font  @(th/cursor state [:font])
        sin (.sin js/Math (/ ticks 100))
        cos (.cos js/Math (/ ticks 100))
        abcts (entities-at-home "ABCT" @state)
        sbcts (entities-at-home "SBCT" @state)
        ibcts (entities-at-home "IBCT" @state)]
    [:object
     #_[:ambient-light {:intensity 0.6}]
     [:object {:position [0 0 5]}
      [:point-light {:intensity 0.5}]]
     [:object {:position [0 -6 5]}
      [:point-light {:intensity 0.6}]]
     [:group ;;container layer
      [northcom font @(th/cursor state [:contents :northcom])]
      [eucom    font @(th/cursor state [:contents :eucom])]
      [centcom  font @(th/cursor state [:contents :centcom])]
      [pacom    font @(th/cursor state [:contents :pacom])]]
     [:group   {:position [0 -6.5 0]}
      [:plane  {:position [0 1.05  -9]
                :width 32 :height 5 :material {:color "white"}}]
      [:group {:position [-12.75 0 -8.5] :scale [0.9 0.9 0.1]}
       [:text {:position [0 0 0]
               :text "C4"
               :material (get-mat "black")
               :font     (@state :font)
               :height   0.1
               :size     0.5}]
       [:text {:position [0 0.85 0]
               :text "C3"
               :material (get-mat "black")
               :font     (@state :font)
               :height   0.1
               :size     0.5}]
       [:text {:position [0 1.7 0]
              :text "C2"
               :material (get-mat "black")
               :font     (@state :font)
               :height   0.1
               :size     0.5}]
       [:text {:position [0 2.505 0]
               :text "C1"
               :material (get-mat "black")
               :font     (@state :font)
               :height   0.1
               :size     0.5}]]
      [:object {:position [-9 0 0]}
       [racetrack "ABCT" 8]]
      ;;have to place these the in top level a
      [:object {:position [-11.75 0 -8.8]
                :scale [0.4 0.4 1]}
       [icons-at-home abcts]]
      [:object {:position [1.5 0 0]}
       [racetrack "IBCT" 18]]
      [:object {:position [-5 0 -8.8]
                :scale    [0.4 0.4 1]}
       [icons-at-home ibcts]]
      [:object {:position [11 0 0]}
          [racetrack "SBCT" 5]]
      [:object {:position [9.5 0 -8.8]
                :scale    [0.4 0.4  1]}
       [icons-at-home sbcts]]]
      [:object {:rotation [1 sin 1]}
       [:object {:position [(+ -5.0 sin) -2.0 -5.0]}
        [row-of-boxes 10 "green"]]]

       [:object {:position [0 0 -15]
                 :scale [40 40 1]}
         [u/sprite {:source "1024px-BlankMap-World-Flattened.svg.png"}]]
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
           #_(swap! state assoc :scene)))

    :component-did-update
    (fn [this]
      (->> (threehelp/render f (rdom/dom-node this) {:render-params {:antialias true :resize true}
                                                     :on-before-render on-before-render})

           #_(swap! state assoc :scene)))
    #_#_:component-will-update
    (fn [this]
      (->> (threehelp/render f this {:render-params {:antialias true}})
           (swap! state assoc :scene)))}))

(def regions {:northcom 5
              :pacom    8
              :eucom    22
              :centcom  2})

(defn tick-regions [contents]
  (reduce-kv (fn [acc region xs]
               (->> (if (> (count xs) (regions region))
                      []
                      (->> (conj  xs [:sprite {:source (rand-nth ["abct.png" "sbct.png" "ibct.png"])}])
                           (sort-by (comp :source second))
                           vec))
                    (assoc acc region))) contents contents))

;;for now, this is doing nothing since we have garbage problems.
(def c-day (th/atom 0))

#_
(defn on-tick [s]
  (let [ticks (s :ticks)]
    (as-> s res
      (if (and (res :animating)
               (zero? (mod ticks 30)))
        (update res :contents (fn [m] (tick-regions (or m  (zipmap (keys regions) (repeat []))))))
        res)
      (update res :ticks inc)
      (assoc res :c-day (quot ticks 30)))))
(def readiness-rate
  {"AC" 0.003
   "NG" 0.005})

(defn tick-home [s]
  (let [entities (-> s :entities)
        home     (-> s :locations (get "home"))
        new-entities (->> home
                          (reduce (fn [m id]
                                    (update m id
                                            (fn [e]
                                              (let [readiness (e :readiness)
                                                    new-readiness (+ readiness (readiness-rate (e :compo)))
                                                    new-readiness (if (>= new-readiness 1.0) 0.0 new-readiness)]
                                                (assoc e :readiness new-readiness)))))
                                  entities))]
   (assoc s :entities new-entities)))

(defn init-entities! [ents]
  (let [emap    (into {}
                    (map (fn [e] [(e :id) e])) ents)
        contents (u/map-vals (fn [xs] (set (map :id xs)))  (group-by :location ents))
        types    (u/map-vals  (fn [xs] (set (map :id xs))) (group-by :SRC ents))]
    (swap! state assoc :entities emap :locations contents :types types)))

(defn app [ratom]
  [:div.header {:style {:display "flex" :flex-direction "column" :width "100%" :height "100%"}}
   [:div {:id "chart-root" :style {:display "flex"}}
    [:div {:style {:flex "1" :width "100%"}}
     [:p "Vega Chart"]
     #_[v/vega-chart "ltn-plot" v/ltn-spec]]]
   [:div  {:style {:display "flex" :width "100%" :height  "auto"  :class "fullSize" :overflow "hidden"
                   :justify-content "space-between"}}
    [three-canvas "root-scene" scene (fn [dt] (swap! ratom (fn [s]
                                                             (let [tickstate (update s :ticks inc)
                                                                   ticks (tickstate :sticks)]
                                                               tickstate
                                                               (if (zero? (mod ticks 60))
                                                                 (tick-home tickstate)
                                                                 tickstate))))
                                       #_(swap! ratom on-tick))]]
   [:div.header  {:style {:display "flex" :width "100%" :height  "auto"  :class "fullSize" :overflow "hidden"
                          :justify-content "space-between"
                          :font-size "xxx-large"}}
    [:p {:id "c-day" :style {:margin "0 auto" :text-align "center" }}
     ;;no idea why this causes a slow memory leak!
     #_(str "C-Day:"  @c-day)
     (str "C-Day:"  @c-day)
     ]]])



;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(defn main []
  (go (do
        (when-not (@state :font)
          (<! (font/init!)))
        (when-not (@state :intitialized)
          (let [one-of-each td/test-entities #_(->> td/test-entities (group-by :SRC) vals (map first))]
            (init-entities! one-of-each )))
        (rdom/render [app state] (.getElementById js/document "app")))))

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  (main)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
