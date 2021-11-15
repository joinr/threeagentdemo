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
   [threeagentdemo.vega :as v]
   [cljs.core.async :as a :refer [chan put! >! <! close!]])
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
                                      :JustifyContent :JUSTIFY_CENTER
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
   [:box {:width 10.5 :height 6 :material {:color "lightgrey"}
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
   [:box {:width 10.5 :height 12 :material {:color "lightgrey"}
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
   [:box {:width 10.5 :height 6 :material {:color "lightgrey"}
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
   [:box {:width 10.5 :height 6 :material {:color "lightgrey"}
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
     [:plane {:width width :height 0.8 :material {:color (u/c->hex :C1)}
            :position [0 3.2 -1]}]
     [:plane {:width width :height 0.8 :material {:color (u/c->hex :C2)}
            :position [0 2.4 -1]}]
     [:plane {:width width :height 0.8 :material {:color (u/c->hex :C3)}
            :position [0 1.6 -1]}]
     [:plane {:width width :height 0.8 :material {:color (u/c->hex :C4)}
              :position [0 0.8 -1]}]
     [:plane {:width width :height 0.8 :material {:color (u/c->hex :C5)}
              :position [0 0 -1]}]
     [:text {:position [-1 4.25 0.1]
             :scale    [ 1 1 0.1]
             :text title
             :material (get-mat "black")
             :font     (@state :font)
             :height   0.1
             :size     0.5}]
     contents]))

;;old c1-c4 racetrack with SR color palette.
#_
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
   (let [ids     (-> s :types (get type))
         at-home (-> s :locations (get :home))
         ids     (clojure.set/intersection ids at-home)
         ents (s :entities)]
     (mapv ents ids)))
  ([type] (entities-at-home type @state)))

(defn icons-at-home
  ([ents]
   ;;(println [:recomputing-icons]) ;;too many calls...
   (when (seq ents)
     (let [idx  (atom 0)
           offset (fn [] (let [res @idx]
                           (swap! idx inc)
                           res))]
       (->> (for [ent ents]
              [:sprite {:source (ent :icon)
                        :position [(offset) (* (ent :readiness) 7) 0]}])
            (into [:group]))))))

(defn missing-items [n]
  (repeat n [:sprite {:source "empty.png"}]))

(defn contents [location]
  (vec (concat @(th/cursor state [:contents location])
               (missing-items @(th/cursor state [:slots location])))))

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
      [northcom font (contents :northcom) #_@(th/cursor state [:contents :northcom])]
      [eucom    font (contents :eucom) #_@(th/cursor state [:contents :eucom])]
      [centcom  font (contents :centcom) #_@(th/cursor state [:contents :centcom])]
      [pacom    font (contents :pacom) #_@(th/cursor state [:contents :pacom])]]
     [:group   {:position [0 -6.5 0]}
      [:plane  {:position [0 1.05  -9]
                :width 32 :height 5 :material {:color "white"}}]
      [:group {:position [-12.85 #_-12.75 -0.3 -8.5] :scale [0.9 0.9 0.1]}
       #_[:text {:position [0 0 0]
               :text "C4"
               :material (get-mat "black")
               :font     (@state :font)
               :height   0.1
               :size     0.4}]
       [:text {:position [0 0.85 0]
               :text "MOD" #_"C3"
               :material (get-mat "black")
               :font     (@state :font)
               :height   0.1
               :size     0.4}]
       [:text {:position [0 1.7 0]
              :text "TRN" #_"C2"
               :material (get-mat "black")
               :font     (@state :font)
               :height   0.1
               :size     0.4}]
       [:text {:position [0 2.505 0]
               :text "MSN" #_"C1"
               :material (get-mat "black")
               :font     (@state :font)
               :height   0.1
               :size     0.4}]]
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

     (when (@state :showbox)
       [:object {:rotation [1 sin 1]}
       [:object {:position [(+ -5.0 sin) -2.0 -5.0]}
        [row-of-boxes 10 "green"]]])

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

    #_#_:component-did-update
    (fn [this]
      (->> (threehelp/render f (rdom/dom-node this) {:render-params {:antialias true :resize false #_true}
                                                     :on-before-render on-before-render})

           #_(swap! state assoc :scene)))
    #_#_:component-will-update
    (fn [this]
      (->> (threehelp/render f this {:render-params {:antialias true}})
           (swap! state assoc :scene)))}))

(def regions
  {:northcom 1
   :pacom    3
   :eucom    3
   :centcom  2})

(def conflict-demand
  {:northcom 4
   :pacom    5
   :eucom    20
   :centcom  0})

;;create a lame demand that is low for 6 months, then goes to conflict.
;;as currently stated, we have a map of demand-by-region.
;;regions are equivalent to demand.
;;So if we increase demand at time 1, we just assoc into the :regions key
;;with the deltas, add increment the corresponding slots.

;;OBE
(defn tick-regions [contents]
  (reduce-kv (fn [acc region xs]
               (->> (if (> (count xs) (regions region))
                      []
                      (->> (conj  xs [:sprite {:source (rand-nth ["abct.png" "sbct.png" "ibct.png"])}])
                           (sort-by (comp :source second))
                           vec))
                    (assoc acc region))) contents contents))

;;for now, this is doing nothing since we have garbage problems.
(def c-day  (th/cursor state [:c-day]))
(def period (th/cursor state [:period]))

(def readiness-rate
  {"AC" 0.003
   "NG" 0.005})

(defn find-deploys [s]
  (let [deps       (filter (fn [[region n]]
                             (pos? n))
                           (s :slots))]
    (when (seq deps)
      (let [open-slots  (->> deps (map second) (reduce +))
            thresh      (s :deploy-threshold)
            deployables (->> s
                             :entities
                             vals
                             (filter #(and (>= (% :readiness) thresh)
                                           (not (pos? (% :wait-time 0))))) ;;not deployed...
                             (take open-slots)
                             (sort-by #(- (% :readiness))))]
        (when (seq deployables)
          (->> (reduce (fn [[remaining fills] [region n]]
                    (let [ents (take n remaining)
                          newfills (mapv (fn [e]
                                           {:id (e :id) :location region :wait-time 270}) ents)]
                      (if (< (count ents) n)
                        (reduced [nil (concat fills newfills)])
                        [(drop (count newfills) remaining) (concat fills newfills)])))
                       [deployables nil] deps)
               second))))))


(defn tick-home [s]
  (let [entities (-> s :entities)
        home     (-> s :locations (get :home))
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

#_
(defn naive-c-rating [ent]
  (let [r (ent :readiness)]
    (cond (>= r 0.75) :C1
          (>= r 0.5)  :C2
          (>= r 0.25) :C3
          :else :C4)))

(defn naive-c-rating [ent]
  (let [r (ent :readiness)]
    (cond (>= r 0.8) :C1
          (>= r 0.6)  :C2
          (>= r 0.4) :C3
          (>= r 0.2) :C4
          :else :C5)))

(defn insert-by [v f itm]
  (if-not (empty? v)
    (->> (conj v itm)
         (sort-by f)
         vec)
    [itm]))

(def c-icons
  {"abct.png" {:C1 "abct-c1.png"
               :C2 "abct-c2.png"
               :C3 "abct-c3.png"
               :C4 "abct-c4.png"
               :C5 "abct-c5.png"
               }
   "sbct.png" {:C1 "sbct-c1.png"
               :C2 "sbct-c2.png"
               :C3 "sbct-c3.png"
               :C4 "sbct-c4.png"
               :C5 "sbct-c5.png"
               }
   "ibct.png" {:C1 "ibct-c1.png"
               :C2 "ibct-c2.png"
               :C3 "ibct-c3.png"
               :C4 "ibct-c4.png"
               :C5 "ibct-c5.png"
               }})

(defn deploy-unit [s id location dt]
  (let [ent (-> s :entities (get id))
        c-rating (naive-c-rating ent)
        icon (ent :icon)
        c-icon (or (get-in c-icons [icon c-rating])
                   (throw (ex-info "unknown sprite!" {:in [icon c-rating]})))]
    (-> s
        (update-in [:locations :home] disj id)
        (update-in [:locations location] (fn [v] (conj (or v #{}) id)))
        (update-in [:contents  location] (fn [v]
                                           (insert-by (or v [])
                                                      (fn [itm] (-> itm second :source))
                                                      ^{:id id}
                                                      [:sprite {:source c-icon #_(ent :icon)}])))
        (update-in [:slots     location] dec)
        (update-in [:entities id] assoc  :wait-time dt :location location)
        (update-in [:stats :deployed c-rating] inc)
        (assoc-in [:waiting id] dt))))

(defn missed-demand [s]
  (->> s :slots vals (filter pos?) (reduce +)))

(defn tick-deploys [s]
  (if-let [deps (find-deploys s)]
    (reduce (fn [acc {:keys [id location wait-time]}]
              (deploy-unit acc id location wait-time))
            s deps)
    s))

(defn tick-missing [s]
  (assoc-in s [:stats :deployed :Missing] (missed-demand s)))

(defn send-home [s id]
  (let [ent      (-> s :entities (get id))
        location (ent :location)
        c-rating (naive-c-rating ent)]
      (-> s
          (update-in [:locations :home] conj id)
          (update-in [:locations location] (fn [v] (disj (or v #{}) id)))
          ;;lame linear scan...
          (update-in [:contents  location] (fn [v] (vec (remove  (fn [v]
                                                                   (= (-> v meta :id) id))
                                                            (or v [])))))
          (update-in [:slots     location] inc)
          (update-in [:entities id] assoc :wait-time 0 :readiness 0)
          (update-in [:stats :deployed c-rating] dec)
          (update-in [:waiting] dissoc id))))

(defn tick-waits [s]
  (let [waits (s :waiting)]
    (reduce-kv (fn [acc id dt]
                 (if (zero? dt)
                   (send-home acc id)
                   (update-in acc [:waiting id] dec)))
               s waits)))

(defn tick-conflict [s]
  (if-let [tc (s :tconflict)]
    (if (= tc (s :c-day))
      (let [added-demand (s :conflict-demand)
            new-demand   (merge-with + added-demand (@state :demand))
            new-slots    (merge-with + added-demand (@state :slots))]
        (assoc s :demand new-demand :slots new-slots :period "Conflict"
                 :deploy-threshold 0))
      s)
    s))

(defn tick-scene [s]
  (if (and (s :animating)
           (<= (s :c-day) (s :tstop)))
    (let [tickstate (update s :ticks inc)
          ticks (tickstate :ticks)]
      (if (zero? (mod ticks 2))
        (-> tickstate
            (update :c-day + 1)
            tick-home
            tick-waits
            tick-conflict ;;lame on purpose
            tick-deploys
            tick-missing)
        tickstate))
    s))

(defn init-entities! [ents]
  (let [emap    (into {}
                    (map (fn [e] [(e :id) e])) ents)
        contents (u/map-vals (fn [xs] (set (map :id xs)))  (group-by :location ents))
        types    (u/map-vals  (fn [xs] (set (map :id xs))) (group-by :SRC ents))]
    (swap! state assoc :entities emap :locations contents :types types :waiting {} :contents {})))

(defn init-demand! [demand]
  (swap! state assoc :demand demand :slots demand
         :conflict-demand conflict-demand
         :tconflict 450
         :period "Competition"
         :deploy-threshold 0.7))

(defn compute-outline [s]
  (let [{:keys [tstart tstop demand conflict-demand tconflict]} s
        compdemand (reduce + (vals demand))
        confdemand (+ compdemand (reduce + (vals conflict-demand)))]
    (concat (for [t (range tstart (dec tconflict))]
              #js[#js{:c-day t :trend "Demand" :value compdemand}])
            (for [t (range tconflict tstop)]
              #js[#js{:c-day t :trend "Demand" :value confdemand}]))))

(defn play! []
  (swap! state assoc :animating true))

(defn stop! []
  (swap! state assoc :animating false))

(defn watch-until [id atm f]
  (let [res (chan)]
    (add-watch atm id
               (fn [_ _ _ m]
                 (when-let [v (f m)]
                   (do (put! res v)
                       (close! res)
                       (remove-watch atm id)))))
    res))

(defn init-state! []
  (let [randomized (map (fn [{:keys [readiness] :as e}]
                          (assoc e :readiness (rand) #_(/  (rand) 2.0))) td/test-entities)
        tstart 0
        tstop  1000]
    (init-entities! randomized)
    (init-demand! regions)
    (swap! state assoc
           :c-day 0
           :tstart tstart
           :tstop  tstop
           :stats {:deployed {:C1 0
                              :C2 0
                              :C3 0
                              :C4 0
                              :C5 0
                              :Missing 0}})
    ;;could be cleaner.  revisit this.
    (watch-until :fill-plot-exists
                 threeagentdemo.vega/charts
                 (fn [m]
                   (when (m :fill-plot-view)
                     (v/push-extents! :fill-plot-view tstart tstop))))))

(defn reset-state! []
  (swap! state assoc :animating false)
  (init-state!))

(defn daily-stats [t]
  (let [stats (get-in @state [:stats :deployed])
        {:keys [C1 C2 C3 C4 C5 Missing]} stats]
    #js[#js{:c-day t :trend "C1" :value C1}
        #js{:c-day t :trend "C2" :value C2}
        #js{:c-day t :trend "C3" :value C3}
        #js{:c-day t :trend "C4" :value C4}
        #js{:c-day t :trend "C5" :value C5}
        #js{:c-day t :trend "Missing" :value Missing}]))

(defn plot-watch! []
  (add-watch c-day :plotting
               (fn [k r oldt newt]
                 (cond (< newt oldt)
                       (do (v/rewind-samples! :fill-plot-view "c-day" newt))
                       (> newt oldt)
                       (do (v/push-samples!   :fill-plot-view (daily-stats newt)))
                       :else nil))))

(defn app [ratom]
  (let [render-scene! (fn [dt] (swap! ratom tick-scene))]
    (fn []
      [:div.header {:style {:display "flex" :flex-direction "column" :width "100%" :height "100%"}}
       [:div {:id "chart-root" :style {:display "flex"}}
        [:div {:style {:flex "0.95" :width "95%"}}
         [v/vega-chart "fill-plot" v/fill-spec]]]
       [:div  {:style {:display "flex" :width "100%" :height  "auto"  :class "fullSize" :overflow "hidden"
                       :justify-content "space-between"}}
        [three-canvas "root-scene" scene render-scene!
         #_(fn [dt] (swap! ratom tick-scene))]]
       [:div.header  {:style {:display "flex" :width "100%" :height  "auto"  :class "fullSize" :overflow "hidden"
                              :justify-content "space-between"
                              :font-size "xxx-large"}}
        [:p {:id "period" :style {:margin "0 auto" :text-align "center" }}
         @period #_(str "Period:"  @period)
         ]
        [:p {:id "c-day" :style {:margin "0 auto" :text-align "center" }}
         ;;no idea why this causes a slow memory leak!
         (str "Day:"  (int @c-day))
         ]]
       [:div.flexControlPanel {:style {:display "flex" :width "100%" :height "auto"}}
        [:button.cesium-button {:style   {:flex "1"} :id "play" :type "button" :on-click #(play!)}
         "play"]
        [:button.cesium-button {:style {:flex "1"} :id "stop" :type "button" :on-click #(stop!)}
         "stop"]
        [:button.cesium-button {:style  {:flex "1"} :id "reset" :type "button" :on-click #(reset-state!)}
         "reset"]]])))



;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(defn main []
  (go (do
        (when-not (@state :font)
          (<! (font/init!)))
        (when-not (@state :intitialized)
          (init-state!))
        (rdom/render [app state] (.getElementById js/document "app"))
        (plot-watch!))))

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  (main)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
