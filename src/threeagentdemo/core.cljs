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
   [threeagentdemo.script :as script]
   [cljs.core.async :as a :refer [chan put! >! <! close!]]
   [threeagentdemo.dash :as dash])
  (:require-macros [cljs.core.async :refer [go]]))

(def ^:dynamic *bbox-color* "black")

;;empirically determined....
(def +sprite-width+ 0.5333333333333333)

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

(defn cocom [position-scale  width height title font items]
  [:object position-scale
   [:object {:position [-2.25 7 -0.99]
             :scale    [1 1 0.1]}
    [:text {:text title
            :material (get-mat "black")
            :font font
            :height 0.1
            :size 0.6}]]
   [:box {:width width :height height :material {:color "lightgrey"}
          :position [0 3.8 -1]
          :scale [1 1 0.03]}]
   [:object {:position [0 1.8 -0.95]
             :scale [0.9 0.9 1.0]}
    [container 10 1
     (filterv (fn [[_ {:keys [source props]}]]
                (or (clojure.string/includes? source "abct")
                    (some-> props :SRC (= "ABCT")))) items)]
    [translate [0.5 2.7 0]
     [container 10 1
      (filterv (fn [[_ {:keys [source props]}]]
                 (or (clojure.string/includes? source "ibct")
                     (some-> props :SRC (= "IBCT")))) items)]
     [translate [0.5 2 0]
      [container 10 1
       (filterv (fn [[_ {:keys [source props]}]]
                  (or (clojure.string/includes? source "sbct")
                      (some-> props :SRC (= "SBCT")))) items)]]]]])

(defn northcom [font items]
  [id :northcom
   [cocom {:position [-10 0 -10]
           :scale    [0.5 0.5 0.5]}
    10.5  6 "USNORTHCOM" font items]])

(defn eucom [font items]
  [id :eucom
   [cocom {:position [-2 0 -10]
           :scale    [0.5 0.5 0.5]}
    10.5  6 "USEUCOM" font items]])

(defn centcom [font items]
  [id :centcom
   [cocom  {:position [4 0 -10]
            :scale    [0.5 0.5 0.5]}
    10.5  6 "USCENTCOM" font items]])

(defn pacom [font items]
  [id :pacom
   [cocom   {:position [10 0 -10]
             :scale    [0.5 0.5 0.5]}
    10.5  6 "USINDOPACOM" font items]])

(defn southcom [font items]
  [id :pacom
   [cocom   {:position [10 0 -10]
             :scale    [0.5 0.5 0.5]}
    10.5  6 "USSOUTHCOM" font items]])

(defn africom [font items]
  [id :pacom
   [cocom   {:position [10 0 -10]
             :scale    [0.5 0.5 0.5]}
    10.5  6 "USAFRICOM" font items]])

(defn irf [font items]
  [id :pacom
   [cocom   {:position [10 0 -10]
             :scale    [0.5 0.5 0.5]}
    10.5  6 "IRF" font items]])

(defn racetrack
  ([title width {:keys [title-position contents]
                 :or   {title-position  [-1 4.25 0.1]}}]
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
     [:text {:position title-position #_[-1 4.25 0.1]
             :scale    [ 1 1 0.1]
             :text title
             :material (get-mat "black")
             :font     (@state :font)
             :height   0.1
             :size     0.5}]
     contents]))
  ([title width] (racetrack title width {})))

(defn entity-sprite [id]
  (when-let [ent (-> @state :entities id)]
    [:sprite {:source (ent :icon)}]))

(defn compo-key [e]
  (case (e :compo)
    "AC" "AC" "RC"))

(defn entities-at-home
  ([type s]
   (let [ids     (-> s :types (get type))
         at-home (-> s :locations (get :home))
         ids     (clojure.set/intersection ids at-home)
         ents (s :entities)]
     (mapv ents ids)))
  ([type] (entities-at-home type @state)))

(defn grouped-entities-at-home
  ([s]
   (let [ids     (-> s :locations (get :home))
         ents    (s :entities)
         home    (mapv ents ids)]
     (->> (for [[src xs] (group-by :SRC home)
                [compo ys] (group-by (fn [e]
                                       (case (e :compo)
                                         "AC" "AC"
                                         "RC")) xs)]
            [src compo (sort-by :readiness ys)])
          (reduce (fn [acc [src compo xs]]
                    (assoc-in acc [src compo] xs)) {}))))
  ([] (grouped-entities-at-home @state)))

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

(defn missing-items
  ([n m]
   (when n (repeat n [:sprite {:source "empty.png" :props m}])))
  ([n] (missing-items n {})))

;;we can grab the missed units by src.
;;maybe we group contents by src...

;;we now group-by a location's contentes by src, then concat
;;the missing items on to each SRC track, returning the resulting
;;vector.
(defn icon-src [icon]
  (let [id (-> icon meta :id)]
    (get-in @state [:entities id :SRC])))

(defn contents [location]
  (let [src-stats  @(th/cursor state [:fill-stats location])
        src-missing (reduce-kv (fn [acc src stats]
                                 (assoc acc src (get stats "Missed" 0))) {} src-stats)]
    (->> (for [xs (partition-by icon-src @(th/cursor state [:contents location]))]
           (let [src (icon-src (first xs))
                 missed (src-missing src)]
             (concat xs (missing-items missed {:SRC src}))))
         (apply concat)
         vec)
    #_
  (vec (concat @(th/cursor state [:contents location])
               (missing-items @(th/cursor state [:slots location]))))))

(defn counts->title [s src]
  (let [cs    (get s src {"AC" 0 "NG" 0 "USAR" 0})
        total (reduce + (vals cs))]
    (str src " (" total ")" ": "  (cs "AC" 0) "/" (+ (cs "NG" 0) (cs "USAR" 0)) #_#_"/" (cs "USAR" 0))))

(defn compo-icons [position-scale offset ac-icons rc-icons]
  [:object position-scale
   [icons-at-home ac-icons]
   [:object {:position [(* +sprite-width+ (+ offset 8)) 0 0]}
    [:box {:position [(- +sprite-width+) 0 0 ]
           :width 0.1
           :height 14.25
           :depth 0.01
           :material (get-mat "black")}]
    [icons-at-home rc-icons]]])

(defn entity-count [counts src compo]
  (or (some-> counts
              (get src)
              (get compo))
      0))

;; Root component render function
(defn scene []
  (let [ticks @(th/cursor state [:ticks])
        font  @(th/cursor state [:font])
        sin (.sin js/Math (/ ticks 100))
        cos (.cos js/Math (/ ticks 100))
        {:strs [IBCT ABCT SBCT]} (grouped-entities-at-home @state)
        abcts (concat (get ABCT "AC") (get ABCT "RC"))
        sbcts (concat (get SBCT "AC") (get SBCT "RC"))
        ibcts (concat (get IBCT "AC") (get IBCT "RC"))
        counts (@state :ac-rc-count)
        titles (@state :titles)]
    [:object
     #_[:ambient-light {:intensity 0.6}]
     [:object {:position [0 0 5]}
      [:point-light {:intensity 0.5}]]
     [:object {:position [0 -6 5]}
      [:point-light {:intensity 0.6}]]
     [:group   {:position [0 2 2]} ;;container layer
      [:object {:position [0 0 -15]
                :scale [40 40 1]}
       [u/sprite {:source "1024px-BlankMap-World-Flattened.svg.png"}]]
      [:group {:position [0 -2 0]}
       [northcom font (contents :northcom)]
       [eucom    font (contents :eucom)   ]
       [centcom  font (contents :centcom) ]
       [pacom    font (contents :pacom)   ]]]
     [:group   {:position [0 -6.5 0]}
      [:plane  {:position [0 1.05  -9]
                :width 32 :height 5 :material {:color "white"}}]
      [:group {:position [-12.85 -0.3 -8.5] :scale [0.9 0.9 0.1]}
       [:text {:position [0 0.85 0]
               :text "MOD"
               :material (get-mat "black")
               :font     (@state :font)
               :height   0.1
               :size     0.4}]
       [:text {:position [0 1.7 0]
              :text "TRN"
               :material (get-mat "black")
               :font     (@state :font)
               :height   0.1
               :size     0.4}]
       [:text {:position [0 2.505 0]
               :text "MSN"
               :material (get-mat "black")
               :font     (@state :font)
               :height   0.1
               :size     0.4}]]
      [:object {:position [-9 0 0]}
       [racetrack (titles "ABCT") 8.1 {:title-position [-1.5 4.25 0.1]}] ]
      ;;have to place these the in top level a
      [compo-icons {:position [-11.75 0 -8.8]
                    :scale [0.4 0.4 1]}
       (inc (entity-count counts "ABCT" "AC")) (get ABCT "AC") (get ABCT "RC")]
      [:object {:position [1.5 0 0]}
       [racetrack (titles "IBCT") 18 {:title-position [-2 4.25 0.1]}]]
      [compo-icons {:position [-5 0 -8.8]
                    :scale    [0.4 0.4 1]}
       (+ (entity-count counts "IBCT" "AC")  5) (get IBCT "AC") (get IBCT "RC")]
      [:object {:position [11.15 0 0]}
       [racetrack (titles "SBCT") 6 {:title-position  [-4 4.25 0.1]}]]
      [compo-icons {:position [9.5 0 -8.8]
                    :scale    [0.4 0.4  1]}
       (entity-count counts "SBCT" "AC") (get SBCT "AC") (get SBCT "RC")]]]))

(defn three-canvas [name f on-before-render]
  (r/create-class
   {:display-name (str name)
    :reagent-render (fn [] [:canvas])
    :component-did-mount
    (fn [this]
      (let [three-node (rdom/dom-node this)
            _          (swap! state assoc :three-canvas three-node)]
        (->> (threehelp/render f three-node {:render-params {:antialias true :resize true}
                                             :on-before-render on-before-render})
             #_(swap! state assoc :scene))))

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

;;another option is to have several slices and use tick-conflict to ramp up
;;based on a time step.

(def conflict-demands
  {450 {:northcom 4
        :pacom    10
        :eucom    10
        :centcom  0}
   470 {:northcom 1
        :pacom    0
        :eucom    4
        :centcom  1}
   500 {:northcom 0
        :pacom    0
        :eucom    3
        :centcom  0}
   520 {:northcom 0
        :pacom    0
        :eucom    2
        :centcom  0}
   550 {:northcom 0
        :pacom    0
        :eucom    2
        :centcom  0}})

;;create a lame demand that is low for 6 months, then goes to conflict.
;;as currently stated, we have a map of demand-by-region.
;;regions are equivalent to demand.
;;So if we increase demand at time 1, we just assoc into the :regions key
;;with the deltas, add increment the corresponding slots.


;;for now, this is doing nothing since we have garbage problems.
(def c-day  (th/cursor state [:c-day]))
(def period (th/cursor state [:period]))
(def demand-profile (th/cursor state [:demand-profile]))

(def readiness-rate
  {"AC"  (u/precision (/ 1 365) 4)
   "NG"  (u/precision (/ 1 (* 5 365)) 4)})

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
                             (sort-by #(- (% :readiness)))
                             (take open-slots))]
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

(defn naive-c-rating [ent]
  (let [r (ent :readiness)]
    (cond (>= r 0.8) :C1
          (>= r 0.6)  :C2
          (>= r 0.4) :C3
          (>= r 0.2) :C4
          :else :C5)))

(defn deploy-unit [s id location dt]
  (let [ent (-> s :entities (get id))
        c-rating (naive-c-rating ent)
        icon     (ent :icon)
        c-icon   (or (get-in u/c-icons [icon c-rating])
                     (throw (ex-info "unknown sprite!" {:in [icon c-rating]})))]
    (-> s
        (update-in [:locations :home] disj id)
        (update-in [:locations location] (fn [v] (conj (or v #{}) id)))
        (update-in [:contents  location] (fn [v]
                                           (u/insert-by (or v [])
                                                        (fn [itm] (-> itm second :source))
                                                        ^{:id id}
                                                        [:sprite {:source c-icon #_(ent :icon)}])))
        (update-in [:slots     location] dec)
        (update-in [:entities id] assoc  :wait-time dt :location location)
        (update-in [:stats :deployed c-rating] inc)
        (update-in [:fill-stats location (ent :SRC) (u/c-rating->fill-stat c-rating)] inc)
        (assoc-in [:waiting id] dt))))

(defn missed-demand [s]
  (->> s :slots vals (filter pos?) (reduce +)))

(defn tick-deploys [s]
  (if-let [deps (find-deploys s)]
    (reduce (fn [acc {:keys [id location wait-time]}]
              (deploy-unit acc id location wait-time))
            s deps)
    s))

#_
(defn update-missed-demand [s]
  (reduce-kv (fn [acc location v]
               (let [missed (get-in acc [:fill-stats location "Missed" "C1"])]
                 (if (= missed v)
                   acc
                   (assoc-in acc [:fill-stats location "Missed" "C1"] v))))
             s (s :slots)))

;;For now we either neuter this to eliminate missed demand, or we
;;add random misses per SRC.  For expedience, I just ignore it for now.
(defn update-missed-demand [s]
  s
  #_
  (reduce-kv (fn [acc location v]
               (let [missed (get-in acc [:fill-stats location "Missed" "C1"])]
                 (if (= missed v)
                   acc
                   (assoc-in acc [:fill-stats location "Missed" "C1"] v))))
             s (s :slots)))

(defn tick-missing [s]
  (-> s
      (assoc-in [:stats :deployed :Missing] (missed-demand s))
      update-missed-demand))

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
          (update-in [:entities id] assoc :wait-time 0 :readiness 0 :location :home)
          (update-in [:stats :deployed c-rating] dec)
          (update-in [:fill-stats location (ent :SRC) (u/c-rating->fill-stat c-rating)] dec)
          (update-in [:waiting] dissoc id))))

(defn tick-waits [s]
  (let [waits (s :waiting)]
    (reduce-kv (fn [acc id dt]
                 (if (zero? dt)
                   (send-home acc id)
                   (update-in acc [:waiting id] dec)))
               s waits)))

#_
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

(defn tick-conflict [s]
  (if-let [tc (s :tconflict)]
    (let [demands (s :conflict-demands)]
      (if-let [added-demand (some-> s :conflict-demands (get  (s :c-day)))]
        (let [new-demand   (merge-with + added-demand (@state :demand))
              new-slots    (merge-with + added-demand (@state :slots))]
          (assoc s :demand new-demand :slots new-slots :period "Conflict"
                 :deploy-threshold 0.2))
        s))
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

(defn general-tick [s]  ((s :tick-fn tick-scene) s))


;;State initialization / load
;;===========================

;;modify state with new information based on entities, works with
;;replays as well.
(defn init-entities [s ents]
  (let [emap     (into {} (map (fn [e] [(e :id) e])) ents)
        contents (u/map-vals (fn [xs] (set (map :id xs)))  (group-by :location ents))
        types    (u/map-vals  (fn [xs] (set (map :id xs))) (group-by :SRC ents))
        counts   (into {} (for [[src xs] (group-by :SRC ents)]
                            [src (frequencies (map :compo xs))]))
        ac-rc-count (into {} (for [[src xs] (group-by :SRC ents)]
                               [src (frequencies (map compo-key xs))]))
        titles   (into {} (map (fn [k] [k (counts->title counts k)])) (keys types))]
    (assoc s :entities emap :locations contents :types types :waiting {} :contents contents
             :counts counts
             :ac-rc-count ac-rc-count
             :titles titles)))

(defn totals [s]
  (->> s :entities vals
       (reduce (fn [acc e]
                 (case (e :location)
                   :home (case (naive-c-rating e)
                           (:C1 :C2) (update acc :available inc)
                           (update acc :unavailable inc))
                   (update acc :mission inc)))
               {:mission 0
                :available 0
                :unavailable 0})))

(defn init-stats [s tstart tstop empty-stats]
  (assoc s
         :c-day 0
         :tstart tstart
         :tstop  tstop
         :stats {:deployed {:C1 0
                            :C2 0
                            :C3 0
                            :C4 0
                            :C5 0
                            :Missing 0}
                 :totals  (totals s)}
         :fill-stats {:northcom empty-stats
                      :eucom    empty-stats
                      :centcom  empty-stats
                      :pacom    empty-stats}
         :render-mode :live))

(defn init-demand [s demand]
  (assoc s :demand demand :slots demand
;         :conflict-demand conflict-demand
         :conflict-demands conflict-demands
         :tconflict 450
         :period "Competition"
         :deploy-threshold 0.7))

(defn conflict-profile [tstop m]
  (let [total  (atom 0)
        final  (atom nil)]
    (concat (vec (for [[l r] (partition 2 1 (sort (keys m)))]
                   (do (reset! final r)
                       [[l r] (reset! total (reduce + @total (vals (m l))))])))
            [[[@final (inc tstop)] (+ @total (reduce + (vals (m @final))))]])))
#_
(defn compute-outline [s]
  (let [{:keys [tstart tstop demand conflict-demand tconflict]} s
        compdemand (reduce + (vals demand))
        confdemand (+ compdemand (reduce + (vals conflict-demand)))]
    (concat (for [t (range tstart (dec tconflict))]
              #js[#js{:c-day t :trend "Demand" :value compdemand}])
            (for [t (range tconflict tstop)]
              #js[#js{:c-day t :trend "Demand" :value confdemand}]))))

(defn compute-outline [s]
  (let [{:keys [tstart tstop demand conflict-demands tconflict]} s
        compdemand (reduce + (vals demand))
        cdemands (for [[[l r] v] (conflict-profile tstop conflict-demands)
                       t         (range l r)]
                   #js[#js{:c-day t :trend "Demand" :value (+ v compdemand)}])]
    (concat (for [t (range tstart (dec tconflict))]
              #js[#js{:c-day t :trend "Demand" :value compdemand}])
            cdemands)))

(def empty-fill-stats
  ;;map of {src {c1 c2 <=c3 empty}}
  {"IBCT" {"C1" 0 "C2" 0 "<=C3" 0 "Missed" 0}
   "SBCT" {"C1" 0 "C2" 0 "<=C3" 0 "Missed" 0}
   "ABCT" {"C1" 0 "C2" 0 "<=C3" 0 "Missed" 0}})

(defn fill-stats->entries [m]
  (let [srcs (keys (first (vals m)))]
    (apply concat ["SRC" "C1" "C2" "<=C3" "Missed"]
           (for [[src cs] m]
             (cons src (vals cs))))))

(def empty-fill-entries
  (fill-stats->entries empty-fill-stats))

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

(defn percentages [counts]
  (let [total (reduce + (vals counts))]
    (reduce-kv (fn [acc k v]
                 (assoc acc k
                        (Math/round (u/precision (double (* 100 (/ v total))) 3))))
               counts counts)))

(defn init-state! []
  (let [randomized (map (fn [{:keys [readiness] :as e}]
                          (assoc e :readiness (rand))) td/test-entities)
        tstart 0
        tstop  1000]
    (swap! state
           #(-> %
                (init-entities randomized)
                (init-demand regions)
                (init-stats  tstart tstop empty-fill-stats)))
    ;;could be cleaner.  revisit this.
    (watch-until :fill-plot-exists
                 threeagentdemo.vega/charts
                 (fn [m]
                   (when (m :fill-plot-view)
                     (v/push-extents! :fill-plot-view tstart tstop)
                     (v/clear-data! :fill-plot-view :table-name "demandtrend")
                     (reset! demand-profile (compute-outline @state)))))))

;;now we use the scripting ns to do the same.
;;the differences here are that we pass in a different setup.
(defn init-replay-state [vstats]
  (let [{:keys [entities tstart tstop demand slots period frames c-day profile] :as vstats}
        (script/init-state vstats)]
    (swap! state
           #(-> %
                (init-entities (vals entities)) ;;necessary, works with entities
                (assoc :demand demand :slots slots :c-day c-day :period period :conflict-demand {} :frames
                       frames) ;;kinda lame.
                ;(init-demand regions);;not necessary?
                (init-stats  tstart tstop empty-fill-stats)
                (assoc :tick-fn script/tick-frame))) ;;should work as normal, if regions are locked.
    ;;could be cleaner.  revisit this.
    (v/push-extents! :fill-plot-view tstart tstop)
    (reset! demand-profile (script/compute-outline vstats))
    #_
    (watch-until :fill-plot-exists
                 threeagentdemo.vega/charts
                 (fn [m]
                   (when (m :fill-plot-view)
                     (v/push-extents! :fill-plot-view tstart tstop)
                     (reset! demand-profile (script/compute-outline vstats)))))))

(defn load-replay-state! [path]
  (println [:loading path])
  (u/make-remote-call path
                      (fn [data] (init-replay-state data)
                        (println [:loaded path]))))

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
        #js{:c-day t :trend "Missed Demand" :value Missing}]))

(defn plot-watch! []
  (add-watch demand-profile :demand-profile
             (fn [k r o n]
               (println [:updating-demand-profile!])
               (v/clear-data! :fill-plot-view :table-name "demandtrend")
               (v/push-samples! :fill-plot-view n
                                :table-name "demandtrend")))
  (add-watch c-day :plotting
               (fn [k r oldt newt]
                 (cond (< newt oldt)
                       (do (v/rewind-samples! :fill-plot-view "c-day" newt))
                       (> newt oldt)
                       (do (v/push-samples!   :fill-plot-view (daily-stats newt)))
                       :else nil))))

;;helpers
(defn current-context []
  (some-> @state :three-canvas threehelp/find-context))

(defn current-camera []
  (some-> (current-context) .-camera))

(defn world-position [nd & {:keys [update? v]
                            :or {update? false}}]
  (let [nd (if (keyword? nd)
             (some-> @state :nodes (get nd))
             nd)
        v  (or v (js/three.Vector3.))]
    (when nd
      (when update? (.updateWorldMatrix nd true false))
      (.getWorldPosition nd v)
      v)))

(defn ->three-vector [x y z]
  (let [tv (js/three.Vector3.)]
    (aset tv "x" x)
    (aset tv "y" y)
    (aset tv "z" z)
    tv))

(defn project-css
  ([v]
   (let [ctx    (current-context)
         _      (.project v (.-camera ctx))
         canvas (.-canvas ctx)]
     [(* (+ (* (.-x v) 0.5) 0.5) (.-clientWidth canvas))
      (* (+ (* (.-y v) 0.5) 0.5) (.-clientHeight canvas))]))
  ([x y z] (project-css (->three-vector x y z))))

(defn css-coordinates [nd]
  (when-let [v (world-position nd)]
    (let [ctx    (current-context)
          _  (.project v (.-camera ctx))
          canvas (.-canvas ctx)]
      [(* (+ (* (.-x v) 0.5) 0.5) (.-clientWidth canvas))
       (* (+ (* (.-y v) 0.5) 0.5) (.-clientHeight canvas))])))

(defn css-bounds [nd]
  (let [bnds (world-bounds nd)]
    {:min (project-css (bnds :min))
     :max (project-css (bnds :max))}))

(defn fill-table [entries]
  [dash/flex-table 5 entries :style (assoc dash/default-style :font-size "0.53em")
   :header-style (assoc dash/default-cell-style :border "solid")])

(def overlay-style
 {:position "absolute" :z-index "10" :bottom "5%" :width "22%" :height "30%"
   :left "0%"})

;;fairly janky but closer to what we want to see...
(defn fill-overlay [northcom eucom centcom pacom]
  [:div
   [:div  {:style (assoc overlay-style :left "0%")}
    [fill-table (fill-stats->entries northcom)]]
   [:div {:style (assoc overlay-style :left "31%")}
    [fill-table (fill-stats->entries eucom)]]
   [:div {:style (assoc overlay-style :left "55%")}
    [fill-table  (fill-stats->entries centcom)]]
   [:div {:style (assoc overlay-style :left "78%")}
    [fill-table (fill-stats->entries pacom)]]])

(defn bordered [c]
  [:div {:style {:border-style "solid"}}
   c])

(defn fill-row [northcom eucom centcom pacom]
  [:div {:style {:display "flex" :flex-direction "row" :width "100%" :height "100%"
                 :justify-content "space-between"
                 :font-size "2em"}}
   (for [e [northcom eucom centcom pacom]]
     (bordered (fill-table (fill-stats->entries e))))
   (comment
   [fill-table (fill-stats->entries northcom)]
   [fill-table (fill-stats->entries eucom)]
   [fill-table  (fill-stats->entries centcom)]
   [fill-table (fill-stats->entries pacom)])])

(defn flex-row
  [& contents]
  (let [c1 (first contents)
        attrs (if (map? c1) c1 {})
        style (->> (get  attrs :style)
                   (merge {:display "flex" :flex-direction "row" :width "100%" :height "100%"}))
        contents (if (map? c1) (rest contents) contents)]
     `[:div.header ~(assoc attrs :style style)
       ~@contents]))

;;WIP
(defn wide-page [ratom]
  (let [render-scene! (fn [dt] (swap! ratom general-tick))
        nc            (th/cursor ratom [:fill-stats :northcom])
        ec            (th/cursor ratom [:fill-stats :eucom])
        cc            (th/cursor ratom [:fill-stats :centcom])
        pc            (th/cursor ratom [:fill-stats :pacom])
        total-stats   (th/cursor ratom [:stats :totals])
        fancy-percents (fn [m]
                         (let [res (percentages m)]
                           ["Mission" "Available" "Unavailable"
                            (str (res :mission) "%")
                            (str (res :available) "%")
                            (str (res :unavailable) "%")]))
        _              (add-watch c-day :percentages
                          (fn [_ _ oldt newt]
                            (when (> newt oldt)
                              (reset! total-stats (totals @ratom)))))]
    (fn []
      [:div.header {:style {:display "flex" :flex-direction "column" :width "100%" :height "100%"}}
       [:div.header  {:style {:display "flex" :width "100%" :height  "auto"  :class "fullSize" :overflow "hidden"
                              :justify-content "space-between"
                              :font-size "xxx-large"}}
        [:p {:id "c-day" :style {:margin "0 auto" :text-align "center" }}
         ;;no idea why this causes a slow memory leak!
         (str "Day:"  (int @c-day))
         ]
        [:p {:id "period" :style {:margin "0 auto" :text-align "center" }}
         #_@period (str "Period: "  @period)
         ]]
       [flex-row {:style {:justify-content "space-between"}}
        [:div {:id "chart" :style {:flex "0.48" :display "flex" :width "48%" :height "auto" :max-width "48%"
                                   :flex-direction "column"}}
         [v/vega-chart "fill-plot" (assoc v/fill-spec :height 260)]]
        [:div  {:style {:display "flex" :flex "0.5" :max-width "50%"
                        :height  "auto"  #_#_:class "fullSize" :overflow "hidden"
                        :justify-content "space-between"}}
         [three-canvas "root-scene" scene render-scene!]
         #_[fill-overlay @nc @ec @cc @pc]]]
       [dash/flex-table 3 (fancy-percents @total-stats)
        :style {:display "flex" :flex-wrap "wrap" ;:background "darkgray"
                :font-size "2em"
                :text-align "center"}
        :cell-style (assoc dash/default-cell-style :justify-content "middle")]
       [flex-row {:style {:justify-content "space-around"}}
        [:img {:src "northcom.png" :style { :width "7%"}}]
        [:p {:style {:font-size "1.5em"}} "USNORTHCOM" ]
        [:img {:src "eucom.png" :style {:width "7%"}}]
        [:p {:style {:font-size "1.5em"}} "USEUCOM" ]
        [:img {:src "centcom.png" :style {:width "7%"}}]
        [:p {:style {:font-size "1.5em"}} "USCENTCOM" ]
        [:img {:src "pacom.png" :style {:width "7%"}}]
        [:p {:style {:font-size "1.5em"}} "USINDOPACOM" ]]
       [fill-row @nc @ec @cc @pc]
       [:div.flexControlPanel {:style {:display "flex" :width "100%" :height "100%" #_"auto"}}
        [:button.cesium-button {:style   {:flex "1" :font-size "1em"} :id "play" :type "button" :on-click #(play!)}
         "play"]
        [:button.cesium-button {:style {:flex "1" :font-size "1em"} :id "stop" :type "button" :on-click #(stop!)}
         "stop"]
        [:button.cesium-button {:style  {:flex "1" :font-size "1em"} :id "reset" :type "button" :on-click #(reset-state!)}
         "reset"]]])))

(defn stacked-page [ratom]
  (let [render-scene! (fn [dt] (swap! ratom general-tick))
        nc            (th/cursor ratom [:fill-stats :northcom])
        ec            (th/cursor ratom [:fill-stats :eucom])
        cc            (th/cursor ratom [:fill-stats :centcom])
        pc            (th/cursor ratom [:fill-stats :pacom])
        total-stats   (th/cursor ratom [:stats :totals])
        fancy-percents (fn [m]
                         (let [res (percentages m)]
                           ["Mission" "Available" "Unavailable"
                            (str (res :mission) "%")
                            (str (res :available) "%")
                            (str (res :unavailable) "%")]))
        _              (add-watch c-day :percentages
                          (fn [_ _ oldt newt]
                            (when (> newt oldt)
                              (reset! total-stats (totals @ratom)))))]
    (fn []
      [:div.header {:style {:display "flex" :flex-direction "column" :width "100%" :height "100%"}}
       [:div {:id "chart-root" :style {:display "flex"}}
        [:div {:style {:flex "0.95" #_"0.95" :max-width "95%"}}
         [v/vega-chart "fill-plot" (assoc v/fill-spec :height 100)]]]
       [dash/flex-table 3 (fancy-percents @total-stats)
        :style {:display "flex" :flex-wrap "wrap" ;:background "darkgray"
                        :font-size "1em"}]
       [:div  {:style {:display "flex" :width "100%" :height  "auto"  :class "fullSize" :overflow "hidden"
                       :justify-content "space-between"}}
        [three-canvas "root-scene" scene render-scene!]
        [fill-overlay @nc @ec @cc @pc]]
       [:div.header  {:style {:display "flex" :width "100%" :height  "auto"  :class "fullSize" :overflow "hidden"
                              :justify-content "space-between"
                              :font-size "xx-large"}}
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

(defn page [ratom]
  #_(stacked-page ratom)
  (wide-page ratom)
  #_(let [layout (@ratom :layout)]
    (case layout
      :stacked (stacked-page ratom)
      :horizontal (wide-page ratom)
      [:p (str "unknown layout!" layout)])))


;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(defn main []
  (go (do
        (when-not (@state :font)
          (<! (font/init!)))
        (when-not (@state :intitialized)
          (init-state!))
        (rdom/render [page state] (.getElementById js/document "app"))
        (plot-watch!))))

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  (main)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
