(ns threeagentdemo.script
  (:require [clojure.set]
            [threeagentdemo.util :as u]))

;;this is kind of janky, but it's time sensitive.
;;we are projectinig the readiness ratinig onto our notion of
;;c-rating for display purposes.  There is a different discrete
;;c-rating in the policies driving this, but for now we ignore that
;;and focus on the normalized dwell / readiness coordinate.

;;In the very near future, we need to have actual rearmm policies
;;that have c5 in them.

(def upper-c
  {:c1 :C1
   :c2 :C2
   :c3 :C3
   :c4 :C4
   :c5 :C5})

;;cribbed from m4.
(defn c-rating [s]
  (->> (clojure.set/intersection
          #{:c1 :c2 :c3 :c4 :c5}
          (if (coll? s) (set s) #{s}))
       first))

;;we could use this as a proxy, right now we don't have
;;any policy with c5.  Alternate is to establish
;;policies with c5
(defn naive-c-rating [ent]
  (let [r (ent :readiness)]
    (cond (>= r 0.8) :C1
          (>= r 0.6) :C2
          (>= r 0.4) :C3
          (>= r 0.2) :C4
          :else :C5)))

;;need to enforce assumptions:

;;locations are by region, so
#_
(def regions
  {:northcom 1
   :pacom    3
   :eucom    3
   :centcom  2})


;; ;;analagous visual commands.
;; ;;assuming we have these in the data already,
;; ;;we can update them.

;;slots only used for finding demand, and computing missed demand.
;;if we ignore it, we can get on without it.

;;then the internal stats should be good to go.

;;extract from frame.
(defn send-home [s id]
  (let [ent      (-> s :entities (get id))
        location (ent :location)
        ;;different...we already know.
        c-rating (naive-c-rating ent) #_(-> ent c-rating upper-c)]
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

(defn deploy-unit [s id location dt]
  (let [ent      (-> s :entities (get id))
        c-rating (naive-c-rating ent) #_(-> ent c-rating upper-c) ;;no longer needed, just get c-rating from state.
        icon     (ent :icon)
        c-icon   (or (get-in u/c-icons [icon c-rating])
                     (throw (ex-info "unknown sprite!" {:in [icon c-rating]})))
        from     (ent :location)]
    (-> s
        (update-in [:locations from] disj id)
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
        (assoc-in  [:waiting id] dt))))

;;moves consist of [:deployed|:home id from to]
(defn tick-moves [s moves]
  (reduce (fn [acc [mv id from to]]
            (case mv
              :deployed  (deploy-unit s id to 1) ;;deploy time is not necessary for replay, remove in future.
              :home      (send-home   s id)
              ;;ignore :dwell moves.
              acc))
          s moves))

;;just merge time varying information into entities.
(defn tick-entities [s ents]
  (let [olds (s :entities)]
    (->>  (reduce-kv (fn [acc id {:keys [curstate location readiness]}]
                      (assoc acc id (assoc (acc id) :state curstate :location location :readiness readiness)))
                     olds ents)
          (assoc s :entities))))

(defn tick-period [s frm] (u/assoc-change s frm :period))

;;since we have velocities, we can lerp entities by adding to their readiness
;;component.  if we have a frame, we just merge the entity data in from it.
;;otherwise we interpolate from the current state.

;;we're just adding up time-variable stats, namely readiness.
(defn lerp-frame [tickstate]
  (reduce-kv (fn [acc id ent]
               (assoc acc id (update ent :readiness + (ent :velocity))))
             tickstate (tickstate :entities)))

(defn tick-frame [tickstate]
  (if-let [next-frame  (first (tickstate :frames))]
    (if (and (tickstate :animating) next-frame)
      (let [{:keys [t period entities moves missed]} next-frame]
        (if (= t (inc (tickstate :c-day)))  ;;merge information from the current frame.
          ;;move entities
          (-> tickstate
              (assoc :c-day  t) ;;update time to frame.
              (tick-moves    moves) ;;process entity movement.
              (tick-entities entities)
              ;;missed demand.
              (assoc-in [:stats :deployed :Missing] missed)
              (assoc :frames (rest (tickstate :frames)))
              #_(tick-period period))
          ;;lerp from the last point, incrementing time.
          (-> tickstate
              (update :c-day inc)
              lerp-frame)))
      tickstate)
    (assoc tickstate :animating false)))

(defn discrete-signal [xs]
  (let [final (atom nil)]
    (concat (for [[[tl vl] [tr vr]] (->> xs
                                         (partition 2 1)
                                         (mapv (fn [[l r :as v]] (reset! final r) v) ))
                  t (range tl tr)]
              [t vl])
            [@final])))

(defn compute-outline [s]
  (let [{:keys [tstart tstop profile]} s]
    (->>  profile
          discrete-signal
          (take-while (fn [[t _]] (<= t tstop)))
          (map (fn [[t v]]
                 #js[#js{:c-day t :trend "Demand" :value v}])))))

(def src->normal
  {"77202K000"  "IBCT"
   "77202K100"  "IBCT" ;;really abn but meh.
   "47112K000"  "SBCT"
   "87312K000"  "ABCT"})

(def icons
  {"IBCT"  "ibct.png"
   "SBCT"  "sbct.png"
   "ABCT"  "abct.png"})

;;for now we need to clean up SRCs, remap knowns to IBCT/SBCT/ABCT....
;;we get our vis-state map from the rendered output.
;; {:c-day    0
;;  :tstart   t0
;;  :tstop    (store/gete ctx0 :parameters :LastDayDefault)
;;  :entities init-entities
;;  :regions  ((store/domains ctx0) :region)
;;  ;;demand and slots don't make sense.
;;  :demand   init-demand
;;  :slots    init-demand
;;  :period   (core/current-period ctx0)
;;  :profile  (core/demand-profile ctx0)
;;  :frames (vec (map frame->vstats (rest h)))}

(defn init-state [m]
  ;;normalize the srcs and add icons.
  (-> m
      (update  :entities
               (fn [es]
                 (reduce-kv (fn [acc e ent]
                              (let [src (-> ent :SRC src->normal)
                                    icon (icons src)]
                                (assoc acc e (assoc ent :SRC src :icon icon)))) es es)))))

#_
(defn init-state! []
  (let [randomized (map (fn [{:keys [readiness] :as e}]
                          (assoc e :readiness (rand))) td/test-entities)
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
                              :Missing 0}
                   :totals  (totals @state)}
           :fill-stats {:northcom empty-fill-stats
                        :eucom    empty-fill-stats
                        :centcom  empty-fill-stats
                        :pacom    empty-fill-stats})
    ;;could be cleaner.  revisit this.
    (watch-until :fill-plot-exists
                 threeagentdemo.vega/charts
                 (fn [m]
                   (when (m :fill-plot-view)
                     (v/push-extents! :fill-plot-view tstart tstop)
                     (reset! demand-profile (compute-outline @state)))))))

(defn read-state [m]
  (-> (cljs.reader/read-string m)
      init-state))
