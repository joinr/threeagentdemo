(ns threeagentdemo.script
  (:require [clojure.set]))

(defn upper-c
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
        c-rating (-> ent c-rating upper-c)]
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
        c-rating (-> ent c-rating upper-c) ;;no longer needed, just get c-rating from state.
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
              :deployed  (deploy-unit s id to)
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
  (if-let [next-frame (first (tickstate :frames))]
    (let [{:keys [t period entities moves missed]} next-frame]
      (if (= t (inc (tickstate :c-day)))  ;;merge information from the current frame.
        ;;move entities
        (-> tickstate
            (assoc :c-day t) ;;update time to frame.
            (tick-moves    moves) ;;process entity movement.
            (tick-entities entities)
            ;;missed demand.
            (assoc-in [:stats :deployed :Missing] missed)
            (tick-period period))
        ;;lerp from the last point, incrementing time.
        (-> tickstate
            (update :c-day inc)
            lerp-frame)))
    tickstate))

#_
(defn missed-demand [s] (->> s :slots vals (filter pos?) (reduce +)))

#_
(defn tick-deploys [s]
  (if-let [deps (find-deploys s)]
    (reduce (fn [acc {:keys [id location wait-time]}]
              (deploy-unit acc id location wait-time))
            s deps)
    s))

#_
(defn tick [tickstate]
  (-> tickstate
     (update :c-day + 1)
     tick-home
     tick-waits
     tick-conflict ;;lame on purpose
     tick-deploys
     tick-missing))
#_
(defn compute-outline [s]
  (let [{:keys [tstart tstop demand conflict-demand tconflict]} s
        compdemand (reduce + (vals demand))
        confdemand (+ compdemand (reduce + (vals conflict-demand)))]
    (concat (for [t (range tstart (dec tconflict))]
              #js[#js{:c-day t :trend "Demand" :value compdemand}])
            (for [t (range tconflict tstop)]
              #js[#js{:c-day t :trend "Demand" :value confdemand}]))))
