;;copied from example.
;;the idea of decomposing the state is pretty smart,
;;and allows other components to load resources, etc.
(ns threeagentdemo.state
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [threeagent.core :as th]))

(def default-options
  {:disclaimer? true
   :marks?      true
   :fill/sampling-frequency 2})

;; Use reactive atom for storing state
(defonce state (th/atom {:animating true
                         :ticks 0
                         :sphere 0
                         :layout :stacked
                         :containers :split
                         :class "UNCLASSIFIED"
                         :disclaimer  "Notional"
                         :options default-options}))


(defn toggle-option [k]
  (do (swap! state (fn [m] (update-in m [:options k] not)))
      nil))
