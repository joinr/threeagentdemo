;;copied from example.
;;the idea of decomposing the state is pretty smart,
;;and allows other components to load resources, etc.
(ns threeagentdemo.state
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [threeagent.core :as th]))

;; Use reactive atom for storing state
(defonce state (th/atom {:ticks 0
                         :sphere 0
                         :ticking? false}))
