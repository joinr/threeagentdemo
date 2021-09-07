(ns threeagentdemo.threehelp
  (:require [threeagent.core :as th]
            [threeagent.impl.scene :as scene]))

(def find-context   #'threeagent.impl.scene/find-context )
(def reset-context! #'threeagent.impl.scene/reset-context!)
(def create-context #'threeagent.impl.scene/create-context)

(defn device-pixel-ratio []
  (.-devicePixelRatio js/window))

;;allows caller to set the pixel ratio of the renderer, defaults
;;to device's pixel ratio if not specified.
(defn set-pixels!
  ([renderer ratio]
   (assert (and (number? ratio) (pos? ratio)))
   (.setPixelRatio renderer ratio)
   renderer)
  ([renderer] (set-pixels! renderer (device-pixel-ratio))))

;;renders with a smarter context than the default.
;;exposes more options from three.js, specifically
;;the ability for higher device pixel ratios...
(defn ^threeagent.imp.scene/Context render [root-fn
                                            dom-root
                                            {:keys [on-before-render
                                                    on-after-render
                                                    shadow-map
                                                    pixel-ratio] :as config}]
  (if-let [existing-context (find-context dom-root)]
    (reset-context! existing-context root-fn config)
    (let [context (create-context root-fn dom-root on-before-render on-after-render shadow-map)
          renderer ^js (-> (.-renderer context)
                           (set-pixels! (or pixel-ratio (device-pixel-ratio))))]
      (.setAnimationLoop renderer (.-animateFn context))
      context)))

