;;ns created to help hack on threeagent from afar for
;;my use case.  improvements will ship upstream.
(ns threeagentdemo.threehelp
  (:require [threeagent.core :as th]
            [threeagentdemo.util :as util]
            [threeagent.impl.scene :as scene]
            ["three" :as three]
            [threeagent.impl.virtual-scene :as vscene]))

;;kind of lame, global resize listener for our canvas.  just getting it working for now.
(defonce resized
  (let [res (th/atom nil)]
    (.addEventListener js/window "resize"
                       (fn []
                         (reset! res true)))
    res))

;;imports since these are private and we need access (prefer to have stuff public)
(def find-context        #'threeagent.impl.scene/find-context ) ;;broken.
;;(def reset-context!    #'threeagent.impl.scene/reset-context!)
;;(def create-context    #'threeagent.impl.scene/create-context)
(def get-canvas           #'threeagent.impl.scene/get-canvas)
(def set-shadow-map!      #'threeagent.impl.scene/set-shadow-map!)
(def init-scene           #'threeagent.impl.scene/init-scene )
(def animate              #'threeagent.impl.scene/animate)
(def remove-all-children! #'threeagent.impl.scene/remove-all-children!)
#_(def contexts            @#'threeagent.impl.scene/contexts)


;;https://threejs.org/docs/#api/en/renderers/WebGLRenderer
(def default-params
  {:canvas nil
   :context nil
   :precision  "highp" ;;highp", "mediump" or "lowp"
   :alpha    false
   :premultipliedAlpha false
   :antialias false
   :stencil  true
   :preserveDrawingBuffer  false
   :powerPreference  :default ;; "high-performance", "low-power" or "default"
   :failIfMajorPerformanceCaveat false
   :depth true
   :logarithmicDepthBuffer false
   ;;not three.js related.
   :resize true})

;;may be obviated with resizing...
;;allows caller to set the pixel ratio of the renderer, defaults
;;to device's pixel ratio if not specified.
(defn set-pixels!
  ([renderer ratio]
   (assert (and (number? ratio) (pos? ratio)))
   (.setPixelRatio renderer ratio)
   renderer)
  ([renderer] (set-pixels! renderer (util/device-pixel-ratio))))

;;alpha: true,
;;antialias: true

;;we ensure the device pixel ratio is always nice.
(defn default-render-setup [r]
  r
  #_(set-pixels! r))

;;responsive canvas sizing.


(defn resize-renderer-to-display-size [renderer]
  (let [canvas (.-domElement renderer)
        pixel-ratio (.-devicePixelRatio js/window)
        width  (bit-or  (* pixel-ratio (.-clientWidth canvas)) 0)
        height (bit-or  (* pixel-ratio (.-clientHeight canvas)) 0)
        need-resize (or @resized
                        (or (not= (.-width canvas) width) (not= (.-height canvas) height)))]
    (when need-resize
      (println {:old  [(.-clientWidth canvas) (.-clientHeight canvas) ]
                :new  [width height]})
      (.setSize renderer width height false)
      (reset! resized false))
    need-resize))


(defn resize-on-render [renderer camera]
  (when (resize-renderer-to-display-size renderer)
    (let [_ (println "resizing")
          canvas (.-domElement renderer)]
      (set! (.-aspect camera) (/ (.-clientWidth canvas) (.-clientHeight canvas)))
      (.updateProjectionMatrix camera))))


(defn ^scene/Context create-context
  ([root-fn dom-root on-before-render-cb on-after-render-cb shadow-map render-params]
   (let [canvas (get-canvas dom-root)
         width (.-offsetWidth canvas)
         height (.-offsetHeight canvas)
         virtual-scene (vscene/create root-fn)
         renderer (new three/WebGLRenderer (clj->js (assoc render-params :canvas canvas)))
         camera   (three/PerspectiveCamera. 75 (/ width height) 0.1 1000)
         cameras  (array)
         scene-root (new three/Scene)
         clock (new three/Clock)]
     (.setSize renderer width height)
     (set-shadow-map! renderer shadow-map)
     (let [context (scene/Context. virtual-scene
                                   scene-root
                                   dom-root nil
                                   canvas camera cameras
                                   clock renderer on-before-render-cb on-after-render-cb)
          animator (if (render-params :resize)
                      (fn []
                        (resize-on-render renderer camera)
                        (animate context))
                      #(animate context))]
      (set! (.-animateFn context) animator #_#(animate context))
      (init-scene context virtual-scene scene-root)
      (.push @#'threeagent.impl.scene/contexts #_contexts context)
      context)))
  ([root-fn dom-root on-before-render-cb on-after-render-cb shadow-map]
   (create-context root-fn dom-root on-before-render-cb on-after-render-cb shadow-map {})))

(defn reset-context! [^scene/Context context root-fn {:keys [on-before-render on-after-render shadow-map
                                                             render-setup]}]
  (let [scene-root ^js (.-sceneRoot context)
        virtual-scene ^vscene/Scene (.-virtualScene context)
        new-virtual-scene (vscene/create root-fn)
        renderer ^js  (.-renderer context)]
    (remove-all-children! context (.-root virtual-scene))
    (vscene/destroy! virtual-scene)
    (set-shadow-map! renderer shadow-map)
    (set! (.-cameras context) (array))
    ;;new, toggle properties if desired.
    (when render-setup (render-setup renderer)) 
    (init-scene context new-virtual-scene scene-root)
    (set! (.-virtualScene context) new-virtual-scene)
    (set! (.-beforeRenderCb context) on-before-render)
    (set! (.-afterRenderCb context) on-after-render)
    context))

;;renders with a smarter context than the default.
;;exposes more options from three.js, specifically
;;the ability for higher device pixel ratios...
(defn ^threeagent.imp.scene/Context render [root-fn
                                            dom-root
                                            {:keys [on-before-render
                                                    on-after-render
                                                    shadow-map
                                                    render-setup
                                                    render-params] :as config
                                             :or {render-setup  default-render-setup
                                                  render-params default-params}}]
  (if-let [existing-context (find-context dom-root)]
    (reset-context! existing-context root-fn config)
    (let [context (create-context root-fn dom-root on-before-render on-after-render shadow-map render-params)
          renderer ^js (.-renderer context)
          ;;user defined renderer hook.
          _       ((or render-setup identity) renderer)]
      (.setAnimationLoop renderer (.-animateFn context))
      context)))


;; function resizeRendererToDisplaySize(renderer) {
;;                                                 const canvas = renderer.domElement;
;;                                                 const pixelRatio = window.devicePixelRatio;
;;                                                 const width  = canvas.clientWidth  * pixelRatio | 0;
;;                                                 const height = canvas.clientHeight * pixelRatio | 0;
;;                                                 const needResize = canvas.width !== width || canvas.height !== height;
;;                                                 if (needResize) {
;;                                                                  renderer.setSize(width, height, false);
;;                                                                  }
;;                                                 return needResize;
;;                                                 }

;; function render(time) {
;;                        time *= 0.001;

;;                        if (resizeRendererToDisplaySize(renderer)) {
;;                                                                    const canvas = renderer.domElement;
;;                                                                    camera.aspect = canvas.clientWidth / canvas.clientHeight;
;;                                                                    camera.updateProjectionMatrix();
;;                                                                    }
