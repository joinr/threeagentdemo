(ns threeagentdemo.util
  (:require [threeagent.core :as th]
            ["three" :as three]
            [threeagentdemo.svg :as svg])
  (:require-macros [threeagent.macros :refer [defcomponent]]))

(defn device-pixel-ratio []
  (.-devicePixelRatio js/window))

(defn map-vals [f m]
  (reduce-kv (fn [acc k v]
               (assoc acc k (f v))) {} m))

;;custom component creation...

;; const map = new THREE.TextureLoader().load( 'sprite.png' );
;; const material = new THREE.SpriteMaterial( { map: map } );
;; const sprite = new THREE.Sprite( material );
;; scene.add( sprite );


;;SPRITES
;;much faster.
(def spritecache (atom {}))

;;https://threejs.org/docs/#api/en/objects/Sprite
(defcomponent :sprite [{:keys [source]}]
  (if-let [mat (get @spritecache source)]
    (three/Sprite. mat)
    (let [texture  (.load (three/TextureLoader.) source)
          material (three/SpriteMaterial. #js{:map texture})
          _        (swap! spritecache assoc source material)]
      (three/Sprite. material))))

(def ratio (device-pixel-ratio))

(defn sprite [{:keys [source]}]
  [:object {:scale [1 (/ 1.0 ratio) 1]}
   [:sprite {:source source}]])


;;svgs load asynchronously, since the shape must be created etc.

;;so we need a promisory/on-load thing.

;;SVG

(defn svg [{:keys [source]}]
  (let [res    (svg/load-svg source)
        cached (th/atom nil)
        _      (add-watch res :loading (fn [k a oldv newv]
                                         (println newv)
                                         (case (newv :status)
                                           :delivered (do (println ["cached is now:" (reset! cached (newv :value))])
                                                         (remove-watch res k))
                                           :error    (remove-watch res k)
                                           nil)))]
    (fn []
      (if-let [v @cached]
        [:instance {:object v}]
        [:sphere {:radius 5
                  :material {:color "white"}}]))))


;;would like to define containers.

(defn container [color contents])
