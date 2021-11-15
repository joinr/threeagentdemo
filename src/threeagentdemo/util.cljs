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


(def colors->rgb
  {:dark-green   [14  152  95]
   :light-green  [102 204   0]
   :amber        [255 201  64]
   :light-red    [240 84   84]
   :blue         [0   0   255]
   ;;older colors
   })

(def colors->hex
  {:dark-green  "#0E985F"
   :light-green "#66cc00"
   :amber       "#ffc940"
   :light-red   "#f05454"
   :blue        "#0000ff"})

(def c->rgb
  (->> {:C1 :dark-green
        :C2 :light-green
        :C3 :amber
        :C4 :light-red
        :C5 :blue}
       (map-vals colors->rgb)))

(def c->hex
  (->> {:C1 :dark-green
        :C2 :light-green
        :C3 :amber
        :C4 :light-red
        :C5 :blue}
       (map-vals colors->hex)))

;;older palette

(def old-c->rgb
  {:C1 [0, 0, 255] ;"blue"
   :C2 [144,238,144] ;"lightgreen"
   :C3 [255,255,0 ] ;"yellow"
   :C4 [255,255,224] ;"lightyellow"
   :C5 [255,255,224 ] ;"lightyellow"
   })

