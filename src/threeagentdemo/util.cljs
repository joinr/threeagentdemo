(ns threeagentdemo.util
  (:require [threeagent.core :as th]
            ["three" :as three]
            [threeagentdemo.svg :as svg]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! go]]
            [clojure.edn])
  (:require-macros [threeagent.macros :refer [defcomponent]]))


;;debug stuff
;;===========
(defn state-where [f xs]
  (some (fn [x] (when (f x) x)) xs))

(defn state-at [t xs]
  (state-where #(= (% :t) t) xs))

;;file i/o stuff
;;==============
(defn first-file
  [e]
  (let [target (.-currentTarget e)
        file (-> target .-files (aget 0))]
    (set! (.-value target) "")
    file))

;;we have a side-effecting load when we invoke the callback for onload,
;;and the result is passed to load-moves!  on-load takes the raw text from
;;the file and does stuff with it.
(defn load-file-text! [file on-load & {:keys [before-load]}]
  (let [reader (js/FileReader.)
        _ (println (str "loading file:" (.-name file)))
        _ (when before-load (before-load))]
    (set! (.-onload reader)
          #(some-> % .-target .-result on-load))
    (.readAsText reader file)))

;;load a local file as text and read it as edn.  probably
;;faster means to serialize this, but meh.
(defn read-file! [file on-load & {:keys [before-load]}]
  (load-file-text! file
     (fn [txt] (some-> txt clojure.edn/read-string on-load))
     :before-load before-load))

;;simplistic drop-down reagent element.
(defn ->drop-down [label id opts & {:keys [on-change]}]
  [:div
   (when label [:p {:style {:font-size "70%"}} label])
   [:select.cesium-button {:id id :name id :on-change #(on-change (keyword (.. % -target -value)))}
    (for [[k v] opts]
      ^{:key k} [:option {:value (name k)} v])]])

;;other utils
;;===========

(defn device-pixel-ratio []
  (.-devicePixelRatio js/window))

(defn map-vals [f m]
  (reduce-kv (fn [acc k v]
               (assoc acc k (f v))) {} m))

(defn memo-1 [f]
  (let [cache (js/Map.)]
    (fn [x]
      (if-let [res (.get cache x)]
        res
        (let [v (f x)
              _ (.set cache x v)]
          v)))))

(defn assoc-change [l r k]
  (let [lv (l k)
        rv (r k)]
    (if (= lv rv)
      l
      (assoc l k rv))))

(defn insert-by [v f itm]
  (if-not (empty? v)
    (->> (conj v itm)
         (sort-by f)
         vec)
    [itm]))

(defn precision [n k]
  (js/parseFloat (.toPrecision n k)))

;;simple repl development helper.
(defn make-remote-call
  ([endpoint]
   (let [res (atom nil)]
     (go (let [response (<! (http/get endpoint) )]
           ;;enjoy your data
           (reset! res (:body response))))
     res))
  ([endpoint f]
   (go (let [response (<! (http/get endpoint) )]
         ;;enjoy your data
         (f (:body response))))))

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
(def current-ar (/ 1.0 ratio))

(defn sprite [{:keys [source]}]
  [:object {:scale [1 1 1]}
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

(defn c-rating->fill-stat [c]
  (case c
    :C1 "C1"
    :C2 "C2"
    "<=C3"))

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
