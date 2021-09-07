;;dedicated ns for svg loading because it's bulky.
(ns threeagentdemo.svg
  (:require   [threeagent.core :as th]
              ["three" :as three]
              ;;janky, but it works.
              [three.svgloader] ;;will get warnings...
              [cljs-bean.core :refer [bean ->clj ->js]]));;gives us THREE.SVGLoader


;;retain a promise for things that are loading.
;;when they are done loading, we render them.
;;need to hook in outside state to watch for loads though.
(def loading (th/atom #{}))

;; instantiate a loader
;;const loader = new SVGLoader();

 ;; // called when the resource is loaded
 ;;            function ( data ) {
 ;;                               const paths = data.paths;
 ;;                               const group = new THREE.Group();
 ;;                               for ( let i = 0; i < paths.length; i ++ ) {
 ;;                                    const path = paths[ i ];
 ;;                                    const material = new THREE.MeshBasicMaterial( {
 ;;                                                                                   color: path.color,
 ;;                                                                                   side: THREE.DoubleSide,
 ;;                                                                                   depthWrite: false
 ;;                                                                                   } );
 ;;                                    const shapes = SVGLoader.createShapes( path );
 ;;                                    for ( let j = 0; j < shapes.length; j ++ ) {
 ;;                                         const shape = shapes[ j ];
 ;;                                         const geometry = new THREE.ShapeGeometry( shape );
 ;;                                         const mesh = new THREE.Mesh( geometry, material );
 ;;                                         group.add( mesh );
 ;;                                         }
 ;;                                    }
 ;;                               scene.add( group );
;;                               },

(defn on-load [target data]
  (let [paths (.-paths data)
        group (js/THREE.Group.)]
    (doseq [path paths]
      (let [mat (js/THREE.MeshBasicMaterial. #js{:color (.-color path)
                                                 :side  js/THREE.DoubleSide
                                                 :depthWrite false})
            shapes (js/THREE.SVGLoader.createShapes path)]
        (doseq [shape shapes]
          (let [geo (js/THREE.ShapeGeometry. shape)
                mesh (js/THREE.Mesh. geo mat)]
            (.add group mesh)))))
    (reset! target {:status :delivered :value group})))

;;load a SVG resource

(defn load-svg [source]
  (let [loader   (js/THREE.SVGLoader.)
        value    (th/atom {:status :pending :value nil})]
    (.load loader source
           #(on-load value %)
           (fn [xhr]
             (-> (/ (.-loaded xhr)
                    (.-total xhr))
                 (* 100)
                 (str " % loaded source: " source )
                 println))
           (fn [error]
             (println (str "an error happened loding " source ": " error))
             (reset! value {:status :error :value nil})))
    value))

;; loader.load(
;;             // resource URL
;;             'data/svgSample.svg',
;;            on-load
;;             // called when loading is in progresses
;;             function ( xhr ) {
;;                               console.log( ( xhr.loaded / xhr.total * 100 ) + '% loaded' );
;;                               },
;;             // called when loading has errors
;;             function ( error ) {
;;                                 console.log( 'An error happened' );
;;                                 }
;;             );
