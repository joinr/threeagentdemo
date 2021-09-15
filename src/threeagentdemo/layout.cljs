;;doing layouts with yoga.
;;the problem we face here is that yoga is run from emscripten.
;;we'd like to have a global object that represents the loaded
;;emscripten lib.  This is normally handled with promises.
;;We'd like to have this happen before we run any code,
;;so we have a simple, consistent API to program against.
;;So the strategy is to have an init that will create
;;the yoga layout var and then we can define a little API
;;to handle it.  Our init will be called inside the app's main,
;;prior to any other code being run, so the require should
;;work out fine.
(ns threeagentdemo.layout
  (:require ["yoga" :as yoga]
            [cljs-bean.core :refer [bean]])
  (:require-macros [threeagentdemo.layout :refer [defproperties set-node get-node do-node]]))


(def layout (atom nil))
(def node-properties (atom {}))

(defn init []
  (-> (js/Yoga.init)
      (.then (fn [x] (reset! layout x)))))

(when-not @layout (init))

;;enums
(def enums
  {:ALIGN_COUNT 8, :ALIGN_AUTO 0, :ALIGN_FLEX_START 1, :ALIGN_CENTER
  2, :ALIGN_FLEX_END 3, :ALIGN_STRETCH 4, :ALIGN_BASELINE
  5, :ALIGN_SPACE_BETWEEN 6, :ALIGN_SPACE_AROUND 7, :DIMENSION_COUNT
  2, :DIMENSION_WIDTH 0, :DIMENSION_HEIGHT 1, :DIRECTION_COUNT
  3, :DIRECTION_INHERIT 0, :DIRECTION_LTR 1, :DIRECTION_RTL 2, :DISPLAY_COUNT
  2, :DISPLAY_FLEX 0, :DISPLAY_NONE 1, :EDGE_COUNT 9, :EDGE_LEFT 0, :EDGE_TOP
  1, :EDGE_RIGHT 2, :EDGE_BOTTOM 3, :EDGE_START 4, :EDGE_END 5, :EDGE_HORIZONTAL
  6, :EDGE_VERTICAL 7, :EDGE_ALL 8, :EXPERIMENTAL_FEATURE_COUNT
  1, :EXPERIMENTAL_FEATURE_WEB_FLEX_BASIS 0, :FLEX_DIRECTION_COUNT
  4, :FLEX_DIRECTION_COLUMN 0, :FLEX_DIRECTION_COLUMN_REVERSE
  1, :FLEX_DIRECTION_ROW 2, :FLEX_DIRECTION_ROW_REVERSE 3, :JUSTIFY_COUNT
  6, :JUSTIFY_FLEX_START 0, :JUSTIFY_CENTER 1, :JUSTIFY_FLEX_END
  2, :JUSTIFY_SPACE_BETWEEN 3, :JUSTIFY_SPACE_AROUND 4, :JUSTIFY_SPACE_EVENLY
  5, :LOG_LEVEL_COUNT 6, :LOG_LEVEL_ERROR 0, :LOG_LEVEL_WARN 1, :LOG_LEVEL_INFO
  2, :LOG_LEVEL_DEBUG 3, :LOG_LEVEL_VERBOSE 4, :LOG_LEVEL_FATAL
  5, :MEASURE_MODE_COUNT 3, :MEASURE_MODE_UNDEFINED 0, :MEASURE_MODE_EXACTLY
  1, :MEASURE_MODE_AT_MOST 2, :NODE_TYPE_COUNT 2, :NODE_TYPE_DEFAULT
  0, :NODE_TYPE_TEXT 1, :OVERFLOW_COUNT 3, :OVERFLOW_VISIBLE 0, :OVERFLOW_HIDDEN
  1, :OVERFLOW_SCROLL 2, :POSITION_TYPE_COUNT 2, :POSITION_TYPE_RELATIVE
  0, :POSITION_TYPE_ABSOLUTE 1, :PRINT_OPTIONS_COUNT 3, :PRINT_OPTIONS_LAYOUT
  1, :PRINT_OPTIONS_STYLE 2, :PRINT_OPTIONS_CHILDREN 4, :UNIT_COUNT
  4, :UNIT_UNDEFINED 0, :UNIT_POINT 1, :UNIT_PERCENT 2, :UNIT_AUTO
   3, :WRAP_COUNT 3, :WRAP_NO_WRAP 0, :WRAP_WRAP 1, :WRAP_WRAP_REVERSE 2})

(def  Yoga$JustifyContent #{
  :JUSTIFY_CENTER
  :JUSTIFY_FLEX_END
  :JUSTIFY_FLEX_START
  :JUSTIFY_SPACE_AROUND
  :JUSTIFY_SPACE_BETWEEN
  :JUSTIFY_SPACE_EVENLY})

(def  Yoga$Align #{
  :ALIGN_AUTO
  :ALIGN_BASELINE
  :ALIGN_CENTER
  :ALIGN_FLEX_END
  :ALIGN_FLEX_START
  :ALIGN_SPACE_AROUND
  :ALIGN_SPACE_BETWEEN
  :ALIGN_STRETCH})

(def  Yoga$FlexDirection #{
  :FLEX_DIRECTION_COLUMN
  :FLEX_DIRECTION_COLUMN_REVERSE
  :FLEX_DIRECTION_COUNT
  :FLEX_DIRECTION_ROW
  :FLEX_DIRECTION_ROW_REVERSE})

(def  Yoga$Direction #{
  :DIRECTION_INHERIT
  :DIRECTION_LTR
  :DIRECTION_RTL})

(def  Yoga$FlexWrap #{
  :WRAP_NO_WRAP
  :WRAP_WRAP
  :WRAP_WRAP_REVERSE})

(def  Yoga$Edge #{
  :EDGE_LEFT
  :EDGE_TOP
  :EDGE_RIGHT
  :EDGE_BOTTOM
  :EDGE_START
  :EDGE_END
  :EDGE_HORIZONTAL
  :EDGE_VERTICAL
  :EDGE_ALL})

(def  Yoga$Display #{
  :DISPLAY_FLEX
  :DISPLAY_NONE})

(def  Yoga$Unit #{
  :UNIT_AUTO
  :UNIT_PERCENT
  :UNIT_POINT
  :UNIT_UNDEFINED})

(def  Yoga$Overflow #{
  :OVERFLOW_HIDDEN
  :OVERFLOW_SCROLL
  :OVERFLOW_VISIBLE})

(def  Yoga$PositionType #{
  :POSITION_TYPE_ABSOLUTE
  :POSITION_TYPE_RELATIVE})


(defn as-param [v]
  (if (keyword? v)
    (or (enums v) (throw (ex-info "unknown param!" {:in v})))
    v))
;;node properties.

;; (:reset :copyStyle
;;  :setPositionType
;;  :setPosition
;;  :setPositionPercent
;;  :setAlignContent
;;  :setAlignItems
;;  :setAlignSelf :setFlexDirection :setFlexWrap :setJustifyContent
;;  :setMargin :setMarginPercent :setMarginAuto :setOverflow :setDisplay
;;  :setFlex :setFlexBasis :setFlexBasisPercent :setFlexGrow :setFlexShrink
;;  :setWidth :setWidthPercent :setWidthAuto :setHeight :setHeightPercent
;;  :setHeightAuto :setMinWidth :setMinWidthPercent :setMinHeight
;;  :setMinHeightPercent :setMaxWidth :setMaxWidthPercent :setMaxHeight
;;  :setMaxHeightPercent :setAspectRatio :setBorder :setPadding
;;  :setPaddingPercent :getPositionType :getPosition :getAlignContent
;;  :getAlignItems :getAlignSelf :getFlexDirection :getFlexWrap
;;  :getJustifyContent :getMargin :getFlexBasis :getFlexGrow :getFlexShrink
;;  :getWidth :getHeight :getMinWidth :getMinHeight :getMaxWidth
;;  :getMaxHeight :getAspectRatio :getBorder :getOverflow :getDisplay
;;  :getPadding :insertChild :removeChild :getChildCount :getParent
;;  :getChild :isReferenceBaseline :setIsReferenceBaseline :setMeasureFunc
;;  :unsetMeasureFunc :setDirtiedFunc :unsetDirtiedFunc :markDirty :isDirty
;;  :calculateLayout :getComputedLeft :getComputedRight :getComputedTop
;;  :getComputedBottom :getComputedWidth :getComputedHeight
;;  :getComputedLayout :getComputedMargin :getComputedBorder
;;  :getComputedPadding :free :freeRecursive :isAliasOf :clone :delete
;;  :isDeleted :deleteLater)


(defproperties
{:AlignContent [n]
 :AlignItems [n]
 :AlignSelf [n]
 :AspectRatio [n]
 :Border [e w]
 :Display [n]
 :Flex [n]
 :FlexBasis [n]
 :FlexBasisPercent [n]
 :FlexDirection [n]
 :FlexGrow [n]
 :FlexShrink [n]
 :FlexWrap [n]
 :Height [n]
 :HeightAuto []
 :HeightPercent [n]
 :JustifyContent [n]
 :Margin [e n]
 :MarginAuto [e]
 :MarginPercent [e m]
 :MaxHeight [n]
 :MaxHeightPercent [n]
 :MaxWidth [n]
 :MaxWidthPercent [n]
 :MeasureFunc [f]
 :MinHeight [n]
 :MinHeightPercent [n]
 :MinWidth [n]
 :MinWidthPercent [n]
 :Overflow [o]
 :Padding [e p]
 :PaddingPercent [e p]
 :Position [e p]
 :PositionPercent [e p]
 :PositionType [x]
 :Width [x]
 :WidthAuto []
 :WidthPercent [n]})

  ;;   setAlignContent(alignContent: Yoga$Align): void,
;;   setAlignItems(alignItems: Yoga$Align): void,
;;   setAlignSelf(alignSelf: Yoga$Align): void,
;;   setAspectRatio(aspectRatio: number): void,
;;   setBorder(edge: Yoga$Edge, borderWidth: number): void,
;;   setDisplay(display: Yoga$Display): void,
;;   setFlex(flex: number): void,
;;   setFlexBasis(flexBasis: number | string): void,
;;   setFlexBasisPercent(flexBasis: number): void,
;;   setFlexDirection(flexDirection: Yoga$FlexDirection): void,
;;   setFlexGrow(flexGrow: number): void,
;;   setFlexShrink(flexShrink: number): void,
;;   setFlexWrap(flexWrap: Yoga$FlexWrap): void,
;;   setHeight(height: number | string): void,
;;   setHeightAuto(): void,
;;   setHeightPercent(height: number): void,
;;   setJustifyContent(justifyContent: Yoga$JustifyContent): void,
;;   setMargin(edge: Yoga$Edge, margin: number): void,
;;   setMarginAuto(edge: Yoga$Edge): void,
;;   setMarginPercent(edge: Yoga$Edge, margin: number): void,
;;   setMaxHeight(maxHeight: number | string): void,
;;   setMaxHeightPercent(maxHeight: number): void,
;;   setMaxWidth(maxWidth: number | string): void,
;;   setMaxWidthPercent(maxWidth: number): void,
;;   setMeasureFunc(measureFunc: ?Function): void,
;;   setMinHeight(minHeight: number | string): void,
;;   setMinHeightPercent(minHeight: number): void,
;;   setMinWidth(minWidth: number | string): void,
;;   setMinWidthPercent(minWidth: number): void,
;;   setOverflow(overflow: Yoga$Overflow): void,
;;   setPadding(edge: Yoga$Edge, padding: number | string): void,
;;   setPaddingPercent(edge: Yoga$Edge, padding: number): void,

;;   setPosition(edge: Yoga$Edge, position: number | string): void,
;;   setPositionPercent(edge: Yoga$Edge, position: number): void,
;;   setPositionType(positionType: Yoga$PositionType): void,
;;   setWidth(width: number | string): void,
;;   setWidthAuto(): void,
;;   setWidthPercent(width: number): void,


;; free(): void,
;;   freeRecursive(): void,


;;   getAlignContent(): Yoga$Align,
;;   getAlignItems(): Yoga$Align,
;;   getAlignSelf(): Yoga$Align,
;;   getAspectRatio(): number,
;;   getBorder(edge: Yoga$Edge): number,
;;   getChild(index: number): Yoga$Node,
;;   getChildCount(): number,
;;   getComputedBorder(edge: Yoga$Edge): number,
;;   getComputedBottom(): number,
;;   getComputedHeight(): number,
;;   getComputedLayout(): Layout,
;;   getComputedLeft(): number,
;;   getComputedMargin(edge: Yoga$Edge): number,
;;   getComputedPadding(edge: Yoga$Edge): number,
;;   getComputedRight(): number,
;;   getComputedTop(): number,
;;   getComputedWidth(): number,
;;   getDisplay(): Yoga$Display,
;;   getFlexBasis(): number,
;;   getFlexDirection(): Yoga$FlexDirection,
;;   getFlexGrow(): number,
;;   getFlexShrink(): number,
;;   getFlexWrap(): Yoga$FlexWrap,
;;   getHeight(): Value,
;;   getJustifyContent(): Yoga$JustifyContent,
;;   getMargin(edge: Yoga$Edge): Value,

;; ;;Value returns don't seem to work good.
;; ;;  getMaxHeight(): Value,
;; ;;  getMaxWidth(): Value,
;; ;;  getMinHeight(): Value,
;; ;;  getMinWidth(): Value,
;; getOverflow(): Yoga$Overflow,
;; ;;
;;   getPadding(edge: Yoga$Edge): Value,
;;   getParent(): ?Yoga$Node,
;; ;;  getPosition(edge: Yoga$Edge): Value,
;;   getPositionType(): Yoga$PositionType,
;; ;;  getWidth(): Value,
;;   insertChild(child: Yoga$Node, index: number): void,
;;   isDirty(): boolean,
;;   markDirty(): void,
;;   removeChild(child: Yoga$Node): void,
;;   reset(): void,
;;   unsetMeasureFun(): void,

;; (:reset :copyStyle
;;  :setPositionType
;;  :setPosition
;;  :setPositionPercent
;;  :setAlignContent
;;  :setAlignItems
;;  :setAlignSelf :setFlexDirection :setFlexWrap :setJustifyContent
;;  :setMargin :setMarginPercent :setMarginAuto :setOverflow :setDisplay
;;  :setFlex :setFlexBasis :setFlexBasisPercent :setFlexGrow :setFlexShrink
;;  :setWidth :setWidthPercent :setWidthAuto :setHeight :setHeightPercent
;;  :setHeightAuto :setMinWidth :setMinWidthPercent :setMinHeight
;;  :setMinHeightPercent :setMaxWidth :setMaxWidthPercent :setMaxHeight
;;  :setMaxHeightPercent :setAspectRatio :setBorder :setPadding
;;  :setPaddingPercent :getPositionType :getPosition :getAlignContent
;;  :getAlignItems :getAlignSelf :getFlexDirection :getFlexWrap
;;  :getJustifyContent :getMargin :getFlexBasis :getFlexGrow :getFlexShrink
;;  :getWidth :getHeight :getMinWidth :getMinHeight :getMaxWidth
;;  :getMaxHeight :getAspectRatio :getBorder :getOverflow :getDisplay
;;  :getPadding :insertChild :removeChild :getChildCount :getParent
;;  :getChild :isReferenceBaseline :setIsReferenceBaseline :setMeasureFunc
;;  :unsetMeasureFunc :setDirtiedFunc :unsetDirtiedFunc :markDirty :isDirty
;;  :calculateLayout :getComputedLeft :getComputedRight :getComputedTop
;;  :getComputedBottom :getComputedWidth :getComputedHeight
;;  :getComputedLayout :getComputedMargin :getComputedBorder
;;  :getComputedPadding :free :freeRecursive :isAliasOf :clone :delete
;;  :isDeleted :deleteLater)


;;we now have a handle on our layout object.
(defn ->node
  ([config]
   (let [nd (.create (.-Node @layout))
         props @node-properties]
     (reduce (fn [acc [k v]]
               (let [f (or (props k) (ex-info "unknown yoga property!" {:in k}))]
                 (f acc v))) nd config)))
  ([] (->node {})))

;;basic demo via interop.
#_
(defn layout-test []
  (let [node  (.-Node layout)
        root  (.create node)
        _     (.setWidth root 500)
        _     (.setHeight root 300)
        _     (.setJustifyContent root (.-JUSTIFY_CENTER layout))
        node1 (.create node)
        _     (.setWidth node1 100)
        _     (.setHeight node1 100)
        node2 (.create node)
        _     (.setWidth node2 100)
        _     (.setHeight node2 100)
        _     (.insertChild root node1 0)
        _     (.insertChild root node2 1)
        _     (.calculateLayout root 500 300 (.-DIRECTION_LTR layout))]
    (println (.getComputedLayout root))
    (println (.getComputedLayout node1))
    (println (.getComputedLayout node2))))


;;basic demo using our new sugar here
#_
(defn layout-test []
  (let [root  (->node {:Width 500 :Height 300 :JustifyContent :JUSTIFY_CENTER :FlexDirection :FLEX_DIRECTION_ROW})
        node1 (->node {:Width 100 :Height 100})
        node2 (->node {:Width 100 :Height 100})
        _     (.insertChild root node1 0)
        _     (.insertChild root node2 1)
        _     (do-node root
                (.calculateLayout 500 300 :DIRECTION_LTR))]
    (println (.getComputedLayout root))
    (println (.getComputedLayout node1))
    (println (.getComputedLayout node2))))

(defn layout-test2 []
  (let [root  (->node {:Width 500 :Height 300
                       :JustifyContent :JUSTIFY_CENTER
                       :FlexDirection :FLEX_DIRECTION_ROW
                      ; :FlexWrap :WRAP_WRAP
                       :AlignContent :ALIGN_SPACE_AROUND})
        node1 (->node {:Width 100 :Height 100})
        node2 (->node {:Width 100 :Height 100})
        _     (.insertChild root node1 0)
        _     (.insertChild root node2 1)
        _     (do-node root
                       (.calculateLayout 500 300 :DIRECTION_LTR))]
    root
    #_#_#_(println (.getComputedLayout root))
    (println (.getComputedLayout node1))
    (println (.getComputedLayout node2))))

(defn children [nd]
  (let [n (.getChildCount nd)]
    (when (pos? n)
      (for [i (range n)]
        (.getChild nd i)))))

(defn layout! [nd width height dir]
  (do-node nd
           (.calculateLayout width height dir)))

(defn walk-layout [nd])

(defn layout-boxes [root-config dir xs]
  (let [root (->node root-config)
        rw   (root-config :Width)
        rh   (root-config :Height)]
    (doseq [[idx child] (map-indexed vector xs)]
      (.insertChild root (->node (select-keys  child [:Width :Height])) idx))
    (do-node root
             (.calculateLayout rw rh dir))
    {:parent (.getComputedLayout root)
     :children (->> root
                    children
                    (map (fn [node bounds]
                           {:node node :bounds (.getComputedLayout bounds)}) xs))}))

;;example layout: wraps the contents in a row.
#_
(layout-boxes {:Width 500 :Height 300
               :JustifyContent :JUSTIFY_CENTER
               :FlexDirection :FLEX_DIRECTION_ROW
               :FlexWrap :WRAP_WRAP
               :AlignContent :ALIGN_SPACE_AROUND} :DIRECTION_LTR (repeat 6 {:Width 100 :Height 100}))
