;;a place for dashboard components, specifically
;;reactive tables.
(ns threeagentdemo.dash)

;;our most primitive component is a table
;;with entries for each src, c1, c2, c3, c4, c5, missing

(def default-style {:display "flex" :flex-wrap "wrap" ;:background "darkgray"
                    :font-size "0.5em"})
(def default-cell-style
  {:box-sizing "border-box"
   :flex-grow 1
   :flex-shrink 2
   :overflow  "hidden"
   :list-style "none"
   :text-align "middle"
   #_#_:animation "backgrounder 2s ease-in-out"})

;;Where src is an icon or textual src.
;;from https://css-tricks.com/accessible-simple-responsive-tables/
(defn flex-table [n-cols entries & {:keys [style cell-style header-style]
                                    :or {style default-style
                                         cell-style default-cell-style}}]
  (let [size          (str (/ 100.0 n-cols) "%")
        n             (atom 0)
        current-style (atom (or header-style cell-style))
        _ (when header-style
            (add-watch n :style-swap
                       (fn [_ _ old new]
                         (when (> new n-cols)
                           (reset! current-style cell-style)
                           (remove-watch n :style-swap)))))]
    [:div.header {:style style}
      (for [e entries]
       (let [non-zero (cond  (number? e) (not (zero? e))
                             (string? e) true #_(not= e "0")
                             :else true)
             txt (string? e)
             k   (swap! n inc)]
         ^{:key k}
         [:div
          {:style (assoc @current-style #_cell-style
                         :width     size
                         :font-weight       (if non-zero "900" "normal")
                         :background-color  (cond txt "verydarkgrey" non-zero "grey")
                         :visibility        (if (or txt non-zero) "visible" "hidden"))}

          e]))]))
