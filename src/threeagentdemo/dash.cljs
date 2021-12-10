;;a place for dashboard components, specifically
;;reactive tables.
(ns threeagentdemo.dash)

;;our most primitive component is a table
;;with entries for each src, c1, c2, c3, c4, c5, missing

(def default-style {:display "flex" :flex-wrap "wrap" ;:background "darkgray"
                    :font-size "0.5em"})
;;Where src is an icon or textual src.
;;from https://css-tricks.com/accessible-simple-responsive-tables/
(defn flex-table [n-cols entries & {:keys [style] :or {style default-style}}]
  (let [size (str (/ 100.0 n-cols) "%")
        n (atom 0)]
    [:div.header {:style style}
     (for [e entries]
       (let [non-zero (cond  (number? e) (not (zero? e))
                             (string? e) true #_(not= e "0")
                             :else true)
             txt (string? e)]
         ^{:key (swap! n inc)}
         [:div
          {:style {:box-sizing "border-box"
                   :flex-grow 1
                   :flex-shrink 2
                   :width     size
                   :overflow  "hidden"
                   :list-style "none"
                   :font-weight      (if non-zero "900" "normal")
                   :text-align "center"
                   :background-color  (cond txt "verydarkgrey"
                                            non-zero "grey")
                   :visibility        (if (or txt non-zero) "visible" "hidden")
                   #_#_:animation "backgrounder 2s ease-in-out"}}
 
          e]))]))
