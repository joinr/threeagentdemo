(ns threeagentdemo.testdata)

(def test-entities
  (->> (concat
              (for [i (range 13)]
                {:SRC "IBCT"
                 :compo "AC"
                 :icon "ibct.png"
                 :location :home
                 :readiness 0 ;;0 to 1, 0 == c4, 1 == c1, even partitions.
                 })
              (for [i (range 20)]
                {:SRC      "IBCT"
                 :compo    "NG"
                 :icon     "ibct.png"
                 :location :home
                 :readiness 0 ;;0 to 1, 0 == c4, 1 == c1, even partitions.
                 })
              (for [i (range 11)]
                {:SRC "ABCT"
                 :compo "AC"
                 :icon "abct.png"
                 :location :home
                 :readiness 0 ;;0 to 1, 0 == c4, 1 == c1, even partitions.
                 })
              (for [i (range 5)]
                {:SRC "ABCT"
                 :compo "NG"
                 :icon "abct.png"
                 :location :home
                 :readiness 0 ;;0 to 1, 0 == c4, 1 == c1, even partitions.
                 })
              (for [i (range 7)]
                {:SRC "SBCT"
                 :compo "AC"
                 :icon "sbct.png"
                 :location :home
                 :readiness 0 ;;0 to 1, 0 == c4, 1 == c1, even partitions.
                 })
              (for [i (range 2)]
                {:SRC "SBCT"
                 :compo "NG"
                 :icon "sbct.png"
                 :location :home
                 :readiness 0 ;;0 to 1, 0 == c4, 1 == c1, even partitions.
                 }))
       (map-indexed (fn [n ent]
                      (assoc ent :id (str (ent :SRC) "_" (ent :compo) "_" n))))))
