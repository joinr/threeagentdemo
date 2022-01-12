(defproject threeagentdemo.core "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.10.3"]
                 [reagent "0.10.0" #_"1.1.0" ]
                 ;;have to declare explicitly now.
                 #_[cljsjs/react "17.0.2-0"]
                 #_[cljsjs/react-dom "17.0.2-0"]
                 [doughamil/threeagent "0.0.10"]
                 ;;probably should consolidate these...
                 [cljsjs/vega "5.17.0-0"]
                 [cljsjs/vega-lite "4.17.0-0"]
                 [cljsjs/vega-embed "6.14.2-0"]
                 [metosin/vega-tools "0.2.0"]
                 [semantic-csv "0.2.1-alpha1"]
                 [org.clojure/core.async "1.3.618"]
                 [cljs-http "0.1.46"]]

  :source-paths ["src"]

  :aliases {"fig"       ["run" "-m" "figwheel.main"]
            "fig:build" ["run" "-m" "figwheel.main" "-b" "dev" "-r"]
            "fig:min"   ["run" "-m" "figwheel.main" "-O" "advanced" "-bo" "dev"]
            "fig:test"  ["run" "-m" "figwheel.main" "-co" "test.cljs.edn" "-m" "threeagentdemo.test-runner"]}

  :profiles {:dev {:dependencies [[org.clojure/clojurescript "1.10.879"]
                                  [com.bhauman/figwheel-main "0.2.13"]
                                  [cljs-bean "1.7.0"]]
                   :resource-paths ["target"]
                   ;; need to add the compiled assets to the :clean-targets
                   :clean-targets ^{:protect false} ["target"]}})

