(defproject slamhound "1.5.0"
  :description "Rips your ns apart and reconstructs it. We have the technology."
  :url "https://github.com/technomancy/slamhound"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot slam.hound
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :profiles {:dev {:dependencies [[org.clojure/tools.trace "0.7.3"]
                                  ;; Last version of CLJS compatible with
                                  ;; Clojure 1.4.0
                                  [org.clojure/clojurescript "0.0-1535"]
                                  [org.clojars.runa/clj-schema "0.7.0"]
                                  [korma "0.3.0-beta11"]]}}
  :test-selectors {:default (constantly true)
                   :integration :integration
                   :unit :unit})
