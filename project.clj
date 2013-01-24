(defproject slamhound "1.3.1"
  :description "Rips your ns apart and reconstructs it. We have the technology."
  :url "https://github.com/technomancy/slamhound"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main slam.hound
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :profiles {:dev {:dependencies [[org.clojure/tools.trace "0.7.3"]
                                  [org.clojars.runa/clj-schema "0.7.0"]
                                  [gui-diff "0.3.9"]
                                  [korma "0.3.0-beta11"]]}}
  :test-selectors {:default (constantly true)
                   :integration :integration
                   :unit :unit}
  ;; retain compatibility with lein1 for now
  :dev-dependencies [[org.clojure/tools.trace "0.7.3"]
                     [org.clojars.runa/clj-schema "0.7.0"]
                     [gui-diff "0.3.9"]
                     [korma "0.3.0-beta11"]]
  :dev-resource-paths "dev-resources")

