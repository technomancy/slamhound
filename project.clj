(defproject slamhound "1.3.0-SNAPSHOT"
  :description "Rips your ns form apart and reconstructs it. We have the technology."
  :plugins [[lein-swank "1.4.4"]]
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :dev-dependencies [[org.clojure/tools.trace "0.7.3"]
                     [org.clojars.runa/clj-schema "0.7.0"]
                     [korma "0.3.0-beta11"]]
  :test-selectors {:default (constantly true)
                   :integration :integration
                   :unit :unit})

