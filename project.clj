(defproject slamhound "1.1.0-SNAPSHOT"
  :description
  "Rips your ns form apart and reconstructs it. We have the technology."
  ;; TODO: will eliminate this dep down the road
  :dependencies [[swank-clojure "1.3.0"]]
  :dev-dependencies [[lein-difftest "1.3.1"]]
  :eval-in-leiningen true
  :hooks [leiningen.hooks.difftest])
