(defproject slamhound "1.1.1-SNAPSHOT"
  :description
  "Rips your ns form apart and reconstructs it. We have the technology."
  :dev-dependencies [[lein-difftest "1.3.1"]]
  :eval-in-leiningen true
  :hooks [leiningen.hooks.difftest])
