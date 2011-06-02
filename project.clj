(defproject slamhound "1.2.0"
  :description
  "Rips your ns form apart and reconstructs it. We have the technology."
  :dev-dependencies [[lein-difftest "1.3.2"]]
  :eval-in-leiningen true
  :hooks [leiningen.hooks.difftest])
