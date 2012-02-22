(ns leiningen.slamhound
  (:use [leiningen.compile :only [eval-in-project]]))

(defn slamhound
  "Rip your ns form apart and stitch together the pieces."
  [project filename]
  (eval-in-project (update-in project [:dependencies] (fnil into {})
                              {'slamhound "1.3.0-SNAPSHOT"})
                   `(do (shutdown-agents)
                        (println (slam.hound/reconstruct ~filename)))
                   nil nil '(require 'slam.hound)))
