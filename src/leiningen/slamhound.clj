(ns leiningen.slamhound
  (:use [leiningen.compile :only [eval-in-project]]))

(defn slamhound
  "Rip your ns form apart and stitch together the pieces."
  [project filename]
  (eval-in-project project `(do (shutdown-agents)
                                (slam.hound/reconstruct ~filename))
                   nil nil '(require 'slam.hound)))
