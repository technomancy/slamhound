(ns slam.hound
  (:use [slam.hound.asplode :only [asplode]]
        [slam.hound.regrow :only [regrow]]
        [slam.hound.stitch :only [stitch-up]])
  (:require [clojure.java.io :as io]))

(defn reconstruct [filename]
  ;; Reconstructing consists of three distinct phases:
  ;; asploding, regrowing, and stitching.
  (-> (io/reader filename)
      asplode
      regrow
      stitch-up))
