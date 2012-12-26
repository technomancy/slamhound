(ns slam.hound
  (:require [clojure.java.io :as io]
            [slam.hound.asplode :refer [asplode]]
            [slam.hound.regrow :refer [regrow]]
            [slam.hound.stitch :refer [stitch-up]])
  (:import (java.io File FileReader PushbackReader)))

(defn reconstruct [filename]
  ;; Reconstructing consists of three distinct phases:
  ;; asploding, regrowing, and stitching.
  (-> (io/reader filename)
      asplode
      regrow
      stitch-up))

(defn swap-in-reconstructed-ns-form
  "Reconstruct file's ns form and rewrite the file on disk with the new form."
  [file]
  (let [new-ns (.trim (reconstruct file))
        rdr (PushbackReader. (io/reader file))]
    ;; move the reader past the namespace form; discard value
    (read rdr)
    ;; copy in the reconstructed ns form
    (io/copy new-ns file)
    ;; append the body
    (with-open [writer (io/writer file :append true)]
      (io/copy rdr writer))))

(defn -main
  "Takes a file or dir and rewrites the .clj files with reconstructed ns forms."
  [file-or-dir]
  (doseq [file (file-seq (io/file file-or-dir))
          :when (re-find #"/[^\./]+\.clj" (str file))]
    (try
      (swap-in-reconstructed-ns-form file)
      (catch Exception e
        (println "Failed to reconstruct:" file)
        (if (System/getenv "DEBUG")
          (.printStackTrace e)
          (println (.getMessage e)))))))
