(ns slam.hound
  (:require [clojure.java.io :as io]
            [slam.hound.asplode :refer [asplode]]
            [slam.hound.regrow :refer [regrow]]
            [slam.hound.stitch :refer [stitch-up]])
  (:import (java.io File FileReader PushbackReader))))


(defn reconstruct [filename]
  ;; Reconstructing consists of three distinct phases:
  ;; asploding, regrowing, and stitching.
  (-> (io/reader filename)
      asplode
      regrow
      stitch-up))

(defn- stacktrace-to-str [^Exception e]
  (cons (.getMessage e)
        (map #(str % "\n") (.getStackTrace e))))

(defn- non-whitespace-char? [ch]
  (re-matches #"\S" (str ch)))

(defn- body-from-file [file-name old-ns-form]
  (let [file-contents (slurp file-name)
        num-non-white-chars-in-old-ns-form (count (filter non-whitespace-char? (str old-ns-form)))
        non-white-so-far (atom 0)]
    (apply str (drop-while (fn [ch]
                             (when (non-whitespace-char? ch)
                               (swap! non-white-so-far inc))
                             (< @non-white-so-far num-non-white-chars-in-old-ns-form))
                           file-contents))))

(defn- swap-in-reconstructed-ns-form [file]
  (let [new-ns (.trim (reconstruct file))
        old-ns-form (read (PushbackReader. (FileReader. file)))
        body (body-from-file file old-ns-form)]
    (spit file (str new-ns body))))

(defn reconstruct-in-place
  "Takes a file or directory and rewrites the files
   with reconstructed ns forms."
  [file-or-dir]
  (doseq [^File f (file-seq (File. file-or-dir))
          :let [^String filename (.getName f)
                ^String file-path (.getAbsolutePath f)]
          :when (and (.endsWith filename ".clj")
                     (not (.startsWith filename "."))
                     (not= filename "project.clj"))]
    (try
      (swap-in-reconstructed-ns-form file-path)
      (catch Exception ex
        (println (str "Failed to reconstruct: " file-path
                      "\nException: " (stacktrace-to-str ex)))))))

(defn -main [file-or-dir]
  (reconstruct-in-place file-or-dir))