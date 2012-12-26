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

(defn- stacktrace-to-str [^Exception e]
  (cons (.getMessage e)
        (map #(str % "\n") (.getStackTrace e))))

(defn- swap-in-reconstructed-ns-form [filename]
  (let [new-ns (.trim (reconstruct filename))
        rdr (PushbackReader. (FileReader. filename))]
    ;; scan past the namespace form
    (read rdr)
    ;; copy in the reconstructed ns form
    (io/copy new-ns (File. filename))
    ;; append the body
    (with-open [writer (io/writer filename :append true)]
      (io/copy rdr writer))))

(defn reconstruct-in-place
  "Takes a file or directory and rewrites the files
   with reconstructed ns forms."
  [file-or-dir]
  (doall
   (for [^File f (file-seq (if (string? file-or-dir)
                             (File. file-or-dir)
                             file-or-dir))
         :let [^String filename (.getName f)
               ^String file-path (.getAbsolutePath f)]
         :when (and (.endsWith filename ".clj")
                    (not (.startsWith filename "."))
                    (not= filename "project.clj"))]
        (try
          (swap-in-reconstructed-ns-form file-path)
          {:status :success
           :file file-path}
          (catch Exception ex
            {:status :failure
             :file file-path
             :exception ex})))))

(defn -main [file-or-dir]
  (doseq [result (reconstruct-in-place file-or-dir)
          :when (= :failure (:status result))]
    (println (str "Failed to reconstruct: " (:file result)
                  "\nException: " (stacktrace-to-str (:exception result))))))
