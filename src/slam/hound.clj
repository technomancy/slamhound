(ns slam.hound
  (:use [slam.hound.asplode :only [asplode]]
        [slam.hound.regrow :only [regrow]]
        [slam.hound.stitch :only [stitch-up]])
  (:require [clojure.java.io :as io]
            [fs.core :as fs])
  (:import (java.io FileReader
                    PushbackReader)))

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
        num-non-white-chars-in-old-ns-form (count (filter non-whitespace-char? (str old-ns-form)))]
    (apply str (loop [non-white-so-far 0
                      file-contents-remaining file-contents]
                 (cond (>= non-white-so-far num-non-white-chars-in-old-ns-form)
                       file-contents-remaining

                       (non-whitespace-char? (first file-contents-remaining))
                       (recur (inc non-white-so-far) (rest file-contents-remaining))
                            
                       :else
                       (recur non-white-so-far (rest file-contents-remaining)))))))

(defn- swap-in-reconstructed-ns-form [file-name]
  (let [new-ns (.trim (reconstruct file-name))
        old-ns-form (read (PushbackReader. (FileReader. file-name)))
        body (body-from-file file-name old-ns-form)]
    (spit file-name (str new-ns body))))

(defn- reconstruct-dir* [root _dirs_ files]
  (doseq [f files
          :when (and (.endsWith f ".clj")
                     (not (.startsWith f "."))
                     (not= f "project.clj"))
          :let [file-path (fs/file root f)]]
    (try
      (swap-in-reconstructed-ns-form file-path)
      (catch Exception ex
        (print (str "Failed to reconstruct: " (.getName file-path)
                    "\nException: " (stacktrace-to-str ex)))))))

(defn reconstruct-dir
  "Reconstructs every file ending in .clj, except project.clj, and writes
 them back into the original files."
  [dir]
  (fs/walk reconstruct-dir* dir))
