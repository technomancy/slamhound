(ns slam.hound
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [slam.hound.asplode :refer [asplode]]
            [slam.hound.regrow :refer [regrow with-regrow-cache]]
            [slam.hound.stitch :refer [stitch-up]])
  (:import (java.io File PushbackReader)))

(defn reconstruct [filename]
  ;; Reconstructing consists of three distinct phases:
  ;; asploding, regrowing, and stitching.
  (with-regrow-cache
    (-> (io/reader filename)
        asplode
        regrow
        stitch-up)))

(defn read-comment-header
  "Read leading blank and comment lines from rdr."
  [^PushbackReader rdr]
  ;; An implementation using bufferedReader#readLine would be simpler, but
  ;; would have to make an assumption about what kind of line terminators the
  ;; file actually contains.
  (loop [buf (StringBuilder.) state :ws]
    (let [c (.read rdr)]
      (if (= c -1) ; EOF
        (str buf)
        (let [ch (char c)]
          (case state
            :comment (recur (.append buf ch)
                            ;; CRLF and LF both end with LF
                            (if (= ch \newline) :ws :comment))
            :ws (cond (= ch \;) (recur (.append buf ch) :comment)
                      (Character/isWhitespace ch) (recur (.append buf ch) :ws)
                      :else (do (.unread rdr c) (str buf)))))))))

(defn- tidy-comment-header [s]
  (-> s
      (string/replace-first #"(?s)\A\s*\n" "")
      (string/trimr)
      (str "\n\n")))

(defn swap-in-reconstructed-ns-form
  "Reconstruct file's ns form and rewrite the file on disk with the new form."
  [file]
  (let [tmp-file (File/createTempFile "slamhound_tmp" ".clj")]
    (try
      (io/copy file tmp-file)
      (let [new-ns (string/trim (reconstruct tmp-file))]
        (with-open [rdr (PushbackReader. (io/reader tmp-file))
                    writer (io/writer file :append true)]
          (io/copy "" file)
          ;; Preserve comment header
          (let [header (read-comment-header rdr)]
            (when-not (string/blank? header)
              (io/copy (tidy-comment-header header) writer)))
          ;; move the reader past the namespace form; discard value
          (read rdr)
          ;; append the reconstructed ns form
          (io/copy new-ns writer)
          ;; append the body
          (io/copy rdr writer)))
      (finally
        (.delete tmp-file)))))

(defn -main
  "Takes a file or dir and rewrites the .clj files with reconstructed ns forms."
  [& file-or-dirs]
  (with-regrow-cache
    (doseq [file-or-dir file-or-dirs
            file (file-seq (io/file file-or-dir))
            :let [file-str (string/replace
                             (str file)
                             (System/getProperty "file.separator")
                             "/")]
            :when (re-find #"/[^\./]+\.clj$" file-str)]
      (try
        (swap-in-reconstructed-ns-form file)
        (catch Exception e
          (println "Failed to reconstruct:" file)
          (if (System/getenv "DEBUG")
            (.printStackTrace e)
            (println (.getMessage e))))))))

;; See https://github.com/technomancy/nrepl-discover
(defn ^{:nrepl/op {:name "slam"
                   :args [["file" "file" "File: "]]
                   :doc "Rewrite the ns form of a given file."}}
  slam-ns
  "Slamhound rewrite command intended to be exposed for direct editor use."
  [{:keys [transport file] :as msg}]
  (let [nrepl-send (resolve 'clojure.tools.nrepl.transport/send)
        response-for (resolve 'clojure.tools.nrepl.misc/response-for)]
    (nrepl-send transport (response-for msg :message "Reconstructing..."))
    (swap-in-reconstructed-ns-form (io/file file))
    (nrepl-send transport (response-for msg :reload file))))
