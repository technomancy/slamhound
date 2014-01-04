(ns slam.hound.search-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [slam.hound.search :refer [classpath-files
                                       namespaces
                                       namespaces-from-files
                                       namespaces-from-jars]])
  (:import (java.io File)))

(def slamhound-namespaces
  '#{slam.hound
     slam.hound.asplode
     slam.hound.future
     slam.hound.prettify
     slam.hound.regrow
     slam.hound.search
     slam.hound.stitch})

(def korma-namespaces
  '#{korma.config
     korma.core
     korma.db
     korma.sql.engine
     korma.sql.fns
     korma.sql.utils})

(def korma-jar
  (first (filter (fn [^File f] (re-find #"\Akorma-.+\.jar\z" (.getName f)))
                 classpath-files)))

(deftest ^:unit test-namespaces-from-files
  (is (= (namespaces-from-files (file-seq (io/file "src")))
         slamhound-namespaces)))

(deftest ^:unit test-namespaces-from-jars
  (is (= (namespaces-from-jars [korma-jar])
         korma-namespaces)))

(deftest ^:unit test-namespaces
  (is (= (namespaces (cons korma-jar (file-seq (io/file "src"))))
         (into korma-namespaces slamhound-namespaces))))
