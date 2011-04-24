(ns slamhound.test-reconstruct
  "Testing some things going on here."
  (:use [clojure.test]
        [slam.hound]))

(def sample-ns-form '(ns slamhound.sample
                       "Testing some things going on here."
                       (:refer-clojure :exclude [compile test])
                       (:use [clojure.test :only [deftest is]]
                             [slam.hound :only [ns-from-map]])
                       (:require [clojure.java.io :as io]
                                 [clojure.set :as set])
                       (:import (java.io File ByteArrayOutputStream)
                                (java.util UUID))))

(def sample-ns-map
  {:name 'slamhound.sample
   :meta {:doc "Testing some things going on here."}
   :use '([clojure.test :only [deftest is]] [slam.hound :only [ns-from-map]])
   :require '([clojure.java.io :as io] [clojure.set :as set])
   :import '((java.io File ByteArrayOutputStream) (java.util UUID))
   :refer-clojure '(:exclude [compile test])})

(def sample-body
  '((set/union #{:a} #{:b})
    (io/copy (File. "/tmp/remotely-human" (str (UUID/randomUUID)))
             (ByteArrayOutputStream.))
    (deftest test-ns-to-map
      (is (= (ns-from-map {:ns 'slam.hound}))))))

(deftest test-ns-to-map
  (is (= sample-ns-map (ns-to-map sample-ns-form))))

(deftest test-ns-from-map
  (is (= sample-ns-form (ns-from-map sample-ns-map))))

(deftest test-roundtrip
  (is (= sample-ns-form (ns-from-map (ns-to-map sample-ns-form)))))

(deftest test-add-import
  (is (= sample-ns-map (reconstruct-ns-form
                        (dissoc sample-ns-map :import)
                        sample-body nil))))
