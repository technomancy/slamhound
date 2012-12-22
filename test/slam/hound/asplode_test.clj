(ns slam.hound.asplode-test
   (:require [clojure.test :refer [deftest is]]
             [slam.hound.asplode :refer [ns-to-map asplode]]))


;; just inline this stuff?
(def sample-ns-map
  {:name 'slamhound.sample
   :meta {:doc "Testing some things going on here."}
   :use '[[slam.hound.stitch :only [ns-from-map]]
          [clojure.test :only [is]]
          [clojure.test :only [deftest]]]
   :require '([clojure.java.io :as io] [clojure.set :as set])
   :import '(java.io.File java.io.ByteArrayInputStream
                          clojure.lang.Compiler$BodyExpr java.util.UUID)
   :refer-clojure '(:exclude [compile test])})

(def sample-ns-form '(ns slamhound.sample
                       "Testing some things going on here."
                       (:use [slam.hound.stitch :only [ns-from-map]]
                             [clojure.test :only [is]]
                             [clojure.test :only [deftest]])
                       (:require [clojure.java.io :as io]
                                 [clojure.set :as set])
                       (:import java.io.File java.io.ByteArrayInputStream
                                clojure.lang.Compiler$BodyExpr
                                java.util.UUID)
                       (:refer-clojure :exclude [compile test])))

(deftest ^:unit test-ns-to-map
  (is (= sample-ns-map (dissoc (ns-to-map sample-ns-form) :old))))