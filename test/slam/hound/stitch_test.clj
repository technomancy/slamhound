(ns slam.hound.stitch-test
  (:require [clojure.test :refer [deftest is]]
            [slam.hound.stitch :refer [ns-from-map stitch-up sort-subclauses collapse-clause]]))

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

(deftest ^:unit test-ns-from-map
  (is (= sample-ns-form (ns-from-map sample-ns-map))))

;; TODO Dec 21st - is this test neccecary? It *should* be covered by tests for stitch-up
(deftest ^:unit test-sort
  (is (= {:name 'slamhound.sample
          :meta {:doc "Testing some things going on here."}
          :use '[[clojure.test :only [deftest]]
                 [clojure.test :only [is]]
                 [slam.hound.stitch :only [ns-from-map]]]
          :require '([clojure.java.io :as io] [clojure.set :as set])
          :import '(clojure.lang.Compiler$BodyExpr
                    java.io.ByteArrayInputStream java.io.File java.util.UUID)
          :refer-clojure '(:exclude [compile test])}
         (sort-subclauses sample-ns-map))))

(deftest ^:unit test-collapse-import
  (is (= {:import '[(clojure.lang Compiler$BodyExpr)
                    (java.io ByteArrayInputStream File)
                    (java.util UUID)]}
         (collapse-clause {:import '(clojure.lang.Compiler$BodyExpr
                                            java.io.ByteArrayInputStream
                                            java.io.File java.util.UUID)}
                                 :import))))

(deftest ^:unit test-collapse-use
  (is (= {:use '[[clojure.test :only [deftest is]]
                 [slam.hound.stitch :only [ns-from-map]]]}
         (collapse-clause {:use '[[clojure.test :only [deftest]]
                                  [slam.hound.stitch :only [ns-from-map]]
                                  [clojure.test :only [is]]]}
                          :use))))

(deftest ^:unit test-stitch-up
  (is (= "(ns slamhound.sample
  \"Testing some things going on here.\"
  (:use [clojure.test :only [deftest is]]
        [slam.hound.stitch :only [ns-from-map]])
  (:require [clojure.java.io :as io] [clojure.set :as set])
  (:import (clojure.lang Compiler$BodyExpr)
           (java.io ByteArrayInputStream File)
           (java.util UUID))
  (:refer-clojure :exclude [compile test]))
" (stitch-up sample-ns-map))))

