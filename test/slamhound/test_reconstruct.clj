(ns slamhound.test-reconstruct
  "Testing some things going on here."
  (:use [clojure.test :only [deftest is]])
  (:require [slam.hound.asplode :as asplode]
            [slam.hound.regrow :as regrow]
            [slam.hound.stitch :as stitch]))

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

(def sample-body
  '((set/union #{:a} #{:b})
    (UUID/randomUUID)
    (instance? Compiler$BodyExpr nil)
    (io/copy (ByteArrayInputStream. (.getBytes "remotely human"))
             (doto (File. "/tmp/remotely-human") .deleteOnExit))
    (deftest test-ns-to-map
      (is (= (ns-from-map {:ns 'slam.hound}))))))

;;; representation transformation

(deftest test-ns-to-map
  (is (= sample-ns-map (dissoc (asplode/ns-to-map sample-ns-form) :old))))

(deftest test-ns-from-map
  (is (= sample-ns-form (stitch/ns-from-map sample-ns-map))))

(deftest test-roundtrip
  (is (= sample-ns-form
         (stitch/ns-from-map (asplode/ns-to-map sample-ns-form)))))

;;; regrow

(deftest test-grow-import
  (is (= sample-ns-map (regrow/regrow [(dissoc sample-ns-map :import)
                                       sample-body]))))

(deftest test-grow-require
  (is (= sample-ns-map (regrow/regrow [(dissoc sample-ns-map :require)
                                       sample-body]))))

(deftest test-grow-use
  (is (= sample-ns-map (regrow/regrow [(dissoc sample-ns-map :use)
                                       sample-body]))))

(deftest test-grow-preserve
  (let [in-orig? (regrow/in-original-pred :import '((:import (java.util Date))))]
    (is (in-orig? 'java.util.Date))
    (is (not (in-orig? 'java.sql.Date))))
  (is (= '(java.io.File java.util.Date)
         (:import (regrow/regrow [{:old '((:import '((java.util Date))))}
                                  '(vector (Date.) (File. "/tmp"))])))))

;;; stitch

(deftest test-sort
  (is (= {:name 'slamhound.sample
          :meta {:doc "Testing some things going on here."}
          :use '[[clojure.test :only [deftest]]
                 [clojure.test :only [is]]
                 [slam.hound.stitch :only [ns-from-map]]]
          :require '([clojure.java.io :as io] [clojure.set :as set])
          :import '(clojure.lang.Compiler$BodyExpr
                    java.io.ByteArrayInputStream java.io.File java.util.UUID)
          :refer-clojure '(:exclude [compile test])}
         (stitch/sort-subclauses sample-ns-map))))

(deftest test-collapse-import
  (is (= {:import '[(clojure.lang Compiler$BodyExpr)
                    (java.io ByteArrayInputStream File)
                    (java.util UUID)]}
         (stitch/collapse-clause {:import '(clojure.lang.Compiler$BodyExpr
                                            java.io.ByteArrayInputStream
                                            java.io.File java.util.UUID)}
                                 :import))))

(deftest test-collapse-use
  (is (= {:use '[[clojure.test :only [deftest is]]
                 [slam.hound.stitch :only [ns-from-map]]]}
         (stitch/collapse-clause {:use '[[clojure.test :only [deftest]]
                                         [slam.hound.stitch :only [ns-from-map]]
                                         [clojure.test :only [is]]]}
                                 :use))))

(deftest test-stitch-up
  (is (= "(ns slamhound.sample
  \"Testing some things going on here.\"
  (:use
    [clojure.test :only [deftest is]]
    [slam.hound.stitch :only [ns-from-map]])
  (:require [clojure.java.io :as io] [clojure.set :as set])
  (:import
    (clojure.lang Compiler$BodyExpr)
    (java.io ByteArrayInputStream File)
    (java.util UUID))
  (:refer-clojure :exclude [compile test]))
" (stitch/stitch-up sample-ns-map))))
