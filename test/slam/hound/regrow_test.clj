(ns slam.hound.regrow-test
  (:require [clojure.test :refer [deftest is]]
            [slam.hound.regrow :refer [regrow in-original-pred]]))


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
    (deftest ^:unit test-ns-to-map
      (is (= (ns-from-map {:ns 'slam.hound}))))))

(deftest ^:unit test-grow-import
  (is (= sample-ns-map (regrow [(dissoc sample-ns-map :import)
                                sample-body]))))

(deftest ^:unit test-grow-require
  (is (= sample-ns-map (regrow [(dissoc sample-ns-map :require)
                                sample-body]))))

(deftest ^:unit test-grow-use
  (is (= sample-ns-map (regrow [(dissoc sample-ns-map :use)
                                sample-body]))))


(deftest ^:unit test-grow-preserve
  (let [in-orig? (in-original-pred '((java.util Date UUID)))]
    (is (in-orig? 'java.util.Date))
    (is (not (in-orig? 'java.sql.Date))))
  (is (= '(java.io.File java.util.Date)
         (:import (regrow [{:old {:import '((java.util Date))}}
                           '(vector (Date.) (File. "/tmp"))])))))