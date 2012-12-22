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
    (deftest test-ns-to-map
      (is (= (ns-from-map {:ns 'slam.hound}))))))

(deftest test-grow-import
  (is (= sample-ns-map (regrow [(dissoc sample-ns-map :import)
                                sample-body]))))

(deftest test-grow-require
  (is (= sample-ns-map (regrow [(dissoc sample-ns-map :require)
                                sample-body]))))

(deftest test-grow-use
  (is (= sample-ns-map (regrow [(dissoc sample-ns-map :use)
                                sample-body]))))


(deftest test-grow-preserve
  (let [in-orig? (in-original-pred '((java.util date uuid)))]
    (is (in-orig? 'java.util.date))
    (is (not (in-orig? 'java.sql.date))))
  (is (= '(java.io.file java.util.date)
         (:import (regrow [{:old {:import '((java.util date))}}
                           '(vector (date.) (file. "/tmp"))])))))