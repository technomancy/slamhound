(ns slam.hound.regrow-test
  (:require [clojure.test :refer [deftest is testing]]
            [slam.hound.regrow :refer [regrow in-original-pred]]))


(def sample-body
  '((set/union #{:a} #{:b})
    (UUID/randomUUID)
    (instance? Compiler$BodyExpr nil)
    (io/copy (ByteArrayInputStream. (.getBytes "remotely human"))
             (doto (File. "/tmp/remotely-human") .deleteOnExit))
    (deftest ^:unit test-ns-to-map
      (is (= (ns-from-map {:ns 'slam.hound}))))))

(deftest ^:unit test-regrow
  (testing "regrows the :require-as, :require-refer, and :import k/v pairs onto the ns-map"
    (is (= {:name 'slamhound.sample
            :meta {:doc "Testing some things going on here."}
            :refer-clojure '(:exclude [compile test])
            :gen-class nil
            :require-as '([clojure.java.io :as io] [clojure.set :as set])
            :require-refer '([slam.hound.stitch :refer [ns-from-map]] [clojure.test :refer [is]] [clojure.test :refer [deftest]])
            :import '(java.io.File java.io.ByteArrayInputStream clojure.lang.Compiler$BodyExpr java.util.UUID)
            :old {:name 'slamhound.sample
                  :meta {:doc "Testing some things going on here."}
                  :use '[[slam.hound.stitch :only [ns-from-map]] [clojure.test :only [is]] [clojure.test :only [deftest]]]
                  :require '([clojure.java.io :as io] [clojure.set :as set])
                  :import '(java.io.File java.io.ByteArrayInputStream clojure.lang.Compiler$BodyExpr java.util.UUID)
                  :refer-clojure '(:exclude [compile test])
                  :gen-class nil}}
           
           (regrow [{:name 'slamhound.sample
                     :meta {:doc "Testing some things going on here."}
                     :refer-clojure '(:exclude [compile test])
                     :gen-class nil
                     :old {:name 'slamhound.sample
                           :meta {:doc "Testing some things going on here."}
                           :use '[[slam.hound.stitch :only [ns-from-map]]
                                  [clojure.test :only [is]]
                                  [clojure.test :only [deftest]]]
                           :require '([clojure.java.io :as io] [clojure.set :as set])
                           :import '(java.io.File java.io.ByteArrayInputStream
                                                  clojure.lang.Compiler$BodyExpr java.util.UUID)
                           :refer-clojure '(:exclude [compile test])
                           :gen-class nil}}
                    sample-body])))))




(deftest ^:unit test-grow-preserve
  (let [in-orig? (in-original-pred '((java.util Date UUID)))]
    (is (in-orig? 'java.util.Date))
    (is (not (in-orig? 'java.sql.Date))))
  (is (= '(java.io.File java.util.Date)
         (:import (regrow [{:old {:import '((java.util Date))}}
                           '(vector (Date.) (File. "/tmp"))])))))