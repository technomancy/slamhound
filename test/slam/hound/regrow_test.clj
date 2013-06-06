(ns slam.hound.regrow-test
  (:require [clojure.test :refer [deftest is testing]]
            [slam.hound.regrow :refer [expand-libs regrow in-originals-pred]]))

(deftest ^:unit test-expand-libs
  (testing "expands prefix lists into a flat list of symbols"
    (is (= (expand-libs '((java.util Date Random UUID)))
           '#{java.util.Date java.util.Random java.util.UUID})))
  (testing "expands incomplete libspecs as symbols"
    (is (= (expand-libs '[[clojure [set] [string]]])
           '#{clojure.set clojure.string})))
  (testing "returns regular libspecs as-is"
    (is (= (expand-libs '[[clojure.set :as set] [clojure.string :as string]])
           '#{[clojure.set :as set] [clojure.string :as string]})))
  (testing "expands nested libspecs"
    (is (= (expand-libs '[[clojure.java [io :as i] [shell :as s]]])
           '#{[clojure.java.io :as i] [clojure.java.shell :as s]})))
  (testing "everything at once"
    (is (= (expand-libs '[java.util.regex.Pattern
                          [java.util Date Random UUID]
                          [clojure.pprint :as pp]
                          [clojure [set] [string :as string]]
                          [clojure.java [io :as io] [shell :refer [sh]]]])
           '#{java.util.regex.Pattern
              java.util.Date
              java.util.Random
              java.util.UUID
              [clojure.pprint :as pp]
              clojure.set
              [clojure.string :as string]
              [clojure.java.io :as io]
              [clojure.java.shell :refer [sh]]}))))

(def sample-body
  '((set/union #{:a} #{:b})
    (string/join ["a" "b"]) ; Try to conflict with set/join
    (UUID/randomUUID)
    (instance? Compiler$BodyExpr nil)
    (io/copy (ByteArrayInputStream. (.getBytes "remotely human"))
             (doto (File. "/tmp/remotely-human") .deleteOnExit))
    (deftest ^:unit test-ns-to-map
      (is (= (ns-from-map {:ns 'slam.hound}))))))

(deftest ^:unit test-regrow
  (testing "regrows the :require-as, :require-refer, and :imports onto ns-map"
    (is (= {:name 'slamhound.sample
            :meta {:doc "Testing some things going on here."}
            :refer-clojure '(:exclude [compile test])
            :gen-class nil
            :require-as '[[clojure.java.io :as io]
                          [clojure.string :as string]
                          [clojure.set :as set]]
            :require-refer '[[slam.hound.stitch :refer [ns-from-map]]
                             [clojure.test :refer :all]]
            :import '(java.io.File java.io.ByteArrayInputStream
                                   clojure.lang.Compiler$BodyExpr
                                   java.util.UUID)
            :old {:name 'slamhound.sample
                  :meta {:doc "Testing some things going on here."}
                  :use '[[slam.hound.stitch :only [ns-from-map]]
                         [clojure.test :only [is]]
                         [clojure.test :only [deftest]]]
                  :require '[[clojure.java.io :as io]
                             [clojure.string :as string]
                             [clojure.set :as set]]
                  :import '[java.io.File java.io.ByteArrayInputStream
                            clojure.lang.Compiler$BodyExpr
                            java.util.UUID]
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
                           :require '[[clojure.java.io :as io]
                                      [clojure.string :as string]
                                      [clojure.set :as set]]
                           :import '[java.io.File
                                     java.io.ByteArrayInputStream
                                     clojure.lang.Compiler$BodyExpr
                                     java.util.UUID]
                           :refer-clojure '(:exclude [compile test])
                           :gen-class nil}}
                    sample-body])))
    (is (= (set (:require-as (regrow [{} sample-body])))
           '#{[clojure.java.io :as io]
              [clojure.string :as string]
              [clojure.set :as set]})
        "Should prefer candidate '[clojure.string :as string]")))

(deftest ^:unit test-grow-preserve
  (let [in-orig? (in-originals-pred '((java.util Date UUID)))]
    (is (in-orig? 'java.util.Date))
    (is (not (in-orig? 'java.sql.Date))))
  (is (= '(java.io.File java.util.Date)
         (:import (regrow [{:old {:import '((java.util Date))}}
                           '[(Date.) (File. "/tmp")]])))))

(def +i-must-be-a-cl-user+ true)

(deftest ^:unit test-plus
  (is (= '[[slam.hound.regrow-test :refer [+i-must-be-a-cl-user+]]]
         (:require-refer (regrow [{} '[+i-must-be-a-cl-user+]])))))
