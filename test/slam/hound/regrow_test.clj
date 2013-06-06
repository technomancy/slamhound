(ns slam.hound.regrow-test
  (:require [clojure.test :refer [deftest is testing]]
            [slam.hound.regrow :refer [regrow in-originals-pred]]))

(def sample-body
  '((set/union #{:a} #{:b})
    (UUID/randomUUID)
    (instance? Compiler$BodyExpr nil)
    (io/copy (ByteArrayInputStream. (.getBytes "remotely human"))
             (doto (File. "/tmp/remotely-human") .deleteOnExit))
    (deftest ^:unit test-ns-to-map
      (is (= (ns-from-map {:ns 'slam.hound}))))))

(def sample-body-with-macros
  '((defmacro no-args []
      `(BitSet.))
    (defmacro more-args [x & xs]
      `(io/file ~x ~@xs))
    (defmacro multiple-arity
      ([a] `(set ~a))
      ([a b] `(set/union (set ~a) (set ~b))))
    (defmacro destructuring-args [[_] & {:keys [a b c] :or [a "" b #"." ":)"]}]
      `(string/replace-first ~a ~b ~c))))

(deftest ^:unit test-regrow
  (testing "regrows the :require-as, :require-refer, and :imports onto ns-map"
    (is (= {:name 'slamhound.sample
            :meta {:doc "Testing some things going on here."}
            :refer-clojure '(:exclude [compile test])
            :gen-class nil
            :require-as '[[clojure.java.io :as io] [clojure.set :as set]]
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
                                      [clojure.set :as set]]
                           :import '[java.io.File
                                     java.io.ByteArrayInputStream
                                     clojure.lang.Compiler$BodyExpr
                                     java.util.UUID]
                           :refer-clojure '(:exclude [compile test])
                           :gen-class nil}}
                    sample-body]))))
  (testing "regrows references inside of macro vars"
    (is (= '{:import (java.util.BitSet)
             :require-as ([clojure.string :as string]
                          [clojure.set :as set]
                          [clojure.java.io :as io])}
           (select-keys (regrow [{} sample-body-with-macros])
                        [:import :require-as])))))

(deftest ^:unit test-grow-preserve
  (let [in-orig? (in-originals-pred ['((java.util Date UUID))])]
    (is (in-orig? 'java.util.Date))
    (is (not (in-orig? 'java.sql.Date))))
  (is (= '(java.io.File java.util.Date)
         (:import (regrow [{:old {:import '((java.util Date))}}
                           '[(Date.) (File. "/tmp")]])))))

(def +i-must-be-a-cl-user+ true)

(deftest ^:unit test-plus
  (is (= '[[slam.hound.regrow-test :refer [+i-must-be-a-cl-user+]]]
         (:require-refer (regrow [{} '[+i-must-be-a-cl-user+]])))))
