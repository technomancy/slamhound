(ns slam.hound.regrow-test
  {:slamhound-skip true}
  (:require [clojure.test :refer [deftest is testing]]
            [korma.core]
            [slam.hound.regrow :refer [candidates
                                       disambiguate
                                       grow-ns-map
                                       regrow]]))

;; Classes and vars for testing
(defrecord RegrowTestRecord [])
(defrecord UUID [])
(def +i-must-be-a-cl-user+ true)

(deftest ^:unit test-candidates
  (testing "finds static and dynamically created Java packages"
    (is (= (candidates :import 'UUID '((UUID/randomUUID)))
           '#{java.util.UUID}))
    (is (= (candidates :import 'Compiler$BodyExpr '(Compiler$BodyExpr))
           '#{clojure.lang.Compiler$BodyExpr}))
    (is (= (candidates :import 'RegrowTestRecord '((RegrowTestRecord.)))
           '#{slam.hound.regrow_test.RegrowTestRecord})))
  (testing "finds aliased namespaces"
    (is (= (candidates :alias 's '((s/join #{:a} #{:b})))
           '#{clojure.set clojure.string korma.core})))
  (testing "finds referred vars"
    (is (= (candidates :refer '+i-must-be-a-cl-user+
                       '((assert +i-must-be-a-cl-user+)))
           '#{slam.hound.regrow-test}))
    (is (= (candidates :refer 'join '((join #{:a} #{:b})))
           '#{clojure.set clojure.string korma.core}))))

(deftest ^:unit test-disambiguate
  (testing "removes candidates matching disambiguator-blacklist"
    (is (nil? (disambiguate '#{swank lancet} :alias 'swank {}))))
  (testing "removes namespaces with excluded vars"
    (is (nil? (disambiguate '#{clojure.string clojure.set}
                            :refer 'join
                            '{:exclude {clojure.string #{join}
                                        clojure.set #{join}}})))
    (is (nil? (disambiguate '#{clojure.core core.logic} :refer '==
                            '{:xrefer #{clojure.core core.logic}
                              :refer {clojure.core #{} core.logic #{}}}))))
  (testing "prefers imports from old ns"
    (is (= (disambiguate '#{java.util.UUID slam.hound.regrow.UUID}
                         :import 'UUID
                         '{:import #{java.util.UUID}})
           '[:import java.util.UUID])))
  (testing "prefers aliases from old ns"
    (is (= (disambiguate '#{clojure.set clojure.string}
                         :alias 's
                         '{:alias {clojure.set s}})
           '[:alias clojure.set])))
  (testing "prefers refers from old ns"
    (is (= (disambiguate '#{clojure.set clojure.string}
                         :refer 'join
                         '{:refer {clojure.set #{join}}})
           '[:refer clojure.set])))
  (testing "prefers aliases where the last segment matches"
    (is (= (disambiguate '#{clojure.set clojure.string} :alias 'set {})
           '[:alias clojure.set])))
  (testing "prefers shortest candidates when no other predicates match"
    (is (= (disambiguate '#{clojure.java.io clojure.set clojure.string}
                         :alias 'a {})
           '[:alias clojure.set])))
  (testing "changes type to :refer-all when top candidate is in old :refer-all"
    (is (= (disambiguate '#{clojure.set clojure.string}
                         :refer 'join
                         '{:refer-all #{clojure.set}})
           '[:refer-all clojure.set]))))

(deftest ^:unit test-grow-ns-map
  (testing "finds basic imports, aliases, and refers"
    (is (= (grow-ns-map {} :import 'RegrowTestRecord '((RegrowTestRecord.)))
           '{:import #{slam.hound.regrow_test.RegrowTestRecord}}))
    (is (= (grow-ns-map {} :alias 'string '((string/join)))
           '{:alias {clojure.string string}}))
    (is (= (grow-ns-map {} :refer 'pprint '((pprint [])))
           '{:refer {clojure.pprint #{pprint}}})))
  (testing "honors old :refer :all"
    (is (= (grow-ns-map '{:old {:refer-all #{clojure.pprint}}}
                        :refer 'pprint '((pprint [])))
           '{:old {:refer-all #{clojure.pprint}}
             :refer-all #{clojure.pprint}}))))

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
                             [clojure.test :refer [is]]
                             [clojure.test :refer [deftest]]]
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
                    sample-body]))))
  (testing "prefers candidate where last segment matches"
    (is (= (set (:require-as (regrow [{} sample-body])))
           '#{[clojure.java.io :as io]
              [clojure.string :as string]
              [clojure.set :as set]})))
  (testing "honors :refer :all in old ns"
    (is (= (set (:require-refer
                  (regrow [{:old {:require '[[clojure.test :refer :all]]}}
                           sample-body])))
           '#{[clojure.test :refer :all]
              [slam.hound.stitch :refer [ns-from-map]]}))))

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
