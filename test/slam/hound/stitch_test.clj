(ns slam.hound.stitch-test
  (:require [clojure.test :refer [deftest is testing]]
            [slam.hound.stitch :refer [collapse-clause
                                       imports-from-map
                                       keyword-list-from-map
                                       ns-from-map
                                       refer-clojure-from-map
                                       requires-from-map
                                       sort-subclauses
                                       stitch-up]]))

(def sample-ns-form '(ns slamhound.sample
                       "Testing some \"things\"
going on here."
                       (:require [clojure.java.io :as io]
                                 [clojure.set :as set]
                                 [slam.hound.stitch :refer [ns-from-map]]
                                 [clojure.test :refer [is]]
                                 [clojure.test :refer [deftest]])
                       (:import java.io.File java.io.ByteArrayInputStream
                                clojure.lang.Compiler$BodyExpr
                                java.util.UUID)
                       (:refer-clojure :exclude [compile test])))

(def sample-ns-map
  {:name 'slamhound.sample
   :meta {:doc "Testing some \"things\"\ngoing on here."}
   :require-refer '[[slam.hound.stitch :refer [ns-from-map]]
                    [clojure.test :refer [is]]
                    [clojure.test :refer [deftest]]]
   :require-as '([clojure.java.io :as io] [clojure.set :as set])
   :import '(java.io.File java.io.ByteArrayInputStream
                          clojure.lang.Compiler$BodyExpr java.util.UUID)
   :refer-clojure '(:exclude [compile test])})

(deftest test-keyword-list-from-map
  (is (= (keyword-list-from-map
           :gen-class '{:gen-class [:name Foo :extends Bar]})
         '(:gen-class :name Foo :extends Bar)))
  (is (nil? (keyword-list-from-map :foo '{:foo []}))))

(deftest test-imports-from-map
  (is (= (imports-from-map '{:import #{java.util.BitSet
                                       java.util.Random
                                       java.io.File}})
         '(:import (java.io File)
                   (java.util BitSet Random))))
  (is (nil? (imports-from-map {:import #{}}))))

(deftest test-requires-from-map
  (is (= (requires-from-map '{:import  #{}
                              :require #{clojure.xml}
                              :alias   {clojure.string string}
                              :refer   {clojure.string #{trim}
                                        clojure.set #{difference}
                                        clojure.java.shell :all}
                              :exclude {clojure.java.shell [with-sh-env]}
                              :rename  {clojure.java.shell {sh ssshhh}}
                              :verbose true
                              :reload  :all})
         '(:require [clojure.java.shell :refer :all :exclude [with-sh-env]
                     :rename {sh ssshhh}]
                    [clojure.set :refer [difference]]
                    [clojure.string :as string :refer [trim]]
                    [clojure.xml]
                    :reload-all :verbose)))
  (testing "special handling of :refer {clojure.core :all}"
    (is (nil? (requires-from-map '{:refer {clojure.core :all}})))))

(deftest test-refer-clojure-from-map
  (is (nil? (refer-clojure-from-map '{:refer {clojure.core :all}})))
  (is (= (refer-clojure-from-map '{:refer {clojure.core :all}
                                   :rename {clojure.core {/ div}}
                                   :exclude {clojure.core [* + -]}})
         '(:refer-clojure :exclude [* + -] :rename {/ div})))
  (is (= (refer-clojure-from-map '{:refer {clojure.core [refer]}})
         '(:refer-clojure :only [refer]))))

(deftest ^:unit test-ns-from-map
  (is (= sample-ns-form (ns-from-map sample-ns-map))))

;; TODO Dec 21st - is this test necessary?
;; It *should* be covered by tests for stitch-up
(deftest ^:unit test-sort
  (is (= {:name 'slamhound.sample
          :meta {:doc "Testing some \"things\"\ngoing on here."}
          :require-refer '[[clojure.test :refer [deftest]]
                           [clojure.test :refer [is]]
                           [slam.hound.stitch :refer [ns-from-map]]]
          :require-as '([clojure.java.io :as io] [clojure.set :as set])
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
  (is (= {:require-refer '[[clojure.test :refer [deftest is]]
                           [slam.hound.stitch :refer [ns-from-map]]]}
         (collapse-clause {:require-refer '[[clojure.test :refer [deftest]]
                                            [slam.hound.stitch :refer
                                             [ns-from-map]]
                                            [clojure.test :refer [is]]]}
                          :require-refer))))

(deftest ^:unit test-stitch-up
  (is (= "(ns slamhound.sample
  \"Testing some \\\"things\\\"\ngoing on here.\"
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :refer [deftest is]]
            [slam.hound.stitch :refer [ns-from-map]])
  (:import (clojure.lang Compiler$BodyExpr)
           (java.io ByteArrayInputStream File)
           (java.util UUID))
  (:refer-clojure :exclude [compile test]))
" (stitch-up sample-ns-map))))
