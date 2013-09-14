(ns slam.hound.asplode-test
  (:require [clojure.test :refer [deftest is]]
            [slam.hound.asplode :refer [asplode]])
  (:import (java.io StringReader)))

(deftest ^:unit test-asplode
  (is (= [{:name 'slamhound.sample
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
                                        clojure.lang.Compiler$BodyExpr
                                        java.util.UUID)
                 :refer-clojure '(:exclude [compile test])
                 :gen-class nil}}
          '((do something))]

         (asplode (StringReader.
                   (str '(ns slamhound.sample
                           "Testing some things going on here."
                           (:use [slam.hound.stitch :only [ns-from-map]]
                                 [clojure.test :only [is]]
                                 [clojure.test :only [deftest]])
                           (:require [clojure.java.io :as io]
                                     [clojure.set :as set])
                           (:import java.io.File java.io.ByteArrayInputStream
                                    clojure.lang.Compiler$BodyExpr
                                    java.util.UUID)
                           (:refer-clojure :exclude [compile test])
                           (:gen-class))
                        '(do something)))))))
