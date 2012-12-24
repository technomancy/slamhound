(ns slam.hound-test
   (:require [clojure.test :refer [deftest is]]
             [slam.hound :refer [reconstruct]])
   (:import java.io.StringReader))


(def basic-ns (str '(ns slamhound.sample
                      "Testing some things going on here."
                      (:use [slam.hound.stitch :only [ns-from-map]]
                            [clojure.test :only [is]]
                            [clojure.test :only [deftest testing]]) ;; 'testing' isn't used, so gets dropped
                      (:require [clojure.java.io :as io]
                                [clojure.set :as set])
                      (:import java.io.File java.io.ByteArrayInputStream
                               clojure.lang.Compiler$BodyExpr
                               java.util.UUID)
                      (:refer-clojure :exclude [compile test])
                      (:gen-class))

                   '(set/union #{:a} #{:b})
                   '(UUID/randomUUID)
                   '(instance? Compiler$BodyExpr nil)
                   '(io/copy (ByteArrayInputStream. (.getBytes "remotely human"))
                             (doto (File. "/tmp/remotely-human") .deleteOnExit))
                   '(deftest ^:unit test-ns-to-map
                      (is (= (ns-from-map {:ns 'slam.hound}))))))

(deftest ^:integration test-slamhound!!!
  (is (= "(ns slamhound.sample
  \"Testing some things going on here.\"
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :refer [deftest is]]
            [slam.hound.stitch :refer [ns-from-map]])
  (:import (clojure.lang Compiler$BodyExpr)
           (java.io ByteArrayInputStream File)
           (java.util UUID))
  (:refer-clojure :exclude [compile test])
  (:gen-class))
"
         (reconstruct (StringReader. basic-ns)))))

(deftest ^:integration test-regression
  (is (= "(ns foo.bar
  (:require [clj-schema.validation :as val]))
"
         (reconstruct (StringReader. (str '(ns foo.bar
                                             (:require [clj-schema.validation :as val]))
                                          '(val/validation-errors [[:name] String] {:name "Bob"})))))))

(deftest ^:integration test-finds-alias-vars-in-nested-maps-and-sets
  (is (= "(ns foo.bar
  (:require [clj-schema.validation :as val]))
"
         (reconstruct (StringReader. (str '(ns foo.bar
                                             (:require [clj-schema.validation :as val]))
                                          '#{:x {:a (val/validation-errors [[:name] String] {:name "Bob"})}}))))))

