(ns slam.hound-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [slam.hound :refer [-main read-comment-header reconstruct
                                swap-in-reconstructed-ns-form]])
  (:import (java.io File PushbackReader StringReader)))

(deftest ^:unit test-read-comment-header
  (testing "preserves comments at top of file"
    (is (= ";;\n;; Copyright © Phil Hagelberg\n;;\n\n"
           (read-comment-header
             (PushbackReader.
               (StringReader.
                 ";;\n;; Copyright © Phil Hagelberg\n;;\n\n(ns test)"))))))
  (testing "returns headers faithfully"
    (is (= "\n\r\n\t;; COPYRIGHT  \n\r\n\t;; LICENSE  \n\r\n"
           (read-comment-header
             (PushbackReader.
               (StringReader.
                 (str "\n\r\n\t;; COPYRIGHT  \n\r\n"
                      "\t;; LICENSE  \n\r\n(ns test)"))))))))

(deftest ^:unit test-swap-in-reconstructed-ns-form
  (testing "original file is preserved on exceptions"
    (let [tmp (File/createTempFile "slamhound_test" ".clj")
          buf "(ns foo)\n(FOO/bar)"]
      (try
        (spit tmp buf)
        (is (thrown? Throwable (swap-in-reconstructed-ns-form tmp)))
        (is (= buf (slurp tmp)))
        (finally
          (.delete tmp))))))

(defrecord ExampleRecord [])

(def basic-ns (str '(ns slamhound.sample
                      "Testing some things going on here."
                      (:use [slam.hound.stitch :only [ns-from-map]]
                            [clojure.test :only [is]]
                            ;; 'testing' isn't used, so gets dropped
                            [clojure.test :only [deftest testing]])
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
                   '(instance? ExampleRecord nil)
                   '(io/copy (ByteArrayInputStream. (.getBytes "remotely"))
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
           (java.util UUID)
           (slam.hound_test ExampleRecord))
  (:refer-clojure :exclude [compile test])
  (:gen-class))
"
         (reconstruct (StringReader. basic-ns)))))

(deftest ^:integration test-regression
  (is (= "(ns foo.bar
  (:require [clj-schema.validation :as val]))
"
         (reconstruct (StringReader.
                       (str '(ns foo.bar
                               (:require [clj-schema.validation :as val]))
                            '(val/validation-errors [[:name] String]
                                                    {:name "Bob"})))))))

(deftest ^:integration test-finds-alias-vars-in-nested-maps-and-sets
  (is (= "(ns foo.bar
  (:require [clj-schema.validation :as val]))
"
         (reconstruct (StringReader.
                       (str '(ns foo.bar
                               (:require [clj-schema.validation :as val]))
                            '#{:x {:a (val/validation-errors
                                       [[:name] String]
                                       {:name "Bob"})}}))))))

(deftest ^:integration test-chooses-referred-vars-preferring-those-referred-to-in-old-ns
  (testing "since both clojure.string and clojure.set are present in the original namespace
            we must disambiguate further by preferring namespaces that also referred the var
            in the old version of the ns form"
    (is (= "(ns foo.bar
  (:require [clojure.set :refer [union]]
            [clojure.string :refer [join]]))
"
           (reconstruct (StringReader.
                         (str '(ns foo.bar
                                 (:use [clojure.string]
                                       clojure.string)
                                 (:require [clojure.string :as str]
                                           [clojure.string :only [join lowercase]]
                                           [clojure.set :refer [union difference]]))
                              '(do
                                 (defn f [xs]
                                   (join "," xs))
                                 (defn g [a b]
                                   (union a b))))))))))

(deftest ^:integration test-prefers-requires-as-clauses-from-orig-ns
  ;; korma.core is on the :dev-dependencies, and was getting erroneously picked
  ;; for these 2 namespaces
  (testing "original ns has a require/refer"
    (is (= "(ns foo.bar
  (:require [clojure.string :refer [join]]))
"
           (reconstruct (StringReader.
                         (str '(ns foo.bar
                                 (:require [clojure.string :refer [join]]))
                              '(defn do-it! []
                                 (join "," ["a" "b" "c"]))))))))

  (testing "original ns has a use/only"
    (is (= "(ns foo.bar
  (:require [clojure.string :refer [join]]))
"
           (reconstruct (StringReader.
                         (str '(ns foo.bar
                                 (:use [clojure.string :only [join]]))
                              '(defn do-it! []
                                 (join "," ["a" "b" "c"])))))))))

(deftest ^:integration test-main
  (let [tmp (File/createTempFile "test_namespace_copy" ".clj")]
    (try
      (io/copy (io/reader (io/resource "test_namespace.clj")) tmp)
      (-main tmp)
      (is (= (slurp (io/resource "reconstructed_namespace.clj"))
             (slurp tmp)))
      (finally
        (.delete tmp)))))
