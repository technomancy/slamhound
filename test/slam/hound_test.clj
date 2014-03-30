(ns slam.hound-test
  (:require [clojure.test :refer [deftest is testing]]
            [slam.hound :refer [-main reconstruct
                                swap-in-reconstructed-ns-form]]
            [slam.hound.test.util :refer [with-tempfile with-transform-test]])
  (:import (java.io StringReader)))

;; For testing
(defrecord ExampleRecord [])
(def intersection (constantly "Conflicts with clojure.set/intersection"))

(deftest ^:unit test-swap-in-reconstructed-ns-form
  (testing "original file is preserved on exceptions"
    (with-tempfile tmp
      (let [buf "(ns foo)\n(FOO/bar)"]
        (spit tmp buf)
        (is (thrown? Throwable (swap-in-reconstructed-ns-form tmp)))
        (is (= buf (slurp tmp))))))
  (testing "accepts a String argument"
    (with-tempfile tmp
      (let [buf "(ns slamhound-test)"]
        (spit tmp buf)
        (swap-in-reconstructed-ns-form (.getPath tmp))
        (is (= buf (slurp tmp)))))))

(deftest ^:integration test-regression
  (is (= "(ns foo.bar\n  (:require [clj-schema.validation :as val]))\n"
         (reconstruct (StringReader.
                       (str '(ns foo.bar
                               (:require [clj-schema.validation :as val]))
                            '(val/validation-errors [[:name] String]
                                                    {:name "Bob"})))))))

(deftest ^:integration test-finds-alias-vars-in-nested-maps-and-sets
  (is (= "(ns foo.bar\n  (:require [clj-schema.validation :as val]))\n"
         (reconstruct (StringReader.
                       (str '(ns foo.bar
                               (:require [clj-schema.validation :as val]))
                            '#{:x {:a (val/validation-errors
                                       [[:name] String]
                                       {:name "Bob"})}}))))))

(deftest ^:unit test-reconstruct
  (with-transform-test "basic ns reconstruction"
    {:in "test-reconstruct.in"
     :out "test-reconstruct.out"}
    [tmp]
    (let [buf (reconstruct tmp)]
      (spit tmp buf))))

(deftest ^:integration test-prefers-references-from-orig-ns
  (with-transform-test "preference for references in original ns"
    {:in "test-prefer-orig-ns-refs.in"
     :out "test-prefer-orig-ns-refs.out"}
    [tmp]
    (swap-in-reconstructed-ns-form tmp))
  (with-transform-test "if both clojure.string/join and clojure.set/join
                        are referred in the original ns, prefer
                        clojure.string/join if clojure.string is also mass
                        referred."
    {:in "test-disambiguation-of-join.in"
     :out "test-disambiguation-of-join.out"}
    [tmp]
    (swap-in-reconstructed-ns-form tmp)))

(deftest ^:integration test-comment-header-preservation
  (with-transform-test "tidy preservation of comment headers"
    {:in "test-comment-headers.in"
     :out "test-comment-headers.out"}
    [tmp]
    (swap-in-reconstructed-ns-form tmp)))

(deftest ^:integration test-long-file-preservation
  (with-transform-test "long files are not truncated"
    {:in "test-long-files.in"
     :out "test-long-files.out"}
    [tmp]
    (binding [slam.hound/*testing?* true]
      (-main tmp))))
