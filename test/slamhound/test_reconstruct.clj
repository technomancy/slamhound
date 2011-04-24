(ns slamhound.test-reconstruct
  "Testing some things going on here."
  (:use [clojure.test]
        [slam.hound]))

(def sample-ns-form '(ns slamhound.test-reconstruct
                       "Testing some things going on here."
                       (:refer-clojure :exclude [compile test])
                       (:use [clojure.test]
                             [clojure.zip :only [zipper up]]
                             [slam.hound])
                       (:require [clojure.java.io :as io]
                                 [clojure.set :as set])
                       (:import (java.io File ByteArrayInputStream)
                                (java.util UUID))))

(def sample-ns-map
  {:name 'slamhound.test-reconstruct
   :meta {:doc "Testing some things going on here."}
   :use '([clojure.test] [clojure.zip :only [zipper up]] [slam.hound])
   :require '([clojure.java.io :as io] [clojure.set :as set])
   :import '((java.io File ByteArrayInputStream) (java.util UUID))
   :refer-clojure '(:exclude [compile test])})

(deftest test-ns-to-map
  (is (= sample-ns-map (ns-to-map sample-ns-form))))

(deftest test-ns-from-map
  (is (= sample-ns-form (ns-from-map sample-ns-map))))
