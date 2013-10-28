(ns slam.hound.asplode-test
  (:require [clojure.test :refer [deftest is]]
            [slam.hound.asplode :refer [asplode
                                        expand-imports
                                        expand-libspecs
                                        parse-refers]])
  (:import (java.io StringReader)))

(deftest ^:unit test-expand-imports
  (is (= (expand-imports '((my.prefix Foo Bar Baz)
                           (empty.prefix.list)
                           my.single.ClassSymbol))
         '#{my.prefix.Foo
            my.prefix.Bar
            my.prefix.Baz
            my.single.ClassSymbol})))

(deftest ^:unit test-expand-libspecs
  (is (= (expand-libspecs '[[my.ns.foo :as f :verbose true]
                            [my.ns [bar :as b] [baz :as z :verbose true]]
                            my.ns.quux
                            :reload-all])
         '#{[my.ns.foo :as f :verbose true :reload-all true]
            [my.ns.bar :as b :reload-all true]
            [my.ns.baz :as z :verbose true :reload-all true]
            [my.ns.quux :reload-all true]})))

(deftest ^:unit test-parse-refers
  (is (= (parse-refers 'my.ns '[:only [foo]
                                :exclude [bar]
                                :rename {baz mybaz}])
         '{:refer {my.ns #{foo}}
           :exclude {my.ns #{bar}}
           :rename {my.ns {baz mybaz}}}))
  (is (= (parse-refers 'my.ns [])
         '{:refer {my.ns :all}})))

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
