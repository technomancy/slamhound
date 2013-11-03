(ns slam.hound.asplode-test
  (:require [clojure.test :refer [deftest is testing]]
            [slam.hound.asplode :refer [asplode
                                        expand-imports
                                        expand-libspecs
                                        ns-to-map
                                        parse-libs
                                        parse-refers
                                        parse-requires
                                        parse-uses
                                        preserve-ns-references]])
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
  (is (= (parse-refers 'my.ns '[:only [foo] :exclude [bar] :rename {baz mybaz}])
         '{:refer {my.ns #{foo}}
           :exclude {my.ns #{bar}}
           :rename {my.ns {baz mybaz}}}))
  (is (= (parse-refers 'my.ns [])
         '{:refer-all #{my.ns}}))
  (testing "also excludes from cljs.core when excluding clojure.core"
    (is (= (parse-refers 'clojure.core '[:exclude [defn]])
           '{:exclude {clojure.core #{defn} cljs.core #{defn}}})))
  (testing "exclusive-refer?"
    (is (= (parse-refers 'my.ns '[:only [foo]] true)
           '{:refer {my.ns #{foo}} :xrefer #{my.ns}}))))

(deftest ^:unit test-parse-requires
  (is (= (parse-requires '[[my.ns.foo :as foo :refer [foo]]
                           [my.ns [bar :as bar] [baz :refer :all]]])
         '{:alias {my.ns.foo foo
                   my.ns.bar bar}
           :refer {my.ns.foo #{foo}}
           :refer-all #{my.ns.baz}}))
  (is (= (parse-requires '[my.ns.foo [my.ns [bar] [baz]]])
         '{:require #{my.ns.foo my.ns.bar my.ns.baz}}))
  (is (= (parse-requires '[[my.ns.foo :as foo]
                           [my.ns.bar :refer [bar]]
                           :verbose :reload-all])
         '{:alias {my.ns.foo foo}
           :refer {my.ns.bar #{bar}}
           :verbose true
           :reload-all true})))

(deftest ^:unit test-parse-uses
  (is (= (parse-uses '[my.ns.base
                       [my.ns [foo :exclude [foo]] [bar :only [bar]]]
                       [my.ns.baz :as baz :only [baz]]
                       [my.ns.quux]])
         '{:refer {my.ns.bar #{bar}
                   my.ns.baz #{baz}}
           :refer-all #{my.ns.base my.ns.quux}
           :alias {my.ns.baz baz}
           :exclude {my.ns.foo #{foo}}})))

(deftest ^:unit test-parse-libs
  (testing "refer-clojure"
    (is (= (parse-libs {:exclude '{foo #{foo}}}
                       :refer-clojure
                       '[:exclude [defn defrecord]])
           '{:exclude {foo #{foo}
                       clojure.core #{defn defrecord}
                       cljs.core #{defn defrecord}}}))
    (is (= (parse-libs {} :refer-clojure '[:only [defn]])
           '{:refer {clojure.core #{defn} cljs.core #{defn}}
             :xrefer #{clojure.core cljs.core}})))
  (testing "load, gen-class"
    (is (= (parse-libs {:load ["/foo"]} :load ["/bar" "/baz"])
           {:load ["/bar" "/baz"]}))
    (is (= (parse-libs {:gen-class [:init 'foo]} :gen-class [:name 'bar])
           {:gen-class [:name 'bar]}))))

(deftest ^:unit test-ns-to-map
  (testing "recognizes maps as metadata"
    (is (= (:meta (ns-to-map '(ns my.ns {:foo "foo"})))
           {:foo "foo"}))
    (is (= (:meta (ns-to-map '(ns my.ns "With docstring" {:bar "bar"})))
           {:bar "bar" :doc "With docstring"})))
  (testing "parses empty :gen-class"
    (is (= (ns-to-map '(ns my.ns (:gen-class)))))))

(deftest ^:unit test-preserve-ns-references
  (testing "retains :gen-class and :load"
    (is (= (preserve-ns-references '{:gen-class [:foo foo :bar bar]
                                     :load ["foo" "bar"]
                                     :refer {my.ns #{foo}}})
           '{:gen-class [:foo foo :bar bar]
             :load ["foo" "bar"]}))
    (is (= (preserve-ns-references {:gen-class [] :load []})
           {:gen-class [] :load []}))
    (is (= (preserve-ns-references {:gen-class nil :load []})
           {:load []})))
  (testing "retains refers, exclusions, and renames for clojure.core"
    (is (= (preserve-ns-references '{:exclude {clojure.core #{==}
                                               my.ns #{foo}}
                                     :rename {clojure.core {/ div}
                                              my.ns {bar -bar}}
                                     :refer {clojure.core #{== + - * /}
                                             my.ns #{baz}}})
           '{:exclude {clojure.core #{==}}
             :rename {clojure.core {/ div}}
             :refer {clojure.core #{== + - * /}}}))))

(deftest ^:unit test-asplode
  (is (= (asplode (StringReader.
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
                         '(do something))))
         '[{:old {:import #{java.io.File
                            java.io.ByteArrayInputStream
                            java.util.UUID
                            clojure.lang.Compiler$BodyExpr}
                  :require #{}
                  :alias {clojure.java.io io
                          clojure.set set}
                  :refer {clojure.test #{deftest is}
                          slam.hound.stitch #{ns-from-map}}
                  :xrefer #{}
                  :refer-all #{}
                  :exclude {clojure.core #{test compile}
                            cljs.core #{test compile}}
                  :rename {}
                  :reload false
                  :reload-all false
                  :verbose false
                  :load nil
                  :gen-class []}
            :gen-class []
            :exclude {clojure.core #{test compile}}
            :meta {:doc "Testing some things going on here."}
            :name slamhound.sample}
           ((do something))])))
