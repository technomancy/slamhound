(ns slam.hound.prettify-test
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [*print-right-margin*]]
            [clojure.test :refer [deftest is testing]]
            [slam.hound.prettify :as p]))

(defn prettify [form]
  (binding [*print-right-margin* 72]
    (p/prettify form)))

(deftest ^:unit test-prettify
  (testing "always inserts newlines inside short requires"
    (is (= (slurp (io/resource "test-prettify-short-requires.out"))
           (prettify
             '(ns foo
                (:require [clojure.java.io :as io]
                          [clojure.pprint :refer [pprint]]))))))
  (testing "does not print :refer vectors in :miser mode"
    (is (= (slurp (io/resource "test-prettify-no-miserly-refers.out"))
           (prettify
             '(ns foo
                (:require [my.very.sequipedalian.namespace
                           :refer [alpha beta gamma delta epsilon]]))))))
  (testing "keeps multiple libspec option keys and values together"
    (is (= (slurp (io/resource "test-prettify-libspecs.out"))
           (prettify
             '(ns foo
                (:require [clojure.pprint
                           :as pp
                           :refer [*print-miser-width* cl-format code-dispatch
                                   formatter-out pprint pprint-logical-block
                                   pprint-newline with-pprint-dispatch
                                   write-out]])))))
    (is (= (slurp (io/resource "test-prettify-long-libspecs.out"))
           (prettify
             '(ns foo
                (:require [clojure.pprint
                           :as pp
                           :refer [formatter-out pprint-logical-block]
                           :rename {formatter-out fout
                                    pprint-logical-block block}])))))))
