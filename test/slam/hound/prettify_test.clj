(ns slam.hound.prettify-test
  (:require [slam.hound.prettify :refer [prettify]]
            [clojure.test :as test :refer [deftest is testing]]))

(deftest ^:unit test-prettify
  (testing "always inserts newlines inside short requires"
    (is (= "(ns foo
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]))\n"
           (prettify '(ns foo
                        (:require [clojure.java.io :as io]
                                  [clojure.pprint :refer [pprint]])))))))
