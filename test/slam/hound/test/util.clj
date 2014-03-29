(ns slam.hound.test.util
  (:require [clojure.java.io :as io]
            [clojure.test :as t])
  (:import (java.io File)))

(defmacro with-tempfile
  {:requires [File]}
  [tmp-sym & body]
  `(let [~tmp-sym (File/createTempFile "slamhound_test" ".clj")]
     (try
       ~@body
       (finally
         (.delete ~tmp-sym)))))

(defmacro with-transform-test
  "Copy contents of `in` to a tempfile, execute body with tempfile bound to
  tmp-sym, then finally compare the transformed contents of the tempfile with
  the contents of `out`.

  `in` and `out` are urls that will be passed to clojure.java.io/resource."
  {:requires [#'t/testing #'with-tempfile]}
  [string {:keys [in out]} [tmp-sym] & body]
  `(t/testing ~string
     (with-tempfile ~tmp-sym
       (try
         (spit ~tmp-sym (slurp (~io/resource ~in)))
         ~@body
         (catch Throwable e#
           (spit ~tmp-sym e#))
         (finally
           (t/is (= (slurp ~tmp-sym)
                    (slurp (~io/resource ~out)))))))))
