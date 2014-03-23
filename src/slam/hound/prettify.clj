(ns slam.hound.prettify
  "Format a namespace declaration using pretty print with custom dispatch.

  Documentation on Common Lisp format strings:
  https://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/html/cltl/clm/node200.html"
  (:require [clojure.pprint :refer [*print-miser-width* cl-format code-dispatch
                                    formatter-out pprint pprint-logical-block
                                    pprint-newline with-pprint-dispatch
                                    write-out]]
            [clojure.string :refer [escape]]))

(defn- brackets
  "Figure out which kind of brackets to use"
  [form]
  (if (vector? form)
    ["[" "]"]
    ["(" ")"]))

(defn- pprint-indented-coll
  "Pretty print a collection, setting an indent point before the head element
  and reflowing to multiple lines as necessary."
  [coll]
  ((formatter-out "~:i~{~w~^ ~:_~}") coll))

(defn- pprint-liblist
  "Pretty print dispatch chunk for ns libspecs and prefix lists."
  [form]
  (let [[start end] (brackets form)
        [head & tail] form]
    (pprint-logical-block :prefix start :suffix end
      (write-out head)
      (when (seq tail)
        ((formatter-out " "))
        (let [opts (partition-all 2 tail)]
          ;; Is this a libspec? i.e. [head :key1 val1 :key2 val2]
          (if (every? keyword? (map first opts))
            (let [ncoll (count (filter coll? (map second opts)))]
              (loop [[[key val] & next-opts] opts]
                (when (> ncoll 1)
                  ((formatter-out "~:_")))
                ((formatter-out "~w ") key)
                (if (sequential? val)
                  (let [[start end] (brackets val)]
                    (pprint-logical-block :prefix start :suffix end
                      (pprint-indented-coll val)))
                  (write-out val))
                (when next-opts
                  ((formatter-out " "))
                  (recur next-opts))))
            (pprint-indented-coll tail)))))))

(defn- pprint-ns-reference
  "Pretty print a single reference (import, use, etc.) from a namespace decl"
  [reference]
  (if (sequential? reference)
    (let [[start end] (brackets reference)
          [keyw & args] reference]
      (pprint-logical-block :prefix start :suffix end
        ((formatter-out "~w~:i") keyw)
        (loop [args args]
          (when (seq args)
            ((formatter-out " "))
            (let [arg (first args)]
              (if (sequential? arg)
                (do (pprint-liblist arg)
                    (when (next args)
                      ;; liblists should always be on their own line
                      ((formatter-out "~:@_"))))
                (do
                  (write-out arg)
                  (when (next args)
                    ((formatter-out "~:_"))))))
            (recur (next args))))))
    (write-out reference)))

(defn- pprint-ns
  "The pretty print dispatch chunk for the ns macro"
  [alis]
  (if (next alis)
    (let [[ns-sym ns-name & stuff] alis
          [doc-str stuff] (if (string? (first stuff))
                            [(first stuff) (next stuff)]
                            [nil stuff])
          [attr-map references] (if (map? (first stuff))
                                  [(first stuff) (next stuff)]
                                  [nil stuff])]
      (pprint-logical-block :prefix "(" :suffix ")"
        ((formatter-out "~w ~1I~@_~w") ns-sym ns-name)
        (when (or doc-str attr-map (seq references))
          ((formatter-out "~@:_")))
        (when doc-str
          (let [doc-str (escape doc-str {\\ "\\\\" \" "\\\""})]
            (cl-format true "\"~a\"~:[~;~:@_~]" doc-str
                       (or attr-map (seq references)))))
        (when attr-map
          ((formatter-out "~w~:[~;~:@_~]") attr-map (seq references)))
        (when references
          (loop [references references]
            (pprint-ns-reference (first references))
            (when-let [references (next references)]
              (pprint-newline :linear)
              (recur references))))))
    (write-out alis)))

(defn augmented-dispatch
  "A wrapper for code-dispatch that supports better ns printing"
  [form]
  (if (and (seq? form) (= 'ns (first form)))
    (pprint-ns form)
    (code-dispatch form)))

(defn prettify
  "Pretty print the ns-form to a string"
  [ns-form]
  (with-out-str
    ;; :miser mode often results in ugly libspecs, so disable it
    (binding [*print-miser-width* nil]
      (with-pprint-dispatch augmented-dispatch
        (pprint ns-form)))))
