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

(defn- pprint-liblist
  "Pretty print dispatch chunk for ns libspecs and prefix lists."
  [form]
  (let [[start end] (brackets form)]
    (pprint-logical-block :prefix start :suffix end
      (if (and (= (count form) 3) (keyword? (second form)))
        (let [[ns kw lis] form]
          ((formatter-out "~w ~w ") ns kw)
          (if (sequential? lis)
            ((formatter-out (if (vector? lis)
                              "~<[~;~@{~w~^ ~:_~}~;]~:>"
                              "~<(~;~@{~w~^ ~:_~}~;)~:>"))
             lis)
            (write-out lis)))
        (apply (formatter-out "~w~^ ~:i~@{~w~^ ~:_~}") form)))))

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
