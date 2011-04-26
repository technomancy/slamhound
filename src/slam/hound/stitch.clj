(ns slam.hound.stitch
  (:use [clojure.pprint :only [pprint code-dispatch with-pprint-dispatch]]))

(def ns-clauses [:use :require :import])

(def ns-clauses-to-preserve [:refer-clojure :gen-class :load])

(defn ns-from-map [ns-map]
  `(~'ns ~(:name ns-map)
     ~@(if-let [doco (:doc (:meta ns-map))] ; avoid inserting nil
         [doco])
     ~@(for [clause-type (concat ns-clauses ns-clauses-to-preserve)
             :when (seq (clause-type ns-map))]
         (cons clause-type (clause-type ns-map)))))

(defmulti collapse-clause (comp second list))

(defn- package-grouper [class-name]
  (-> class-name resolve .getPackage .getName))

(defn group-by-package [imports]
  (for [[package classes] (group-by package-grouper imports)]
    (cons (symbol package)
          (sort (for [c classes]
                  (-> c resolve .getSimpleName symbol))))))

(defmethod collapse-clause :import [ns-map clause]
  (update-in ns-map [:import] group-by-package))

(defmethod collapse-clause :require [ns-map clause]
  ns-map)

(defn group-by-namespace [uses]
  (for [[namespace subclause] (group-by first uses)]
    [namespace :only (vec (sort (for [[_ _ [var]] subclause]
                                  var)))]))

(defmethod collapse-clause :use [ns-map clause]
  (update-in ns-map [:use] group-by-namespace))

(defn collapse-clauses [ns-map]
  (reduce collapse-clause ns-map ns-clauses))

(defn sort-subclauses [ns-map]
  ;; lists aren't comparable? huh?
  (reduce #(update-in %1 [%2] (partial sort-by str))
          ns-map ns-clauses))

;; TODO: indentation here is all wrong, but pprint gets line length right.
(defn prettify [ns-form]
  (with-out-str
    (with-pprint-dispatch code-dispatch
      (pprint ns-form))))

(defn stitch-up [ns-map]
  (-> ns-map
      collapse-clauses
      sort-subclauses
      ns-from-map
      prettify
      (.replace "(ns\n " "(ns")))
