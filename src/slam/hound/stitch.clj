(ns slam.hound.stitch
  (:use [slam.hound.prettify :only [prettify]]))

(def ^:private ns-clauses [:use :require :import])

(def ^:private ns-clauses-to-preserve [:refer-clojure :gen-class :load])

(defn- get-package [class-name]
  (-> class-name resolve .getPackage .getName))

(defn- group-by-package [imports]
  (for [[package classes] (group-by get-package imports)]
    (cons (symbol package)
          (sort (for [c classes]
                  (-> c resolve .getName (.split "\\.") last symbol))))))

(defn- group-by-namespace [uses]
  (for [[namespace subclause] (group-by first uses)]
    [namespace :only (vec (sort (for [[_ _ [var]] subclause]
                                  var)))]))

(defn collapse-clause [ns-map clause]
  (case clause
    :use    (update-in ns-map [:use]    group-by-namespace)
    :import (update-in ns-map [:import] group-by-package)
    ns-map))

(defn- collapse-clauses [ns-map]
  (reduce collapse-clause ns-map ns-clauses))

(defn sort-subclauses [ns-map]
  ;; lists aren't comparable? huh?
  (reduce #(update-in %1 [%2] (partial sort-by str))
          ns-map
          ns-clauses))

(defn ns-from-map [ns-map]
  `(~'ns ~(:name ns-map)
     ~@(if-let [doco (:doc (:meta ns-map))] ; avoid inserting nil
         [doco])
     ~@(for [clause-type (concat ns-clauses ns-clauses-to-preserve)
             :when (seq (clause-type ns-map))]
         (cons clause-type (clause-type ns-map)))))

(defn stitch-up [ns-map]
  (-> ns-map
      collapse-clauses
      sort-subclauses
      ns-from-map
      prettify))
