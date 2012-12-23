(ns slam.hound.stitch
  (:use [slam.hound.prettify :only [prettify]]))

(def ^:private ns-map-clauses [:require-as :require-refer :import])

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
    [namespace :refer (vec (sort (for [[_ _ [var]] subclause]
                                   var)))]))

(defn collapse-clause [ns-map clause]
  (case clause
    :require-refer (update-in ns-map [:require-refer] group-by-namespace)
    :import (update-in ns-map [:import] group-by-package)
    :require-as ns-map))

(defn- collapse-clauses [ns-map]
  (reduce collapse-clause ns-map ns-map-clauses))

(defn sort-subclauses [ns-map]
  ;; lists aren't comparable? huh?
  (reduce #(update-in %1 [%2] (partial sort-by str))
          ns-map
          ns-map-clauses))

(defn ns-from-map [ns-map]
  (let [ns-map (-> ns-map ;; combining :require-as and :require-refer into :require
                   (assoc :require (concat (:require-as ns-map) (:require-refer ns-map)))
                   (dissoc :require-as :require-refer))]
    `(~'ns ~(:name ns-map)
       ~@(if-let [doco (:doc (:meta ns-map))] ; avoid inserting nil
           [doco])
       ~@(for [clause-type (concat [:require :import] ns-clauses-to-preserve)
               :when (seq (clause-type ns-map))]
           (cons clause-type (clause-type ns-map))))))

(defn stitch-up [ns-map]
  (-> ns-map
      collapse-clauses
      sort-subclauses
      ns-from-map
      prettify))
