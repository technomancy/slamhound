(ns slam.hound.stitch
  (:require [slam.hound.prettify :refer [prettify]]))

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
  (for [[namespace subclause] (group-by first uses)
        :let [referred (nth (first subclause) 2)]]
    (if (= :all referred)
      [namespace :refer :all]
      [namespace :refer (vec (sort (for [[_ _ [var]] subclause]
                                     var)))])))

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
  (let [ns-map (assoc ns-map :require (concat (:require-as ns-map)
                                              (:require-refer ns-map)))]
    `(~'ns ~(:name ns-map)
       ~@(if-let [doco (:doc (:meta ns-map))] ; avoid inserting nil
           [doco])
       ~@(for [clause-type (concat [:require :import] ns-clauses-to-preserve)
               :when (and (contains? ns-map clause-type)
                          (or (= clause-type :gen-class)
                              (seq (clause-type ns-map))))]
           (cons clause-type (clause-type ns-map))))))

(defn stitch-up [ns-map]
  (-> ns-map
      collapse-clauses
      sort-subclauses
      ns-from-map
      prettify))
