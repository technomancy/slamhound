(ns slam.hound.stitch
  (:require [slam.hound.future :refer [cond->]]
            [slam.hound.prettify :refer [prettify]]))

(def ^:private ns-map-clauses [:require-as :require-refer :import])

(def ^:private ns-clauses-to-preserve [:refer-clojure :gen-class :load])

(defn- get-package [class-name]
  (let [cls ^Class (resolve class-name)]
    (if-let [pkg (.getPackage cls)]
      (.getName pkg)
      ;; Fall back to string matching for dynamically generated classes
      (second (re-find #"(.*)\." (.getCanonicalName cls))))))

(defn- group-by-package [imports]
  (for [[package classes] (group-by get-package imports)]
    (cons (symbol package)
          (sort (for [c classes]
                  (-> c resolve .getName (.split "\\.") last symbol))))))

(defn- group-by-namespace [uses]
  (for [[namespace subclause] (group-by first uses)
        :let [sc (for [[_ _ vars] subclause]
                   (if (coll? vars) (first vars) vars))
              vars (if (= [:all] sc) :all (vec (sort sc)))]]
    [namespace :refer vars]))

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

(defn keyword-list-from-map
  "Returns a cons list of keyword and the value of the keyword in ns-map"
  [kw ns-map]
  (let [s (ns-map kw)]
    (when (seq s)
      (cons kw s))))

(defn imports-from-map
  "Returns a collapsed :import form from an ns-map with {:import #{class-syms}}"
  [ns-map]
  (let [imports (:import ns-map)]
    (when (seq imports)
      (cons :import (group-by-package imports)))))

(defn- conj-refer [v refer ns-sym]
  (let [rs (refer ns-sym)]
    (if (= rs :all)
      (conj v :refer :all)
      (conj v :refer (vec (sort (refer ns-sym)))))))

(defn- ns-requires [ns-sym alias refer exclude rename]
  (cond-> [ns-sym]
    (get alias ns-sym) (conj :as (alias ns-sym))
    (get refer ns-sym) (conj-refer refer ns-sym)
    (get exclude ns-sym) (conj :exclude (vec (sort (exclude ns-sym))))
    (get rename ns-sym) (conj :rename (into (sorted-map) (rename ns-sym)))))

(defn requires-from-map
  "Returns a collapsed :require form from an ns-map with:
   {:require #{ns-syms}
    :alias   {ns-sym ns-sym}
    :refer   {ns-sym #{var-syms}/:all}
    :exclude {ns-sym #{var-syms}}
    :rename  {ns-sym {var-sym var-sym}}
    :verbose true/false
    :reload  true/false/:all}"
  [ns-map]
  (let [{:keys [require alias refer exclude rename verbose reload]} ns-map
        ;; Build the set of namespaces that will be required
        nss (into (set require) (mapcat keys [alias refer exclude rename]))
        ;; clojure.core should be required via :refer-clojure
        nss (disj nss 'clojure.core)]
    (when (seq nss)
      (let [reqs (reduce (fn [v ns]
                           (conj v (ns-requires ns alias refer exclude rename)))
                         [] nss)]
        (cond-> (cons :require (sort-by str reqs))
          reload (concat [(if (= reload :all) :reload-all :reload)])
          verbose (concat [:verbose]))))))

(defn refer-clojure-from-map
  "Return a :refer-clojure form from an ns-map with:
  {:refer   {'clojure.core #{var-syms}/:all}
   :exclude {'clojure.core #{var-syms}}
   :rename  {'clojure.core {var-sym var-sym}}}"
  [ns-map]
  (let [{:keys [refer exclude rename]} ns-map
        c 'clojure.core]
    (when (seq (mapcat keys [refer exclude rename]))
      (let [refs (cond-> []
                   (not= (get refer c) :all) (conj :only (vec (sort (refer c))))
                   (get exclude c) (conj :exclude (vec (sort (exclude c))))
                   (get rename c) (conj :rename (into (sorted-map) (rename c))))]
        (when (seq refs)
          (cons :refer-clojure refs))))))

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
