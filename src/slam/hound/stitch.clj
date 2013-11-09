(ns slam.hound.stitch
  (:require [slam.hound.future :refer [cond->*]]
            [slam.hound.prettify :refer [prettify]]))

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

(defn metadata-from-map
  "Returns a vector of: [docstring? meta-map?]"
  [ns-map]
  (let [meta (:meta ns-map)
        doc (:doc meta)
        meta (into (sorted-map) (dissoc meta :doc))]
    (cond->* []
      doc (conj doc)
      (seq meta) (conj meta))))

(defn keyword-list-from-map
  "Returns a cons list of keyword and the value of the keyword in ns-map"
  [kw ns-map]
  (when-let [s (ns-map kw)]
    (cons kw s)))

(defn imports-from-map
  "Returns an :import form from an ns-map with {:import #{class-syms}}"
  [ns-map]
  (let [imports (:import ns-map)]
    (when (seq imports)
      (cons :import (sort-by str (group-by-package imports))))))

(defn- ns-requires [ns-sym ns-map]
  (let [{:keys [alias refer refer-all exclude rename]} ns-map]
    (cond->* [ns-sym]
      (get alias ns-sym) (conj :as (alias ns-sym))
      (get refer ns-sym) (conj :refer (vec (sort (refer ns-sym))))
      (get refer-all ns-sym) (conj :refer :all)
      (get exclude ns-sym) (conj :exclude (vec (sort (exclude ns-sym))))
      (get rename ns-sym) (conj :rename (into (sorted-map) (rename ns-sym))))))

(defn requires-from-map
  "Returns a :require form from an ns-map with:
  {:require    #{ns-syms}
   :alias      {ns-sym ns-sym}
   :refer      {ns-sym #{var-syms}}
   :refer-all  #{ns-sym}
   :exclude    {ns-sym #{var-syms}}
   :rename     {ns-sym {var-sym var-sym}}
   :reload     true/false
   :reload-all true/false
   :verbose    true/false}"
  [ns-map]
  (let [{:keys [require alias refer refer-all exclude
                rename reload reload-all verbose]} ns-map
        ;; Build the set of namespaces that will be required
        nss (reduce into #{} [require refer-all
                              (mapcat keys [alias refer exclude rename])])
        ;; clojure.core should be required via :refer-clojure
        nss (disj nss 'clojure.core)]
    (when (seq nss)
      (let [reqs (map #(ns-requires % ns-map) nss)]
        (cond->* (cons :require (sort-by str reqs))
          reload (concat [:reload])
          reload-all (concat [:reload-all])
          verbose (concat [:verbose]))))))

(defn refer-clojure-from-map
  "Return a :refer-clojure form from an ns-map with:
  {:refer     {'clojure.core #{var-syms}}
   :refer-all #{'clojure.core}
   :exclude   {'clojure.core #{var-syms}}
   :rename    {'clojure.core {var-sym var-sym}}}"
  [ns-map]
  (let [{:keys [refer refer-all exclude rename]} ns-map
        c 'clojure.core]
    (when (some #{c} (concat refer-all (mapcat keys [refer exclude rename])))
      (let [rs (if (and (not (contains? refer-all c))
                        (contains? refer c))
                 [:only (vec (sort (refer c)))]
                 [])
            rs (cond->* rs
                 (get exclude c) (conj :exclude (vec (sort (exclude c))))
                 (get rename c) (conj :rename (into (sorted-map) (rename c))))]
        (when (seq rs)
          (cons :refer-clojure rs))))))

(defn ns-from-map
  "Generate an ns-form from an ns-map in the form of
   #'slam.hound.asplode/empty-ns-references"
  [ns-map]
  `(~'ns ~(:name ns-map)
     ~@(metadata-from-map ns-map)
     ~@(filter identity [(requires-from-map ns-map)
                         (imports-from-map ns-map)
                         (refer-clojure-from-map ns-map)
                         (keyword-list-from-map :gen-class ns-map)
                         (keyword-list-from-map :load ns-map)])))

(defn stitch-up [ns-map]
  (-> ns-map
      ns-from-map
      prettify))
