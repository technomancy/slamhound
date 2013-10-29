(ns slam.hound.stitch
  (:require [slam.hound.future :refer [cond->]]
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

(defn keyword-list-from-map
  "Returns a cons list of keyword and the value of the keyword in ns-map"
  [kw ns-map]
  (let [s (ns-map kw)]
    (when (seq s)
      (cons kw s))))

(defn imports-from-map
  "Returns an :import form from an ns-map with {:import #{class-syms}}"
  [ns-map]
  (let [imports (:import ns-map)]
    (when (seq imports)
      (cons :import (sort-by str (group-by-package imports))))))

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
  "Returns a :require form from an ns-map with:
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
    (when (some '#{clojure.core} (mapcat keys [refer exclude rename]))
      (let [refs (let [r (get refer c)]
                   (if (and r (not= r :all))
                     [:only (vec (sort r))]
                     []))
            refs (cond-> refs
                   (get exclude c) (conj :exclude (vec (sort (exclude c))))
                   (get rename c) (conj :rename (into (sorted-map) (rename c))))]
        (when (seq refs)
          (cons :refer-clojure refs))))))

(defn ns-from-map
  "Generate an ns-form from an ns-map in the form of
   #'slam.hound.asplode/default-namespace-references"
  [ns-map]
  `(~'ns ~(:name ns-map)
     ~@(if-let [doco (:doc (:meta ns-map))] ; avoid inserting nil
         [doco])
     ~@(filter identity [(requires-from-map ns-map)
                         (imports-from-map ns-map)
                         (refer-clojure-from-map ns-map)
                         (keyword-list-from-map :gen-class ns-map)
                         (keyword-list-from-map :load ns-map)])))

(defn stitch-up [ns-map]
  (-> ns-map
      ns-from-map
      prettify))
