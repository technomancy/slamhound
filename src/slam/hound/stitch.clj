(ns slam.hound.stitch
  (:use [clojure.pprint :only [code-dispatch pprint with-pprint-dispatch]])
  (:require [clojure.string :as string]))

(def ns-clauses [:use :require :import])

(def ns-clauses-to-preserve [:refer-clojure :gen-class :load])

(defn- get-package [class-name]
  (-> class-name resolve .getPackage .getName))

(defn- group-by-package [imports]
  (for [[package classes] (group-by get-package imports)]
    (cons (symbol package)
          (sort (for [c classes]
                  (-> c resolve .getName (.split "\\.") last symbol))))))

(defn segments [ns-sym]
  (string/split (name ns-sym) #"\."))

(defn group-by-parent [uses]
  (->> uses
       (map (juxt identity (comp segments first)))
       (group-by (comp butlast second))))

(defmulti write-grouped (fn [[parent children]]
                          (if parent
                            (count children)
                            :top-level)))

(defmethod write-grouped :top-level
  [[_ namespaces]]
  (map first namespaces))

(defmethod write-grouped 1 ; don't "group" a single namespace
  [[_ decl]]
  (map first decl))

(defmethod write-grouped :default
  [[parent children]]
  (list
   (cons (symbol (string/join "." parent))
         (for [[[ns & more]] children]
           (vec (cons (symbol (last (segments ns)))
                      more))))))

(defn- group-by-namespace [uses]
  (for [[namespace subclause] (group-by first uses)]
    [namespace :only (vec (sort (for [[_ _ [var]] subclause]
                                  var)))]))

(defmulti collapse-clause (comp second list))

(defmethod collapse-clause :import [ns-map clause]
  (update-in ns-map [:import] group-by-package))

(defmethod collapse-clause :require [ns-map clause]
  ns-map)

(defmethod collapse-clause :use [ns-map clause]
  (update-in ns-map [:use]
             (comp (partial mapcat write-grouped)
                   group-by-parent
                   group-by-namespace)))

(defn collapse-clauses [ns-map]
  (reduce collapse-clause ns-map ns-clauses))

(defn sort-subclauses [ns-map]
  ;; lists aren't comparable? huh?
  (reduce #(update-in %1 [%2] (partial sort-by str))
          ns-map ns-clauses))

(defn ns-from-map [ns-map]
  `(~'ns ~(:name ns-map)
     ~@(if-let [doco (:doc (:meta ns-map))] ; avoid inserting nil
         [doco])
     ~@(for [clause-type (concat ns-clauses ns-clauses-to-preserve)
             :when (seq (clause-type ns-map))]
         (cons clause-type (clause-type ns-map)))))

;; TODO: indentation here is all wrong, but pprint gets line length right.
(defn prettify [ns-form]
  (.replace (with-out-str
              (with-pprint-dispatch code-dispatch
                (pprint ns-form)))
            "(ns\n " "(ns"))

(defn stitch-up [ns-map]
  (-> ns-map
      collapse-clauses
      sort-subclauses
      ns-from-map
      prettify))
