(ns slam.hound.stitch
  (:use [clojure.pprint :only [pprint code-dispatch with-pprint-dispatch]]))

(def ns-clauses [:use :require :import])

(defn ns-from-map [ns-map]
  ;; TODO: arbitrary ns-level metadata?
  `(~'ns ~(:name ns-map)
     ~(:doc (:meta ns-map))
     ~@(for [clause-type (cons :refer-clojure ns-clauses)
             :when (clause-type ns-map)]
         (cons clause-type (clause-type ns-map)))))

(defn collapse-clause [ns-map]
  ns-map)

(defn sort-subclauses [ns-map]
  (reduce #(update-in %1 [%2] sort)
          ns-map ns-clauses))

;; TODO: indentation here is all wrong, but pprint gets line length right.
(defn prettify [ns-form]
  (with-out-str
    (with-pprint-dispatch code-dispatch
      (pprint ns-form))))

(defn stitch-up [ns-map]
  (-> ns-map
      ns-from-map
      collapse-clauses
      sort-subclauses
      prettify
      (.replace "(ns \n +" "(ns ")))
