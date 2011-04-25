(ns slam.hound.stitch)

(def ns-clauses [:refer-clojure :use :require :import])

(defn ns-from-map [ns-map]
  ;; TODO: arbitrary ns-level metadata?
  `(~'ns ~(:name ns-map)
     ~(:doc (:meta ns-map))
     ~@(for [clause-type ns-clauses
             :when (clause-type ns-map)]
         (cons clause-type (clause-type ns-map)))))

(defn stitch-up [ns-map]
  ;; TODO: sort each clause
  ;; TODO: collapse subclauses
  ;; TODO: indent
  ;; TODO: wrap long lines
  (ns-from-map ns-map))
