(ns slam.hound
  (:use [clojure.java.io :only [reader]])
  (:import (java.io PushbackReader)))

(def ns-clauses [:refer-clojure :use :require :import])

(defn ns-to-map [ns-form]
  (let [[_ ns-name doco & clauses] ns-form
        ns-meta (meta ns-name)
        [ns-meta clauses] (if (string? doco)
                            [(assoc ns-meta :doc doco) clauses]
                            [ns-meta (cons doco clauses)])]
    (into {:meta ns-meta :name ns-name}
          (map (juxt first rest) clauses))))

(defn ns-from-map [ns-map]
  `(~'ns ~(:name ns-map)
     ~(:doc (:meta ns-map))
     ~@(for [clause-type ns-clauses
             :when (clause-type ns-map)]
         (cons clause-type (clause-type ns-map)))))

(defn missing-var [e]
  (->> (.getMessage e)
       (re-seq #"Unable to resolve symbol: (\w+) in this context")
       (second)
       (symbol)))

(defn check-for-failure [ns-map body]
  (try (eval `(do ~(ns-from-map ns-map) ~@body))
       nil
       (catch Exception e
         (missing-var e))))

(defn resolve-failure [failure ns-map]
  ;; tricky bits
  )

(defn reconstruct-ns-form [ns-map body]
  (if-let [failure (check-for-failure ns-map body)]
    (recur (resolve-failure failure ns-map) body)))

(defn write-new-ns [filename ns-map]
  ;; very tricky bits
  )

(defn reconstruct
  ([filename]
     (let [rdr (PushbackReader. (reader filename))
           ns-map (ns-to-map (read rdr))
           stripped-ns (apply dissoc ns-map (rest ns-clauses))
           body (take-while #(not= ::done %)
                            (repeatedly #(read rdr false ::done)))]
       (write-new-ns filename (reconstruct-ns-form stripped-ns body)))))
