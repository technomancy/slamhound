(ns slam.hound
  (:use [clojure.java.io :only [reader]]))

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

(defn missing-var [msg]
  (symbol (second (re-seq #"Unable to resolve symbol: (\w+)" msg))))

(defn check-for-failure [ns-map body]
  (try (eval `(do ~(ns-from-map ns-map) ~@body))
       nil
       (catch Exception e
         (missing-var (.getMessage e)))))

(defn class-name? [x]
  (Character/isUpperCase (first (name x))))

(defn add-import [failure ns-map])
(defn add-require [failure ns-map])
(defn add-use [failure ns-map])

(defn resolve-failure [failure ns-map]
  (cond (class-name? failure) (add-import failure ns-map)
        (namespace failure) (add-require failure ns-map)
        :else (add-use failure ns-map)))

(defn reconstruct-ns-form [ns-map body]
  (if-let [failure (check-for-failure ns-map body)]
    (recur (resolve-failure failure ns-map) body)))

(defn reconstruct [filename]
  (let [rdr (java.io.PushbackReader. (reader filename))
        ns-map (ns-to-map (read rdr))
        stripped-ns (apply dissoc ns-map (rest ns-clauses))
        body (take-while #(not= ::done %)
                         (repeatedly #(read rdr false ::done)))]
    (println (ns-from-map (reconstruct-ns-form stripped-ns body)))))
