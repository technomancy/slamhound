(ns slam.hound
  (:use [clojure.java.io :only [reader]]
        [clojure.pprint :only [pprint]]
        ;; TODO: extract
        [swank.util.class-browse :only [available-classes]]))

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
  (println :got msg)
  ;; TODO: this doesn't catch require/as or static methods yet
  (when-let [[match] (re-seq #"Unable to resolve \w+: (\w+)" msg)]
    (second match)))

(defn check-for-failure [ns-map body]
  (pprint ns-map)
  (try (binding [*ns* (the-ns 'user)]
         (eval (ns-from-map ns-map))
         (doseq [form body] (eval form))
         nil)
       (catch Exception e
         (missing-var (.getMessage e)))))

(defn class-name? [x]
  (Character/isUpperCase (first (name x))))

(defn qualified-class [class-name]
  (some (fn [{full-name :name}]
          (if (= (last (.split full-name "\\.")) class-name)
            (symbol full-name)))
        available-classes))

(defn add-import [failure ns-map]
  (let [class-name (qualified-class failure)]
    ;; TODO: use style (:import (java.util UUID))
    (update-in ns-map [:import] conj class-name)))

(defn add-require [failure ns-map]
  (println :require failure)
  ns-map)

(defn add-use [failure ns-map]
  (println :use failure)
  ns-map)

(defn resolve-failure [failure ns-map]
  (cond (class-name? failure) (add-import failure ns-map)
        (namespace failure) (add-require failure ns-map)
        :else (add-use failure ns-map)))

(defn reconstruct-ns-form [ns-map body last-failure]
  (if-let [failure (check-for-failure ns-map body)]
    (if (= failure last-failure)
      (throw (Exception. (str "Couldn't resolve " failure)))
      (recur (resolve-failure failure ns-map) body failure))
    ns-map))

(defn reconstruct [filename]
  (let [rdr (java.io.PushbackReader. (reader filename))
        ns-map (ns-to-map (read rdr))
        stripped-ns (apply dissoc ns-map (rest ns-clauses))
        body (take-while #(not= ::done %)
                         (repeatedly #(read rdr false ::done)))]
    (pprint (ns-from-map (reconstruct-ns-form stripped-ns body nil)))))
