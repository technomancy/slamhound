(ns slam.hound.regrow
  (:use [clojure.pprint :only [pprint]]
        ;; TODO: stop using swank
        [swank.util.class-browse :only [available-classes]])
  (:require [slam.hound.stitch :as stitch]))

(def ^{:dynamic true} *debug* false)

(defn debug [& msg]
  (when *debug* (apply prn msg)))

(defn class-name? [x]
  (Character/isUpperCase (first (name x))))

(defn missing-sym-name [msg]
  (if-let [[match] (re-seq #"Unable to resolve \w+: (\w+)" msg)]
    (second match)
    (second (first (re-seq #"No such namespace: (\w+)" msg)))))

(defn failure-details [msg]
  (let [sym (missing-sym-name msg)]
    {:missing-sym sym
     :type (cond (class-name? sym) :import
                 (re-find #"namespace" msg) :require
                 :else :use)}))

(defn check-for-failure [ns-map body]
  (let [ns-form (stitch/ns-from-map ns-map)]
    ;; (debug :checking ns-form)
    (try (binding [*ns* (create-ns `foo#)]
           (refer 'clojure.core)
           (eval ns-form)
           (doseq [form body]
             (eval form))
           (remove-ns (.name *ns*))
           nil)
         (catch Exception e
           (debug :ex (.getMessage e))
           (failure-details (.getMessage e))))))

(defmulti candidates (fn [type missing-sym] type))

(defmethod candidates :import [type missing-sym]
  (for [{full-name :name} available-classes
        :when (= (last (.split full-name "\\.")) missing-sym)]
    (symbol full-name)))

(defmethod candidates :require [type missing-sym]
  (for [n (all-ns)
        :let [segments (.split (name (ns-name n)) "\\.")]
        :when (= missing-sym (last segments))]
    [(ns-name n) :as (symbol missing-sym)]))

(defmethod candidates :use [type missing-sym]
  (for [n (all-ns)
        [sym var] (ns-publics n)
        :when (= missing-sym (name sym))]
    [(ns-name n) :only [sym]]))

(defn grow [missing-sym type ns-map disambiguate]
  (update-in ns-map [type] conj (disambiguate (candidates type missing-sym) )))

(defn regrow
  ([[ns-map body]]
     (regrow [ns-map body] first nil))
  ([[ns-map body] disambiguate last-missing-sym]
     (if-let [{:keys [missing-sym type]} (check-for-failure ns-map body)]
       (if (= last-missing-sym missing-sym)
         (throw (Exception. (str "Couldn't resolve " missing-sym)))
         (recur [(grow missing-sym type ns-map disambiguate) body]
                disambiguate missing-sym))
       ns-map)))
