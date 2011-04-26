(ns slam.hound.regrow
  ;; TODO: stop using swank
  (:use [swank.util.class-browse :only [available-classes]])
  (:require [slam.hound.stitch :as stitch]))

(def ^{:dynamic true} *debug* false)

(defn debug [& msg]
  (when *debug* (apply prn msg)))

(defn class-name? [x]
  (Character/isUpperCase (first (name x))))

(defn missing-sym-name [msg]
  (second (or (re-find #"Unable to resolve \w+: ([-\w]+)" msg)
              (re-find #"No such namespace: ([-\w]+)" msg)
              (re-find #"No such var: \w+/([-\w]+)" msg))))

(defn failure-details [msg]
  (when-let [sym (missing-sym-name msg)]
    {:missing-sym sym
     :type (cond (class-name? sym) :import
                 (re-find #"(namespace|No such var)" msg) :require
                 :else :use)}))

(defn check-for-failure [ns-map body]
  (let [ns-form (stitch/ns-from-map ns-map)]
    (binding [*ns* (create-ns `foo#)]
      (try
        (refer 'clojure.core)
        (eval ns-form)
        (doseq [form body]
          (eval form))
        nil
        (catch Exception e
          (debug :ex (.getMessage e))
          (or (failure-details (.getMessage e))
              (throw e)))
        (finally
         (remove-ns (.name *ns*)))))))

(defmulti candidates (fn [type missing-sym] type))

(defmethod candidates :import [type missing-sym]
  (for [{full-name :name} available-classes
        :when (= missing-sym (last (.split full-name "\\.")))]
    (symbol full-name)))

(defmethod candidates :require [type missing-sym]
  (for [n (all-ns)
        :when (= missing-sym (last (.split (name (ns-name n)) "\\.")))]
    [(ns-name n) :as (symbol missing-sym)]))

(defmethod candidates :use [type missing-sym]
  (for [n (all-ns)
        [sym var] (ns-publics n)
        :when (= missing-sym (name sym))]
    [(ns-name n) :only [sym]]))

(defn grow [missing-sym type ns-map disambiguate]
  (update-in ns-map [type] conj (disambiguate (candidates type missing-sym))))

(defn regrow
  ([[ns-map body]]
     ;; TODO: better way to use custom disambiguator
     (regrow [ns-map body] first nil))
  ([[ns-map body] disambiguate last-missing-sym]
     (if-let [{:keys [missing-sym type]} (check-for-failure ns-map body)]
       (if (= last-missing-sym missing-sym)
         (throw (Exception. (str "Couldn't resolve " missing-sym)))
         (recur [(grow missing-sym type ns-map disambiguate) body]
                disambiguate missing-sym))
       ns-map)))
