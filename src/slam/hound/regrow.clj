(ns slam.hound.regrow
  (:use [clojure.pprint :only [pprint]]
        ;; TODO: stop using swank
        [swank.util.class-browse :only [available-classes]])
  (:require [slam.hound.stitch :as stitch]))

(def ^{:dynamic true} *debug* false)

(defn debug [& msg]
  (when *debug*
    (apply prn msg)))

(defn missing-var [msg]
  (if-let [[match] (re-seq #"Unable to resolve \w+: (\w+)" msg)]
    (second match)
    (second (first (re-seq #"No such namespace: (\w+)" msg)))))

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
           (missing-var (.getMessage e))))))

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

(defn regrow [[ns-map body last-failure]]
  (if-let [failure (check-for-failure ns-map body)]
    (if (= failure last-failure)
      (throw (Exception. (str "Couldn't resolve " failure)))
      (recur [(resolve-failure failure ns-map) body failure]))
    ns-map))
