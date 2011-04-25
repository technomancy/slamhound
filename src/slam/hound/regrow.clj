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

(defn import-subclause [class-name]
  (some (fn [{full-name :name}]
          (if (= (last (.split full-name "\\.")) class-name)
            (symbol full-name)))
        available-classes))

(defn require-subclause [failure]
  ;; TODO: allow custom disambiguators
  (first (for [n (all-ns)
               :let [segments (.split (name (ns-name n)) "\\.")]
               :when (= failure (last segments))]
           [(ns-name n) :as (symbol failure)])))

(defn use-subclause [failure]
  (first (for [n (all-ns)
               [sym var] (ns-publics n)
               :when (= failure (name sym))]
           [(ns-name n) :only [sym]])))

(defn grow-import [failure ns-map]
  (let [class-name (import-subclause failure)]
    (debug :grow-import class-name)
    (update-in ns-map [:import] conj class-name)))

(defn grow-require [failure ns-map]
  (let [required-ns (require-subclause failure)]
    (debug :grow-require required-ns)
    (update-in ns-map [:require] conj required-ns)))

(defn grow-use [failure ns-map]
  (let [used-ns (use-subclause failure)]
    (debug :grow-use used-ns)
    (update-in ns-map [:use] conj used-ns)))

(defn class-name? [x]
  (Character/isUpperCase (first (name x))))

(defn resolve-failure [failure ns-map]
  (cond (class-name? failure) (grow-import failure ns-map)
        ;; TODO: need a better way to distinguish between require/use
        :else #_(namespace (symbol failure)) (grow-require failure ns-map)
        :else (grow-use failure ns-map)))

(defn regrow [[ns-map body last-failure]]
  (if-let [failure (check-for-failure ns-map body)]
    (if (= failure last-failure)
      (throw (Exception. (str "Couldn't resolve " failure)))
      (recur [(resolve-failure failure ns-map) body failure]))
    ns-map))
