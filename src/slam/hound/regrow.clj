(ns slam.hound.regrow
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [slam.hound.stitch :as stitch]
            [slam.hound.search :as search]))

(def ^:dynamic *debug* false)

;; sometimes we can't rely on stdout (testing slamhound.el)
(def debug-log (atom []))

(defn- debug [& msg]
  (when *debug*
    (swap! debug-log conj msg)
    (apply prn msg)))

(defn- class-name? [x]
  (Character/isUpperCase (first (name x))))

(defn- missing-sym-name [msg]
  (second (or (re-find #"Unable to resolve \w+: ([-\+_\w\$\?!\*\>\<]+)" msg)
              (re-find #"Can't resolve: ([-_\w\$\?!\*\>\<]+)" msg)
              (re-find #"No such namespace: ([-_\w\$\?!\*\>\<]+)" msg)
              (re-find #"No such var: \w+/([-_\w\$\?!\*\>\<]+)" msg))))

(defn- failure-details [msg]
  (if-let [sym (missing-sym-name msg)]
    {:missing sym
     :possible-types (cond (class-name? sym)
                           [:import :require-refer]
                           (re-find #"Unable to resolve var: \w+/" msg)
                           [:require-as :require-refer]
                           (re-find #"No such (var|namespace)" msg)
                           [:require-as]
                           :else
                           [:require-refer :import])}))

(defn- check-for-failure [ns-map body]
  (let [sandbox-ns `slamhound.sandbox#
        ns-form (stitch/ns-from-map (assoc ns-map :name sandbox-ns))]
    (binding [*ns* (create-ns sandbox-ns)]
      (try
        (eval `(do ~ns-form ~@body nil))
        (catch Exception e
          (or (failure-details (.getMessage e))
              (do (debug :not-found ns-form)
                  (throw e))))
        (finally
         (remove-ns (.name *ns*)))))))

(defn- symbols-in-body [body]
  (filter symbol? (remove coll? (rest (tree-seq coll? seq body)))))

(def ^:private ns-qualifed-syms
  (memoize (fn [body]
             (apply merge-with set/union {}
                   (for [value (symbols-in-body body)
                         :let [[_ alias var-name] (re-matches #"(.+)/(.+)"
                                                              (str value))]
                         :when alias]
                     {alias #{(symbol var-name)}})))))

(defn candidates [type missing body]
  (case type
    :import (for [class-name search/available-classes
                  :when (= missing (last (.split class-name "\\.")))]
              (symbol class-name))
    :require-as (for [n (all-ns)
                      :let [syms-with-alias (get (ns-qualifed-syms body)
                                                 missing)]
                      :when (and (seq syms-with-alias)
                                 (every? (set (keys (ns-publics n)))
                                         syms-with-alias))]
                  [(ns-name n) :as (symbol missing)])
    :require-refer (for [n (all-ns)
                         [sym var] (ns-publics n)
                         :when (= missing (name sym))]
                     [(ns-name n) :refer [sym]])))

(defn- butlast-regex [candidate]
  (if (symbol? candidate)
    (re-pattern (string/join "." (butlast (.split (name candidate) "\\."))))
    (re-pattern (name (first candidate)))))

(defn in-originals-pred [originals]
  (fn [candidate]
    (some #(re-find (butlast-regex candidate) (str %)) originals)))

(def ^:private disambiguator-blacklist
  (if-let [v (resolve 'user/slamhound-disambiguator-blacklist)]
    @v
    #"swank|lancet"))

(defn- new-type-to-old-types [new-type]
  (case new-type
    :require-as [:require]
    :require-refer [:require :use] ; could've been require/refer or use/only
    [new-type]))

(defn- referred-to-in-originals-pred [type originals]
  (if-not (= type :require-refer)
    (constantly false)
    (fn [[ns1 _ [alias1]]]
      (some (fn [[[ns2 _ [alias2] :as arg]]]
              (and (= alias1 alias2) (= ns1 ns2)))
            originals))))

(defn- disambiguate [candidates missing ns-map type]
  ;; TODO: prefer things in src/classes to jars
  (debug :disambiguating missing :in candidates)
  (let [orig-clauses (map #(get (:old ns-map) %)
                          (new-type-to-old-types type))]
    (->> candidates
         (sort-by (juxt (complement (in-originals-pred orig-clauses))
                        (complement (referred-to-in-originals-pred
                                     type orig-clauses))
                        ;; TODO: prefer candidates where last segment matches
                        (comp count str)))
         (remove #(re-find disambiguator-blacklist (str %)))
         first)))

(defn- grow-step [missing type ns-map body]
  (if-let [addition (disambiguate (candidates type missing body)
                                  missing ns-map type)]
    (update-in ns-map [type] conj addition)
    ns-map))

(defonce pre-load-namespaces
  (delay
   (doseq [namespace (search/namespaces)
           :when (not (re-find #"example|lancet$" (name namespace)))]
     (try (with-out-str (require namespace))
          (catch Throwable _)))))

(defn regrow [[ns-map body]]
  (force pre-load-namespaces)
  (if (:slamhound-skip (:meta ns-map))
    ns-map
    (loop [ns-map ns-map
           last-missing nil
           type-to-try 0]
      (if-let [{:keys [missing possible-types]} (check-for-failure ns-map body)]
        (let [type-idx (if (= last-missing missing)
                         (inc type-to-try)
                         0)]
          (if-let [type (get possible-types type-idx)]
            (recur (grow-step missing type ns-map body) missing type-idx)
            (throw (Exception. (str "Couldn't resolve " missing
                                    ", got as far as " ns-map)))))
        ns-map))))

