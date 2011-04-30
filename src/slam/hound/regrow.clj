(ns slam.hound.regrow
  (:require [clojure.string :as string]
            [slam.hound.stitch :as stitch]
            [slam.hound.search :as search]))

(def *debug* false)

;; sometimes we can't rely on stdout (testing slamhound.el)
(def debug-log (atom []))

(defn debug [& msg]
  (when *debug*
    (swap! debug-log conj msg)
    (apply prn msg)))

(defn class-name? [x]
  (Character/isUpperCase (first (name x))))

(defn missing-sym-name [msg]
  (second (or (re-find #"Unable to resolve \w+: ([-_\w\$\?!\*]+)" msg)
              (re-find #"Can't resolve: ([-_\w\$\?!\*]+)" msg)
              (re-find #"No such namespace: ([-_\w\$\?!\*]+)" msg)
              (re-find #"No such var: \w+/([-_\w\$\?!\*]+)" msg))))

(defn failure-details [msg]
  (when-let [sym (missing-sym-name msg)]
    {:missing sym
     :type (cond (class-name? sym) :import
                 (re-find #"No such (var|namespace)" msg) :require
                 :else :use)}))

(defn check-for-failure [ns-map body]
  (let [sandbox-ns `slamhound.sandbox#
        ns-form (stitch/ns-from-map (assoc ns-map :name sandbox-ns))]
    (binding [*ns* (create-ns sandbox-ns)]
      (try
        (refer 'clojure.core)
        (eval `(do ~ns-form ~@body))
        nil
        (catch Exception e
          (or (failure-details (.getMessage e))
              (do (debug :not-found ns-form)
                  (throw e))))
        (finally
         (remove-ns (.name *ns*)))))))

(defmulti candidates (fn [type missing] type))

(defmethod candidates :import [type missing]
  (for [{full-name :name} search/available-classes
        :when (= missing (last (.split full-name "\\.")))]
    (symbol full-name)))

(defmethod candidates :require [type missing]
  (for [n (all-ns)
        :when (= missing (last (.split (name (ns-name n)) "\\.")))]
    [(ns-name n) :as (symbol missing)]))

(defmethod candidates :use [type missing]
  (for [n (all-ns)
        [sym var] (ns-publics n)
        :when (= missing (name sym))]
    [(ns-name n) :only [sym]]))

(defn butlast-regex [candidate]
  (if (symbol? candidate)
    (re-pattern (string/join "." (butlast (.split (name candidate) "\\."))))
    (re-pattern (name (first candidate)))))

(defn in-original-pred [type old]
  (let [[old-clause] (filter #(and (seq? %) (= type (first %))) old)]
    (fn [candidate]
      (re-find (butlast-regex candidate) (str old-clause)))))

(def disambiguator-blacklist
  (if-let [v (resolve 'user/slamhound-disambiguator-blacklist)]
    @v
    #"swank|lancet"))

(defn default-disambiguator
  "Pick the shortest matching candidate by default."
  [candidates missing ns-map type]
  ;; TODO: prefer things in src/classes to jars
  (debug :disambiguating missing :in candidates)
  (or (->> candidates
           (sort-by (juxt (complement (in-original-pred type (:old ns-map)))
                          (comp count str)))
           (remove #(re-find disambiguator-blacklist (str %)))
           first)
      (throw (Exception. (str "Couldn't resolve "
                              (or missing "candidates"))))))

(defn grow-step [missing type ns-map disambiguate]
  (update-in ns-map [type] conj
             (disambiguate (candidates type missing) missing ns-map type)))

(defn regrow
  ([[ns-map body]]
     (doseq [namespace (search/namespaces)
             :when (not (re-find #"example|lancet$" (name namespace)))]
       (try (with-out-str (require namespace))
            (catch Exception _)
            (catch Error _)))
     (regrow [ns-map body] default-disambiguator nil))
  ([[ns-map body] disambiguate last-missing]
     (if-let [{:keys [missing type]} (check-for-failure ns-map body)]
       (if (= last-missing missing)
         (throw (Exception. (str "Couldn't resolve " missing)))
         (recur [(grow-step missing type ns-map disambiguate) body]
                disambiguate missing))
       ns-map)))
