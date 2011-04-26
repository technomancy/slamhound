(ns slam.hound.regrow
  (:require [slam.hound.stitch :as stitch]
            [slam.hound.search :as search]))

(def ^{:dynamic true} *debug* (System/getenv "DEBUG"))

(defn debug [& msg]
  (when *debug* (apply prn msg)))

(defn class-name? [x]
  (Character/isUpperCase (first (name x))))

(defn missing-sym-name [msg]
  (second (or (re-find #"Unable to resolve \w+: ([-_\w\$\?!\*]+)" msg)
              (re-find #"No such namespace: ([-_\w\$\?!\*]+)" msg)
              (re-find #"No such var: \w+/([-_\w\$\?!\*]+)" msg))))

(defn failure-details [msg]
  (when-let [sym (missing-sym-name msg)]
    {:missing-sym sym
     :type (cond (class-name? sym) :import
                 (re-find #"No such (var|namespace)" msg) :require
                 :else :use)}))

(defn check-for-failure [ns-map body]
  (binding [*ns* (create-ns `slamhound.sandbox#)]
    (let [ns-form (slam.hound.stitch/ns-from-map
                   (assoc ns-map :name (.name *ns*)))]
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

(defmulti candidates (fn [type missing-sym] type))

(defmethod candidates :import [type missing-sym]
  (for [{full-name :name} search/available-classes
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

(defn default-disambiguator
  "Pick the shortest matching candidate by default."
  [candidates & [missing-sym]]
  ;; TODO: prefer things in src/classes to jars
  (debug :disambiguating missing-sym :in candidates)
  (or (->> candidates
           (sort-by (comp count str))
           (remove #(re-find #"swank|lancet" (str %)))
           first)
      (throw (Exception. (str "Couldn't resolve "
                              (or missing-sym "candidates"))))))

(defn grow-step [missing-sym type ns-map disambiguate]
  (update-in ns-map [type] conj (disambiguate (candidates type missing-sym)
                                              missing-sym)))

(defn regrow
  ([[ns-map body]]
     (doseq [namespace (search/namespaces)
             :when (not (re-find #"example|lancet$" (name namespace)))]
       (try (with-out-str (require namespace)) (catch Exception _)))
     (regrow [ns-map body] default-disambiguator nil))
  ([[ns-map body] disambiguate last-missing-sym]
     (if-let [{:keys [missing-sym type]} (check-for-failure ns-map body)]
       (if (= last-missing-sym missing-sym)
         (throw (Exception. (str "Couldn't resolve " missing-sym)))
         (recur [(grow-step missing-sym type ns-map disambiguate) body]
                disambiguate missing-sym))
       ns-map)))
