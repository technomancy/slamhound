(ns slam.hound.regrow
  (:require [clojure.string :as string]
            [slam.hound.stitch :as stitch]
            [slam.hound.search :as search]))

(def *debug* false)

;; sometimes we can't rely on stdout (testing slamhound.el)
(def debug-log (atom []))

(defn- debug [& msg]
  (when *debug*
    (swap! debug-log conj msg)
    (apply prn msg)))

(defn- class-name? [x]
  (Character/isUpperCase (first (name x))))

(defn- missing-sym-name [msg]
  (second (or (re-find #"Unable to resolve \w+: ([-_\w\$\?!\*\>\<]+)" msg)
              (re-find #"Can't resolve: ([-_\w\$\?!\*\>\<]+)" msg)
              (re-find #"No such namespace: ([-_\w\$\?!\*\>\<]+)" msg)
              (re-find #"No such var: \w+/([-_\w\$\?!\*\>\<]+)" msg))))

(defn- failure-details [msg]
  (when-let [sym (missing-sym-name msg)]
    {:missing sym
     :types (cond (class-name? sym) [:import :use]
                  (re-find #"Unable to resolve var: \w+/" msg) [:require :use]
                  (re-find #"No such (var|namespace)" msg) [:require]
                  :else [:use :import])}))

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

(defmulti candidates (fn [type missing] type))

(defmethod candidates :import [type missing]
  (for [class-name search/available-classes
        :when (= missing (last (.split class-name "\\.")))]
    (symbol class-name)))

(defmethod candidates :require [type missing]
  (for [n (all-ns)
        :when (= missing (last (.split (name (ns-name n)) "\\.")))]
    [(ns-name n) :as (symbol missing)]))

(defmethod candidates :use [type missing]
  (for [n (all-ns)
        [sym var] (ns-publics n)
        :when (= missing (name sym))]
    [(ns-name n) :only [sym]]))

(defn- butlast-regex [candidate]
  (if (symbol? candidate)
    (re-pattern (string/join "." (butlast (.split (name candidate) "\\."))))
    (re-pattern (name (first candidate)))))

(defn in-original-pred [original]
  (fn [candidate]
    (re-find (butlast-regex candidate) (str original))))

(def ^:private disambiguator-blacklist
  (if-let [v (resolve 'user/slamhound-disambiguator-blacklist)]
    @v
    #"swank|lancet"))

(defn- disambiguate [candidates missing ns-map type]
  ;; TODO: prefer things in src/classes to jars
  (debug :disambiguating missing :in candidates)
  (->> candidates
       (sort-by (juxt (complement (in-original-pred (type (:old ns-map))))
                      (comp count str)))
       (remove #(re-find disambiguator-blacklist (str %)))
       first))

(defn- grow-step [missing type ns-map]
  (if-let [addition (disambiguate (candidates type missing)
                                  missing ns-map type)]
    (update-in ns-map [type] conj addition)
    ns-map))

(defn- pre-load-namespaces []
  (doseq [namespace (search/namespaces)
          :when (not (re-find #"example|lancet$" (name namespace)))]
    (try (with-out-str (require namespace))
      (catch Throwable _))))

(defn regrow [[ns-map body]]
  (pre-load-namespaces)
  (if (:slamhound-skip (:meta ns-map))
    ns-map
    (loop [ns-map ns-map
           last-missing nil
           type-to-try 0]
      (if-let [{:keys [missing types]} (check-for-failure ns-map body)]
        (let [type-idx (if (= last-missing missing)
                         (inc type-to-try)
                         0)]
          (if-let [type (get types type-idx)]
            (recur (grow-step missing type ns-map) missing type-idx)
            (throw (Exception. (str "Couldn't resolve " missing
                                    ", got as far as " ns-map)))))
        ns-map))))

