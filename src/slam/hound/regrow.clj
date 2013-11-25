(ns slam.hound.regrow
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.walk :refer [prewalk]]
            [slam.hound.future :refer [cond->*]]
            [slam.hound.search :as search]
            [slam.hound.stitch :as stitch])
  (:import (java.util.regex Pattern)))

(def ^:dynamic *debug* false)

;; sometimes we can't rely on stdout (testing slamhound.el)
(def debug-log (atom []))

(defn- debug [& msg]
  (when *debug*
    (swap! debug-log conj msg)
    (apply prn msg)))

(defn- capitalized? [x]
  (Character/isUpperCase ^Character (first (name x))))

(def ^:private missing-sym-patterns
  (let [sym-pat #"([-\+_\w\$\?!\*\>\<]+'*|/)"
        prefixes [#"Unable to resolve \w+: "
                  "Can't resolve: "
                  "No such namespace: "
                  #"No such var: \S+/"]]
    (mapv #(Pattern/compile (str % sym-pat)) prefixes)))

(defn- missing-sym-name [msg]
  (second (some #(re-find % msg) missing-sym-patterns)))

(defn- failure-details [msg old-ns-map]
  (when-let [sym-name (missing-sym-name msg)]
    (let [sym (symbol sym-name)]
      {:missing sym ; now returns a symbol
       :possible-types (cond (capitalized? sym-name)
                             (if (first (filter (partial some #{sym})
                                                (vals (:refer old-ns-map))))
                               [:refer :import]
                               [:import :refer])
                             (re-find #"Unable to resolve var: \w+/" msg)
                             [:alias :refer]
                             (re-find #"No such (var|namespace)" msg)
                             [:alias]
                             :else
                             [:refer :import])})))

(defn- check-for-failure [ns-map body]
  (let [sandbox-ns `slamhound.sandbox#
        ns-form (stitch/ns-from-map (assoc ns-map :name sandbox-ns))]
    (binding [*ns* (create-ns sandbox-ns)]
      (try
        (eval `(do ~ns-form ~@body nil))
        (catch Exception e
          (or (failure-details (.getMessage e) (:old ns-map))
              (do (debug :not-found ns-form)
                  (throw e))))
        (finally
          (remove-ns (.name *ns*)))))))

(defn- symbols-in-body [body]
  (filter symbol? (remove coll? (rest (tree-seq coll? seq body)))))

(def ^:private ns-qualifed-syms
  (memoize
    (fn [body]
      (apply merge-with set/union {}
             (for [ss (symbols-in-body body)
                   :let [[_ alias var-name] (re-matches #"(.+)/(.+)" (str ss))]
                   :when alias]
               {(symbol alias) #{(symbol var-name)}})))))

(defn- ns-import-candidates
  "Search (all-ns) for imports that match missing-sym, returning a set of
  class symbols. This is slower than scanning through the list of static
  package names, but will successfully find dynamically created classes such
  as those created by deftype and defrecord."
  [missing-sym]
  (reduce (fn [s nspace]
            (if-let [cls ^Class ((ns-imports nspace) missing-sym)]
              (conj s (symbol (.getCanonicalName cls)))
              s))
          #{} (all-ns)))

(defn candidates
  "Return a set of class or ns symbols that match the given constraints."
  [type missing body]
  (case type
    :import (let [m (str missing)
                  ss (for [class-name search/available-classes
                           :when (= m (last (.split class-name "\\.")))]
                       (symbol class-name))]
              (into (ns-import-candidates missing) ss))
    :alias (set
             (for [ns (all-ns)
                   :let [syms-with-alias (get (ns-qualifed-syms body) missing)]
                   :when (and (seq syms-with-alias)
                              (set/subset? syms-with-alias
                                           (set (keys (ns-publics ns)))))]
               (ns-name ns)))
    :refer (set
             (for [ns (all-ns)
                   [sym _] (ns-publics ns)
                   :when (= missing sym)]
               (ns-name ns)))))

(defn- filter-excludes
  "Disjoin namespace symbols from candidates that match the :exclude, :xrefer,
  and :refer values in old-ns-map."
  [type missing old-ns-map candidates]
  (if (= type :refer)
    (let [{:keys [exclude xrefer refer]} old-ns-map
          cs (reduce (fn [s [ns syms]]
                       (if (and (contains? s ns)
                                (contains? syms missing))
                         (disj s ns)
                         s))
                     candidates exclude)
          cs (reduce (fn [s ns]
                       (if (and (contains? s ns)
                                (not (contains? (refer ns) missing)))
                         (disj s ns)
                         s))
                     cs xrefer)]
      cs)
    candidates))

(def ^:private disambiguator-blacklist
  (if-let [v (resolve 'user/slamhound-disambiguator-blacklist)]
    @v
    #"swank|lancet"))

(defn- in-originals-fn [type missing old-ns-map]
  (fn [candidate]
    (case type
      :import (if (contains? (:import old-ns-map) candidate)
                0
                1)
      :alias (let [as (:alias old-ns-map)]
               (if (and (contains? as candidate)
                        (= (as candidate) missing))
                 0
                 1))
      :refer (let [[all rs] ((juxt :refer-all :refer) old-ns-map)
                   all? (contains? all candidate)
                   ref? (and (contains? rs candidate)
                             (contains? (rs candidate) missing))]
               (cond (and all? ref?) 0
                     ref? 1
                     all? 2
                     :else 3)))))

(defn- last-segment-matches-fn [type missing]
  (let [alias (name missing)]
    (fn [candidate]
      (if (and (= type :alias)
               (= alias (peek (string/split (name candidate) #"\."))))
        0
        1))))

(defn- is-project-namespace-fn [type]
  (fn [candidate]
    (if (and (contains? #{:alias :refer} type)
             (contains? (search/namespaces-from-files) candidate))
      0
      1)))

(defn disambiguate
  "Select the most likely class or ns symbol in the given set of candidates,
  returning [type candidate-sym]"
  [candidates type missing ns-maps]
  ;; TODO: prefer things in classes to jars
  (debug :disambiguating missing :in candidates)
  (let [{:keys [old-ns-map new-ns-map]} ns-maps
        cs (cond->* candidates
             ;; Current ns is never a valid reference source
             true (disj (:name old-ns-map))
             ;; Prevent multiple aliases to a single namespace (ugh)
             (= type :alias) (set/difference (set (keys (:alias new-ns-map)))))
        cs (->> cs
                (filter-excludes type missing old-ns-map)
                (remove #(re-find disambiguator-blacklist (str %)))
                (sort-by (juxt (in-originals-fn type missing old-ns-map)
                               (last-segment-matches-fn type missing)
                               (is-project-namespace-fn type)
                               (comp count str))))]
    (when-let [c (first cs)]
      ;; Honor any old [c :refer :all] specs - issue #50
      (if (and (= type :refer)
               (contains? (:refer-all old-ns-map) c))
        [:refer-all c]
        [type c]))))

(defn grow-ns-map
  "Return a new ns-map augmented with a single candidate ns reference."
  [ns-map type missing body]
  (let [cs (candidates type missing body)
        old-ns-map (:old ns-map)]
    (if-let [[type c] (disambiguate cs type missing {:old-ns-map old-ns-map
                                                     :new-ns-map ns-map})]
      (case type
        :import (update-in ns-map [:import] #(conj (or % #{}) c))
        :alias (update-in ns-map [:alias] assoc c missing)
        :refer (update-in ns-map [:refer c] #(conj (or % #{}) missing))
        :refer-all (update-in ns-map [:refer-all] #(conj (or % #{}) c)))
      ns-map)))

(defonce pre-load-namespaces
  (delay
    (doseq [namespace (search/namespaces)
            :when (not (re-find #"example|lancet$" (name namespace)))]
      (try (with-out-str (require namespace))
           (catch Throwable _)))))

(defn- strip-ns-qualified-symbols
  "De-qualify symbols in body that are qualified with ns-sym."
  [ns-sym body]
  (let [pat (Pattern/compile (str "\\A\\Q" ns-sym "\\E/(.+)"))
        walk-fn (fn [expr]
                  (if (symbol? expr)
                    (if-let [m (re-find pat (str expr))]
                      (symbol (second m))
                      expr)
                    expr))]
    (prewalk walk-fn body)))

(defn regrow [[ns-map body]]
  (force pre-load-namespaces)
  ;; Since body was likely acquired through the reader, we must de-qualify
  ;; symbols that may have been erroneously qualified to the current ns within
  ;; syntax-quoted forms.
  (if (:slamhound-skip (:meta ns-map))
    (merge ns-map (:old ns-map))
    (let [body (strip-ns-qualified-symbols (:name ns-map) body)]
      (loop [ns-map ns-map
             last-missing nil
             type-to-try 0]
        (if-let [{:keys [missing possible-types]} (check-for-failure ns-map body)]
          (let [type-idx (if (= last-missing missing)
                           (inc type-to-try)
                           0)]
            (if-let [type (get possible-types type-idx)]
              (recur (grow-ns-map ns-map type missing body) missing type-idx)
              (throw (Exception. (str "Couldn't resolve " missing
                                      ", got as far as " ns-map)))))
          ns-map)))))
