(ns slam.hound.regrow
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [slam.hound.search :as search]
            [slam.hound.stitch :as stitch]))

(def ^:dynamic *debug* false)

;; sometimes we can't rely on stdout (testing slamhound.el)
(def debug-log (atom []))

(defn- debug [& msg]
  (when *debug*
    (swap! debug-log conj msg)
    (apply prn msg)))

(defn- capitalized? [x]
  (Character/isUpperCase ^Character (first (name x))))

(defn- missing-sym-name [msg]
  (second (or (re-find #"Unable to resolve \w+: ([-\+_\w\$\?!\*\>\<]+)" msg)
              (re-find #"Can't resolve: ([-_\w\$\?!\*\>\<]+)" msg)
              (re-find #"No such namespace: ([-_\w\$\?!\*\>\<]+)" msg)
              (re-find #"No such var: \w+/([-_\w\$\?!\*\>\<]+)" msg))))

(defn- failure-details [msg]
  (when-let [sym-name (missing-sym-name msg)]
    {:missing (symbol sym-name) ; now returns a symbol
     :possible-types (cond (capitalized? sym-name)
                           [:import :refer]
                           (re-find #"Unable to resolve var: \w+/" msg)
                           [:alias :refer]
                           (re-find #"No such (var|namespace)" msg)
                           [:alias]
                           :else
                           [:refer :import])}))

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
  (memoize
    (fn [body]
      (apply merge-with set/union {}
             (for [sym (symbols-in-body body)
                   :let [[_ alias var-name] (re-matches #"(.+)/(.+)" (str sym))]
                   :when alias]
               {(symbol alias) #{(symbol var-name)}})))))

(defn- mass-refer-namespaces
  "Extract the set of namespace symbols that match [_ :refer :all] from a
  collection of libspecs."
  [coll]
  (reduce
    (fn [s spec]
      (if (and (coll? spec) (= (rest spec) [:refer :all]))
        (conj s (first spec))
        s))
    #{} coll))

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
              ;; ns-import-candidates is slower, so only call when necessary
              (if (seq ss)
                (set ss)
                (ns-import-candidates missing)))
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
  [candidates type missing old-ns-map]
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

(defn- expand-prefix-list [[prefix & more]]
  (map (fn [expr]
         (if (coll? expr)
           (let [[x & xs] expr
                 sym (symbol (str prefix \. x))]
             (if (seq xs)
               (into [sym] xs)
               sym))
           (symbol (str prefix \. expr))))
       more))

(defn expand-libs
  "Reduce collection of symbols, libspecs, and prefix lists into a regular set
  of symbols and libspecs"
  [coll]
  (reduce
    (fn [s lib]
      (if (and (coll? lib) (not (keyword? (second lib))))
        (into s (expand-prefix-list lib))
        (conj s lib)))
    #{} coll))

(defn in-originals-pred [originals]
  (let [libs (expand-libs originals)]
    (fn [candidate]
      (contains? libs candidate))))

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
    (fn [[ns1 _ aliases1]]
      (->> originals
           (filter sequential?)
           (filter #(contains? #{:only :refer} (second %)))
           (some (fn [[ns2 _ aliases2]]
                   (and (sequential? aliases1)
                        (sequential? aliases2)
                        (= (first aliases1) (first aliases2))
                        (= ns1 ns2))))))))

(defn- last-segment-matches? [expr]
  (when (and (coll? expr) (= (second expr) :as))
    (let [[x _ y] expr]
      (= (peek (string/split (name x) #"\.")) (name y)))))

(defn- disambiguate [candidates missing ns-map type]
  ;; TODO: prefer things in src/classes to jars
  (debug :disambiguating missing :in candidates)
  (let [orig-clauses (mapcat #(get (:old ns-map) %)
                             (new-type-to-old-types type))]
    (->> candidates
         (sort-by (juxt (complement (in-originals-pred orig-clauses))
                        (complement (referred-to-in-originals-pred
                                     type orig-clauses))
                        (complement last-segment-matches?)
                        (comp count str)))
         (remove #(re-find disambiguator-blacklist (str %)))
         first)))

(defn disambiguate
  "Select the most likely class or ns symbol in the given set of candidates,
  returning [type candidate-sym]"
  [candidates type missing old-ns-map]
  ;; TODO: prefer things in src/classes to jars
  (let [cs (filter-excludes candidates type missing old-ns-map)
        cs (remove #(re-find disambiguator-blacklist (str %)) cs)]
    (when-let [c (first cs)]
      ;; Honor any old [c :refer :all] specs - issue #50
      (if (and (= type :refer)
               (contains? (:refer-all old-ns-map) c))
        [:refer-all c]
        [type c]))))

(defn- grow-step [missing type ns-map body]
  (let [mass-refers (mass-refer-namespaces (get-in ns-map [:old :require]))
        libspecs (candidates type missing body)
        ;; Prefer original [_ :refer :all] specs
        libspecs (reduce (fn [specs mref]
                           (map (fn [spec]
                                  (if (and (coll? spec)
                                           (= (take 2 spec) [mref :refer]))
                                    [mref :refer :all]
                                    spec))
                                specs))
                         libspecs mass-refers)]
    (if-let [addition (disambiguate libspecs missing ns-map type)]
      (update-in ns-map [type] conj addition)
      ns-map)))

(defn grow-ns-map
  "Return a new ns-map augmented with a single candidate ns reference."
  [ns-map type missing body]
  (let [cs (candidates type missing body)
        old-ns-map (:old ns-map)]
    (if-let [[type c] (disambiguate cs type missing old-ns-map)]
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
            (recur (grow-ns-map ns-map type missing body) missing type-idx)
            (throw (Exception. (str "Couldn't resolve " missing
                                    ", got as far as " ns-map)))))
        ns-map))))
