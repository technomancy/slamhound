(ns slam.hound.regrow
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [slam.hound.future :refer [as->* cond->*]]
            [slam.hound.search :as search]
            [slam.hound.stitch :as stitch])
  (:import (clojure.lang IMapEntry IRecord IType)
           (java.util.regex Pattern)))

(def ^:dynamic *debug* false)

;; sometimes we can't rely on stdout (testing slamhound.el)
(def debug-log (atom []))

(defn- debug [& msg]
  (when *debug*
    (swap! debug-log conj msg)
    (apply prn msg)))

(def ^:dynamic *cache* nil)

(defmacro with-regrow-cache [& body]
  `(binding [*cache* (or *cache* (atom {}))]
     ~@body))

(defmacro ^:private caching [key & body]
  `(if *cache*
     (if-let [v# (get @*cache* ~key)]
       v#
       (let [v# (do ~@body)]
         (swap! *cache* assoc ~key v#)
         v#))
     (do ~@body)))

(defn- all-ns-imports []
  (caching :all-ns-imports
    (mapv ns-imports (all-ns))))

(defn- ns->symbols []
  (caching :ns->symbols
    (let [xs (all-ns)]
      (zipmap xs (mapv (comp set keys ns-publics) xs)))))

(defn- symbols->ns-syms []
  (caching :symbols->ns
    (reduce
      (fn [m ns] (let [ns-sym (ns-name ns)]
                   (reduce
                     (fn [m k] (assoc m k (conj (or (m k) #{}) ns-sym)))
                     m (keys (ns-publics ns)))))
      {} (all-ns))))

(defn- walk
  "Adapted from clojure.walk/walk and clojure.walk/prewalk; this version
  preserves metadata on compound forms."
  [f form]
  (-> (cond
        (list? form) (apply list (map f form))
        (instance? IMapEntry form) (vec (map f form))
        (seq? form) (doall (map f form))
        (instance? IRecord form) (reduce (fn [r x] (conj r (f x))) form form)
        (coll? form) (into (empty form) (map f form))
        :else form)
      (as->* form'
        (if-let [m (meta form)]
          (with-meta form' m)
          form'))))

(defn- prewalk [f form]
  (walk (partial prewalk f) (f form)))

(defn- capitalized? [x]
  (Character/isUpperCase ^Character (first (name x))))

(def ^:private missing-sym-patterns
  (let [sym-pat #"(/|\D[^\p{javaWhitespace},/]*)"
        patterns (->> [#"Unable to resolve \w+: "
                       "Can't resolve: "
                       "No such namespace: "
                       "Cannot resolve type: " ; core.typed
                       ]
                      (mapv #(Pattern/compile (str % sym-pat))))]
    (into [#"No such var: (\S+)/.*"] patterns)))

(defn- missing-sym-name [msg]
  (second (some #(re-find % msg) missing-sym-patterns)))

(defn- failure-details [msg old-ns-map]
  (when-let [sym-name (missing-sym-name msg)]
    (let [sym (symbol sym-name)
          ts (cond (capitalized? sym-name)
                   (if (some #(contains? % sym) (vals (:refer old-ns-map)))
                     [:refer :import]
                     [:import :refer])

                   (re-find #"Unable to resolve var: \w+/" msg)
                   [:alias :refer]

                   (re-find #"No such (var|namespace)" msg)
                   [:alias]

                   :else
                   [:refer :import])
          rename? (some #{sym} (mapcat (comp vals val) (:rename old-ns-map)))]
      ;; Always attempt a rename first if there is a valid candidate
      {:missing sym
       :possible-types (if (and rename? (some #{:refer} ts))
                         (into [:rename] ts)
                         ts)})))

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

(defn- remove-var-form
  "Remove (var symbol) forms from body"
  [expr]
  (if (and (coll? expr) (= (first expr) 'var))
    nil
    expr))

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
  (reduce (fn [s imports]
            (if-let [^Class cls (get imports missing-sym)]
              (conj s (symbol (.getCanonicalName cls)))
              s))
          #{} (all-ns-imports)))

(defn- alias-candidates [type missing body]
  (set
    (let [syms-with-alias (get (ns-qualifed-syms body) missing)]
      (when (seq syms-with-alias)
        (let [ns->syms (ns->symbols)]
          (for [ns (all-ns)
                :when (set/subset? syms-with-alias (ns->syms ns))]
            (ns-name ns)))))))

(defn- candidates
  "Return a set of class or ns symbols that match the given constraints."
  [type missing body old-ns-map]
  (case type
    :import (into (ns-import-candidates missing)
                  (get @search/available-classes-by-last-segment missing))
    :alias (let [cs (alias-candidates type missing body)]
             (if (seq cs)
               cs
               ;; Try the alias search again without dynamically resolved vars
               ;; in case #' was used to resolve private vars in an aliased ns
               (let [body' (prewalk remove-var-form body)]
                 (if (= body' body)
                   cs
                   (alias-candidates type missing body')))))
    :refer (get (symbols->ns-syms) missing)
    :rename (reduce-kv
              (fn [s ns orig->rename]
                (cond->* s
                  (some #{missing} (vals orig->rename)) (conj ns)))
              #{} (:rename old-ns-map))))

(defn- filter-excludes
  "Disjoin namespace symbols from candidates that match the :exclude and
  :xrefer values in old-ns-map."
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

(defn- last-segment [s]
  (peek (string/split s #"\.")))

(def ^:private disambiguator-blacklist
  (if-let [v (resolve 'user/slamhound-disambiguator-blacklist)]
    @v
    #"\Acljs\.|swank|lancet"))

(defn- in-originals-fn
  "To what extent is the candidate present in the original ns-map?"
  [type missing old-ns-map]
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
      :refer (let [{:keys [refer-all refer]} old-ns-map
                   all? (contains? refer-all candidate)
                   ref? (and (contains? refer candidate)
                             (contains? (refer candidate) missing))]
               (cond (and all? ref?) 0
                     ref? 1
                     all? 2
                     :else 3))
      ;; Renames are only considered when they exist in the original ns
      :rename 0)))

(defn- last-segment-matches-fn
  "Does the last segment of the candidate match the missing alias?"
  [type missing]
  (let [alias (name missing)]
    (fn [candidate]
      (if (and (= type :alias)
               (= alias (last-segment (name candidate))))
        0
        1))))

(defn- is-project-namespace-fn
  "Is the namespace defined in a file on the classpath, as opposed to a jar?"
  [type]
  (fn [candidate]
    (if (and (contains? #{:alias :refer :rename} type)
             (contains? (search/namespaces-from-files) candidate))
      0
      1)))

(defn- alias-distance [^String alias ^String cand]
  (if (= (first alias) (first cand))
    (let [alen (.length alias)
          clen (.length cand)]
      (loop [d 0           ; alias-distance
             i 1           ; alias index
             j 1           ; candidate index
             matched? true ; current alias match state
             ]
        (if (or (>= i alen) (>= j clen))
          (if (and matched? (= i alen))
            d
            Long/MAX_VALUE)
          (if (= (.charAt alias i) (.charAt cand j))
            (recur d (inc i) (inc j) true)
            (recur (inc d) i (inc j) false)))))
    Long/MAX_VALUE))

(defn- alias-distance-fn
  "If the candidate shares the same first character with the missing alias,
  how many characters must be added between the first and last characters of
  the alias to form a subsequence of the last segment of the candidate?

  e.g. 0: st -> clojure.string
       1: st -> clojure.set
       2: st -> my.switchboard
       MAX_VALUE: str  -> clojure.set
       MAX_VALUE: ring -> clojure.string"
  [type missing]
  (let [alias (name missing)]
    (fn [candidate]
      (if (= type :alias)
        (alias-distance alias (last-segment (name candidate)))
        Long/MAX_VALUE))))

(defn- initials-match-alias-fn
  "Do the initials of the candidate match the missing alias?"
  [type missing]
  (let [alias (name missing)]
    (fn [candidate]
      (if (and (= type :alias)
               (= alias (->> (string/split (name candidate) #"\.")
                             (map first)
                             (string/join))))
        0
        1))))

(defn disambiguate
  "Select the most likely class or ns symbol in the given set of candidates,
  returning [type candidate-sym]"
  [candidates type missing ns-maps]
  ;; TODO: prefer things in classes to jars
  (debug :disambiguating missing :in candidates)
  (let [{:keys [old-ns-map new-ns-map]} ns-maps
        cs (cond->* candidates
             ;; Current ns is never a valid reference source
             true (disj (:name new-ns-map))
             ;; Prevent multiple aliases to a single namespace (ugh)
             (= type :alias) (set/difference (set (keys (:alias new-ns-map)))))
        cs (->> cs
                (filter-excludes type missing old-ns-map)
                (remove #(re-find disambiguator-blacklist (str %)))
                (sort-by (juxt (in-originals-fn type missing old-ns-map)
                               (last-segment-matches-fn type missing)
                               (is-project-namespace-fn type)
                               (alias-distance-fn type missing)
                               (initials-match-alias-fn type missing)
                               (comp count str))))]
    (when-let [c (first cs)]
      ;; Honor any old [c :refer :all] specs - issue #50
      (if (and (= type :refer)
               (contains? (:refer-all old-ns-map) c))
        [:refer-all c]
        [type c]))))

(defn- deftype? [cls]
  (or (.isAssignableFrom IType cls)
      (.isAssignableFrom IRecord cls)))

(defn- make-munged-ns-pattern [package-name]
  (->> (string/split package-name #"_")
       (map #(Pattern/quote %))
       (string/join "[_-]")
       (#(Pattern/compile (str "\\A" % "_*\\z")))))

(defn- find-matching-ns
  "Returns a ns symbol or nil"
  [package-name]
  ;; Try the simple case before doing a search
  (let [ns-sym (symbol (string/replace package-name \_ \-))]
    (if (find-ns ns-sym)
      ns-sym
      (let [pat (make-munged-ns-pattern package-name)]
        (first (filter #(re-find pat (str %)) (map ns-name (all-ns))))))))

(defn- update-imports-in
  "Adds candidate to :import entry in ns-map, and also adds matching namespace
  to :require if the candidate class was created by deftype or defrecord."
  [ns-map candidate]
  (let [class-name (str candidate)
        cls (Class/forName class-name)
        ns-map (update-in ns-map [:import] #(conj (or % #{}) candidate))]
    (if (deftype? cls)
      ;; cf. slam.hound.stitch/get-package
      (let [package-name (second (re-find #"(.*)\." class-name))
            ns-sym (find-matching-ns package-name)]
        (cond->* ns-map
          ns-sym (update-in [:require] #(conj (or % #{}) ns-sym))))
      ns-map)))

(defn grow-ns-map
  "Return a new ns-map augmented with candidate ns reference(s)."
  [ns-map type missing body]
  (let [old-ns-map (:old ns-map)
        cs (candidates type missing body old-ns-map)]
    (if-let [[type c] (disambiguate cs type missing {:old-ns-map old-ns-map
                                                     :new-ns-map ns-map})]
      (case type
        :import (update-imports-in ns-map c)
        :alias (update-in ns-map [:alias] assoc c missing)
        :refer (update-in ns-map [:refer c] #(conj (or % #{}) missing))
        :refer-all (update-in ns-map [:refer-all] #(conj (or % #{}) c))
        :rename (let [renames (get-in old-ns-map [:rename c])
                      orig (first (first (filter #(= missing (val %)) renames)))]
                  (-> ns-map
                      (update-in [:refer c] #(conj (or % #{}) orig))
                      (update-in [:rename c] assoc orig missing))))
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
  (if (:slamhound-skip (:meta ns-map))
    (merge ns-map (:old ns-map))
    ;; Since body was likely acquired through the reader, we must de-qualify
    ;; symbols that may have been erroneously qualified to the current ns
    ;; within syntax-quoted forms.
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
