(ns slam.hound.asplode
  (:require [slam.hound.future :refer [as->* cond->*]])
  (:import (java.io PushbackReader)))

(def empty-ns-references
  ;; NOTE: :verbose, :reload, and :reload-all can actually be specified per
  ;;       libspec, but this is not mentioned in the docstring for require, so
  ;;       we consider it an implementation detail.
  {:import     #{}   ; #{class-sym}
   :require    #{}   ; #{ns-sym}
   :alias      {}    ; {ns-sym ns-sym}
   :refer      {}    ; {ns-sym #{var-sym}}
   :xrefer     #{}   ; #{ns-sym} - exclusively referred namespaces
   :refer-all  #{}   ; #{ns-sym}
   :exclude    {}    ; {ns-sym #{var-sym}}
   :rename     {}    ; {ns-sym {var-sym var-sym}}
   :reload     #{}   ; #{ns-sym}
   :reload-all #{}   ; #{ns-sym}
   :verbose    #{}   ; #{ns-sym}
   :load       nil   ; a seq of file paths
   :gen-class  nil   ; a seq of option pairs, possibly empty
   })

(def ns-clauses
  "Set of valid keys that begin clauses in ns forms."
  #{:refer-clojure :use :require :import :load :gen-class})

(defn- libspec?
  "Returns true if x is a libspec.
  Copied from clojure.core/libspec?"
  [x]
  (or (symbol? x)
      (and (vector? x)
           (or
             (nil? (second x))
             (keyword? (second x))))))

(defn- prependss
  "Prepends a symbol or a seq to coll.
  Adapted from clojure.core/prependss"
  [prefix x coll]
  (letfn [(p [sym] (if prefix (symbol (str prefix \. sym)) sym))]
    (if (symbol? x)
      (into [(p x)] coll)
      (let [[x & xs] x]
        (into [(p x)] (concat xs coll))))))

(defn- vmerge
  "Value-dependent merge, intended for merging ns reference maps."
  [m1 m2]
  (reduce-kv
    (fn [m k v]
      (cond (map? v) (merge-with vmerge m {k v})
            (set? v) (assoc m k (into (or (m k) #{}) v))
            (vector? v) (assoc m k (into (or (m k) []) v))
            :else (assoc m k v)))
    m1 m2))

(defn expand-imports
  "Expand import-lists into a set of symbols.

  cf. clojure.core/import"
  [specs]
  (reduce
    (fn [s spec]
      (if (symbol? spec)
        (conj s spec)
        (let [[p & qs] spec]
          ;; Note that prefix lists with only a prefix are ignored
          (into s (map #(symbol (str p \. %)) qs)))))
    #{} specs))

(defn expand-libspecs
  "Expand any prefix lists and flags within specs and return a flat set of
  libspecs.

  cf. clojure.core/load-libs"
  [specs]
  (let [flags (filter keyword? specs)
        opts (interleave flags (repeat true))
        args (remove keyword? specs)]
    (reduce
      (fn [s arg]
        (if (libspec? arg)
          (conj s (prependss nil arg opts))
          (let [[prefix & more] arg]
            (into s (mapv #(prependss prefix % opts) more)))))
      #{} args)))

(defn parse-refers
  "Parse as `(clojure.core/refer ~ns-sym ~@filters), returning a map with
  :exclude, :refer, :xrefer, :refer-all, and :rename.

  If the :exclusive option is true, the :only option also adds ns-sym to the
  :xrefer set."
  [ns-sym filters & opts]
  (if (seq filters)
    (let [{:keys [exclude only rename]} (apply hash-map filters)
          {:keys [exclusive]} (apply hash-map opts)]
      (cond->* {}
        exclude (assoc :exclude {ns-sym (set exclude)})
        only (as->* m
               (cond->* (assoc m :refer {ns-sym (set only)})
                 exclusive (assoc :xrefer #{ns-sym})))
        rename (assoc :rename {ns-sym rename})))
    {:refer-all #{ns-sym}}))

(defn parse-requires
  "Parse a list of require libspecs, returning a map with :require, :alias,
  :refer, :refer-all, :reload, :reload-all, and :verbose. Expands prefix lists.

  cf. clojure.core/load-libs"
  [specs]
  (reduce
    (fn [m [ns-sym & opts]]
      (if (seq opts)
        (let [{:keys [as refer rename reload reload-all verbose]}
              (apply hash-map opts)]
          (vmerge m (cond->* {}
                      as (assoc :alias {ns-sym as})
                      refer (as->* m (if (= refer :all)
                                       (assoc m :refer-all #{ns-sym})
                                       (assoc m :refer {ns-sym (set refer)})))
                      rename (as->* m (vmerge m (parse-refers
                                                  ns-sym [:rename rename])))
                      reload (assoc :reload (conj (get m :reload #{}) ns-sym))
                      reload-all (assoc :reload-all
                                        (conj (get m :reload-all #{}) ns-sym))
                      verbose (assoc :verbose
                                     (conj (get m :verbose #{}) ns-sym)))))
        (vmerge m {:require #{ns-sym}})))
    {} (expand-libspecs specs)))

(defn parse-uses
  "Parse a list of use libspecs, via parse-refers and parse-requires."
  [specs]
  (reduce
    (fn [m [ns-sym & opts]]
      (if (seq opts)
        (reduce-kv
          (fn [m k v]
            (vmerge m (if (contains? #{:exclude :only :rename} k)
                        (parse-refers ns-sym [k v])
                        (parse-requires [[ns-sym k v]]))))
          m (apply hash-map opts))
        (vmerge m (parse-refers ns-sym []))))
    {} (expand-libspecs specs)))

(defn parse-libs
  "Parse ns reference declaration and intelligently merge into nsrefs."
  [nsrefs kw specs]
  (case kw
    :refer-clojure (vmerge nsrefs (parse-refers
                                    'clojure.core specs :exclusive true))
    :use (vmerge nsrefs (parse-uses specs))
    :require (reduce (fn [m s] (vmerge m (parse-requires s))) nsrefs specs)
    :import (vmerge nsrefs {:import (expand-imports specs)})
    :load (assoc nsrefs :load specs)
    :gen-class (assoc nsrefs :gen-class specs)))

(defn ns-to-map [ns-form]
  (let [[_ ns-name maybe-doc & clauses] ns-form
        ns-meta (meta ns-name)
        [ns-meta clauses] (if (string? maybe-doc)
                            [(assoc ns-meta :doc maybe-doc) clauses]
                            [ns-meta (cons maybe-doc clauses)])
        [ns-meta clauses] (if (map? (first clauses))
                            [(merge ns-meta (first clauses)) (rest clauses)]
                            [ns-meta clauses])]
    (reduce
      (fn [m [clause & body]]
        ;; (:gen-class) with no arguments is valid
        (let [clause (keyword clause)
              body (if (and (nil? body) (= clause :gen-class))
                     []
                     body)
              ;; Separate require clauses may have different flags
              add (if (= clause :require) conj into)]
          (assoc m clause (add (get m clause []) body))))
      {:meta ns-meta :name ns-name} clauses)))

(defn parse-ns-map
  "Reduce namespace map into slam.hound.asplode/empty-ns-references"
  [ns-map]
  (reduce (fn [m k]
            (if-let [v (ns-map k)]
              (parse-libs m k v)
              m))
          empty-ns-references ns-clauses))

(defn preserve-ns-references
  "Extract map of :gen-class, :load, :reload, :reload-all, :verbose, :refer,
  :exclude, and :rename that should be preserved in an ns declaration."
  [ns-map]
  (let [{:keys [gen-class load refer exclude rename
                reload reload-all verbose]} ns-map
        assoc-clojure (fn [m kw vs]
                        (if-let [v (get vs 'clojure.core)]
                          (assoc m kw {'clojure.core v})
                          m))]
    (-> (cond->* {}
          gen-class (assoc :gen-class gen-class)
          load (assoc :load load)
          (seq reload) (assoc :reload reload)
          (seq reload-all) (assoc :reload-all reload-all)
          (seq verbose) (assoc :verbose verbose))
        (assoc-clojure :refer refer)
        (assoc-clojure :exclude exclude)
        (assoc-clojure :rename rename))))

(defn asplode [rdr]
  (let [rdr (PushbackReader. rdr)
        ns-map (ns-to-map (read rdr))
        old-ns (parse-ns-map ns-map)
        stripped-ns (-> (apply dissoc ns-map ns-clauses)
                        (assoc :old old-ns)
                        (merge (preserve-ns-references old-ns)))
        body (take-while #(not= ::done %)
                         (repeatedly #(read rdr false ::done)))]
    [stripped-ns body]))
