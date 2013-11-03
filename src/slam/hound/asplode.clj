(ns slam.hound.asplode
  (:require [slam.hound.future :refer [as->* cond->*]])
  (:import (java.io PushbackReader)))

(def default-ns-references
  ;; NOTE: :verbose, :reload, and :reload-all can actually be specified per
  ;;       libspec, but this is not mentioned in the docstring for require, so
  ;;       we consider it an implementation detail.
  '{:import     #{}   ; #{class-sym}
    :require    #{}   ; #{ns-sym}
    :alias      {}    ; {ns-sym ns-sym}
    :refer      {}    ; {ns-sym #{var-sym}}
    :xrefer     #{}   ; #{ns-sym} - exclusively referred namespaces
    :refer-all  #{}   ; #{ns-sym}
    :exclude    {}    ; {ns-sym #{var-sym}}
    :rename     {}    ; {ns-sym {var-sym var-sym}}
    :reload     false ; true/false
    :reload-all false ; true/false
    :verbose    false ; true/false
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
        args (filter (complement keyword?) specs)]
    (reduce
      (fn [s arg]
        (if (libspec? arg)
          (conj s (prependss nil arg opts))
          (let [[prefix & more] arg]
            (into s (mapv #(prependss prefix % opts) more)))))
      #{} args)))

(defn parse-refers
  "Parse as `(clojure.core/refer ~ns-sym ~@filters), returning a map with
  :exclude, :refer, :refer-all, and :rename.

  If exclusive-refer? is true, the :only option also adds ns-sym to the
  :xrefer set."
  ([ns-sym filters]
   (parse-refers ns-sym filters false))
  ([ns-sym filters exclusive-refer?]
   (if (seq filters)
     (let [{:keys [exclude only rename]} (apply hash-map filters)]
       (cond->* {}
         exclude (assoc :exclude {ns-sym (set exclude)})
         only (as->* m
                (cond->* (assoc m :refer {ns-sym (set only)})
                  exclusive-refer? (assoc :xrefer #{ns-sym})))
         rename (assoc :rename {ns-sym (into {} rename)})))
     {:refer-all #{ns-sym}})))

(defn parse-requires
  "Parse a list of require libspecs, returning a map with :require, :alias,
  :refer, :refer-all, :reload, :reload-all, and :verbose. Expands prefix lists.

  cf. clojure.core/load-libs"
  [specs]
  (reduce
    (fn [m [ns-sym & opts]]
      (if (seq opts)
        (let [{:keys [as refer reload reload-all verbose]}
              (apply hash-map opts)]
          (vmerge m (cond->* {}
                      as (assoc :alias {ns-sym as})
                      refer (as->* m (if (= refer :all)
                                      (assoc m :refer-all #{ns-sym})
                                      (assoc m :refer {ns-sym (set refer)})))
                      reload (assoc :reload true)
                      reload-all (assoc :reload-all true)
                      verbose (assoc :verbose true))))
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
    :refer-clojure (vmerge nsrefs (parse-refers 'clojure.core specs true))
    :use (vmerge nsrefs (parse-uses specs))
    :require (vmerge nsrefs (parse-requires specs))
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
    (into {:meta ns-meta :name ns-name}
          (for [[clause & body] clauses
                ;; (:gen-class) with no arguments is valid
                :let [body (if (= clause :gen-class) [] body)]]
            [clause body]))))

(defn preserve-ns-references
  "Extract map of :gen-class, :load, :refer, :exclude, and :rename that should
  be preserved in an ns declaration."
  [ns-map]
  (let [{:keys [gen-class load refer exclude rename]} ns-map
        maybe-assoc (fn [m kw x]
                      (if (and x (x 'clojure.core))
                        (assoc m kw {'clojure.core (x 'clojure.core)})
                        m))]
    (-> (cond->* {}
          gen-class (assoc :gen-class gen-class)
          load (assoc :load load))
        (maybe-assoc :refer refer)
        (maybe-assoc :exclude exclude)
        (maybe-assoc :rename rename))))

(defn asplode [rdr]
  (let [rdr (PushbackReader. rdr)
        ns-map (ns-to-map (read rdr))
        old-ns (reduce (fn [m k]
                         (if-let [v (ns-map k)]
                           (parse-libs m k v)
                           m))
                       default-ns-references ns-clauses)
        stripped-ns (assoc (apply dissoc ns-map ns-clauses) :old old-ns)
        stripped-ns (merge stripped-ns (preserve-ns-references old-ns))
        body (take-while #(not= ::done %)
                         (repeatedly #(read rdr false ::done)))]
    [stripped-ns body]))
