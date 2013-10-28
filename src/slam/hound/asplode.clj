(ns slam.hound.asplode
  (:import (java.io PushbackReader)))

(def default-namespace-references
  ;; NOTE: :verbose, :reload, and :reload-all can actually be specified per
  ;;       libspec, but this is not mentioned in the docstring for require, so
  ;;       we consider it an implementation detail.
  '{:load      []    ; ["/path/to/ns.clj"]}
    :gen-class []    ; [:option value ...]
    :import    #{}   ; #{class-sym}
    :require   #{}   ; #{ns-sym}
    :alias     {}    ; {ns-sym ns-sym}
    :refer     {}    ; {ns-sym #{var-sym}}
    :exclude   {}    ; {ns-sym #{var-sym}}
    :rename    {}    ; {ns-sym {var-sym var-sym}}
    :verbose   false ; true/false
    :reload    false ; true/false/:all
    })

;; TODO: Remove when upgrading to Clojure 1.5+
(defmacro cond->*
  "Takes an expression and a set of test/form pairs. Threads expr (via ->)
  through each form for which the corresponding test
  expression is true. Note that, unlike cond branching, cond-> threading does
  not short circuit after the first true test expression."
  {:added "1.5"}
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        pstep (fn [[test step]] `(if ~test (-> ~g ~step) ~g))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (map pstep (partition 2 clauses)))]
       ~g)))

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
      (cond (map? v) (assoc m k (merge (m k) v))
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
  :exclude, :refer, and :rename"
  [ns-sym filters]
  (if (seq filters)
    (let [{:keys [exclude only rename]} (apply hash-map filters)]
      (cond->* {}
        exclude (assoc :exclude {ns-sym (set exclude)})
        only (assoc :refer {ns-sym (set only)})
        rename (assoc :rename {ns-sym (into {} rename)})))
    {:refer {ns-sym :all}}))

(defn parse-requires
  "Parse a list of require libspecs, returning a map with :require, :alias,
  :refer, :reload, :reload-all, and :verbose. Expands prefix lists.

  cf. clojure.core/load-libs"
  [specs]
  (reduce
    (fn [m [ns-sym & opts]]
      (if (seq opts)
        (let [{:keys [as refer reload reload-all verbose]}
              (apply hash-map opts)]
          (vmerge m (cond->* {}
                      as (assoc :alias {ns-sym as})
                      refer (assoc :refer {ns-sym (if (= refer :all)
                                                    :all (set refer))})
                      reload (assoc :reload true)
                      reload-all (assoc :reload :all)
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
  ([kw specs]
   (parse-libs default-namespace-references kw specs))
  ([nsrefs kw specs]
   (case kw
     :refer-clojure (vmerge nsrefs (parse-refers 'clojure.core specs))
     :use (vmerge nsrefs (parse-uses specs))
     :require (vmerge nsrefs (parse-requires specs))
     :import (vmerge nsrefs {:import (expand-imports specs)})
     :load (assoc nsrefs :load specs)
     :gen-class (assoc nsrefs :gen-class specs))))

(defn- ns-to-map [ns-form]
  (let [[_ ns-name maybe-doc & clauses] ns-form
        ns-meta (meta ns-name)
        [ns-meta clauses] (if (string? maybe-doc)
                            [(assoc ns-meta :doc maybe-doc) clauses]
                            [ns-meta (cons maybe-doc clauses)])]
    (into {:meta ns-meta :name ns-name}
          (for [[clause & body] clauses]
            [clause body]))))

(defn asplode [rdr]
  (let [rdr (PushbackReader. rdr)
        ns-map (ns-to-map (read rdr))
        stripped-ns (-> ns-map
                        (assoc :old ns-map)
                        (dissoc :use :require :import))
        body (take-while #(not= ::done %)
                         (repeatedly #(read rdr false ::done)))]
    [stripped-ns body]))
