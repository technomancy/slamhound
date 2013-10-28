(ns slam.hound.asplode
  (:import (java.io PushbackReader)))

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
