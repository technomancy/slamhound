(ns slam.hound.asplode)

(def ns-clauses-to-reconstruct [:use :require :import])

(defn ns-to-map [ns-form]
  (let [[_ ns-name doco & clauses] ns-form
        ns-meta (meta ns-name)
        [ns-meta clauses] (if (string? doco)
                            [(assoc ns-meta :doc doco) clauses]
                            [ns-meta (cons doco clauses)])]
    (into {:meta ns-meta :name ns-name}
          (map (juxt first rest) clauses))))

(defn asplode [rdr]
  (let [rdr (java.io.PushbackReader. rdr)
        ns-map (ns-to-map (read rdr))
        stripped-ns (apply dissoc ns-map ns-clauses-to-reconstruct)
        body (take-while #(not= ::done %)
                         (repeatedly #(read rdr false ::done)))]
    [stripped-ns body]))
