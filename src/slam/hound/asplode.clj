(ns slam.hound.asplode
  (:import java.io.PushbackReader))

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