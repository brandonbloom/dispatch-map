(ns dispatch-map.hierarchy
  (:refer-clojure :exclude [isa?]))

(defprotocol IHierarchy
  (-isa [this child parent]))

(defn isa?
  "Returns true if (= child parent), or child is directly or
  indirectly derived from parent in hierarchy."
  [hierarchy child parent]
  (if (satisfies? IHierarchy hierarchy)
    (-isa hierarchy child parent)
    (clojure.core/isa? hierarchy child parent)))
