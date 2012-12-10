(ns dispatch-map.core)

(declare empty-cache reset-cache! update-map cache-best)

(defrecord DispatchCache [hierarchy table])

(defprotocol IDispatch
  (prefer [this dispatch-val-x dispatch-val-y]))

(deftype DispatchMap [dispatch-fn default hierarchy m preferences cache]

  clojure.lang.Associative
  (containsKey [this k]
    (.containsKey m k))
  (entryAt [this k]
    (let [dispatch-val (dispatch-fn k)
          c @cache]
      (when-not (= (.hierarchy c) @hierarchy)
        (reset-cache! this))
      (or ((.table c) dispatch-val)
          (let [c (swap! cache #(cache-best this % dispatch-val))]
            ((.table c) dispatch-val)))))

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (if-let [entry (.entryAt this k)]
      (.val entry)
      not-found))

  clojure.lang.IPersistentMap
  (assoc [this k v]
    (update-map this (.assoc m k v)))
  (assocEx [this k v]
    (update-map this (.assocEx m k v)))
  (without [this k]
    (update-map this (.without m k)))

  java.lang.Iterable
  (iterator [this]
    (.iterator m))

  clojure.lang.IPersistentCollection
  (count [this]
    (.count m))
  (cons [this o]
    (update-map this (.cons m o)))
  (empty [this]
    (update-map this {}))
  (equiv [this o]
    ;;TODO also compare dispatch-fn, default, hierarchy, and preferences?
    (and (isa? (class o) DispatchMap)
         (.equiv m (.m o))))

  clojure.lang.Seqable
  (seq [this]
    (.seq m))

  clojure.lang.IFn
  (invoke [this key]
    (.valAt this key))
  (invoke [this key not-found]
    (.valAt this key not-found))
  )

(defn- empty-cache [dm]
  (DispatchCache. (.hierarchy dm) {}))

(defn- reset-cache! [dm]
  (reset! (.cache dm) (empty-cache dm)))

(defn- update-map [dm m]
  (DispatchMap. (.dispatch-fn dm) (.default dm) (.hierarchy dm)
                m (.preferences dm)
                (atom (empty-cache dm))))

(defn- prefers? [dm x y]
  (or (contains? ((.preferences dm) x) y)
      (some #(prefers? dm x %) (parents y))
      (some #(prefers? dm % y) (parents x))))

(defn- dominates? [dm x y]
  (or (prefers? dm x y) (isa? x y)))

(defn- find-best [dm dispatch-val]
  (reduce (fn [best entry]
            (if (isa? dispatch-val (key entry))
              (let [best* (if (or (not best)
                                  (dominates? dm (key entry) (key best)))
                            entry
                            best)]
                (when-not (dominates? dm (key best*) (key entry))
                  (throw (Exception.
                           (str "Multiple keys match: " dispatch-val
                                " -> " (key entry) " and " (key best*)
                                ", but neither is preferred"))))
                best*)
              best))
          nil
          (.m dm)))

(defn- cache-best [dm cache dispatch-val]
  (let [best (find-best dm dispatch-val)
        entry (or best (find (.m dm) (.default dm)))
        table (assoc (.table cache) dispatch-val entry)]
    (DispatchCache. (.hierarchy cache) table)))

;;TODO: What to do about private var usages?

(defn dispatch-map
  {:arglists '([dispatch-fn options? & keyvals])}
  [dispatch-fn & args]
  (let [[options & keyvals] (if (even? (count args)) (cons {} args) args)
        default (get options :default :default)
        hierarchy (get options :hierarchy #'clojure.core/global-hierarchy)]
    (#'clojure.core/check-valid-options options :default :hierarchy)
    (let [m (apply hash-map keyvals)]
      (DispatchMap. dispatch-fn default hierarchy
                    m {} (atom (DispatchCache. hierarchy {}))))))
