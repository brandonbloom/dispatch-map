(ns dispatch-map.core
  (:refer-clojure :exclude [isa?])
  (:require [dispatch-map.hierarchy :refer (isa?)]))

(declare empty-cache reset-cache! update-map cache-best prefers?)

(defrecord DispatchCache [hierarchy table])

(defprotocol IDispatch
  (-dispatch-fn [this])
  (-hierarchy [this])
  (-find-dispatched [this dispatch-val])
  (-prefer [this dispatch-val-x dispatch-val-y])
  (-preferences [this]))

(deftype DispatchMap [dispatch-fn hierarchy m preferences cache]

  Object
  (hashCode [this]
    (bit-xor (hash dispatch-fn)
             (hash hierarchy)
             (hash m)
             (hash preferences)))
  (equals [this obj]
    (.equiv this obj))
  (toString [this]
    ;;TODO Something better here?
    (str "#<DispatchMap " (pr-str m) ">"))

  clojure.lang.Associative
  (containsKey [this k]
    (.containsKey m k))
  (entryAt [this k]
    (-find-dispatched this (dispatch-fn k)))

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

  clojure.lang.Counted
  (count [this]
    (.count m))

  clojure.lang.IPersistentCollection
  (cons [this o]
    (update-map this (.cons m o)))
  (empty [this]
    (update-map this {}))
  (equiv [this o]
    (and (clojure.core/isa? (class o) DispatchMap)
         (= dispatch-fn (.dispatch-fn o))
         (= hierarchy (.hierarchy o))
         (= m (.m o))
         (= preferences (.preferences o))))

  clojure.lang.Seqable
  (seq [this]
    (.seq m))

  clojure.lang.IFn
  (invoke [this key]
    (.valAt this key))
  (invoke [this key not-found]
    (.valAt this key not-found))

  IDispatch
  (-dispatch-fn [this] dispatch-fn)
  (-hierarchy [this] hierarchy)
  (-find-dispatched [this dispatch-val]
    (let [c @cache]
      (when-not (= (.hierarchy c) @hierarchy)
        (reset-cache! this))
      (let [entry (get (.table c) dispatch-val ::not-found)]
        (if (= entry ::not-found)
          (let [c (swap! cache #(cache-best this % dispatch-val))]
            (get (.table c) dispatch-val))
          entry))))
  (-prefer [this dispatch-val-x dispatch-val-y]
    (when (prefers? this dispatch-val-y dispatch-val-x)
      (throw (Exception. (str "Preference conflict: " dispatch-val-y
                              " is already preferred to " dispatch-val-x))))
    (let [preferences* (update-in preferences [dispatch-val-x]
                                  (fnil conj #{})
                                  dispatch-val-y)]
      (DispatchMap. dispatch-fn hierarchy
                    m preferences*
                    (atom (empty-cache this)))))
  (-preferences [this]
    preferences)

  java.util.Map
  (get [this k]
    (.valAt this k))
  (isEmpty [this]
    (empty? this))
  (size [this]
    (count this))
  (keySet [this]
    (-> this keys set))
  (put [_ _ _]
    (throw (UnsupportedOperationException.)))
  (putAll [_ _]
    (throw (UnsupportedOperationException.)))
  (clear [_]
    (throw (UnsupportedOperationException.)))
  (remove [_ _]
    (throw (UnsupportedOperationException.)))
  (values [this]
    (vals this))
  (entrySet [this]
    (-> this seq set))

  )

(defn- empty-cache [dm]
  (DispatchCache. @(.hierarchy dm) {}))

(defn- reset-cache! [dm]
  (reset! (.cache dm) (empty-cache dm)))

(defn- update-map [dm m]
  (DispatchMap. (.dispatch-fn dm) (.hierarchy dm)
                m (.preferences dm)
                (atom (empty-cache dm))))

(defn- prefers? [dm x y]
  (or (contains? ((.preferences dm) x) y)
      (some #(prefers? dm x %) (parents y))
      (some #(prefers? dm % y) (parents x))))

(defn- dominates? [dm hierarchy x y]
  (or (prefers? dm x y) (isa? hierarchy x y)))

(defn- find-best [dm hierarchy dispatch-val]
  (reduce
    (fn [best entry]
      (if (isa? hierarchy dispatch-val (key entry))
        (let [best* (if (or (not best)
                            (dominates? dm hierarchy (key entry) (key best)))
                      entry
                      best)]
          (when-not (dominates? dm hierarchy (key best*) (key entry))
            (throw (IllegalArgumentException.
                     (str "Multiple keys match: " dispatch-val
                          " -> " (key entry) " and " (key best*)
                          ", but neither is preferred"))))
          best*)
        best))
    nil
    (.m dm)))

(defn- cache-best [dm cache dispatch-val]
  (let [hierarchy (.hierarchy cache)
        entry (find-best dm hierarchy dispatch-val)
        table (assoc (.table cache) dispatch-val entry)]
    (DispatchCache. hierarchy table)))

(defn dispatch-map
  {:arglists '([dispatch-fn hierarchy? & keyvals])}
  [dispatch-fn & args]
  (let [[hierarchy & keyvals] (if (even? (count args))
                                (cons #'clojure.core/global-hierarchy args)
                                args)
        m (apply hash-map keyvals)]
    (DispatchMap. dispatch-fn hierarchy
                  m {} (atom (DispatchCache. @hierarchy {})))))

(defn hierarchy
  "Gets the hierarchy used by a dispatch-map."
  [dispatch-map]
  (-hierarchy dispatch-map))

(defn dispatch-fn
  "Gets the dispatch function used by a dispatch-map."
  [dispatch-map]
  (-dispatch-fn dispatch-map))

(defn find-dispatched
  "Returns the map entry for a pre-dispatched key, nil if key not present."
  [dispatch-map dispatch-val]
  (-find-dispatched dispatch-map dispatch-val))

(defn get-dispatched
  "Returns the value mapped to a pre-dispatched key,
  not-found or nil if key not present."
  ([dispatch-map dispatch-val]
   (get-dispatched dispatch-map dispatch-val nil))
  ([dispatch-map dispatch-val not-found]
   (if-let [entry (-find-dispatched dispatch-map dispatch-val)]
     (val entry)
     not-found)))

(defn prefer
  "Causes the dispatch-map to prefer matches of dispatch-val-x over
  dispatch-val-y when there is a conflict. Analogous to prefer-method."
  [dispatch-map dispatch-val-x dispatch-val-y]
  (-prefer dispatch-map dispatch-val-x dispatch-val-y))

(defn preferences
  "Given a dispatch-map, returns a map of preferred value -> set of
  other values. Analogous to prefers for multimethods."
  [dispatch-map]
  (-preferences dispatch-map))
