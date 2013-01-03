(ns dispatch-map.multi-fn
  (:refer-clojure :exclude [defmulti defmethod remove-all-methods remove-method
                            prefer-method methods get-method prefers])
  (:require [dispatch-map.core :as dm :refer (dispatch-map)]))

;; Taken from ClojureScript, since Clojure doesn't have it.
(defprotocol IMultiFn
  (-reset [mf])
  (-add-method [mf dispatch-val method])
  (-remove-method [mf dispatch-val])
  (-prefer-method [mf dispatch-val-x dispatch-val-y])
  (-get-method [mf dispatch-val])
  (-methods [mf])
  (-prefers [mf])
  (-dispatch [mf args]))

(deftype DispatchMapFn [name fresh current default-val]

  IMultiFn
  (-reset [mf]
    (reset! current fresh)
    mf)
  (-add-method [mf dispatch-val method]
    (swap! current assoc dispatch-val method)
    mf)
  (-remove-method [mf dispatch-val]
    (swap! current dissoc dispatch-val)
    mf)
  (-prefer-method [mf dispatch-val dispatch-val-y]
    (swap! current dm/prefer dispatch-val dispatch-val-y)
    mf)
  (-get-method [mf dispatch-val]
    (let [dm @current]
      (when-let [entry (or (dm/find-dispatched dm dispatch-val)
                           (dm/find-dispatched dm default-val))]
        (val entry))))
  (-methods [mf]
    (.m @current))
  (-prefers [mf]
    (.preferences @current))
  (-dispatch [mf args]
    (let [dispatch-fn (dm/dispatch-fn @current)
          dispatch-val (apply dispatch-fn args)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw (Exception. (str "No method in multimethod '" name
                                "' for dispatch value: " dispatch-val))))
      (apply target-fn args)))

  clojure.lang.IFn
  ;; Man, this *sucks*.
  (invoke [this]
    (-dispatch this []))
  (invoke [this a]
    (-dispatch this [a]))
  (invoke [this a b]
    (-dispatch this [a b]))
  (invoke [this a b c]
    (-dispatch this [a b c]))
  (invoke [this a b c d]
    (-dispatch this [a b c d]))
  (invoke [this a b c d e]
    (-dispatch this [a b c d e]))
  (invoke [this a b c d e f]
    (-dispatch this [a b c d e f]))
  (invoke [this a b c d e f g]
    (-dispatch this [a b c d e f g]))
  (invoke [this a b c d e f g h]
    (-dispatch this [a b c d e f g h]))
  (invoke [this a b c d e f g h i]
    (-dispatch this [a b c d e f g h i]))
  (invoke [this a b c d e f g h i j]
    (-dispatch this [a b c d e f g h i j]))
  (invoke [this a b c d e f g h i j k]
    (-dispatch this [a b c d e f g h i j k]))
  (invoke [this a b c d e f g h i j k l]
    (-dispatch this [a b c d e f g h i j k l]))
  (invoke [this a b c d e f g h i j k l m]
    (-dispatch this [a b c d e f g h i j k l m]))
  (invoke [this a b c d e f g h i j k l m n]
    (-dispatch this [a b c d e f g h i j k l m n]))
  (invoke [this a b c d e f g h i j k l m n o]
    (-dispatch this [a b c d e f g h i j k l m n o]))
  (invoke [this a b c d e f g h i j k l m n o p]
    (-dispatch this [a b c d e f g h i j k l m n o p]))
  (invoke [this a b c d e f g h i j k l m n o p q]
    (-dispatch this [a b c d e f g h i j k l m n o p q]))
  (invoke [this a b c d e f g h i j k l m n o p q r]
    (-dispatch this [a b c d e f g h i j k l m n o p q r]))
  (invoke [this a b c d e f g h i j k l m n o p q r s]
    (-dispatch this [a b c d e f g h i j k l m n o p q r s]))
  (invoke [this a b c d e f g h i j k l m n o p q r s t]
    (-dispatch this [a b c d e f g h i j k l m n o p q r s t]))
  (invoke [this a b c d e f g h i j k l m n o p q r s t rest]
    (-dispatch this (concat [a b c d e f g h i j k l m n o p q r s t] rest)))

  )

;;; Macros adapted from clojure.core

(defmacro defmulti
  "Creates a new multimethod with the associated dispatch function.
  The docstring and attribute-map are optional.

  Options are key-value pairs and may be one of:
    :default    the default dispatch value, defaults to :default
    :hierarchy  the isa? hierarchy to use for dispatching
                defaults to the global hierarchy"
  {:arglists '([name docstring? attr-map? dispatch-fn & options])}
  [mm-name & options]
  (let [docstring   (if (string? (first options))
                      (first options)
                      nil)
        options     (if (string? (first options))
                      (next options)
                      options)
        m           (if (map? (first options))
                      (first options)
                      {})
        options     (if (map? (first options))
                      (next options)
                      options)
        dispatch-fn (first options)
        options     (next options)
        m           (if docstring
                      (assoc m :doc docstring)
                      m)
        m           (if (meta mm-name)
                      (conj (meta mm-name) m)
                      m)]
    (when (= (count options) 1)
      (throw (Exception. "The syntax for defmulti has changed. Example: (defmulti name dispatch-fn :default dispatch-value)")))
    (let [options   (apply hash-map options)
          default   (get options :default :default)
          hierarchy (get options :hierarchy #'clojure.core/global-hierarchy)]
      (#'clojure.core/check-valid-options options :default :hierarchy)
      `(let [v# (def ~mm-name)
             dm# (dispatch-map ~dispatch-fn ~hierarchy)]
         (when-not (and (.hasRoot v#) (instance? DispatchMapFn (deref v#)))
           (def ~(with-meta mm-name m)
                (DispatchMapFn. ~(name mm-name) dm# (atom dm#) ~default)))))))

(defmacro defmethod
  "Creates and installs a new method of multimethod associated with dispatch-value. "
  [multifn dispatch-val & fn-tail]
  `(-add-method ~(with-meta multifn {:tag `DispatchMapFn}) ~dispatch-val (fn ~@fn-tail)))

(defn remove-all-methods
  "Removes all of the methods of multimethod."
 [multifn]
 (-reset multifn))

(defn remove-method
  "Removes the method of multimethod associated with dispatch-value."
 [multifn dispatch-val]
 (-remove-method multifn dispatch-val))

(defn prefer-method
  "Causes the multimethod to prefer matches of dispatch-val-x
  over dispatch-val-y when there is a conflict"
  [multifn dispatch-val-x dispatch-val-y]
  (-prefer-method multifn dispatch-val-x dispatch-val-y))

(defn methods
  "Given a multimethod, returns a map of dispatch values -> dispatch fns"
  [multifn] (-methods multifn))

(defn get-method
  "Given a multimethod and a dispatch value, returns the dispatch fn
  that would apply to that value, or nil if none apply and no default"
  [multifn dispatch-val] (-get-method multifn dispatch-val))

(defn prefers
  "Given a multimethod, returns a map of
  preferred value -> set of other values"
  [multifn] (-prefers multifn))
