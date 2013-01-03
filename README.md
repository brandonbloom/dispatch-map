# dispatch-map

A Clojure library which provides the internals of Clojure's multimethods as a persistent, immutible map implementation.

Also includes an implementation of the multimethod macros and functions in terms of a dispatch-map and an atom.

## Motivation

See http://blog.brandonbloom.name/2012/12/clojures-multimethod-dispatch-as-library.html


## Example

```clojure
(require '[dispatch-map.core :refer (dispatch-map)])

(derive ::rect ::shape)
(let [m (-> (dispatch-map identity
              [::rect ::shape] :rect-shape
              [::shape ::rect] :shape-rect)
            (prefer [::rect ::shape] [::shape ::rect]))]
  (println (m [::rect ::rect]))
  (println (m [::shape ::rect]))
  (println (m [::rect ::circle] :not-found)))
```

Prints:

```
:rect-shape
:shape-rect
:not-found
```


## IHierarchy Abstraction

Java's type hierarchy gets special treatment in Clojure's native multimethods via the isa? function.

You can now get similarly special treatment by implementing the  `-isa` method on the `dispatch-map.hierarchy/IHierarchy` protocol.
This is useful if you have a hiearchy defined by some other means that you don't wish to manually reproduce by successive calls to `derive`.

See [the tests](https://github.com/brandonbloom/dispatch-map/blob/93631c/test/dispatch_map/core_test.clj#L76-86) for an example.

Note that your `-isa` method must produce a result that is safe to memoize.


## DispatchMapFn

This is how you use the custom multimethod implementation. The only compelling reason to prefer this over
Clojure's native MultiFn or a raw dispatch-map is if you need to provide a custom IHierarchy implementation.

```clojure
(ns your-ns
  (:refer-clojure :exclude [defmulti defmethod remove-all-methods remove-method
                            prefer-method methods get-method prefers])
  (:use [dispatch-map.multi-fn])))
```


## License

Copyright © 2012 Brandon Bloom

Distributed under the Eclipse Public License, the same as Clojure.
