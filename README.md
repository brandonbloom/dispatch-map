# dispatch-map

A Clojure library which provides the internals of Clojure's multimethods as a persistent, immutible map implementation.


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


## Bonus Features

Java's type hierarchy gets special treatment in Clojure's native multimethods via the isa? function.

You can now get similarly special treatment by implementing the  `-isa` method on the `dispatch-map.hierarchy/IHierarchy` protocol.
See [the tests](https://github.com/brandonbloom/dispatch-map/blob/93631c/test/dispatch_map/core_test.clj#L76-86) for an example.


## License

Copyright © 2012 Brandon Bloom

Distributed under the Eclipse Public License, the same as Clojure.
