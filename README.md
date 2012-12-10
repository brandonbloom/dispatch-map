# dispatch-map

A Clojure library which provides the internals of Clojure's multimethods as a persistent, immutible map implementation.

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

## License

Copyright © 2012 Brandon Bloom

Distributed under the Eclipse Public License, the same as Clojure.
