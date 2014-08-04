# sanity

Available from clojars:

[![Clojars Project](http://clojars.org/com._0xab/sanity/latest-version.svg)](https://clojars.org/com._0xab/sanity)

Use with:

```clojure
(use '[sanity core improvements reader])
```

Sanity fixes some issues with Clojure's semantics and syntax, fills in
some missing parts of the standard API, fixes \` by adding a sane
version named #\`, and provides some functions which are more
compatible with their better thought out Scheme counterparts.

It provides combine-namespaces which takes a name along with one or
more namespaces, unions those namespaces and exposes them all under
the given name. Particularly useful with incanter and loom:

```clojure
(combine-namespaces i incanter.core incanter.io incanter.optimize incanter.stats)
(combine-namespaces l loom.graph loom.alg loom.gen loom.attr loom.label loom.io)
```

It adds syntax for #\`. This is a quasiquote and works just like \`
except that it does not qualify symbols with the current namespace.
This default behaviour leads to a lot of strange and broken code.  Try
out \`a vs #\`a (you would need to do \`~'a to get this (sane)
behaviour otherwise). #\` also handles `#`(~@())`
correctly and returns `'()` instead of `'(nil)`.

Individual functions are mostly documented, but this project as a
whole does not yet have documentation.

Other broken behavior to hopefully be patched one day:

* `(seq '())` returns nil instead of failing. An example
  of where this misguided propensity to return nil everywhere leads to
  trouble, consider that `(cdr (cdr (seq [1 2])))` and
  `(seq [])` are paradoxically not equal. seq should
  return the empty element of the collection, not nil. This ripples
  out and causes many other bugs, including some in the clojure
  compiler. An example is where
```clojure
`(~@(map (fn [x] (+ x 1)) '(1 2 3)))
```
  returns, as expected `(2 3 4)` but as
  the list becomes empty we would expect `()` whereas
  instead you get something pretty broken `(nil)`
  because of this sloppy seq behavior.
* `:keyword` on a map without the given key returns nil instead of failing. This
  hides bugs and causes errors to appear far away from their real
  source.
* `(first '())` returns nil instead of failing.
* The scoping of def and defn is broken yet for some reason they're
  still allowed outside the toplevel.
* map mangles the type of object you pass in and always returns a lazy
  sequence. This leads to a lot of difficult to read code that has to
  rebuild the original object constantly, not to mention bugs where
  strict objects become lazy at unexpected times. map should do what
  it's meant to do, preserve the type of the object. map-lazy can have
  the current behavior.

## License

Copyright © 2014 Andrei Barbu

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

Yourkit supports this open source project through their
[Java profiler](http://www.yourkit.com/java/profiler/index.jsp).
