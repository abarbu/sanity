# sanity

Available from clojars:

    [com._0xab/sanity "1.0.0"]

Use with:

    (use 'sanity.core)

Sanity fixes some issues with Clojure's semantics and syntax, fills in
some missing parts of the standard API, fixes \` by adding a sane
version named #\`, and provides some compatibility functions that make
porting scheme code much easier.

It provides combine-namespaces which takes a name along with one or
more namespaces, unions those namespaces and exposes them all under
the given name. Particularly useful with incanter and loom:

    (combine-namespaces i incanter.core incanter.io incanter.optimize incanter.stats)
    (combine-namespaces l loom.graph loom.alg loom.gen loom.attr loom.label loom.io)

It adds syntax for #\`. This is a quasiquote and works just like \`
except that it does not qualify symbols with the current namespace.
This default behaviour leads to a lot of strange and broken code.  Try
out \`a vs #\`a (you would need to do \`~'a to get this (sane)
behaviour otherwise).

Individual functions are mostly documented, but this project as a
whole does not yet have documentation.

At the moment loading this package causes a number of warnings.

## License

Copyright © 2014 Andrei Barbu

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
