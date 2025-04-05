# Implementation/performance notes.

The first release of CLJ-COLL has _not_ been optimized for
performance.  Some basic function declarations were made, and the odd
fixnum declaration here and there, but by and large performance _testing_
has not been done, and there should be some low hanging fruit for later
improvements. 

That said, some algorithms are better than Clojure's (and some are probably
worse). For example `partition-by` does O(4n) sequence traversal in
Clojure, but not in CLJ-COLL, at the cost of less elegant code.

The implementation relies heavily on generic function dispatch and
`typecase`, for better or worse.

CLJ-COLL's assumption that CL:VECTOR types are generally adjustable and
fill-pointered means that they are not portably of type `SIMPLE-VECTOR`,
and so we can't use an optimizable `svref` on them.  Neither does CLJ-COLL
make any assumptions about specialized element types beyond those APIs that
happen to allow strings, which are a type of cl:vector.

CLJ-COLL's persistent data structure performance may not be on
par with Clojure's, if you're dealing with million entry collections you
may find native CL collections a better for performance (but
you can now use them with Clojure APIs!). I can live with this.

Most CLJ-COLL immutable collection support comes from
[FSet](https://github.com/slburson/fset).  The version I used for
development did not implement the same type of vector tail push
optimizations that Clojure's vectors do, but I believe the author is
experimenting in that area and perhaps there are improvements to be made.

Some immutable map implementations like [Daniel Keogh's
hashtrie](https://github.com/DanielKeogh/hashtrie) have support for
[transient](https://clojure.org/reference/transients) APIs, some don't.
That library wasn't in quicklisp when I started CLJ-COLL, but I see it's
now there (dated 2024-Oct-12) so perhaps it should be revisited.

[DartsCLHashTree](https://github.com/deterministic-arts/DartsCLHashTree)
was also considered.  It is available in quicklisp, but provides only map
support (vs sets and vectors/seqs), and does not support transient functionality.

It may be that someday CLJ-COLL will offer options for multiple immutable
data structures, e.g. multiple persistent vector implementations.  It may
also be that whatever is used now (FSet) will not be used in the future.
To that extent please use the supplied the following symbols to reference
dependenty types:

* `+immutable-map-type+` for maps
* `+immutable-set-type+` for sets
* `+immutable-vector-type+` for vectors

If CLJ-COLL changes the underlying types, a recompile should fix you up.
You may need to `#.` the type reference if you're using for things like
generic function type specialization.

The persistent list and persistent queue implementations are not expected
to change and are experted.

