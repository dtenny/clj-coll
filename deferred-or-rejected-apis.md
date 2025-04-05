# Clojure APIs that were deferred or rejected for first CLJ-COLL release

## Deferred: sorted collections/APIs

- `compare`, though FSET won't be able to use it.
- `sort`, `sort-by`
- `sorted-map`, `sorted-map-by`
- `sorted-set`, `sorted-set-by`
- `sorted?`, `subseq`, `rsubseq` 
   The last two functions reject collections which don't implement `sorted?`.

## Deferred 

These were deferred or rejected for now because one or more of the
following applies:

- They require more pondering.
- They didn't seem essential for a first release.
- They weren't essential collection/sequence API functions.

The first three bullets involve potential use of CLJ-CON, the clojure
concurrency library, either using it or augmenting it, or both.

- `pmap`, `seque`, I wasn't ready to pull CLJ-CON in as a dependency of CLJ-COLL ... yet
  Maybe revisit this before release, I'd really like to have `seque` handy.
- `delay` and `deref` on Delays. Might be better _in_ CLJ-CON? Not really important
   for collection/seq activity.
- `memoize` - would want to use CLJ-CON:ATOM, hesitate to pull it as a
  dependency yet, also not critical for collection/seq activity.

- `for` - just lazyness on my part, and I don't value it highly because I
  don't use it often. Maybe do this when we also fix up doseq for nested iterations
- `clojure.walk/` - the following are left for another day, or even another project:
  `walk` `prewalk` `prewalk-demo` `prewalk-replace` `postwalk` 
  `postwalk-demo` `postwalk-replace`
- `file-seq`: lazy seqs on file system queries would be great, but requires
  grooming some file system utilities and testing for portability.
  UIOP might be good except that it's may be too eager (for lazy sequence APIs).
  In CL `file-seq` would take a pathname designator instead of java.io.File.
- `for` - because the author so seldome finds it useful and was lazy.

- For `iteration` and `eduction`, We would need some combination of
  Seqable, Iterable, and IReduceInit 
  for functions to be useful, I _think_.  All the things that accept seqs would
  need to accept these.  W.r.t. `eduction`, you'll just have to call
  `sequence` or `transduce` on your xforms for now.  Related reading:
  [Inside Transducers](https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/InsideTransducers-mostly-text.md).

## Unnecessary, redundant, or not a good fit for CLJ-COLL

-  `iterator-seq`, I'm not sure how we'd use this without adoption of some
   standard of 'iterators' interfaces, and there isn't such a thing in
   vanilla Common Lisp that I know of.  Maybe I should just polish the
   private CLJ-COLL::ITERATOR APIs and provide a seq that works on them
   unless someone has a better suggestion. 

   CL's idea of iterators is:

    do              do-external-symbols  dotimes
    do*             do-symbols           loop
    do-all-symbols  dolist

   Alexandria and Serapeum don't really seem to offer improvements.

- `trampoline` use `labels`, or use `serapeum:trampoline`, or a TCO-enabled lisp like SBCL
- `true?`, `false?` as they are specifically for Boolean true/false values
  required for Clojure interop with the JVM, and which Common Lisp does not
  have or need. We return non-nil and nil.
- `identity` and other things which are already provided by Common Lisp and
  work just fine for CLJ-COLL APIs/entities.
- `resultset-seq`, `xml-seq`, `iterator-seq`, `enumeration-seq`.
  Not a fit for clj-coll, but you can use `lazy-seq` to easily implement your own.

- `re-seq` - no, but with the lazy seq tools here and with 
  [clj-re](https://github.com/dtenny/clj-re) you can easily implement
  this. Regexes are beyond the scope of clj-coll.  Might be worth putting
  into the `:CLJ-COLL-USER` and `:CLJ-USER` package though.  TBD.
  
- `stream-reduce!`
  Probably needs to be bound up with some useful typed streams package.
  I'm not sure how to make it work on vanilla CL streams.
- All string-specific operations, e.g. clojure.string/ `join`/`escape`/`split`
  but perhaps most importantly `str`, which requires some attentive care 
  via generic functions.  That means also that `spit` isn't provided,
  because `spit` is implemented interms of `str`. String stuff would
  benefit from being a separate CL package/project.
- Array interfaces.
  `*-array` (e.g. `make-array`, `byte-array`), 
  `a*`(e.g. `areduce`, `amap`, `alength`)
  There's a bunch of Clojure array functions/macros to create
  and modify arrays. However they don't really add syntactic or semantic
  power to CL.

