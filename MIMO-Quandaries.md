# Goals

This is not recommended reading, it was just hashing out some lines of
thought during the design process.

CLJ-COLL has several driving goals:

1. Clojure APIs on immutable collections/sequences should behave like
   Clojure. Top priority, and generally 100% met within the limits of
   what's possible for any single API. 

   Caveats being `apply`, `=` (vs `EQUAL?`), and object equality as noted
   in README.md.  Also collections and keywords as predicates, see `coll-fun`.

2. M functions (see README.md) as a contract by which Common Lisp data types will be
   returned for namesake Clojure APIs. We're about 90% on this goal.
   In some cases there just doesn't seem to be a need, particularly for
   non-M-function APIs that support transducers.
 
3. Performance.  Let's talk about that below.

4. Seamless integration of CL collections with Clojure collections, 
   pass what you like to the APIs, get the right answer. We're mostly
   there, and you have options as to whether you want immutable results or
   mutable results (by way of Clojure APIs vs M functions).

Note that 'performance' is higher priority than 'seamless integration',
that's deliberate, at least for the M functions.

# Performance tricks

There are a number of ways to achieve performance, generally speaking, on
Clojure operations.

## Type-specific iteration code

One way is to use collection-type-specific iterator logic, for example 

    (LOOP FOR X ACROSS VECTOR) ; no funcalls, no SEQ/NEXT/FIRST generic functions

instead of heavily consing seqs on vector such as:

    (LOOP FOR S = (SEQ V) THEN (NEXT V) 
          WHILE S
          AS X = (FIRST S))

CLJ-COLL uses type-specific techniques for common operations such as
`reduce`, `run!`, and `doseq`.

## Use efficient accumulators

    (LOOP AS X = ... COLLECT X)

Will likely be faster than conjing onto a persistent list or vector,
though a persistent list should come close, it will stil have more
function call overhead.

## Use mutable collections 

Your mileage may vary here. But returning any CL collection type is likely
to be more speed and space efficient than the immutable alternatives.

The M functions try to be smart about this, since they are not only
contractually obligated to return CL types, but they have no obligation to
uphold certain silly semantics, such as `partition`'s semantics of
returning a _realized lazy list_ for each partition.

## Destructive collection update ...

... instead of allocating and populating new collections.

Here we get to the crux of many dilemmas in the CLJ-COLL API design. Here
are the multiply abused metaphors, horns of the dilemmas, designs caught
between the rock and the hard place.

### Dilemma #1: Mutate an input argument, or make a copy?

TL;DR: mutate it, the caller has control options.

It isn't a dilemma if you know you don't want the "old" pre-update
collection, in that case it makes sense to mutate the input collection.
Further, the user has control, if they don't want the input updated, they
can make a copy beforehand.

It also isn't a dilemma if the API is one of those things which need to
cons new data derived from some input, which is most Clojure sequence
functions, e.g. `interpose`, `interleave`, and so on. In fact nearly all
CLojure Sequence APIs return new immutable collections, whether the input
was mutable or immutable.

### Dilemma #2: Should mutating functions be named "N-fun" or "fun!"?

Common Lisp inconsistently uses the letter N as a prefix for functions
which may modify their inputs (e.g. `nreverse` but not `sort`).

Clojure similarly inconsistently uses a suffixed exclamation mark `(!)` to
indicate that functions have side effects up to and including mutation
(e.g. `swap!` but not `doall`).

We don't want to rename existing Clojure APIs even to correct them.
We could rename M functions, which convention do we use?  `mnreverse` is
getting ridiculous.

Let's defer the answer on this until after we consider the next item.
However note that there is only one M function in the whole suite that mutates.

### Dilemma #3: Should clojure APIs mutate mutable inputs?

This one's the gorilla.

As an example, consider `rename-keys`.  If you call it with immutable
inputs, it behaves identically to Clojure, no animals will be harmed by the
operation. But what if you call `rename-keys` on a mutable CL:HASHTABLE?

You could make a copy of the hash table, but for mutable data types that
could be a _lot_ of overhead just to rename a key or two, and it's totally
unnecessary if the keys in the map of keys to rename aren't even present in
the map whose keys are to be renamed.

You could try to be smart about it, and only copy if it's updated, but then you
have to document it, the documentation is more confusing, and the caller
may need to check for whether they got back a map which is `EQ` to the input,
versus relying on it being `EQ` to an input hashtable.

We could make an `mrename-keys` function whose specific intent is to allow
update-in-place on mutable collections.  Only ... we've previously made a
design decision to allow `assoc` to update mutable read tables that are
input so this seems inconsistent.  Do you really want to have to `massoc`
as well?  We normally save M functions for things which generate "seqs",
"lazy seqs", and other Clojure constructs which can't be interpreted by
purely CL package functions.

After much tail chasing, the following decisions were made on dilemmas 2 & 3:

* Dilemma 3: It was decided to let some of the Clojure APIs
mutate the mutable inputs.  It saves a lot of thinking and code
duplication, and "feels right" most of the time when using it. It doesn't
conflict with clojure because you can't do this (pass mutable structures
in except as iterator-seqs) with clojure in the first place.
The short list of such operations is documented in README.md in the section
labeled "Mutating APIs".

* Dilemma 2: Having decided to allow some Clojure APIs to update mutable inputs instead
of copy them, we can't rename them to "N-foo" or "foo!" because then they
wouldn't be the same function name as Clojure's, which on the whole we like
(`=` and `apply` notwithstanding).

## MIMO: Mutable In, Mutable Out  (not really a thing)

Originally this was going to be our catch phrase. However having now
completed the work on the first pass of CLJ-COLL, it's clear it does NOT
apply to so-called "sequence APIs".

Sequence APIs are happy to take mutable or immutable inputs, and nearly
always create a new immutable result.  And it is for this series of
functions we created M functions, which always create a mutable Common Lisp
collection for the same inputs as the Clojure namesake.  Whether or not the
input was mutable is irrelevant.

The collection APIs (on the left side of your Clojure CheatSheet), may
function more along the lines of mutable-in mutable-out, YMMV.

# Summary

ALL the CLJ-COLL APIs will accept mutable or immutable inputs.

* Sequence APIs return immutable results.
* M functions versions of sequence APIs return mutable CL collection results.
* A very limited number of functions that are "logically mutating" APIs
  only physically mutate a collection if it was a mutable input.
