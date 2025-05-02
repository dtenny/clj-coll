# CLJ-COLL: looks like Clojure, tastes like Common Lisp!

This is a Common Lisp implementation of Clojure's APIs for collections,
seqs, and lazy-seqs.  It provides immutable Cons, Queue, PersistentList,
capabilities as well as Vector, Set,
and Map analogues built on [FSet](https://github.com/slburson/fset) (but
accessed entirely via Clojure APIs).

`CLJ-COLL` is intended to give a "most naturally integrated" experience of Clojure
APIs and immutable data structures within a Common Lisp environment, and to make
Common Lisp more approachable to Clojure programmers. If you're a developer
who regularly writes both Common Lisp and Clojure, this library is for you.

This is _not_ a Clojure implementation. There is no `def`, `defn`, `fn`,
or Clojure-style destructuring. Similarly, there no
Clojure-compatible LOOP/RECUR support, use CL:LOOP or other favored
CL iteration mechanism. Real FP programmers don't LOOP anyway :-)
CLJ-COLL does provide a DOSEQ as well as the full range of Clojure compatible
map/reduce/transduce behaviors.

If your goal is to have a full-on Clojure language implementation implemented in
Common Lisp, consider [Cloture](https://github.com/ruricolist/cloture) or
some other tool, though frankly you may as well use [Clojure](https://clojure.org).

## Contents

* [Clojure APIs provided](#clojure-apis-provided)
* [What else is in the box besides collection and seq APIs?](#what-else-is-in-the-box-besides-collection-and-seq-apis)
* [Things that aren't in the box that you can obtain elsewhere](#things-that-arent-in-the-box-that-you-can-obtain-elsewhere)
* [Usage](#usage)
  - [Try it out: clj-coll-user package](#try-it-out-with-the-clj-coll-user-package)
  - [Print hash-tables or other data structures like Clojure](#print-hash-tables-or-other-data-structures-like-clojure)
  - [Enable read syntax for hashtables (with or without immutable maps)](#enable-read-syntax-for-hashtables-with-or-without-immutable-maps)
  - [Selective CLJ-COLL access](#selective-clj-coll-access)
  - [Recommended: The Whole Enchilada](#recommended-the-whole-enchilada)
* [Running unit tests](#to-run-the-unit-tests)
* [Readtable syntax support](#readtable-syntax-support)
  - [Note on named-readtables and the REPL](#note-on-named-readtables-and-the-repl)
* [Comparisons and equality](#comparisons-and-equality)
  - [`defstruct` and `defclass` instance comparisons](#defstruct-and-defclass-instance-comparisons)
  - [Use `EQUAL?`, not `=` for CLJ-COLL equivalence testing](#use-equal-not-for-clj-coll-equivalence-testing)
* [Keywords and keyed collections as predicates](#keywords-and-keyed-collections-as-predicates)
* ["M functions"](#m-functions)
  - [The complete list of M functions](#the-complete-list-of-m-functions)
  - [_Most_ M functions do not have (or need) transducer arities](#most-m-functions-do-not-have-or-need-transducer-arities)
* [What works well, what doesn't](#what-works-well-what-doesnt)
  - [Works well](#works-well)
  - [A bit rough](#a-bit-rough)
  - [Missing capabilities](#missing-capabilities)
  - [Differences from Clojure (mostly non-API)](#differences-from-clojure)
* [Notable semantics & cautions](#notable-semantics-cautions)
  - [Mutating APIs](#mutating-apis)
  - [APIS that reject CL:HASH-TABLEs](#clhash-table-rejecting-apis)
  - [Syntax Caveat: don't quote](#evaluation-environmentresult-is-not-identical-to-clojure)
* [Provided named readtables](#named-readtables-provided-by-clj-coll)
* [Vector/set/map print representations](#print-representations-of-vectors-sets-and-maps)
  - [Printing of mutable vs. immutable maps and lists](#printing-of-mutable-vs-immutable-maps,-and-immutable-lists)
* [Emacs tips for brace-delimited form travel](#emacs-tips-for-brace-delimited-form-travel)
  - [API differences](#api-differences-from-clojure)
* [Lisp implementations tested](#tested-lisps)
* [CLJ-COLL non-goals](#clj-coll-non-goals)
* [Related projects and differences from CLJ-COLL](#related-projects-differences-from-clj-coll)
* [Upward/future compatibility](#a-word-on-upwardfuture-compatibility)
* [Frequently asked questions](#frequently-asked-questions)
* [Additional reading - including deferred APIs](#additional-clj-coll-readingnotes)
* [Feedback welcome](#feedback welcome)

## Clojure APIs provided

At the time of this writing, the first release of CLJ-COLL exports 257
functions, of which 16 are [M functions](#m-functions).  There are a
small handful of capability predicates and miscellaneous things which are
not in Clojure, but most of the functions are straight out of Clojure's
`clojure.core` and `clojure.set` namespaces. 

The [Clojure Cheatsheet](https://clojure.org/api/cheatsheet), which by the
way has an _excellent_ [downloadable version](https://jafingerhut.github.io/cheatsheet/clojuredocs/cheatsheet-tiptip-cdocs-summary.html),
is your friend here. It summarizes all the APIs grouped by
area of functionality, offers popup doc strings, and click-through
descriptions with examples at [Clojuredocs.org](https://clojuredocs.org).

Nearly every Clojure 1.12 collection and sequence API is present in
CLJ-COLL. There are just a few that aren't (and are documented in this
README). (Note that at last glance the cheatsheet is missing 1.12 functionality).

CLJ-COLL also exports a bunch of miscellaneous Clojure functions, e.g `inc` and
`dec`, `odd?` and `even?` as well as higher order function helpers like
`comp`, `juxt`, and so on.  They moslty just wrap Common Lisp functions that do the
same thing and are purely for your clojure-cognitive convenience, ensuring
that formal parameters match Clojure's APIs.

If you don't want to use the cheatsheet, refer to the CLJ-COLL package
export list (which is annotated and occasionally tabulated) in 
`package.lisp`.  Every exported function has a doc string.  And of course
there's the CL `apropos` function to help you find things.

## What else is in the box besides collection and seq APIs?

* Full interoperability with Common Lisp collection types CL:LIST,
  CL:VECTOR (which implies strings), and CL:HASH-TABLE.  Clojure APIs like `doseq`
  or `filter` will work with these collections as well as immutable collections.
  
  Multidimensional and specialized Common Lisp arrays are NOT
  interoperable, though of course they're still available to you because
  it's Common Lisp, yay!

* All the core unsorted immutable collections. Lists, vectors, maps,
  sets, and queues.

* Lazy sequences and full `seq` support on all collections.

* All of Clojure's core transducers as of the time of this writing.

* Optional printing support for CL hash-tables and immutable collections so
  collections will print similarly (if imperfectly) to Clojure's print style.

* Optional read syntax via [named readtables](https://github.com/melisgl/named-readtables)
  so you can type `{:a 1 :b 2}`, `#{1 2 3}`, and `[1 2 3]` to your heart's content.
  Note that syntax doesn't extend to Clojure's way of treating commas as
  whitespace, you can't use commas that way in Common Lisp.

* A clojure-like equality/equivalence predicate `equal?`
  (instead of Clojure's `=`), so you can compare vectors to lists to seqs
  with wild abandon in those unit tests.

* Some trivial Clojure functions to make sharing code back and forth
  between Clojure and CL a bit easier, e.g. `inc` and `dec`.

* Some trivial higher order function support, e.g. `comp`, `partial`,
  `juxt`.

* A set of corresponding ["M Functions"](#m-functions) which emulate lazy
  Clojure APIs but always return eager and mutable CL collection types.

### `queue` - a constructor function for queues

Who can resist the elite practice of requiring clojure Queues to be
constructed by references to `clojure.lang.PersistentQueue/EMPTY` and a
bunch of additional `conj` calls?

We can. Being as there's no java code that provides
`clojure.lang.PersistentQueue/EMPTY` in Common Lisp, you can use either the
`CLJ-COLL:*EMPTY-QUEUE*` constant to construct your queues as in Clojure, 
or you can use the `queue` function and save yourself a lot of typing.

## Things that aren't in the box that you can obtain elsewhere

If you're reading this, you may also find these other Clojure functionality
packages useful.  They are not required to use `CLJ-COLL`, but
should work well with it.  Each of the packages below were designed
to closely adhere to Clojure semantics, though they predate CLJ-COLL
and may return multiple values in one or two cases where Clojure would
return persistent vectors.

* [Clojure compatible arrow macros](https://github.com/dtenny/clj-arrows)
  (which is bundled in the `:CLJ-COLL-USER` system provided alongside CLJ-COLL).
* [Clojure concurrency APIs](https://github.com/dtenny/clj-con)
* [Clojure compatible regular expression APIs](https://github.com/dtenny/clj-re)

# Usage

`CLJ-COLL` is available via Ultralisp or via [github](https://github.com/dtenny/clj-coll).

If you didn't get this via quickload using a quicklisp/ultralisp repo, add it to
your `~/quicklisp/localprojects/` directory and update or delete the
`system-index.txt` file (so it will be re-built), and then you can quickload it.

    (ql:quickload :clj-coll) ; to use the code

There are a number of ways you might choose to use the CLJ-COLL package.
[Jump ahead to the recommended usage](#4-recommended-the-whole-enchilada) or
use bits of CLJ-COLL a la carte.

## Try it out with the CLJ-USER package

If you just want to play around with CLJ-COLL, try the `CLJ-USER`
package (which is in the `CLJ-COLL-USER` _system_).

    (ql:quickload :clj-coll-user)
    (in-package :clj-user)
    (named-readtables:in-readtable clj-coll:readtable) ; if you're not using Slime
    {:a 1} ; => {:a 1} - congratualtions, you just created an immutable map

Note that you must invoke the `in-readtable` if your repl interaction is a
simple terminal session.  Slime knows how to save you the trouble, perhaps
Sly does too.

The CLJ-USER package pulls in CLJ-COLL, and the clojure arrow macros so you
can start banging away at Clojure-like constructs.  Just remember to `#'`
your functions passed as arguments, and that you're using Common Lisp
variants of `let`, `cond`, and so on, which use parenthesis, not bracketed
vector syntax, for their form syntax.  CLJ-COLL is only about collections
and sequences, not so much Clojure's macros and special forms.

## 1. Print hash-tables or other data structures like Clojure

By default CLJ-COLL doesn't mess with your print methods for CL or FSet
data types.  If you would like them to print more like Clojure, you can
enable each types's printing individually or collectively as follows:

    ;; Use zero or more of the following to suit your collection printing tastes
    (enable-printing)        ; Enable pretty printing for maps/sets/vectors

    (enable-map-printing)    ; Enable pretty printing for maps only
    (enable-set-printing)    ; Enable pretty printing for sets only
    (enable-vector-printing) ; Enable pretty printing for vectors only

## 2. Enable read syntax for hashtables (with or without immutable maps)

CLJ-COLL doesn't mess with the global read-table.  You can used 
[named readtables](https://github.com/melisgl/named-readtables) to
individually or collectively enable CLojure reader syntax, e.g. `{:a 1 :b
2}` for hash tables. 

    ;; Use zero or more of the following to suit your (Clojure) syntax tastes
    (named-readtables:in-readtable clj-coll:readtable)        ; For all set/vector/map syntax

    (named-readtables:in-readtable clj-coll:map-readtable)    ; For map syntax only
    (named-readtables:in-readtable clj-coll:vector-readtable) ; For vector syntax only
    (named-readtables:in-readtable clj-coll:set-readtable)    ; For set syntax only

By default the hash-table syntax (`{}`) reader will create immutable maps.  If you
would rather forego immutable data types and have the reader syntax create
CL:HASH-TABLE objects, simply set or bind `CLJ-COLL:*DEFAULT-HASHMAP-CONSTRUCTOR*` 
to `CLJ-COLL:CL-HASH-MAP`.

By default the vector syntax (`[]`) reader will create immutable vectors.
If you want it to create mutable CL vectors, set or bind 
`CLJ-COLL:*DEFAULT-VECTOR-CONSTRUCTOR*` to `CLJ-COLL:CL-VECTOR`.

There is no equivalent for defaulting Clojure set syntax (`#{}`) such that
it creates mutable CL data set representations. Set representations in
'pure' CL are simply CL lists.

See [Note on named-readtables and the REPL](#note-on-named-readtables-and-the-repl) 
of you use your REPL from a terminal instead of via Slime.

## 3. Selective CLJ-COLL access

V1.0.1: This has been made easier.

There are three main subsets of the CLJ-COLL interfaces that provide useful
functionality.

### Common Lisp functions that have been shadowed.

CLJ-COLL functions that shadow CL namesakes, e.g. `cons`, `first`, and so on, that
enable CLJ-COLL to traverse a _much_ wider selection of sequence types,
including Common Lisp sequence types.  The trick here is that if your
package definition "uses" the CL package, you'll need to do a
`(shadowing-import-from :clj-coll ... list of symbols)`.

If you want these functions, in your package, you can now succinctly put
this in your package definition as follows:

    (defpackage :my-package
      (:use :cl :clj-coll)
      (:shadowing-import-from :clj-coll . #.(clj-coll:cl-symbol-names))
      ...)

There's no need for you to elaborate all the relevant symbols to shadow,
using dotted lisp notation and read-time eval it's just one line. 

In the above example, the remaining clj-coll symbols are inherited into
MY-PACKAGE via the `:USE` directive.  You could also import selective
symbols with the `defpackage` `:IMPORT-FROM`  directive (see next section)

### CLJ-COLL functions that do not shadow CL functions

If you want all the symbols in CLJ-COLL, you can perform `:USE` with the
`:SHADOWING-IMPORT-FROM` as shown in the preceding section.

However what if you want most but not all symbols?  For example, say you
want all the CLJ-COLL symbols _except_ `cons`, `list`, and `list*`.  For
this you can use the `CLJ-COLL:NON-CL-SYMBOL-NAMES` with the `:exclude`
option. This lists all CLJ-COLL symbol names _that do not shadow CL
symbols_ (minus optional exclusions) and you can import them as follows:

    (defpackage :my-package
      (:use :cl)
      (:import-from :clj-coll . #.(clj-coll:non-cl-symbol-names 
                                   :exclude '(:cons :list :list*)))
      ...)

Note that the `clj-coll:cl-symbol-names` that returns symbol names to be
shadowed accepts an similar exclusion argument.

### Import just the CL compatible functions in CLJ-COLL ("M functions" and more)

Perhaps you'd like to use the subset of CLJ-COLL functions which will only
return types (collections or otherwise) that pure Common Lisp can
understand.  In this way you can take advantage of many features
without entiering the realm of immutable data.

This is partly what the "M functions" will do, as they always return CL
collection types.  However there's a large number of Clojure "Collection
APIs", like `walk`, `transduce`, and `select-keys` and so on, which return CL data
types and not Clojure-style seqs or lazyseqs (assuming you feed them CL
collections as inputs and not mutable collections).

The subset of CLJ-COLL that functions are useful this way are named by the 
`CLJ-COLL:CL-COMPATIBLE-SYMBOL-NAMES`, which returns all the M functions as
well as compatible collection and other symbols. It does not return any CL
package shadowing symbols, which, sadly, means it does not include `count`,
`get`, `assoc`, and `dissoc`, you'll need to do shadowing-imports for those
if you want the CLJ-COLL versions, or simply use the `CLJ-COLL:` package
prefix to access them.

Example usage:

    (defpackage :my-package
      (:use :cl)
      (import-from :clj-coll . #.(clj-coll:cl-compatible-symbol-names))
      ...)

This function also provides an exclusion option if there are some of those
symbols you don't want to import.

Invoke `CLJ-COLL:CL-COMPATIBLE-SYMBOL-NAMES` or examine its source code
for an exhaustive list of symbols returned.

If you are using the readtable syntax assists for vectors and maps,
you may wish to configure them to return mutable CL collections instead
of immutable collections, as follows:

    (setf clj-coll:*default-hashmap-constructor* #'clj-coll:cl-hash-map)
    (setf clj-coll:*default-vector-constructor* #'clj-coll:cl-vector)


## 4. [Recommended] The Whole Enchilada

If you're a fairly frequent Clojure programmer, this is the recommended package 
setup for your package:

    (defpackage :my-package
      ;; Use clj-coll and other optional goodies
      (:use :cl :clj-coll :clj-arrows)
      ;; Let clj-coll shadow some CL symbols for maximum immutable data immersion.
      ;; You can always call the CL stuff with a `CL:` prefix.
      (:shadowing-import-from :clj-coll . #.clj-coll:cl-symbol-names))

    (in-package :my-package)
    (named-readtables:in-readtable clj-coll:readtable) ;all syntax read assists
    (enable-printing) ;all syntax print assists

The above has been done for you in the form of the `:CLJ-COLL-USER` ASDF
system, which you can use with `quickload`. It defines a `CLJ-USER`
package, imports everything from CLJ-COLL, shadows the necessary CL package
symbols, sets up the named readtable with Clojure syntax, and printing to
resemble Clojure's. So you can experiment a bit with it if you quickload
`:CLJ-COLL-USER` and `(in-package :clj-user)`.

When using a REPL in the above package, you'll be able to use all the APIs,
all the set/map/vector/queue syntax, and all the APIs pretty much as you would in
clojure, except that it handles all the CL collection types as well.
See `package.lisp` for a full set of symbols exported by CLJ-COLL.
You'll still need to `#'` your functions when using them as arguments
though, i.e. `(filter #'odd? coll)`, not `(filter odd? coll)`.  Tip: 
you can run `(filter #'odd? coll)` in Clojure, and so copy some forms
directly from CL to Clojure for comparison purposes.

Note that named-readtables have package-local effect.  If you use
`in-readtable` in :CL-USER to enable map syntax, that doesn't mean it will
work in another package unless and until you perform an `in-readtable` in
that package.

For more on reading and printing see [readtables](#named-readtables-provided-by-clj-coll) and
[printing](#print-representations-of-vectors-sets-and-maps) sections later
in this document.

### A word on the Serapeum package

[Serapeum](https://github.com/ruricolist/serapeum) is a very popular lisp
utility collection which has many Clojure-inspired functions. There are a
number of symbols it exports that collide with CLJ-COLL symbols, these are
the ones known at this time on which you will either need to shadow or
otherwise avoid duplicate imports if you wish to `:USE` both `:CLJ-COLL`
and `:SERAPEUM`:

    DROP, JUXT, RANGE, TAKE, CONCAT, NTHREST, DISTINCT, QUEUE, FREQUENCIES,
    PARTIAL, FILTER, DROP-WHILE, FNIL, TAKE-WHILE, PARTITION.

Use whichever suits your needs, bearing mind that Serapeum's namesakes
may perform the same function but will not operate on or return CLJ-COLL types.
That said, they may be quite a bit more efficient.

It is recommended you prefer `CLJ-COLL:FNIL` over Serapeum's, as the Serapeum
version has a known [bug](https://github.com/ruricolist/serapeum/issues/185).

## To run the unit tests

    ;; To run the tests
    (ql:quickload :clj-coll-test)
    (clj-coll-test::run-tests)

# Readtable syntax support

Map, set, and vector syntax is provided via
[named readtables](https://github.com/melisgl/named-readtables).  For
example:

* Maps: `{:a 1 :b 2}` 
* Sets: `#{1 2 3}`
* Vectors: `[1 2 3]`

The use of any syntax is optional, you can also create your own read-tables
with only select syntax, e.g. vectors without sets, via single-function
readtables that may be used as mixins, descrbed further below.

Each collection type has a special variable which is used to create
collections of the appropriate type, but which you can rebind
to create other types of collections, including mutable Common Lisp
collections. The default behavior is to create immutable data structures
when the syntax assists are used.

Choices of readtable syntax support does not in any way affect the printed
representations of objects.  Print representations are discused later.

## Note on named-readtables and the REPL

Something to be aware of if you use the lisp repl without Slime (and
perhaps Sly, unknown to the author).

Say you have a lisp file/package you've loaded which invokes
`(named-readtables:in-readtable clj-coll:readtable)`
in the context of some package X (such as the
`CLJ-COLL-TEST` package).

If you are using the Slime repl and you execute `(in-package :x)`, Slime
will nicely ensure that the package's `in-readtable` context is in effect for your
repl so that you can invoke the modified syntax.

However if you're using a lisp repl from a terminal, none of the vendor
specific repls tested from a terminal did that. So this package<->readtable
observance for repls is a Slime benefit. 

# Comparisons and equality

CLJ-COLL relies on FSet to provide the implementations of immutable sets, maps, and
vectors.  CLJ-COLL provides the implementations for immutable lists and
queues, and all seqs and lazy-seqs. Everything else is Common Lisp

TL;DR: Beware mixing mutable and immutable types, particularly hash-tables
of either sort, and know that associative things with keys tend to use
CL:EQUAL-ish semantics.

Clojure relies on a number of things in its various equality and
comparison predicates, starting with the java Object.equals() method
which is used in part when you use Clojure's `=` predicate, as well as
all the numerical tests such as `== < > <= >=`.
 
Perhaps unknown to casual Clojure programmers is Clojure's `equiv` logic.
It is `equiv` that let's you compare a sequence represented by vectors
with a sequence represented by lists, for example.  While Common Lisp's
`equal` and `equalp` will do certain flavors of structural equivalence
between CL collections, they won't do anything for user packages like the
immutable classes used by CLJ-COLL.

CLJ-COLL attemps to approximate the equivalence logic of Clojure's `=`.
Collections of different types which have similar sequential or
associative behaviors and similar content will compare equal.
E.g. CL:VECTOR will compare in a sane way with an immutable vector or
elements of a CLJ-COLL/Clojure 'seq'. This is mostly
straightforward (though not terribly performant) but there are a few caveats.

Equality tests have the following limitations at this time:

* Most CLJ-COLL APIs _that do not involve hash-table and set key
  comparisons_ will use CLJ-COLL:EQUAL? for equality, which is similar to
  Clojure. Otherwise key comparison tends to resemble CL:EQUAL.
* CL:HASH-TABLE objects instantiated CLJ-COLL APIs will specify CL:EQUAL as
  the equality predicate.
* FSET:MAP keys are only compared with FSET:EQUAL?, which is more liberal
  than CL:EQUAL, but not as liberal as CLJ-COLL:EQUAL?. Notably, it will
  not do structural equivalence comparisons of CL:HASH-TABLE objects with
  FSET:MAP types (so don't use a CL hash-table as a key in an FSet map).
* FSET:SET keys are only compared with FSET:EQUAL?
* FSET:EQUAL?, and this is important for FSET:SET and FSET:MAP collections
  only uses EQ on CL:HASHTABLE objects.  This basically precludes
  use of CL:HASH-TABLE inputs on the clojure.set namespace functions
  which look for "rels" (sets of maps).  CLJ-COLL will signal an error
  if mutable inputs are used for incompatible operations on "rels".

The key restrictions apply mostly when you're populating collections.  If
you're comparing one collection/seq to another with CLJ-COLL:EQUAL?,
CLJ-COLL:EQUAL? is used for all types except those provided by FSet
(immutable maps, sets, and vectors).

Here are rules of thumb about what is being compared:

* FSET:EQUAL? knows how to compare fset collections and contents of fset
  collections, but devolves to something approximating CL:EQUAL for
  anything that isn't an FSET data structure.

* CLJ-COLL:EQUAL? knows how to compare based on structural equivalence of
  collections, but you can't use this for hashtable/map or set keys right now.

* CL structure-object and standard-object instances
  be compared with EQUAL in both FSET:EQUAL?
  and CLJ-COLL:EQUAL? (meaning it's really an EQ test), though you can
  widen the CLJ-COLL:EQUAL? logic for these objects through some special
  variables described in the next section.

The effect of this is that if you use only scalar or CLJ-COLL immutable
types, then CLJ-COLL key semantics are close to Clojure's.

If you want mutable hash tables that accept `CLJ-COLL::EQUAL?` then more work
needs to be done, such as using https://github.com/metawilm/cl-custom-hash-table
(`:cl-custom-hash-table` in quicklisp, and the basis for `:generic-cl.map`).

Your choice of lisp implementation may also allow you to use
`CLJ-COLL:EQUAL?` as a key equality predicate (SBCL has such support),
however CLJ-COLL does not try to make use of this, that's up to
you. Feedback / discussion is welcome, it would be nice to throw off the
shackles of `CL:HASH-TABLE` test limitations. The longer range plan is
(perhaps) to
use something like
[cl-custom-hash-table](https://github.com/metawilm/cl-custom-hash-table)
for portable and CLJ-COLL:EQUAL? capabilities.

For an example of map/set key comparison behaviors, look at the `contains?`
unit test in `clj-coll-test.lisp`.

Note for CLJ-COLL purposes strings are treated as scalar
values, with cautions about mutating them. Fset treats them similarly,
aiding in Clojure-like semantics.

Implementation note: FSET does not allow specification of user-supplied
equality predicates for its MAP and SET implementations at the time of this writing,
if did we could supply the more liberal `CLJ-COLL:EQUAL?` predicate to it for
key comparisons.

## `defstruct` and `defclass` instance comparisons

TL;DR: `defstruct` and `defclass` instances are compared by identity, like
Clojure, but CLJ-CON provides you a number of probably-bad-idea options to
change that if you want to.  *TBD*: whether they're worth the added machine
instructions they require.

While Clojure and CLJ-COLL compare maps by value (of content), Clojure a
bit quirky when it comes to `defrecord` types.

If you compare a map to a Clojure `defrecord` or `deftype` instances, the
test is an identity test.  Both `defrecord` and `deftype` result in new
types, and instances of those types result in equality tests via identity.

Clojure APIs generally treat defrecord instances as if they were maps. For
example you can `assoc` and `dissoc` it like a map. Enter the quirks for
record types.  If you `assoc` a new key that wasn't in the `defrecord`
declaration, you will get back a new instance of the record type with the
added key.  However if you `dissoc` a key that was in the `defrecord`
declaration, you get back a map that is not of the record type, and will
compare by value with other maps instead of only by identity.

We _could_ imagine Common Lisp `defstruct` to be like `defrecord` if we
wanted to, we could further imagine `defclass` to be an analogy to
`deftype`. But we don't, though it might be interesting to let `assoc` and
`dissoc` work on Common Lisp object instances (again, we don't for now,
though that could be really useful for stylistic functional programming reasons).
We could also imagine objects to be comparable to maps & hash-tables, but we don't for now.

CLJ-COLL allows you to extend the manner in which equality is computed on
struct/class isntances as follows (noting that such behavior is not at all 
like Clojure):

1. Bind `CLJ-COLL:*COMPARE-OBJECTS-BY-VALUE*` to true, in which case the
   behavior when comparing `defstruct` and `defclass` instances
   with _other instances of identical type_ is by comparisons based on
   the values and/or boundness (e.g. `slot-boundp`) of all slots in the instance.

2. Specify function designators for `CLJ-COLL:*STANDARD-OBJECT-EQUALITY-FN*`
   to compare standard-object instances, or
   `CLJ-COLL:*STRUCTURE-OBJECT-EQUALITY-FN*` for structure-object instances.
   These variables default to NIL, meaning use the default `CLJ-COLL::EQUIV?`
   comparison semantics (which defaults to `EQ` unless option 1 above is used).

3. define CLJ-COLL::EQUIV? methods on the class or structure types you care
   about, or redefine them for STANDARD-OBJECT and STRUCTURE-OBJECT.
   Note that doing this will likely break the CLJ-COLL-TEST unit tests, and
   any other dependency on default behavior by other dependencies you may
   have loaded using CLJ-COLL (an unlikely situation for most people).

## Use `EQUAL?`, not `=` for CLJ-COLL equivalence testing

Basically, where Clojure would use `=`, you should use `CLJ-COLL::EQUAL?`
if you want Clojure-esque equality semantics. Redefining an an `=` method
was just one step too many for the author's taste, which prefers to let
sleeping `=`'s lie and provide Common Lisp semantics.  The same logic is
why we didn't attempt to define `CLJ-COLL::EQUAL`, it carries too much
Common Lisp baggage. Granted this is inconsistent with all the other
methods shadowed by CLJ-COLL, it was a matter of degree and the particularly sensitive
semantics, for example, we didn't want anybody to think they could specify
a `CLJ-COLL::EQUAL` predicate as a Common Lisp hash table test.

# Keywords and keyed collections as predicates

## Description

Unlike Common Lisp, Clojure lets you use keywords, sets, and maps as
functions, i.e. in the function position of an sexp.  Most of them allow
'not-found' arguments too, except for use of symbols.

   Keywords: (:a x)         == (get x :a)
             (:a x :no)     == (get x :a :no)
   Maps:     ({:a 1} x)     == (get x :a)
             ({:a 1} x :no) == (get x :a :no)
   Sets:     (#{:a} x)      == (get x :a)
             (#{:a} x :no)  == (get x :a :no)
   Symbols   ('a x)         == (get x 'a)    ;*NO* not-found parameter, can't use 'nil

This is not something you're going to be able to do in Common Lisp, though
in some lisps you could set the symbol-function of symbols in the keyword
package, but it isn't portable.

We _could_ make life a bit more clojure-like in some cases by automatically
turning keywords, maps, and sets into predicates on all clojure APIs that
accept predicates. **BUT THIS HAS NOT BEEN DONE**

If we did that, then you could could invoke CLJ-COLL APIs like this example:

    (filter #{:a :b} '(:c :b)) => (:b)

Very clojure-like indeed.

We would transform the data types and pass on the appropriate function.
So a set `s` passed to `filter` would be transformed as

    (let ((f (collection-lookup-function #{:a :b} :none)))
     ... use F ...

 where F is approximated as

   (lambda (item) (get #{:a b} item :none))

Non-keyword symbols would not be usable as predicates, in part
because symbols already represent function-designators and because
they only work in clojure in the function position which we can't do
in CL. 

Right now there is a funciton `CLJ-COLL:COLL-FUN` which does the datatype
to predicate conversions if you would like to use it.  However this has not
been incorporated in all the APIs for performance sanity reasons. It was
felt that if every API that takes a predicate or function has to check for
conversion with `COLL-FUN` there would be too many checks.  Perhaps this
can be automated (in upwardly compatible fashion) in the future but for now
it needs a bit more thought.

# "M Functions"

Functional programming and immutable data structurs are fine until they
aren't. Maybe you want to CL:APPLY a function to an actual bona fide
CL:LIST, or `loop for x in (some-funciton-returning-a-cl:list)` because you
love CL:LOOP. Or maybe the immutable data structures are just too slow for
a particular problem.

When you want Clojure API functionality but want a Common Lisp collection
from it, use an M function, which will be named the same as the Clojure
function except for an 'm' prefix.

The M functions generally have the same argument signatures as their
CLojure API counterparts except that:

- Most do not have transducer arities.
- Most accept a optional arguments that let you specify if you want a
  CL:LIST or CL:VECTOR result, and generally default to CL:LIST return values.

In order to give CL:SEQUENCE typed returns (which encompasses CL:LIST and
CL:VECTOR), all M functions are of necessity _eager_ functions that will
process all values in the seqs or collections they receive as input, so be
careful not to pass any infinite lazy sequences!

## M functions are sometimes more efficient

M functions try to be more efficient than their lazy counterparts. 

One of the ways they do this is through structure sharing, where a CL:LIST
or CL:VECTOR input may share structure with the M function return value. Of
course immutable data structures try to do this too - with safer effects,
but sometimes fail for algorithmic reasons.

M functions which perform structure sharing on CL types always document the
behavior in the docstring.

Another way M functions may try to reduce consing is through the use of
iteration techniques that do not cons. ArraySeqs are notoriously consing
immutable seq abstractions.  Some of the M functions endeavor to be smarter
about it, since they are relieved of the obligation to return specific things like
"realized lazy sequences" (part of the `partition` Clojure contract), for example.

## Which Clojure APIs have M functions?

The criteria for whether or not to have an M version of a Clojure API
function is usually one of these things:

1. The API would return a lazy sequence.
2. The API would return an immutable result regardless of the input.

M functions try to offer an alternative to Clojure semantics such that the
resulting data is something that be processed by standard Common Lisp APIs.

### The complete list of M functions

Here are all of the M functions returning CL collection types.

The "NoVec Reason" is the rationale for not having a CL:VECTOR alternative.

"LAZY" means there just wasn't enough motivation to add what may be a
useful CL:VECTOR alternative for the first release.

"OKAY" means it the value proposition of a CL:VECTOR option seemed potentially dubious,
though you may gather from those labelled "OKAY?" that more contemplation is in order.

    M-function      Default   CL:VECTOR  NoVec   Comments
                    Return    option?    Reason

    mbutlast        CL:LIST   YES 
    mconcat         CL:LIST   YES
    mcycle          CL:LIST   NO         OKAY?
    mdedupe         CL:LIST   YES
    mdistinct       CL:LIST   YES
    mdrop           CL:LIST   YES                Also has :BEST result-type option
    mdrop-last      CL:LIST   YES
    mdrop-while     CL:LIST   YES
    mfilter         CL:LIST   NO         OKAY
    mflatten        CL:LIST   NO         LAZY
    minterleave     CL:LIST   NO         LAZY
    minterpose      CL:LIST   NO         LAZY
    mjuxt           CL:LIST   NO         OKAY?   JUXT produces immutable vectors.
    mkeep           CL:LIST   NO         OKAY
    mkeep-indexed   CL:LIST   NO         OKAY
    mkeys           CL:LIST   YES
    mmap            CL:LIST   YES                Optimized for single-collection use
    mmapcat         CL:LIST   YES
    mpartition      CL:LIST   YES                Uses KWARGS for _two_ result-type specs.
    mpartition-all  CL:LIST   YES                Uses KWARGS for _two_ result-type specs.
    mpartition-by   CL:LIST   NO         OKAY?   Two potential result-types like `mpartition`
    mremove         CL:LIST   NO         OKAY
    mrandom-sample  CL:LIST   NO         OKAY?
    mrange          CL:LIST   YES
    mrepeat         CL:LIST   NO         OKAY
    mrepeatedly     CL:LIST   NO         OKAY
    mreplace        <varies>  MAYBE      OKAY    `mreplace` is quirky because `replace` is quirky
    mreverse        CL:LIST   YES
    mshuffle        CL:VECTOR                    Matches `shuffle` semantics, no CL:LIST return
    msplit-at       CL:LIST   YES                Uses KWARGS for _two_ result-type specs.
    msplit-with     CL:LIST   YES                Uses KWARGS for _two_ result-type specs.
    mtake           CL:LIST   YES
    mtake-last      CL:LIST   YES
    mtake-while     CL:LIST   YES
    mvals           CL:LIST   YES

## _Most_ M functions do not have (or need) transducer arities

M functions generally take the same inputs as their Clojure namesakes and
produce mutable Common Lisp collections instead of immutable collections.
You can call them with all the usual CLJ-COLL supported collections
(mutable and immutable collections, and [lazy]seqs on them), you'll just
get a CL collection back instead of a lazy-seq or other immutable
collection type.

M functions generally do not benefit from transducer arities, you can turn
any transducer into one that returns a mutable collection by using an
appropriately mutable initial value (e.g. `(cl-vector)`) and `cl-conj` as
the reducing function instead of `conj`.  `cl-conj` creates CL:LIST and
CL:VECTOR where `conj` would create persistent lists and vectors.

However there are some APIs for which this isn't enough to get a fully
mutable set of CL data types.  For example, the `partition`,
`partition-by`, and `partition-all` APIs do not allow the caller to specify
the type of collection used for each partition in the result. Transducer
parameters can influence the result type of `transduce`, but not the result
type of the partitions.

M functions such as MPARTITION-ALL let you obtain common lisp collections for
results, (which you _can_ do with transducers) and common lisp collections
for partition values (which you _cannot_ do with transducers), and
sometimes offer transducer arities as well.

# What works well, what doesn't

Comparing CLJ-COLL to the Clojure collections APIs.

## Works well

* The API, still the same friendly API you know, extended to embrace Common
  Lisp data structures (lists, arrays, hash-tables).
* Immutable data structures too, yay!
* Seqs, lazyseqs, all seamless like in Clojure.
* Clojure collection syntax (`{}`, `#{}`, `[]`).

## A bit rough

* Inconsistent Clojure equality semantics, documented
  [below](#comparisons-and-equality).  This could be improved in future
  releases, it's just a SMOP (with classic connotations), though
  for native CL:HASH-TABLE improvements depend on your lisp implementation.

* Immutable data structure performance may not compare well to
  Clojure's in the current release for data structure intensive code. The
  current immutable data structures are not (necessarily) optimized for
  Clojure's use cases, vector additions in particular.
  
* There is no chunking of lazy sequences, and no intention of ever trying
  to make them faster with chunking. They have their uses, but you must weigh whether
  the cost justifies their use. Anecdotally, lazy seqs are very fast as is
  in CL.

## Missing capabilities

* 'transient' capabilities on immutable data structures.
* Clojure equality/equivalence semantics for CL:HASH-TABLE, and FSET-assisted
  implementations of immutable sets/vectors/maps.
* Sorted collections (sorted-set, sorted-map).  While you may observe some sorted behaviors
  in collections returned by hash-set or hash-map, do not count on them. CLJ-COLL
  doesn't support them yet.
* Thread-safe stateful transducers, although note that Clojure's claims of
  thread-safe transducers depend on what you consider "safe".  Simply using
  volatiles for state (as Clojure does) does not mean that Clojure's
  transducers will behave as expected if used by concurrent threads.
* No interfaces (CLOS mixins in this case) like ISeq. If you want to add
  more collections, it is done via generic functions. Seriously, do you
  really want an IDrop mixin in Common Lisp? Interfaces are for broken-ass
  OOP systems that aren't CLOS ;-)  That said, we may eventually need some interfaces
  for `iteration`, `eduction`, and a couple of other APIs, as mentioned previously.

## Differences from Clojure

* `=` is `CL:=`, `CLJ-COLL:EQUAL?` is its replacement.
* `()` is a an empty persistent list in Clojure, and is not `nil?`. However
  `()` in Common Lisp *is* `nil?`, and list syntax will result in standard mutable CL lists.
* Keyed collection equality semantics differ as described in the section on
  [Comparisons and equality](comparisons-and-equality).
  Otherwise CLojure structural equality semantics are very
  similar between Clojure's `=` and CLJ-COLL's `EQUAL?`.
* Clojure syntax for vectors/sets/maps behaves differently in _quoted_ contexts,
  described in more detail
  [here](evaluation-environment-result-is-not-identical-to-clojure).
* In Clojure you can `apply` functions to clojure collections in the
  trailing argument position, whereas `cl:apply` requires a `cl:list`.
  At this time, CLJ-COLL supplies a `clj-apply` function do emulate
  Clojure's `apply` instead of shadowing `cl:apply`. (See `clj-apply` docstring
  for reasoning). Feedback is welcome.
* Keywords and keyed collections as predicates aren't supported as
  syntactic predicates like Clojure does. However the `coll-fun` function
  can be used to turn keywords and collections into equivalent predicates you can 
  pass as functions.
* There are no Java libraries, or java interop to them.  Methods like
  `.indexOf` and `.lastIndexOf` instead have CLJ-COLL namesakes of
  `index-of` and `last-index-of`.
  
All documentation strings are careful to note any differences from Clojure
(to a point, they're not a substitute for reading this README file).

# Notable semantics & cautions 

General rules of thumb:

* When you call Clojure APIs and pass in CLJ-COLL immutable data
  structures, the semantics of the function should be 100% Clojure, subject
  to caveats such as that there are no Clojure/java type hierarchies.

* Mutable In, Mutable Out, for "collection" functions

  This applies mostly to "collection" APIs that are intended to operate
  on a collection vs a seq. If you pass a mutable collection in, you'll
  likely get a mutable collection out.  Unlike the "sequence" APIs which
  always return some type of 'seq', whether it's a lazy-seq, an ArraySeq,
  or what have you.

* Mutation

  If a Clojure API logically changes a colleciton/seq, and if you pass a
  mutable input to that function, your mutable collection may be changed.
  See [Mutating APIs](#mutating-apis)
  for a complete and blissfully short list things that mutate.
  
  Doc strings are also careful to say whether a given function mutates.

  For some boring discussion/rationales/design-decisions on mutation in the CLJ-COLL API
  see [Mutable data, destructive functions, API conventions](./MIMO-Quandaries.md).

* If you put mutable elements into immutable collections, they remain
  mutable but you should avoid such mutations, they will likely cause
  troublesome behavior.

  E.g. changing a mutable key (e.g. a string) in an immutable map "is an
  error", nothing good will come of it, but CLJ-COLL isn't going to detect
  it and give you a rational error (most of the time, there are _some_
  safeguards that detect mutations that affect seq behaviors).

* When CL hash-tables are created by `cl-hash-map` element equality is
  `cl:equal` until such time as we can provide maps that support `equal?`.
  Fset sets and maps use `FSET:EQUAL?` which is similar to `CL:EQUAL`.
  If you call `make-hash-table` yourself, then equality is up to you.

* On thread safety: immutable data structures and seqs should be thread-safe, but thus
  far no intensive CLJ-COLL testing has been done to validate that.
  
  Mutable data strutures have no thread-safety guarantees, you'll have to
  add concurrency controls to them if it matters.

* CLJ-COLL does not detect cyclic data structures for any operation,
  including EQUAL?.  The pretty printer _may_ catch them, it hasn't been tested.

* Stateful transducers are not thread-safe. Clojure's claim to be, but it
  would depend on your definition of thread safefy.  Just because they
  declare state using Java's `volatile` doesn't mean they will provide the
  desired semantics.  CLJ-COLL makes no pretenses, stateful transducers
  should not be shared across threads.

* Dotted pairs are dotted pairs, and nothing else.
  They aren't MapEntry representations. They aren't lists.
  They aren't vectors.  They will not meaningfully compare with anything
  except another dotted pair.  There is no CLJ-COLL API that will
  give you a dotted pair as output except for where they were input
  to the underlying collection.

* Many APIs will accept things which conform to java MapEntry
  semantics, which is to say that they represent a key/value pair.
  Such pairs may be expressed as as any 2 element list or vector 
  (mutable or immutable).  We are more liberal here than Clojure
  which does not allow lists to represent mapentries.

  CLJ-COLL APIs that return logical map-entry pairs will return cl:list
  values if the underlying hash-table was mutable, and persistent vectors
  if the source map was immutable.

## Mutating APIs

The core APIs that mutate (mutable) inputs, and on which other 'changing'
APIs are built are are as follows:

- ASSOC
- CONJ
- DISSOC (only on cl:hash-table)
- POP    (only on cl:vector)
- RENAME-KEYS
- MREPLACE (only if the input is a CL:LIST or CL:VECTOR)

Each of the above check `*MUTATION-CONSIDERED-HARMFUL*` and act
accordingly. It is exported, feel free to bind it if you are debugging
unexpected mutations, but running with it always set is not a supported activity.
See the variable docstring for more details.  Also note that if you pass
mutable collections in such that they become keys or other components of
immutable collections, and you change them after the fact, APIs will misbehave.
That's on you :-)

Other mutating APIs defined in terms of the above are:

- ASSOC-IN
- MERGE, MERGE-WITH
- RENAME
- REPLACE (only if the input is a CL:VECTOR)
- UPDATE, UPDATE-IN

Some of the APIs you'd think might be updating your mutable collections,
such as `update-keys`, `update-vals`, `dedupe`, and so on, do not actually
modify the collections they receive.  This is because they tend to build up
new collections (regardless of whether the inputs were mutable or
immutable).  This is also true of nearly all sequence APIs.  So the list of
mutating functions above is fairly short.

## Mutable vs immutable map example

Here we show the same APIs operating on immutable and mutable maps:

    (let ((m {:a 1}))
      (print (clj-coll:assoc m :b 2))
      m)
    {:b 2 :a 1}    ;printed structure is not EQ m
    => {:a 1}      ;m is unchanged

    (let ((m (serapeium:dict :a 1))) ; mutable Common Lisp hash table, EQUAL test
      (print (clj-coll:assoc m :b 2))
      m)
    {:b 2 :a 1}    ;printed structure was actually m, not new map
    => {:b 2 :a 1} ;m has mutated

## CL:HASH-TABLE rejecting APIs

Some of the `clojure.set` APIs will reject the presence of CL:HASH-TABLE
elements in so-called `rels`, as documented and restricted by the `index`
and `join` APIs.  This is because CL:HASH-TABLE membership in keyed FSet
collections are tested with `EQ`, which is pretty much a total fail for
using CL:HASH-TABLE objects as collection members in certain Clojure APIs.

Right now the 'no mutable hashtables in rels' restriction is enforced by a
structure traversing function called `require-immutable-rel`.  Hopefully
this is a temporary restriction, but it may require using home grown persistent
maps and sets (which would be a win in general so we can support
CLJ-COLL:EQUAL? in key semantics).

## `{}` `[]` `#{}` evaluation environment/result is not identical to Clojure

TL;DR: You can skip this, just don't `QUOTE` (`'`) collections using reader syntax.

It is a great boon to us Common Lispers that the formulators of the ANSI Common Lisp
specification provided user-definable readtable interfaces. These
capabilities let us formulate the Clojure map/set/vector
readtable syntax in a portable, standard-compliant way.

There are some limitations. For example you probably can't embed any of
`{}[]` characters in your symbol names if you're using the modified
read-tables. We can live with that (or avoid using those read-tables, since
named-readtables are very flexible, and _optional_, in their use.) However
there are some things about the Common Lisp environment that differ from
the Clojure model and and the syntax-assists for sets/vectors/maps are not
identical in all use-cases.

TL;DR: _quoted_ forms with embedded syntax won't have a chance to evaluate
the result of the reader macro, leaving you an unexpected and unevaluated S-EXP
where you expected a set/vector/hashtable.

There are two main implementation choices available to use to implement
the syntax reader macros for `{}`, `[]`, and `#{}`:

1. As a macro, returning an expansion to be evaluated after reading.
2. As construct evaluated at read-time to produce an object.

### Macro Expansion Downside

If we opt for the first behavior, a macro expansion, then there's this problem.

In Clojure you can write:

    '(1 2 3 {:e 4})

And the resulting list will have a map as its last value.  I.e. 

    (mapv type '(1 2 3 {:e 4}))
    => [java.lang.Long java.lang.Long java.lang.Long clojure.lang.PersistentArrayMap]

However if the reader macro returns `(FUNCALL *DEFAULT-HASHMAP-CONSTRUCTOR* :E 4)`
then the result of the expression read with the syntax in a quoted
environment will not work like Clojure:

    '(1 2 3 {:e 4})
    => (1 2 3 (FUNCALL *DEFAULT-HASHMAP-CONSTRUCTOR* :E 4))

To get the Clojure effect you need to use `(list 1 2 3 {:e 4})` 
instead of a quoted literal.

    (list 1 2 3 {:e 4}))
    => (1 2 3 {:e 4})

### Evaluated read-time construct problems

The good: if the reader processes the map creation at read time, then
literals like '(1 2 3 {:e 4}) will return the expected map in the resulting
list.

    '(1 2 3 {:e 4}))  ;`list` not required
    => (1 2 3 {:e 4})
    
The bad: evaluating the expression at read-time is problematic in that the code
you're reading may not have its environment sufficiently populated to do
useful read-time processing.

Anything you want to evaluate at read-time has to be defined at
read-time.  Mostly this means `*DEFAULT-HASHMAP-CONSTRUCTOR*`, and perhaps 
other constructs. So this won't work:

    (type-of (fourth (let ((*DEFAULT-HASHMAP-CONSTRUCTOR* 'serapeum:dict)) '(1 2 3 {:e 4}))))
    => CLJ-COLL::<immutable-map-type>

Which isn't what we wanted at all, it should have been a CL:HASH-TABLE, but
the reader hasn't seen the binding of `*DEFAULT-HASHMAP-CONSTRUCTOR*` at read-time.

Note that read-time evaluation is also not what clojure is doing either.  For example

    '(1 2 3 {:a (make-foo)})
    => (1 2 3 {:a (make-foo)})

Where the last value of that quoted list is
clojure.lang.PersistentArrayMap but its subexpressions were not evaluated.

Perhaps there's a way to do this, but it seems doubtful.  Clojure's
read-time environment is _the_ environment.  When literals are processed,
everything read before that is available in the environment. There is no separate
environment for compilation vs execution.  Of course that's also why you
have to be careful what you initialize in Clojure variables, but that's a
story for another day.

### Resolution: treat syntax like `{}` as macros, `{}` in quoted contexts will not work.

The current implementation assumes reader macro expansions will be evaluated.
If you need to put anything that isn't self-evaluating into your
syntax-assisted collection creations, make sure it isn't in a quoted
context, you need the syntax expression to be evaluated.

Bad: `'(1 2 3 {:a 1})` 
Good: `(list 1 2 3 {:a 1})`

In writing the CLJ-COLL tests this has not been particularly inconvenient.

# Named readtables provided by CLJ-COLL

Named readtables are terrific things. Among their features are the ability
to merge read-tables, mixin-style, into new or existing readtables. For
example you could define a read-table with the map and vector syntax, but
without set syntax.

`CLJ-COLL` provides the following exported readtables which may be used
whole with `NAMED-READTABLES:IN-READTABLE`, or with the readtable merging
behaviors of 
`NAMED-READTABLES:MAKE-READTABLE`,
`NAMED-READTABLES:DEFREADTABLE` (via `:FUSE` or `:MERGE` options), and
`NAMED-READTABLES:MERGE-READTABLES-INTO`.

* `CLJ-COLL:READTABLE`           (for `IN-READTABLE`) provides both vector, set, and map syntax.
* `CLJ-COLL:READTABLE-MAP-MIXIN` provides mergeable syntax for just maps.
* `CLJ-COLL:READTABLE-SET-MIXIN` provides mergeable syntax for just sets.
* `CLJ-COLL:READTABLE-VECTOR-MIXIN` provides mergeable syntax for just vectors.

## Readtable munging functions

In addition to the named readtables above, there are functions
you can use to clobber and unclobber existing read-tables as follows:

* `CLJ-COLL:ENABLE-MAP-SYNTAX`     - enables map read-table syntax
* `CLJ-COLL:ENABLE-SET-SYNTAX`     - enables set read-table syntax
* `CLJ-COLL:ENABLE-VECTOR-SYNTAX`  - enables vector read-table syntax
* `CLJ-COLL:DISABLE-MAP-SYNTAX`    - disables map read-table syntax
* `CLJ-COLL:DISABLE-SET-SYNTAX`    - disables set read-table syntax
* `CLJ-COLL:DISABLE-VECTOR-SYNTAX` - disables vector read-table syntax

# Print representations of vectors, sets, and maps

If you're using the reader syntax, it's common for clojure programmers to
like to print vectors/sets/arrays with the same syntax used for reading. 

It may not actually be readable if you use it in Common Lisp depending on
when and how you use the Clojure syntax in conjunction with CLJ-COLL, but
for Clojure programmers it is generally nicer to see the content of
collections rather than something like `#<SOME-VECTOR-TYPE 0xDEADBEEF>`.

CLJ-COLL by default does not enable special printing syntax, however you
can enable it by using the following functions:

* `CLJ-COLL:ENABLE-MAP-PRINTING`     - enable pretty printing for maps
* `CLJ-COLL:ENABLE-SET-PRINTING`     - enable pretty printing for sets
* `CLJ-COLL:ENABLE-VECTOR-PRINTING`  - enable pretty printing for vectors*
* `CLJ-COLL:ENABLE-PRINTING`         - enable pretty printing for maps/sets/vectors
* `CLJ-COLL:DISABLE-MAP-PRINTING`    - disable pretty printing for maps
* `CLJ-COLL:DISABLE-SET-PRINTING`    - disable pretty printing for sets
* `CLJ-COLL:DISABLE-VECTOR-PRINTING` - disable pretty printing for vectors(*)
* `CLJ-COLL:DISABLE-PRINTING`        - disable pretty printing for maps/sets/vectors

(*): `CLJ-COLL::ENABLE-VECTOR-PRINTING` only enables special printing for
immutable vectors.  Common Lisp vectors (and importantly, strings,
which are vectors), are left as is.  It's also useful to see `#(1 2)`
vs `[1 2]`, because you know the first is mutable, and the second is not.

Note that the augmented printing only works if `*PRINT-PRETTY*` is true,
such as by calling `PPRINT` (which binds the variable to true). The default
value is implementation dependent. (SBCL's is T, CCL's is NIL, for example).

## Printing of mutable vs. immutable maps, and immutable lists

The print methods attempt to print contents of collections such that you
can see the elements for debugging subject to `*print-length*`, and so that
you can, perhaps, read the printed representations back in, though little
time has been spent worrying about that.

### Printing lists

Given that Common Lisp and CLJ-COLL use the standard lisp list syntax to
read CL lists, we choose not to print the _persistent_ lists with the same
syntax.  Persistent lists are printed with `(list 1 2 ...)` intead of `(1 2 ...)`
so that you know when you're dealing with a persistent list. 

If you're in the CLJ-COLL package and/or using the shadows symbols then
`list` will create a persistent list. The printer deliberately eschews
printing the package qualified symbol (e.g. `(CLJ-COLL:LIST ...)`) because
it's more ugliness than I can stand, which means that persistent lists will
print as `(list 1 2 ..)` even if you print it from the `CL-USER` package.

### Printing maps

Similar to the issue of overloading print representations for lists, we
have the same issue for CL:HASH-TABLE vs persistent hash maps.

The CLJ-COLL map printer provided by `enable-map-printing` 
will print immutable maps using the reader syntax, i.e. `{:a 1 :b 2}`.
It will mutable cl:hash-table maps as `(cl-hash-table :a 1 :b 2)`,
`cl-hash-table` being a CLJ-COLL constructor for such maps.

## PRINT-OBJECT methods are not used by on CL collections, here's why

First, a note on what conforming Common Lisp programs may and may not do
with regard to customized printing and `PRINT-OBJECT`:

1. [22.4 PRINT-OBJECT](https://www.lispworks.com/documentation/HyperSpec/Body/f_pr_obj.htm)
   "Users may write methods for print-object for their own classes" with no
   mention of allowability for implementation classes/types.

2. [11.2.1.2](https://www.lispworks.com/documentation/HyperSpec/Body/11_abab.htm)
   "Except where explicitly allowed, the consequences are undefined if any
   of the following actions are performed on an external symbol of the
   COMMON-LISP package: ... 19. Defining a method for a standardized
   generic function which is applicable when all of the arguments are
   direct instances of standardized classes." 

More simply, defining PRINT-OBJECT methods on Common Lisp standard types
may lead to problems. Indeed, this was encountered trying to define the method
for CL:VECTOR types while developing CLJ-COLL.

Fortunately the venerable if flawed Common Lisp standard has not left us in
tne lurch, it privides for the `PPRINT-DISPATCH` mechanism which allows us to
not only define _pretty printing_ behavior for common lisp types, but also
allows you to override these if you like, without clobbering system
`PRINT-OBJECT` methods.

# Some boring notes on CLJ-COLL's immutable datatypes

Like Clojure, CLJ-COLL provides (directly or indirectly) immutable Cons
cells, PersistentList objects (which do _not_ use persistent Conses, and
which are O(1) countable unlike CL lists), and persistent sets, vectors,
and maps.

Do not expect a Clojure/CLJ-COLL Cons to behave like a Common Lisp cons.
They are not used the same way in Clojure, and CLJ-COLL goes to some
trouble to behave like Clojure when operating on immutable conses and
lists.  In Clojure, the immutable Cons is more like a "glue" datatype,
allowing you to string together multiple collections and [lazy]seqs into a
larger collection. 

And of course a persistent cons is the generally returned value from lazy
seq thunk.  Little known tidbit, you can return data types other than Cons
cells from Clojure (and CLJ-COLL) lazy seqs!

In terms of integration with CL, each of CL:CONS, CLJ-COLL:CONS,
CLJ-COLL:PERSISTENTLIST all act as "seqs", supporting `first`, `next`, and
`rest` behaviors.

# Emacs tips for brace-delimited form travel

Once you start using Clojure map syntax in your Common Lisp code, e.g.

    (let ((m {:a 1 :b 2 :c {:d 4}}))
       ...)

You may find that your stock `lisp-mode` form travel with `forward-sexp`
and related functions does not seem to treat braces as form delimiters.

A fix that seems to work is to put the following in `lisp-mode-hook` in
`.emacs`:

    (add-hook
      'lisp-mode-hook
      (lambda () 
        ; ... whatever you already have, if anything
        (modify-syntax-entry ?\{ "(}" lisp-data-mode-syntax-table)  ;for form traveling
        (modify-syntax-entry ?\} "){" lisp-data-mode-syntax-table)))

*TBD*, the following _may_ be useful as well:

    (modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)       ;for highlighting
    (modify-syntax-entry ?\} "){" lisp-mode-syntax-tabletable)


I'm not much for customizing emacs, please let me know if there are
problems with this and provide the proper solution to use. I also don't use `paredit`
or `smart-parens` or all those other tools and cannot avise on that.  Let
me know if you have tips.

# API differences from Clojure

CLJ-COLL functions that match CLojure functions are 100% compatible with
Clojure semantics, however the semantics are sometimes extended, i.e. a superset,
of Clojure functionality. However there are some semantic differences:

* clojure.set namespace APIs reside in the CLJ-COLL package, so instead of
  calling `(clojure.set/union ...)` it's just `(union ...)`.  Note that
  UNION shadows the Common Lisp function of the same name, as do many
  Clojure functions.

* There are no Clojure/java data types, so instead of returning, for
  example, a `clojure.lang.PersistentList`, a function may instead return a
  CLJ-COLL PersistentList (or other) type.  Normally you don't have to
  worry about this when using Clojure collection/seq APIs, it all just works.

* Set/Map membership tests have tighter equality semantics (as documented
  elsewhere in this file) than Clojure.

## APIs

Bearing in mind that we strive for more-or-less `EQUAL` comparison behavior
in all `CLJ-COLL` data APIs, the following functions create data structures
usable by the APIs.

* `hash-map`    creates a new immutable hash table
* `cl-hash-map` creates a new mutable hash table with `equal` equality
* `vector`      creates a new immutable vector
* `cl-vector`   creates a new mutable vector (adjustable and fill pointered)
* `hash-set`    creates a new immutable hash set
* `list`        creates a new persistent list
* `queue`       creates a new immutable FIFO queue, vs `clojure.lang.PersistentQueue/EMPTY`

# Tested Lisps

Testing done on Fedora 40 invoking `clj-coll-test:run-tests` in each lisp.
I've listed results in approximate decending order of success.

Tl;DR: SBCL, CCL, and ECL were champs. ABCL was okay,
and in an ironic twist, free versions of the _commercial_ lisps failed even
to load dependencies without running out of and/or corrupting memory.

If you have wisdom to share on Lispworks or Allegro Common Lisp let me
know, its also possible I failed to install or launch them, correctly. 


- SBCL 2.4.6 => EXCELLENT

- CCL 1.13   => VERY GOOD

  * SET-PRINTING was overly aggressive with `*PRINT-RIGHT-MARGIN*`.
    Cause not investigated. May have applied to map printing too.

  * Could not figure out how to have constant structs reliably used with EQ in CCL
    and had to change `+EMPTY-LIST+` and friends from constant values to
    `*EMPTY-LIST*` `defvar`s.

- ECL 24.5.10 => VERY GOOD

  * Just once, and I couldn't reproduce it, I got the environment in a
    state where these two tests started failing after working the previously.
 
         SET-PRINTING in TEST-SUITE []: 
              Unexpected Error: #<a SIMPLE-ERROR 0x7f3b19866480>
         Tried to modify a read-only pprint dispatch table: #<pprint-dispatch-table  0x7f3b241a07c0>.

         VECTOR-PRINTING in TEST-SUITE []: 
              Unexpected Error: #<a SIMPLE-ERROR 0x7f3b198666c0>
         Tried to modify a read-only pprint dispatch table: #<pprint-dispatch-table  0x7f3b241a07c0>.

- ABCL 1.9.2, OpenJDK 21.0.5 => OKAY

  * Structure comparison via MOP interfaces didn't work. As
    this is not a default operational mode of `CLJ-COLL:EQUAL?` I've disabled
    the structure comparison test for now with ABCL.

    Error was:
    ```"The value #(SYSTEM::DEFSTRUCT-SLOT-DESCRIPTION CLJ-COLL-TEST::A 0
    CLJ-COLL-TEST::TEST-STRUCT-A NIL T NIL) is not of type #<STANDARD-CLASS
    SYSTEM:SLOT-DEFINITION {4DA27116}>." on a call to `MOP:SLOT-DEFINITION-NAME`.```

    CLOS instance equivalence tests were also disabled until such time as
    the test code for standard-class and structure-class equality is split
    into discrete pieces.

  * ABCL also had a pretty messy bunch of warnings loading all the
    dependencies, but apparently nothing fatal for CLJ-COLL.

- Allegro CL Express 11.0 (`alisp` executable) => UNABLE TO TEST, MULTIPLE FAILURES

    I checked for a more recent Allegro versions, 11.0 has been out for a while, but
    there was nothing. I also keep looking for switches to increae memory
    to `alisp` but it doesn't respond to any --help or similar arguments
    and the online Franz docs are not helpful.  Somehow it's hard to
    believe it couldn't obtain the requested 64MB of memory noted below.
    
    Attempts to test CLJ-COLL failed while trying to load dependencies.
    First, loading `shinmera-random-state` failed with ```"Error: Attempt to
    take the value of the unbound variable `EXCL::.CASE-FAILURE'."```

    I skipped past that error seemingly without issue, only to have the lisp die
    while loading FSET-USER. With the error ```".Allegro CL(pid 667495):
    System Error (gsgc) Couldn't get 63438848 bytes from operating system
    The internal data structures in the running Lisp image have been
    corrupted and execution cannot continue."```

- LispWorks 8.0.1 Personal Edition. => UNABLE TO TEST, MEMORY FAILURE

    Couldn't even load the modest dependencies without running out of memory
    on the personal edition.

# CLJ-COLL non-goals

* Implementing the Clojure language or special form syntax (e.g. CLojure's destructuring) 
  is not a goal.  CLJ-COLL still relies on DEFUN, CL:DEFMACRO, CL:LET, CL:LOOP, and so on.
  It may be that CLJ-COLL gives you enough syntax and such to implement
  another library for all those other Clojure control forms and `[]`
  syntax, if you want to try.

* There is no attempt to make CLJ-COLL trivially extensible for arbitrary
  new collection types.  Defining the matrix of interoperation between many
  collection types, seqs on them, and equivalence, is a fairly nitty-gritty
  detail-oriented thing.` There's a reason there are so many assertions in
  the unit tests.

# Related projects, differences from CLJ-COLL

It seems to be a rite of passage for programmers who like both Clojure and
Common Lisp to implement varying amounts of Clojure in Common Lisp. The
reverse tends not to be true (implementing Common Lisp constructs in
Clojure), but kudos to the people who implemented 
[symbol-macrolet](https://github.com/clojure/tools.macro) in CLojure.  :-)

Two notable projects Clojure-in-CL projects are:

* [Cloture](https://github.com/ruricolist/cloture) An implementation of
  Clojure in Common Lisp.
* [Clclojure](https://github.com/joinr/clclojure) An experimental port of
  Clojure to Common Lisp.
  
Both of these are shooting for bigger goals than `CLJ-COLL`.

# Frequently asked questions

1. Q: How do I turn various collection types into CL:LIST types?
   A: Use `mconcat`, or consider an appropriate M function for the call which
      generated the non-cl:list collection in the first place.

2. Q: How do I use transducers to produce mutable collection results?
   A: Use the appropriate mutable 'init' value to `transduce` or `into`, and
      `cl-conj` as your reducing function.

Astute readers will wonder why the initial release of CLJ-COLL claims to
have frequently asked questions.  It's because these are questions &
answers for which the author had to remind himself repeatedly during the project.

# A word on upward/future compatibility

Careful though will be given to making sure changes to CLJ-COLL don't break
your code that uses it.  However CLJ-COLL would like to reserve the
following changes to the API for future use:

* Any public API in Clojure may have a corresponding exported symbol added
  to CLJ-COLL.
* Any CLJ-COLL API that directly corresponds to a Clojure API may alter
  behavior and become incompatible if it fixes a bug. I.e. if CLJ-COLL
  accepts somehting that it shouldn't have (and that Clojure would reject),
  CLJ-COLL may be fixed to be compatible with CLojure without flagging it
  as a major release update.
* Where there are potentially breaking changes, a note will appear in 
  [Changelog.md](./Changelog.md).
* Any API that has "mutable in, mutable out" semantics should expect that 
  the API _may_, at its discretion, mutate the mutable inputs. Doc strings
  attempt to detail the exact behavior in this regard, but there may be a
  few misses in the this 4 week old release.

In general your code should be future proof so long as it isn't defining
other symbols that conflict with Clojure's API, and so long as it doesn't
rely on broken behavior that gets fixed.

# Additional CLJ-COLL reading/notes

* [Deferred or rejected APIs](./deferred-or-rejected-apis.md)
* [Future Work](./future-work.md)
* [Implementation/performance notes](./performance-notes.md)
* [Mutable data, destructive functions, API conventions](./MIMO-Quandaries.md)
* [Miscellaneous performance notes / speed tests](./MiscPerformanceNotes.md)

# Feedback welcome

gitrepo-feedback@protonmail.com
