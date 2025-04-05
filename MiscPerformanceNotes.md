# Miscellaneous notes related to performance

## Efficiency vs abstraction, to SEQUENCE or not to SEQUENCE?

And similar questions of "to TAKE and DROP, or not to TAKE and DROP".

Clojure code is elegant, concise, generally admirable from the standpoint
of brevity.  But if you're reading this, you're probably of the Common Lisp
persuasion, or at least open to it, and one of the things many Common
Lispers care about is efficiency.  Which is a longwinded way of saying that
sometimes CLJ-COLL uses ugly code.

For example, the Clojure version of PARTITION-BY uses TAKE and DROP in such
a way as to make for brevity.  However that same logic turns O(n) sequence
traversals into O(2n) traversals.  CLJ-COLL chose not to do that.

### The SEQUENCE function

A recurring frustration when implementing APIs with transducer arities
(which is most of the ones returning lazy sequences) is that you basically
have to code the aglorithm twice, once in transducer form, and once in
lazy-sequence form. Sometimes this is trivial but sometimes it just feels
like a waste of time.

Take DEDUPE.  Clojure does this nice elegant thing where the lazy-sequence
arity simply uses the DEDUPE transducer with the SEQUENCE function to
provide lazy semantics. The entire lazy-sequence implementation of DEDUPE becomes
`(sequence (dedupe) coll`.  Really nice, not least because the logic is one
of those things with an bug-prone edge case you can implement in multiple
ways, and you are spared doing it twice.

Nevertheless, SEQUENCE isn't free. And traversal of Clojure seqs
is also not free, there is a cost to using them.  Most of them cons for
every item visited in a sequence.  So it was time to do a little _entirely
unoptimized_ benchmarking on a work in progress where basically none of
CLJ-COLL has had any performance analysis or tweaking.  You can see these
numbers in the dedupe-notes.txt file.

Most of the DEDUPE scenarios tested are using standard 'seq' abstractions
to iterate over the vector, i.e. `for s = (seq coll) then (next s) while s`.
This does not optimize for vector access and will cons proportional
to the number of elements in the input collection.

In comparing DEDUPE numbers (vs MDEDUPE), there's also the cost of creating and
realizing all the elements of the lazy sequence it returns as part of the
timed content. This isn't chunked or otherwise efficient either.

TL;DR: Use of SEQUENCE for a lazy-seq arity resulted in a 68% slowdown.
CLJ-COLL probably shouldn't use it as a shortcut to implementation.

    Comparison of DEDUPE on a million number vector (precomputed, not part
    of the cost measured), where ~10% of the elements can be deduped.
    Everything was done with defualt compiler settings on SBCL on a Linux
    laptop. 

    Here's the data definition:

    (defparameter *nums* (make-array 1000000 :initial-element 0))
    (loop for i from 0 below 1000000 do (setf (svref *nums* i) (random 10)))

    There are 900265 numbers after deduping.

    (loop repeat 3 do (time (count (dedupe *nums*))))

    USING SEQUENCE: (best of 3)
    Evaluation took:
      0.266 seconds of real time
      0.265270 seconds of total run time (0.249244 user, 0.016026 system)
      [ Real times consist of 0.095 seconds GC time, and 0.171 seconds non-GC time. ]
      [ Run times consist of 0.095 seconds GC time, and 0.171 seconds non-GC time. ]
      99.62% CPU
      611,310,424 processor cycles
      219,201,504 bytes consed

    WITHOUT USING SEQUENCE: (best of 3)
    Evaluation took:
      0.158 seconds of real time
      0.157995 seconds of total run time (0.147957 user, 0.010038 system)
      [ Real times consist of 0.033 seconds GC time, and 0.125 seconds non-GC time. ]
      [ Run times consist of 0.032 seconds GC time, and 0.126 seconds non-GC time. ]
      100.00% CPU
      363,673,300 processor cycles
      147,226,608 bytes consed

    >>>> From the above, we can see that there was about a 68% increase in cost <<<<
    soley from using the SEQUENCE machinery, a fair bit of which was consing.
    147M bytes consed in the second case is the lazy-seq created, and the
    cost of seq traversal of the input.

    Then let's look at MDEDUPE, which eliminates the cost of the lazy
    sequence as well as creating a result you can traverse in pure CL code.
    Here are four flavors of MDEDUPE, none of which are necessarily what's
    in the currently released version, just as a test of various internal
    tooling and as data for design decisions.
    
    (loop repeat 3 do (time (count (mdedupe *nums*))))

    Option A, cons a list result, and use seq abstractions to traverse coll (best of 3)
    Evaluation took:
      0.053 seconds of real time
      0.053049 seconds of total run time (0.052966 user, 0.000083 system)
      100.00% CPU
      121,753,824 processor cycles
      60,807,952 bytes consed

    Note absence of GC impact on times in the `time` output.
    MDEDUPE's returning a CL:LIST instead of a lazy sequence yielded a 66%
    time savings compared to the previous best case for the lazy DEDUPE.
    Note that the benchmark test traverses the full lazy-seq or cl:list
    to perform a COUNT, and this is part of the timing.

    Option B: cons a list, use CLJ-CO:: internal iterator traversal instead of seq traversal
    Evaluation took:
      0.039 seconds of real time
      0.038456 seconds of total run time (0.030572 user, 0.007884 system)
      97.44% CPU
      88,302,620 processor cycles
      28,821,888 bytes consed

    Option B does away with using the 'seq' abstraction and instead uses an
    internal iterator that does a better job optimizing for traversal of
    diverse collection types.  All other things being equal using
    a slightly more optimized traversal of the input collection needed
    us a 26% improvement over the use of 'seqs' by option A.

    Option C. Return a adjustable vector same size as coll, use iterator
    Evaluation took:
      0.029 seconds of real time
      0.029394 seconds of total run time (0.029300 user, 0.000094 system)
      100.00% CPU
      67,413,280 processor cycles
      8,000,016 bytes consed

    Option C exchanges consing the returned CL:LIST for returning
    a single vector of same size as the input vector (but with fewer elements).
    An adjustable/fill-pointered array was used, which is slower for array
    access than simple vector (i.e. you can tweak it harder if you want to).
    
    The consing of option C is pretty much just the return vector, and 
    the overall dedupe logic and now O(1) COUNT behavior was 
    25% faster than the version the returned a CL:LIST.

    Option D. Eliminate consing entirely, mutate the input vector,
    return a count of surviving deduped elements. 

    This test was done on a simple-array copy of the original data without 
    adjustable/fill-pointer properties. Though without compiler
    optimization settings and use of SVREF you're not getting full
    speed.

    Evaluation took:
      0.025 seconds of real time
      0.025469 seconds of total run time (0.025370 user, 0.000099 system)
      100.00% CPU
      58,441,852 processor cycles
      0 bytes consed

    Zero bytes consed, music to my ears.

So here are the takeaways:

* Using SEQUENCE as a short cut to lazy-seq arities of functions with
  transducers costs quite a bit more, 68% in this test.
* Using MDEDUPE to return a CL:LIST instead of a lazy sequence
  saves an additional 66% runtime.
* Using an internal iterator instead of the seq abstractions
  saves an additional 26%
* Returning a vector instead of a CL:LIST saves an additioanl 25%.

From Clojure-style DEDUPE lazy-seq via SEQUENCE to vector returning MDEDUPE
with all savings above (Option C) means took time from 0.266 -> 0.029
seconds elapsed time. That's an order of magnitude improvement.

It's also part of the rationale for the M functions in CLJ-COLL. You have
options.  Clojure abstractions at increased cost, or CL-friendly versions
of the APIs that are leaner and meaner.  No judgement, just choices.

Note that over time the some of layers will be improved, whether it's the
lazy seq implementation, the 'seq' abstractions used to traverse
collections, or just the code that implements `equal?` for structural
equivalence. Very little was optimized at the time of this writing. That
said, there's no intention of implementing Clojure's style of chunked lazy
sequences.
    
In thinking about performance, remember that Common Lisp's generic function
capabilities are meant to be much more dynamic than Clojure's, many of
which are compiled java code. This comes at a cost, the upside being you
can redefine all kinds of things during development and it is a very
flexible and powerful tool for that.  At some point I may
use https://github.com/marcoheisig/fast-generic-functions to speed up core
aspects of the API that aren't particularly meant for CLJ-COLL users to
extend, such as CLJ-COLL::FIRST.  Feedback in advance is welcome.

#### Rabbit hole: Clojure DEDUPE numbers

This is just a quick and dirty REPL test, not a Criterium-jit-cache-warmed jvm.

Clojure `lein repl` on same box as above but without the proper controls
for any real validity. `lein repl` may have disabled JVM optimization, 
and tool like Clojure's Criterium should be used to warm up the JIT /
cache.

clojure 1.12:

    (def nums (into [] (repeatedly 1000000 #(rand-int 10))))
    (count (dedupe nums)) ;=> 899564
    (dotimes [i 3] (time (count (dedupe nums))))
    
    Best of three: "Elapsed time: 107.52775 msecs"

Clojure's lazy DEDUPE and seq traversals are more than twice as fast as
CLJ-COLL's (with or without CLJ-COLL's use of SEQUENCE). Considering
CLJ-COLL has had no optimization work of any kind (or compilation
settings) and is a work in progress, I can live with that.

The test also differs in that the input in CL was a CL vector, not an
FSet:seq. FSet:seq traversal may further slow things down in CL, I'm
guessing it can't be as fast as `aref` on general vectors, but perhaps I'm
doing it a disservice.  It's wild speculation on my part.

