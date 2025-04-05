(in-package :clj-coll)

;;;; Constructors, conversion, and capability predicates for persistent data structures.
;;;;
;;;; There are a number of Common Lisp packages which might do this.
;;;; Those considered so far:
;;;;
;;;; - FSet (in quicklisp) 
;;;;   https://github.com/slburson/fset
;;;; - persistent-tree-map (NOT in quicklisp - perhaps abandoned for his hashtrie?)
;;;;   https://github.com/DanielKeogh/persistent-tree-map
;;;; - https://github.com/DanielKeogh/hashtrie - has support for 'transient' behavior too
;;;;   Now in quicklisp.
;;;; - darts.lib.hashtrie & darts.lib.wbtree for unsorted and sorted maps (in quicklisp)
;;;;   Was using this as a first pass, but then I wanted persistent vectors which it doesn't have
;;;;   and switched to FSet.
;;;; - 'hashtrie' in quicklisp, is eq https://github.com/DanielKeogh/hashtrie?
;;;;
;;;; We use whatever seems to get us closest to Clojure immutable map behavior with the least
;;;; baggage (and, ideally, compatible licenses).
;;;;
;;;; To be considered an implementation has to be available in quicklisp,
;;;; I didn't want people to have to download third party libs by hand.
;;;;
;;;; See also apis.lisp for non-constructor or higher level APIs that 
;;;; implement things like `assoc` on the data structures bridged here (and on Common Lisp
;;;; hash-tables).
;;;;
;;;; For immutable vectors:
;;;; - FSet (in quicklisp) 
;;;;   https://github.com/slburson/fset - its 'seqs' are persistent vectors I think
;;;; - https://github.com/DanielKeogh/persistent-vector (not in quicklisp)
;;;; - https://github.com/joinr/clclojure (not in quicklisp or actively maintained)
;;;;   :clclojure.pvector perhaps, its persistent map says its incomplete.
;;;; - https://github.com/ruricolist/cloture (not in quicklisp)
;;;;   (uses FSet, potentially with mods which may or may not be of value)
;;;; - ?? 'lil' (in quicklisp a.k.a. fare-lisp-interface-library - also has maps)
;;;;   ?? or 'fare-utils' couldn't find "pure" vectors

;;;
;;; FSET notes: 
;;; (fset:domain <map>) returns set of keys
;;; (fset:range <map>) returns set of values
;;; (fset:image <x>) is basically cl:mapcar on <x>
;;; (fset:convert <to-x> <from-x>S) ; ubiquitous conversions
;;; (fset:reduce fn map ...) ; fn called with three args: accumulator k v, see also `do-map`

(eval-when (:compile-toplevel :load-toplevel) 

  (defconstant +immutable-map-type+ 'fset:map
    "If you need to reference the type of immutable map supported by CLJ-COLL,
use this constant.   You may need to `#.` it depending on need.

Hard coding third-party persistent types used by CLJ-COLL may result in breakage
if CLJ-COLL moves to a different persistent type implementation (though you'll
still need to recompile your code if the underlying dependencies change).")

  (defconstant +immutable-set-type+ 'fset:set
    "If you need to reference the type of immutable set supported by CLJ-COLL,
use this constant.   You may need to `#.` it depending on need.

Hard coding third-party persistent types used by CLJ-COLL may result in breakage
if CLJ-COLL moves to a different persistent type implementation (though you'll
still need to recompile your code if the underlying dependencies change).")

  (defconstant +immutable-vector-type+ 'fset:seq
    "If you need to reference the type of immutable vector supported by CLJ-COLL,
use this constant.   You may need to `#.` it depending on need.

Hard coding third-party persistent types used by CLJ-COLL may result in breakage
if CLJ-COLL moves to a different persistent type implementation (though you'll
still need to recompile your code if the underlying dependencies change)."))

;;;
;;; Conj (with overlapping MERGE capabilities) early in our dependency cycle.
;;;

(defun key (map-entry)
  "Return the key of a MapEntry representation (some sequence with two values)."
  (first map-entry))

(defun val (map-entry)
  "Return the value of a MapEntry representation (some sequence with two values)."
  (second map-entry))

(defun map-entry-p (obj)
  "True if OBJ is a logical MapEntry being a list/vector (immutable or mutable)
of exactly two elements representing a map key and value.

Dotted conses and SEQS of any kind are NOT valid mapentry representations.
Note that we liberally diverge from Clojure by permitting list representations
as MapEntries, clojure does not allow this."
  (typecase obj
    (cl:cons (if (atom-cdr-p obj) nil (= 2 (cl:length obj))))
    (persistentlist (= 2 (count obj)))
    (cl:vector (= 2 (cl:length obj)))
    (fset:seq (= 2 (fset:size obj)))
    (t nil)))

(defun map-entry? (x)
  "([x])
Return true if x is a map entry, that is to say a key/value pair.

Any length-2 general purpose sequential collection is treated as a map entry,
e.g. [:a 1], '(:a 1).

Unlike Clojure, CLJ-COLL doesn't provide any type of MapEntry type.
Unlike Common Lisp, we don't allow dotted conses as key/value pairs.
Seqs on collections are not valid map entries."
  (map-entry-p x))

(defun alternative-map-entry (pair)
  "Given a cl:list or fset:seq type with two values, turn one into the other.
So (:a 1) <=> [:a 1].  Used only under control of *map-entry-comparison-hacks-enabled*.

This function is used to return canonical values of MapEntries
in APIs returning MapEntries, and always returns a new pair.

Assumes `map-entry-p` is true of pair and has already been performed."
  (etypecase pair
    ;; Mutable -> Canonical immutable form
    (cl:list    (vector (car pair) (cadr pair)))
    (cl:vector  (vector (aref pair 0) (aref pair 1)))
    ;; Immutable -> Canonical mutable form
    (fset:seq (cl:list (first pair) (second pair)))
    (PersistentList (cl:list (first pair) (second pair)))
    ;; CLJ-COLL:CONS - could be a list, but for now it is not map-entry-p.
    ))

(defvar *map-entry-comparison-hacks-enabled* nil
  "True if we want to engage in hacks to better compare alternative representations
of pairs representing possible map entries.")

(defun validate-map-entry (obj)
  "Signal an error if an object used as a logical MapEntry is invalid. Return OBJ otherwise"
  (unless (map-entry-p obj)
    (error "~s is not a valid MapEntry representation, use a 2 element sequential collection." 
           obj))
  obj)
  
(defun map-entry-values (obj)
  "Return two values (via VALUES) being a map entry key and value. Signal an error if this cannot
be done"
  (validate-map-entry obj)
  (values (first obj) (second obj)))

(defgeneric merge-aux (target source)
  (:documentation "MERGE-AUX is for merging things into mutable hash-tables and mutable maps.
It is called by CONJ, but not by merge as the name might suggest, though it addresses
the behavior of maps which is identical in CONJ and MERGE.")
  (:method ((target cl:hash-table) (source cl:hash-table))
    (maphash (lambda (k v) (setf (gethash k target) v)) source)
    target)
  (:method ((target cl:hash-table) (source fset:map))
    (fset:do-map (k v source) (setf (gethash k target) v))
    target)
  (:method ((target cl:hash-table) source)
    ;; MapEntry values to be put into hashtables/maps
    (multiple-value-bind (k v)
        (map-entry-values source)       ;signals error if invalid MapEntry source
      (setf (gethash k target) v))
    target)
  (:method ((target fset:map) source)
    (multiple-value-bind (k v)
        (map-entry-values source)       ;signals error if invalid MapEntry source
      (fset:with target k v)))  (:method ((target fset:map) (source cl:hash-table))
    (maphash (lambda (k v) 
               (setf target (fset:with target k v)))
             source)
    target)
  (:method ((target fset:map) (source fset:map))
    (fset:do-map (k v source)
      (setf target (fset:with target k v)))
    target)
  (:method ((target fset:seq) source)   ;vector as conj target
    (fset:with target (fset:size target) source))
  (:method ((target fset:set) source)   ;set as conj target
    (fset:with target source)))

(defgeneric conj-aux (coll xs)
  (:documentation 
   "Generic function that specializes on collection type for cases where `conj` is called
with a collection, vs the edge-case zero-arg call. There is always at least one
value in 'xs'.")
  ;; xs are known to be safe for cl:apply
  (:method ((coll null) xs)
    ;; (cons nil item) returns (item) - a PersistenList in Clojure
    (apply #'list xs))
  (:method ((coll cl:vector) xs)
    (check-mutation-barrier coll)
    (loop for x in xs
          as new-vec = (safe-vector-push-extend x coll)
          do (when new-vec (setf coll new-vec)))
    coll)
  (:method ((coll cons) xs)
    (loop for x in xs
          do (setf coll (cons x coll))) ;clojure compatible ordering
    coll)
  (:method ((coll cl:cons) xs)
    (loop for x in xs
          do (setf coll (cl:cons x coll)))
    coll)
  (:method ((coll persistentqueue) xs)
    (loop for x in xs
          do (setf coll (pq-cons x coll)))
    coll)
  ;; emptylist isa ipersistentlist and is handled there.
  (:method ((coll ipersistentlist) xs)   ;persistentlist result
    (loop for x in xs
          do (setf coll (pcons x coll)))
    coll)
  ;; Lazyseqs and seqs result in PersistentList of xs
  (:method ((coll lazyseq) xs)
    (loop with result = (cons (first xs) (seq coll))
          for x in (cl:cdr xs)
          do (setf result (conj result x))
          finally (return result)))
  (:method ((coll seq) xs)
    (loop with result = (cons (first xs) coll)
          for x in (cl:cdr xs)
          do (setf result (conj result x))
          finally (return result)))
  (:method ((coll cl:hash-table) xs)
    ;; NOTE: (conj <map>|<ht> ...) behaves identically to (merge <map>|<ht> ...)
    ;; Xs can be MapEntry compatible tuples (as lists or fset:seqs), or HT/MAPs
    ;; Also, (conj <map> [[:a 1]]) gets an error: "Vector arg to map conj must be a pair"
    ;; And quirkily, (conj|merge <map>|<ht> '(:b 2)) fails where [:b 2] does not.
    ;; Incompatibility: We are more liberal and accept cons or cl:cons mapentries.
    (check-mutation-barrier coll)
    (loop for x in xs
          do (merge-aux coll x))
    coll)
  (:method ((coll fset:map) xs)         ;same caveats as HT target
    (loop for x in xs
          do (setf coll (merge-aux coll x)))
    coll)
  (:method ((coll fset:seq) xs)
    (loop for x in xs
          do (setf coll (fset:with-last coll x)))
    coll)
  (:method ((coll fset:set) xs)
    (loop for x in xs
          do (setf coll (fset:with coll x)))
    coll))

(defun conj (&optional (coll nil coll-p) &rest xs)
  ;; Can't use defgeneric, can't specialize on optional arguments
  "([] [coll] [coll x] [coll x & xs])
conj[oin]. 

Returns a new collection with the xs added.
xs are added at different places depending on the collection.
For lists, xs are prepended.  For vectors, xs are appended.

For FIFO queues, xs are placed at the end of the queue, in the order
specified (so the first x in Xs is closer to the head of the queue).

Common Lisp adds support for mutable CL collections as well as immutable collections.
What comes out of conj, mutability wise, depends on what went into conj.
If you pass a mutable CL collection as input, you'll get the same collection back.
An error may be raised if input CL vectors are not adjustable and fill-pointered.

(conj nil item) returns the immutable list (item), as clojure would.

(conj coll) returns coll.

(conj) returns [], a persistent vector.

Conj works with maps and hashtable too, in which case xs should be pairs of some kind, 
e.g. (:a 1) [:a 1] (:a . 1)

This function is fully Clojure compatible in its semantics.

See also: CL-CONJ for a version of CONJ that creates mutable CL types instead of immutable
CLJ-COLL types."
  (cond ((not coll-p) (vector)) ;(conj)
        ((not xs) coll)         ;(conj coll)
        (t (conj-aux coll xs))));(conj coll x ...)

(defun cl-conj (&optional (coll nil coll-p) &rest xs)
  "([] [coll] [coll x] [coll x & xs])

This is a Common Lisp counterpart to Clojure's CONJ.  However where CONJ
defaults to immutable list or vector creation, CL-CONJ creates Common Lisp lists or vectors.

The real utility of CL-CONJ is as a reducing function to be used with transducers
when you'd like to get a mutable Common Lisp collection out of the tranducing process
instead of a an immutable collection or seq.

(cl-conj nil item) returns the mutable CL:LIST (item).
(cl-conj coll) returns coll.
(cl-conj) returns a mutable CL:VECTOR.

CL-CONJ works with maps and hashtable too, in which case xs should be pairs of some kind
serving as logical MapEntry values e.g. (:a 1) [:a 1]"
  (cond ((not coll-p) (cl-vector))    ;(cl-conj)
        ((not xs) coll)               ;(cl-conj coll)
        ((null coll) xs)              ;(cl-conj nil x ...)
        (t (conj-aux coll xs))))      ;(cl-conj coll x ...)

(defun hash-table-pairs (ht &optional n)
  "Return N pairs for a mutable CL hash table in the form of a list of 2 element lists.
If the hash table has fewer than N keys, the resulting list will be shorter.
If N is nil, return all map entries.

I.e. (hash-table-pairs {:a 1} 2) => ((a 1)).
The result is NIL if HT is empty.

There are no ordering guarantees across calls.

See also: MAP-PAIRS"
  ;; Needs to return in order of maphash, not reverse consed order
  ;; otherwise (first (hash-table-pairs .. 1)) is going to equal (second (hash-table-pairs .. 2))
  (let ((n (or n (hash-table-count ht))))
    (serapeum:with-collector (result)
      (maphash (lambda (k v)
                 (if (> n 0)
                     (result (cl:list k v))
                     (return-from hash-table-pairs (result)))
                 (decf n))
               ht)
      (result))))

(defun map-pairs (map &optional n)
  "Return N pairs for an immutable map in the form of an immutable vector of 
2 element immutable vectors.

If the map has fewer than N keys, the resulting vector will be shorter.
If N is nil, return all map entries.

I.e. (map-pairs {:a 1} 2) => [[a 1]].
The result is NIL if MAP is empty.

There are no oredering guarantees across calls.

See also: HASH-TABLE-PAIRS"
  (if (empty? map)
      nil
      (let ((result (vector))
            (n (or n (fset:size map))))
        (fset:do-map (k v map)
          (if (> n 0)
              (setq result (conj result (vector k v)))
              (return-from map-pairs result))
          (decf n))
        result)))


#+(OR) ;unused?
(defun pairify (thing &optional dotted)
  "Given some THING representing a pair, return a single cons (if it isn't already) representing
a canonical pair of values.

THING is normally 2 element sequence (CL:CONS CL:LIST, CL:VECTOR, CLJ-COLL:VECTOR).
If it's more than 2 elements only the first two values are used.
If it's fewer than 2 elements NIL is used for the missing elements.

If THING is a list of 2+ elements, take the first two values of the list UNLESS
DOTTED is true, in which case the second value returned is the cdr of THING.
DOTTED is observed only if THING is a CONS whose CDR is another CONS.

If THING is a cons but not a list, the two values are the CAR and CDR of the cons.

Note that the convention in CLJ-COLL, like Clojure, that map-entries for immutable maps
are vectors of two elements.  For CL:HASH-TABLE values, the CLJ-COLL convention is for mapentry
representations to be proper lists of two elements.  Thus PAIRIFY does not return something
you can use for a CLJ-COLL MapEntry, it always returns a value to be interpreted as a single cons
whose CDR is the second value in the pair.  The reason being to a cheap and canonical
result from PAIRIFY for any collection input.

    (pairify '(1 . 2))   ;=> (1 . 2)  ;two values of a single cons, input is returned
    (pairify '(1 2))     ;=> (1 . 2)  ;two values of a list.
    (pairify '(1 2) t)   ;=> (1  2)   ;second value is a list
    (pairify '(1))       ;=> (1)      ;second value is nil, input is returned
    (pairify  NIL)       ;=> (NIL)    ;first & second value are nil
    (pairify #(1 2))     ;=> (1 . 2)  ;and similarly for immutable vectors
    (pairify (vector 1)) ;=> (1)      ;second value is nil
    (pairify #())        ;=> (NIL)    ;first and second value are nil

There are two cases where the input is returned, use care when mutating
the result.  All other cases return fresh cons cells."
  (if (null thing)
      (cl:cons nil nil)
      (if (consp thing)
          (let ((cdr (cdr thing)))
            (cond ((atom cdr) thing)
                  ((null cdr) thing)
                  ;; cdr is a cons
                  (dotted (cl:cons (car thing) cdr))
                  (t (cl:cons (car thing) (cadr cdr)))))
          ;; Vector of some type
          (cl:cons (first thing) (second thing)))))

;;;
;;; Constructors
;;;

(defun hash-map (&rest key-val-plist)
  "([] [& keyvals])
keyval => key val
Returns an immutable hash map with the supplied mappings. If any keys are
equal, they are handled as if by repeated uses of assoc."
  ;; All the FSet map creators seem to want key/val pair sexps, e.g. `(:a 1)`.
  ;; That goes for CONVERT as well, which calls `(WB-Map-Tree-With key val)`
  ;; Rather than cons an temporary alist from a KEY-VAL-PLIST 
  ;; we may as well just do the one-key-at-a-time loop similar to whaet `convert` does.
  ;; Note that fset:map ultimately turns into fset:with, so while it takes many args
  ;; there's no improvement in speed or added reason to cons pair sublists.
  (loop with m = (fset:empty-map)
        for (k v) on key-val-plist by #'cddr
        do (setq m (fset:with m k v)) ;no new map created if k/v already present
        finally (return m)))

(defun cl-hash-map (&rest key-val-plist)
  "([] [& keyvals])
keyval => key val
Returns a *mutable* hash map with the supplied mappings. If any keys are
EQUAL, they are handled as if by repeated uses of assoc."
  (loop with m = (cl:make-hash-table :test #'equal 
                                     :size (ceiling (cl:length key-val-plist) 2))
        for (k v) on key-val-plist by #'cddr
        do (setf (gethash k m) v)
        finally (return m)))

(defun vector (&rest vals)
  "[] [a] [a b] [a b c] ...
Creates a new immutable vector containing vals."
  (fset:convert 'fset:seq vals))

(defun cl-vector (&rest vals)
  "[] [a] [a b] [a b c] ...
Supports the same syntax as the VECTOR function, but CL-VECTOR creates
a *mutable* Common Lisp vector which is adjustable and fill-pointered, so that the CLJ-COLL
APIs approximating Clojure functionality will be efficient as values are added to or removed
from the vector."
  ;; Initial-contents must be same as length of array, grr.
  (let ((n (length vals)))
    (make-array n :fill-pointer n :adjustable t :initial-contents vals)))

(defun set (coll)
  "([coll])
Returns an immutable unordered set of the distinct elements of coll.

If coll is a set, it is returned as-is.
Empty collections and/or NIL return an empty set.

If coll is an immutable map or mutable hash-table, the set will consist
of MapEntry representations.

Note that the distinct key criteria of the set is presently
based on FSet:EQUAL?  This may be broadened to the more Clojure
compatible CLJ-COLL:EQUAL? in the future.

Examples:
    (set nil) => #{}
    (set [])  => #{}
    (set #{1 2}) => #{1 2} ;which is `identical?`/EQ 
    (set {:a 1 :b 2}) => #{[:a 1] [:b 2]}
    (set '(1 2 3 1)) => #{1 2 3}
"
  (reduce #'conj (hash-set) coll))       ;yay!

(defun hash-set (&rest vals)
  "([] [& keys])
Returns a new immutable set with supplied keys. Any equal (*) keys are
handled as if by repeated uses of conj.  

Clojure incompatibility:
Equality on hash-sets is presently defined by `FSET:EQUAL?` (which approximates EQUAL
semantics with structural equivalence) instead of CLJ-COLL:EQUAL?.  This may fixed at a
future date."
  (fset:convert 'fset:set vals))

(defun hash-thing-from-collection (hash-thing collection)
  "Given a cl:hash-table or fset:map, populate it by conjing in local mapentry entities
from the supplied collection."
  (reduce #'conj hash-thing collection))

(defgeneric convert (to-type from-obj)
  (:documentation "A generic CONVERT for CLJ-COLL used in preference to FSET:CONVERT
where we desire different semantics.  Some of these exist only for unit tests,
this function isn't meant to be exported.

Returns something appropriate to TO-TYPE.
If the type of FROM-OBJ is the same as TO-TYPE, FROM-OBJ is returned (does not create
a copy).

The main things we do in these methods are:
- eliminate dotted pairs representing logical MapEntry values, Fset gives out dotted pairs,
  we don't want 'em.
- immutable map inputs generate immutable vector of immutable vector k/v pairs when queried
- mutable hash-table inputs generate mutable lists of list k/v pairs when queried

CONVERT is not exhaustive in the type combinations is supports, nor is it a public function.
It's just a CLJ-COLL internal tool augmented as needed.")
  ;; To CL:LIST
  (:method ((to-type (eql 'cl:list)) (l cl:list))
    l)
  (:method ((to-type (eql 'cl:list)) (v cl:vector))
    (cl:coerce v 'cl:list))
  (:method ((to-type (eql 'cl:list)) (ls lazyseq))
    (loop for s = (seq ls) then (next s)
          while s
          collect (first s)))
  (:method ((to-type (eql 'cl:list)) (coll ipersistentlist))
    ;; (seq persistentlist) == persistentlist, so (convert to-type (seq coll)) is a nope
    ;; it will infinite loop
    (loop for s = (seq coll) then (next s)
          while s
          collect (first s)))
  (:method ((to-type (eql 'cl:list)) (coll seq))
    (loop for s = (seq coll) then (next s)
          while s
          collect (first s)))
  (:method ((to-type (eql 'cl:list)) (coll cl:hash-table))
    (hash-table-pairs coll))
  (:method ((to-type (eql 'cl:list)) (coll fset:map))
    ;; Can't call FSET:CONVERT, it's going to give us dotted conses
    ;; And `map-pairs` returns a FSET:SEQ, we want a CL:LIST
    (serapeum:with-collector (result)
      (fset:do-map (k v coll)
        (result (vector k v)))))

  ;; To CL:VECTOR
  (:method ((to-type (eql 'cl:vector)) (v cl:vector))
    v)
  (:method ((to-type (eql 'cl:vector)) (l cl:list))
    (cl:coerce l 'cl:list))
  (:method ((to-type (eql 'cl:vector)) (map fset:map))
    ;; Convert dotted pair mapentries from fset to immutable vector mapentries
    (cl-vec (map-pairs map)))
  (:method ((to-type (eql 'cl:vector)) (ht cl:hash-table))
    (cl-vec (hash-table-pairs ht)))
  (:method ((to-type (eql 'cl:vector)) (coll ipersistentlist))
    (cl-vec coll))
  (:method ((to-type (eql 'cl:vector)) (coll seq))
    (cl-vec coll))
  (:method ((to-type (eql 'cl:vector)) (coll persistentqueue))
    (cl-vec (seq coll)))
  (:method ((to-type (eql 'cl:vector)) (coll lazyseq))
    (cl-vec (seq coll)))

  ;; CHAOS below, fixme, pretty much just adding them as I need them
  (:method ((to-type (eql 'cl:hash-table)) (ht cl:hash-table))
    ht)

  (:method ((to-type (eql 'persistentlist)) (coll t))
    (list-from coll))

  (:method ((to-type (eql 'fset:seq)) (ht cl:hash-table))
    "Create an FSET:SEQ (a.k.a. clj-coll:vector) from a CL:hash-table type.
FSET:CONVERT won't do this at the time of this writing.
Returns an immutable vector, however the pairs in the vector are mutable 2-element lists
representing the hash table map entries, under the policy 'mutable in, mutable out'."
    (fset:convert 'fset:seq (hash-table-pairs ht)))

  (:method ((to-type (eql 'fset:seq)) (map fset:map))
    "Create an FSET:SEQ from an FSET:MAP type.
FSET:CONVERT generates the SEQ with dotted pair conses as members.
What we want is immutable vector of immutable vectors, like Clojure"
    (map-pairs map))


  ;;
  ;; Create cl:hash-table and fset:map from sequences of mapentries. Fset won't do
  ;; this.  And while it has some map-from-list logic, it doesan't like our things
  ;; like (cl:list (vector :a 1)) (a vector of one logical mapentry).  Seems to work
  ;; for (convert 'fset:map <cl:hash-table>).
  ;;
  ;; Note that clojure doesn't have collection-based constructors for hash-maps, like
  ;; `(vec coll)` is to `(vector <elts...>)`.
  ;; 
  (:method ((to-type (eql 'cl:hash-table)) (coll cl:sequence))
    (hash-thing-from-collection (cl:make-hash-table :test 'equal :size (cl:max 7 (count coll))) coll))
  (:method ((to-type (eql 'fset:map)) (coll cl:sequence))
    (hash-thing-from-collection (hash-map) coll))
  (:method ((to-type (eql 'cl:hash-table)) (coll fset:seq))
    (hash-thing-from-collection (cl:make-hash-table :test 'equal :size (cl:max 7 (count coll))) coll))
  (:method ((to-type (eql 'fset:map)) (coll fset:seq))
    (hash-thing-from-collection (hash-map) coll))
  (:method ((to-type (eql 'cl:hash-table)) (coll fset:set))
    (hash-thing-from-collection (cl:make-hash-table :test 'equal :size (cl:max 7 (count coll))) coll))
  (:method ((to-type (eql 'fset:map)) (coll fset:set))
    (hash-thing-from-collection (hash-map) coll))

  ;; Catchall
  (:method (to-type from-type)
    "If we don't have specific CLJ-COLL conversion needs, trampoline to FSET:CONVERT."
    (fset:convert to-type from-type)))

(defun vec (coll)
  "([coll])
Creates a new immutable vector containing the contents of coll. Common Lisp arrays
will be aliased and should not be modified.

Hash-tables and/or maps as input will result in a vector of key value pairs
whose order is undefined."
  ;; Clojure accepts maps as input too, 
  ;; (vec {:a 1 :b 2}) => [[:a 1] [:b 2]]
  ;; Fset: (vec {:a 1 :b 2}) => #[ (:A . 1) (:B . 2) ]  (with unpatched printing)
  ;; which is an FSET:WB-SEQ, like all `vec` results.
  (reduce #'conj (vector) coll)
  #+NIL ;replaced by reduce+conj, since convert doesn't handle [lazy]seqs
  (convert 'fset:seq coll))

(defun cl-vec (coll)
  "Similar to the VEC function, but CL-VEC creates
a mutable Common Lisp vector which is adjustable and fill-pointered, so that the CLJ-COLL
APIs approximating Clojure functionality will be efficient as values are added to or removed
from the vector."
  (typecase coll
    (cl:list
     (let ((n (count coll)))
       (make-array n :fill-pointer n :adjustable t :initial-contents coll)))
    (cl:vector
     (let ((v (make-array (count coll) :fill-pointer 0 :adjustable t)))
       (loop for e across coll
             do (vector-push e v))
       v))
    (cl:hash-table
     (let ((v (make-array (count coll) :fill-pointer 0 :adjustable t)))
       ;; Can't quite bring self to make it a vector of vectors. 
       ;; Clojure would however, of course that's the immutable route. Hmmm.
       (maphash (lambda (k val) 
                  (vector-push-extend (cl:list k val) v))
                coll)
       v))
    ((or lazyseq seq persistentlist persistentqueue cons)
     (reduce #'conj (make-array (count coll) :fill-pointer 0 :adjustable t) coll))
    (t (convert 'cl:vector coll))))  ;Assume fset can do it for seqs/sets/maps

(defvar *hash-table-size-hint* nil
  "If bound, it is used as the :size value to cl:make-hash-table calls
by zipmap.")

(defvar *default-hashmap-constructor* 'hash-map
  "This variable is bound to a function designator called when the `{}` syntax is called
upon to create a immutable or mutable hash structure.  It is also used to determine
the type of map created for `ZIPMAP`.

By default it is bound to 'HASH-MAP which creates an immutable unordered map with EQUAL
equality and SXHASH hash semantics.  EQUAL is about as close to Clojure's '=' as we
can get in Common Lisp without a substantial effort at Clojure's notion of 'equiv'.

This symbol exists for users to bind to other functions if they so desire.
The function should accept arguments as per Clojure's `HASH-MAP` function,
namely a plist of key/value pairs.

SERAPIUM:DICT and CLJ-COLL:CL-HASH-MAP are compatible functions for this
variable.

NOTE: functions returning something other than CL:HASH-TABLE instances
will may work, but CLJ-COLL only supports CL-HASH-TABLE and FSET:MAP types for
associative key/value collections.")

(defvar *default-vector-constructor* 'vector
  "This variable is bound to a function designator called when the `[]` syntax is called
upon to create a vector.

By default it is 'CLJ-COLL:VECTOR which creates an immutable vector.

This symbol exists for users to bind to other functions if they so desire.

Note while you can bind this to 'CL:VECTOR, the SIMPLE-VECTOR objects that it produces 
will not work with some CLJ-COLL APIs that need to grow or shrink the vector.
Use `CLJ-COLL:CL-VECTOR for a Common Lisp vector which is adjustable and fill-pointered.")

(defun zipmap (keys vals)
  "Returns a map with the keys mapped to the corresponding vals.
The function designated by *DEFAULT-HASHMAP-CONSTRUCTOR* is used create the immutable map
or common lisp hash-table similar to map syntax (`{:a 1}`) use of the binding.

Efficiency note: if you have a large number of keys you'll likely have faster performance 
calling `cl:make-hash-table` with a :size argument before populating the table, assuming
you can tolerate a mutable hash-table."
  (unless (and (sequential-or-nil? keys) (sequential-or-nil? vals))
    (error "Key and value inputs should be collections with sequential behavior."))
  (let* ((*hash-table-size-hint* (count keys))
         (map (funcall *default-hashmap-constructor*)))
    ;; CLJ-COLL:ASSOC would be the nice abstract way to do this.
    ;; I've dubiously opted for efficiency over abstraction, but as a result
    ;; any new map types will require support here...
    (flet ((map-update (k v) (setf map (fset:with map k v)))
           (ht-update (k v) (setf (gethash k map) v)))
      (loop with updater function = (if (fset:map? map) #'map-update #'ht-update)
            for kseq = (seq keys) then (next kseq)
            for vseq = (seq vals) then (next vseq)
            while (and kseq vseq)
            do (funcall updater (first kseq) (first vseq))
            finally (when (or kseq vseq)
                      (error "The number of keys and values did not match."))))
    map))


(defun run-arrayseq (f coll)
  "Helper to apply function F to Arrayseq elements for side effects
with optimized ascending order traversal. Returns nil."
  (declare (function f) (arrayseq coll))
  (let ((array (arrayseq-array coll)))
    (if (cl:vectorp array)
        (loop for idx from (arrayseq-index coll) below (cl:length array)
              do (funcall f (aref array idx)))
        ;; fset:seq
        (fset:do-seq (item array :start (arrayseq-index coll))
          (funcall f item))))
  nil)

(defun run-arrayrseq (f coll)
  "Helper to apply function F to ArrayRseq elements for side effects
with optimal reverse order traversal. Returns nil."
  (declare (function f) (arrayrseq coll))
  (let ((array (arrayrseq-array coll)))
    (if (cl:vectorp array)
        (loop for idx from (arrayrseq-index coll) downto 0
              do (funcall f (aref array idx)))
        ;; fset:seq 
        (fset:do-seq (item array :end (1+ (arrayrseq-index coll)) :from-end? t)
          (funcall f item))))
  nil)


(defun run! (f coll)
  "([f coll])
Runs the supplied function (of one argument)
for side-effect purposes on items in the collection. Returns nil.

F receives logical map entries for hashtables/maps, 
(k v) for hash-tables, [k v] for maps, differing by mutability."
  (declare ((or symbol function) f))
  (let ((f (alexandria:ensure-function f)))
    (declare (function f))
    (etypecase coll
      (cl:list (loop for x in coll do (funcall f x)))
      (cl:vector (loop for x across coll do (funcall f x)))
      (arrayseq (run-arrayseq f coll))  ;optimized to avoid seq consing
      (arrayrseq (run-arrayrseq f coll)) ;optimized to avoid seq consing
      (seq (loop for s = coll then (next s) ;s=(seq coll) superfluous for seq
                 while s
                 do (funcall f (first s))))
      ((or lazyseq ipersistentlist)
       (loop for s = (seq coll) then (next s)
             while s
             do (funcall f (first s))))
      (fset:seq (fset:do-seq (x coll) (funcall f x)))
      (fset:set (fset:do-set (x coll) (funcall f x)))
      ;; Attention to mapentries - no dotted pairs
      (cl:hash-table (loop for x being the hash-key in coll
                             using (hash-value v)
                           do (funcall f (cl:list x v))))
      (fset:map (fset:do-map (k v coll) (funcall f (vector k v)))))))


;; FSet has iterators for fset collections, cl:sequence cl:string, cl:vector, cl:list
(defgeneric mapv1 (f coll)
  (:documentation "mapv optimized for a single source collection and vector result,
See also: RUN!.")
  (:method (f (coll fset:map))
    (let ((result (vector))
          (f (alexandria:ensure-function f)))
      (declare (function f))
      (fset:do-map (k v coll)
        (setf result (fset:with-last result (funcall f (vector k v)))))
      result))
  (:method (f (coll fset:collection))
    ;; On seqs/single-value gets, 
    ;; (funcall iterator :get) returns 2 values, value & successp
    ;; successp is nil if there were no more values, in which case val is niil
    (let ((result (vector))
          (iter (fset:iterator coll))
          (f (alexandria:ensure-function f)))
      (declare (function f))
      (loop (multiple-value-bind (v successp)
                (funcall iter :get)
              (if successp
                  (setf result (fset:with-last result (funcall f v)))
                  (return-from mapv1 result))))))
  ;; *FINISH* *PERFORMANCE*: provide optimized versions for arrayseq and mapseq
  (:method (f (coll seq))
    (loop with result = (vector)
          with f function = (alexandria:ensure-function f)
          for s = coll then (next s)
          while s
          do (setf result (fset:with-last result (funcall f (first s))))
          finally (return result)))
  (:method (f (coll IPersistentList)) ; behaves as seq, but isn't ISA seq, unfortunate
    (loop with result = (vector)
          with f function = (alexandria:ensure-function f)
          for s = coll then (next s)
          while s
          do (setf result (fset:with-last result (funcall f (first s))))
          finally (return result)))
  (:method (f (coll lazyseq))
    (mapv1 f (seq coll)))

  ;; More efficient source iteration, no fset iterators needed
  (:method (f (coll cl:list))
    (loop with result = (vector)
          with f function = (alexandria:ensure-function f)
          for val in coll
          do (setf result (fset:with-last result (funcall f val)))
          finally (return result)))
  (:method (f (coll cl:vector))
    (loop with result = (vector)
          with f function = (alexandria:ensure-function f)
          for val across coll
          do (setf result (fset:with-last result (funcall f val)))
          finally (return result)))
  (:method (f (coll cl:hash-table))
    (loop with result = (vector)
          with f function = (alexandria:ensure-function f)
          for key being the hash-keys of coll
          using (hash-value val)
          ;; Mutable in, list-type MapEntry out.
          do (setf result (fset:with-last result (funcall f (cl:list key val))))
          finally (return result))))

(defun cl-map-aux (result-type f colls)
  "See CL-MAP.
Call F on successive elements of COLLS like CL:MAP or MAPV
returning a collection of type RESULT-TYPE with the return values of each call to F
when RESULT-TYPE is not NIL."
  (loop with result = (make-collector result-type)
        with f function = (alexandria:ensure-function f)
        with n-values = (apply #'min (mapcar #'count colls))
        with eof = '#:eof
        with iterators = (cl:mapcar (lambda (coll) (iterator coll eof)) colls)
        for n from 0 below n-values     ;so we avoid eof
        ;; *TBD*/*PERFORMANCE*: could we reuse the vals list each iteration?
        ;; Only if we knew F wouldn't retain list references, so ... no.
        as vals = (loop for iterator function in iterators
                        collect (funcall iterator))
        do (if result-type 
               (collect result (apply f vals))
               (apply f vals))
        finally (return (and result-type (grab result)))))

(defun cl-map (result-type f coll &rest colls)
  "A CL:MAP clone extended to accept CLJ-COLL collections and CL:HASH-TABLEs as well as
CL sequences. Applies F to successive elements of COLLS like CL:MAP or MAPV
returning a collection of type RESULT-TYPE with the return values of each call to F.
F must have arity equal to the number of collections.

RESULT-TYPE may be one of 'CL:LIST, 'CL:VECTOR, 'CL:HASH-TABLE,
+IMMUTABLE-VECTOR-TYPE+, +IMMUTABLE-SET-TYPE+, +IMMUTABLE-MAP-TYPE+,
'CLJ-COLL::PERSISTENTLIST, or 'CLJ-COLL::PERSISTENTQUEUE.

A RESULT-TYPE of NIL is also permitted, in which case F is called purely for side
effects and its return value is ignored.  CL-MAP will return NIL if RESULT-TYPE is
NIL.  Note that if you just want to call F for side effects, RUN! or DOSEQ would be
more efficient assuming a single collection as input.

See also: CLJ-COLL:MMAP which is slightly optimized for CL:LIST & CL:VECTOR
return values, and for single-column argument lists."
  (cl-map-aux result-type f (cl:list* coll colls)))

(defun mapv (f coll &rest colls)
  "([f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls])
Returns an immutable vector consisting of the result of applying f to the
set of first items of each coll, followed by applying f to the set
of second items in each coll, and so on until any one of the colls is
exhausted.  Any remaining items in other colls are ignored. Function
f should accept number-of-colls arguments.

See also: CL-MAP, MMAP."
  (if (null colls)
      (mapv1 f coll)
      (apply #'cl-map 'fset:seq f coll colls)))

;;;
;;; Type support.
;;;
;;; Note that Fset defines abstract types for:
;;;     collection, set, replay-set, bag, map, replay-map, seq, tuple
;;; Types such as `WB-Map` are also defined with defstruct and use `include`, which apparently
;;; creates desired inheritance of abstract type. So (subtypep 'fset:wb-map 'fset:map) => true
;;; This is portable/conformant lisp behavior assuming it meets the requirements 
;;; of other defstruct option interactions (particularly :type specifications and lack thereof).
;;;

(defun coll? (x)
  "([x])
Clojure: Returns true if x implements IPersistentCollection.
Common Lisp: return true if x implements an immutable collection, nil otherwise.

Note that COLL? returns true for seqs and lazyseqs, not just immutable collections,
these are Clojure semantics.

We have one hiccup, COLL? returns NIL when passed a CL:CONS, even though CLJ-COLL
uses CL:CONS as seqs and (SEQ? <cl:cons>) _may_ be true (see `seq?`). We do this to
avoid confusion w.r.t. immutable collections, we don't want to think of cl:cons as an
immutable collection.

See also: collp, list?, vector?, set?, map?, seq?, and map-entry?."
  (or (ipersistentlist-p x)
      (and (atom x) (seq? x))           ;not CL:LIST
      (typep x 'fset:collection)))

(defun collp (x)
  "A more general predicate than COLL?, returns true if x is a mutable CL collection
or an immutable CLJ-COLL collection, nil otherwise.

(COLLP NIL) returns nil.

The focus of COLLP is on collections which are useful with CLJ-COLL, as such
it returns nil for strings (as does coll? in Clojure), and bitvectors.

See also: COLL?"
  (or (coll? x)
      (typecase x
        (null nil)
        (cl:string nil)
        (cl:bit-vector nil)
        (cl:sequence t)
        (cl:hash-table t)
        (t nil))))

(defun map? (x)
  "([x])
Clojure: Return true if x implements IPersistentMap.
CLJ-COLL does not support clojure interfaces at this time, 
nor could we add Clojure-named interface support to FSet maps.

Common Lisp: Return true if x implements an immutable map, nil otherwise.

See also: MAPP"
  (typep x 'fset:map))

(defun mapp (x)
  "A more general predicate than MAP?, returns true if x is a CL hash-table or an
immutable CLJ-COLL map, nil otherwise.

See also: MAP?"
  (typecase x
    (fset:map t)
    (cl:hash-table t)
    (t nil)))

(defun vector? (x)
  "([x])
Clojure: Return true if x implements IPersistentVector
CLJ-COLL does not support clojure interfaces at this time, 
nor could we add Clojure-named interface support to FSet vectors.
Note that in CLojure (vector? \"abc\") => NIL.

Common Lisp: return true if x implements an immutable collection, nil otherwise.

See also: VECTORP"
  (fset:seq? x))

(defun vectorp (x)
  "A more general predicate than VECTOR?, returns true if x is a mutable CL vector
or an immutable CLJ-COLL vector, NIL otherwise.

Shadows CL:VECTORP.

Note that while some CLJ-COLL operations on vectors require fill-pointered adjustable
arrays, this predicate returns true any general vector.

With regard to mutable CL types, VECTORP discriminates on the applicability of the vector
for use in CLJ-COLL APIs.  VECTORP will return NIL for arrays that are not of rank 1 or
specialized arrays such as strings and bitvectors. Note that strings are not `vector?`
in Clojure either.

See also: VECTOR?, STRING?, STRINGP, BIT-VECTOR-P, SIMPLE-BIT-VECTOR-P."
  (typecase x
    (string nil)
    (bit-vector nil)
    (cl:vector t)
    (fset:seq t)
    (t nil)))

(defun set? (x)
  "Return true if x is an immutable set."
  (fset:set? x))


(defun list? (x)
  "([x])
Returns true if x implements IPersistentList"
  (typep x 'IPersistentList))

(defun listp (x)
  "([x])
A more general predicate than LIST?, returns true if x is a mutable CL:LIST
or an immutable IPersistentList.

Shadows CL:LISTP

See also: LIST?"
  (typecase x
    (cl:list t)
    (ipersistentlist t)
    (t nil)))


;;;
;;; Capability predicates
;;;

(defun sequential? (coll)
  "([coll])
Clojure: Returns true if coll implements Sequential
Common Lisp: returns true if coll implements sequential (predictable) access.

Note that Clojure does not consider NIL to be `sequential?`, and this predicate
is compatible with Clojure in this regard. However in Common Lisp
it is sometimes useful to have NIL be synonymous with an empty list, hence sequential.
The SEQUENTIAL-OR-NIL? predicate provides that interpretation of NIL."
  (typecase coll
    (cl:string nil)                     ;Clojure compatible

    ;; Disallow NIL as being `sequential?`. Note that CL:SEQUENCE/CL:LIST types include NIL.
    (cl:cons t)
    (cl:vector t)

    (ipersistentlist t)
    (fset:seq t)
    (seq t)                             ;even if it's a seq on maps, Clojure compatible
    (lazyseq t)
    (t nil)))
    
(defun sequential-or-nil? (coll)
  "([coll])
Identical to SEQUENTIAL? except that it interprets NIL as being an empty list,
hence sequential.  As Common Lisp relies heavily on nil as a representation
of an empty list (unlike Clojure), this may be useful."
  (or (null coll)
      (sequential? coll)))

(defun associative? (coll)
  "([coll])
Clojure: Returns true if coll implements Associative
Common Lisp, returns true if coll implements clojure-like associative semantics."
  (typecase coll
    (cl:string nil)                     ;clojure compatible
    (cl:list nil)                       ;clojure compatible
    (cl:vector t)
    (fset:seq t)
    (cl:hash-table t)
    (fset:map t)))

(defun counted? (coll)
  "([coll])
Returns true if coll implements count in constant time"
  (typecase coll
    (cl:string nil)                     ;clojure compatible
    (cl:list nil)
    (cl:vector t)
    (fset:collection t)
    (cl:hash-table t)
    (ipersistentlist t)
    (queueseq t)
    (arrayseq t)
    (mapseq t)))
  
(defun reversible? (coll)
  "([coll])
Returns true if coll supports reversible O(1) RSEQ behavior.
Presently only supports vector types.
Does not support strings (clojure compatibility)."
  (typecase coll
    (cl:string nil)                     ;clojure compatible semantics
    (cl:vector t)
    (fset:seq t)
    ;; Eventually sorted maps and sets, 
    ;; but we can't do it for CL:HASH-TABLE in O(1) time,
    ;; and we need support for "from-end t" on FSet iterators.
    (t nil)))

;; *TBD*: sorted?

(defgeneric count (coll)
  (:documentation "Returns the number of items in the collection. (count nil) returns 0.
Performance may be O(n) for some collection/seq types.
Also works on strings, arrays, and hash-tables.
Shadows CL:COUNT.")
  (:method ((coll cl:sequence)) (cl:length coll)) ;includes NIL
  (:method ((coll cl:hash-table)) (hash-table-count coll))
  (:method ((coll fset:collection)) (fset:size coll))
  (:method ((coll EmptyList)) 0)
  (:method ((coll PersistentList)) (persistentlist-count coll))
  (:method ((coll PersistentQueue)) (persistentqueue-count coll))
  (:method ((coll QueueSeq)) (queueseq-count coll))
  (:method ((coll Cons)) ;remembering that the more field of a cons may not be a cons
    (loop with count = 0
          for s = coll then (next s)
          while s
          if (counted? s)
             return (+ count (count s))
          else
             do (incf count)
          finally (return count)))
  (:method ((coll lazyseq))
    (loop for count from 0
          for s = (seq coll) then (next s)
          while s
          finally (return count)))
  (:method ((coll arrayseq))
    (- (count (arrayseq-array coll)) (arrayseq-index coll)))
  (:method ((coll arrayrseq))
    (1+ (arrayrseq-index coll)))
  (:method ((coll mapseq))
    (- (cl:length (mapseq-keys coll)) (mapseq-index coll))))

(defgeneric empty? (coll)
  (:documentation "([coll])
Returns T if coll has no items, NIL otherwise.")
  (:method ((coll EmptyList)) t)
  (:method ((coll PersistentList)) nil)
  (:method ((coll PersistentQueue)) (zerop (persistentqueue-count coll)))
  (:method ((coll hash-table)) (= 0 (cl:hash-table-count coll)))
  (:method ((coll fset:collection)) (fset:empty? coll))
  (:method ((coll cl:vector)) (= 0 (cl:length coll))) ;incl strings
  (:method ((coll null)) t)
  (:method ((coll lazyseq)) (null (seq coll)))
  (:method ((coll seq)) nil)
  (:method ((coll cl:cons)) nil))

(defgeneric empty (coll)
  (:documentation "([coll])
Returns an empty collection of the same type as coll, or NIL
if the input collection is not supported or not a collection.

Strings are NOT valid collections for this function (NIL returned).
Seqs are supported and return an empty persistent list.

Clojure incompatibility:
This being Common Lisp, NIL is also returned when COLL is a CL list,
as it represents an empty list.")
  (:method ((coll cl:string)) nil)     ;not treated as a vector in clojure semantics
  (:method ((coll cl:vector)) (cl-vector))
  (:method ((coll cl:hash-table)) (cl-hash-map))
  (:method ((coll EmptyList)) *EMPTY-LIST*)
  (:method ((coll PersistentList)) *EMPTY-LIST*)
  (:method ((coll PersistentQueue)) *EMPTY-QUEUE*)
  (:method ((coll fset:seq)) (vector))
  (:method ((coll fset:set)) (hash-set))
  (:method ((coll fset:map)) (hash-map))
  (:method ((coll lazyseq)) *EMPTY-LIST*)
  (:method ((coll seq)) *EMPTY-LIST*)
  (:method ((coll t)) nil))

;;;
;;; Abstract way to (shallow) copy a CLJ-COLL collection
;;;

(defgeneric copy (source)
  (:documentation "Copy a collection, if necessary,
so that mutations to the SOURCE collection will not appear
in the copy made by this function.

For persistent data types this function returns SOURCE as is.

For CL VECTOR and HASH-TABLE types, we do not preserve the _capacity_ of the source
collection, only the content.  So if the fill pointered vector had room for 100
elements but only 10 were in use, we return a vector with capacity 10 (while
presering other attributes such as fill pointer and adjustability).

Doesn't support arbitrary array types, only those routinely used by CLJ-COLL.")
  (:method ((source cl:list)) (cl:copy-list source))
  (:method ((source cl:vector)) 
    ;; Could use alexandria:copy-array, except that we don't want to copy unused capacity
    (let* ((n (cl:length source))
           (v (make-array n
                          :adjustable (cl:adjustable-array-p source)
                          :fill-pointer n
                          :element-type (array-element-type source))))
      (dotimes (i n)
        (setf (aref v i) (aref source i)))
      v))
  (:method ((source cl:hash-table)) 
    ;; Could use alexandria:copy-hash-table except except for capacity correction, like vector
    ;; Note that the :SIZE option to MAKE-HASH-TABLE is a hint, not a guarantee
    ;; *TBD* shrink threshold too?
    (let* ((n (cl:hash-table-count source))
           (ht (cl:make-hash-table :test (hash-table-test source)
                                   :size n
                                   :rehash-size (hash-table-rehash-size source)
                                   :rehash-threshold (hash-table-rehash-threshold source))))
      (maphash (lambda (k v)
                 (setf (gethash k ht) v))
               source)
      ht))
  (:method ((source ipersistentlist)) source)
  (:method ((source fset:collection)) source)
  (:method ((source seq)) source)
  ;; This is perhaps dubious as lazyseqs do mutate, but it's
  ;; thread-safe, and the sequence of objects derived from it won't change.
  (:method ((source lazyseq)) source)
  )


