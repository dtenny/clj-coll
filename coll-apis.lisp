(in-package :clj-coll)

;;;; Collection (but not lazy seq) APIs we try to express without type-specific
;;;; knowledge in glue.lisp using the type-aware functions defined in glue.lisp.
;;;; We consider functions that deal with collections that don't generate lazy
;;;; sequences for return to be domain of this file.  If they return lazy sequences
;;;; they're usually in 'seq-apis.lisp'.  Not that anything is water tight
;;;; in our mapping of functions to files, just stating the rough intent.

(defun merge (&rest maps)
  "([& maps])
Returns a map that consists of the rest of the maps or k/v pairs conj-ed onto
the first.  If a key occurs in more than one map, the mapping from
the latter (left-to-right) will be the mapping in the result.

(MERGE) returns NIL.

MAPS may be mutable hash-tables, immutable maps, and logical MapEntry
values expressed as cl:list or clj-con:vector objects containing 2 values
representing key/val pairs. CL:VECTOR objects are not permitted, see README.md

Clojure quirk (& compatibility): it doesn't really matter what the initial collection
is so long as it works with CONJ, and remaining args are conjed onto the first.  So
(merge #{1 2} 3) => #{1 2 3}."
  (if (null maps)
      nil
      (let ((result (cl:first maps)))
        (loop for map in (cl:rest maps)
              do (setf result (conj result map)))
        result)))

(defun not-empty (coll)
  "([coll])
If coll is empty, returns nil, else coll.
Clojure quirk: strings _are_ treated as collections for this function."
  (if (empty? coll)
      nil
      coll))

(defun bounded-count (n coll)
  "([n coll])
If COLL is counted? returns its count, otherwise count at most the first N
elements of COLL using its seq"
  (if (counted? coll)
      (count coll)
      (loop for s = (seq coll) then (next s)
            for i from 0 below n
            while s
            finally (return i))))

(defgeneric assoc (map key val &rest kvs)
  (:documentation "([map key val] [map key val & kvs])
assoc[iate]. When applied to a map, updates or adds the key/val pair.

When applied to a vector, update vector to contain val at index 'key'. For vectors, index/key
must be <= (count vector). If it's equal to the count, it has the effect of growing the vector.

For mutable Common Lisp vectors this works only for vectors created explicitly as
adjustable arrays, or those vectors created with clojure-namesake functions in the
clj-coll package, e.g. `vector`.

IF THE INPUT COLLECTION IS MUTABLE, THIS IS A MUTATING OPERATION, UNLIKE CLOJURE's.
Returns the updated `map` argument for mutable inputs, and a new map for immutable inputs.

Seems like we ought to allow assoc on lists like we do vectors, but we don't because clojure 
doesn't. (Of course the user of this package could define methods for lists if they want to.)

If MAP is nil, a new immutable hash-map is created.

Shadows CL:ASSOC.")
  (:method ((map null) key val &rest kvs)
    (apply #'hash-map key val kvs))
  (:method ((map hash-table) key val &rest kvs)
    (check-mutation-barrier map)
    (loop for (k v) on (cl:list* key val kvs) by #'cddr
          do (setf (gethash k map) v))
    map)
  (:method ((map fset:map) key val &rest kvs)
    (loop for (k v) on (cl:list* key val kvs) by #'cddr
          do (setf map (fset:with map k v)))
    map)
  (:method ((vec cl:vector) key val &rest kvs)  
    (check-mutation-barrier vec)
    (flet ((alter-vec (k v)
             (if (= k (length vec))
                 (when-let ((new (safe-vector-push-extend v vec)))
                   (setf vec new))
                 (setf (aref vec k) v))))
      (alter-vec key val)
      (loop for (k v) on kvs by #'cddr
            do (alter-vec k v)))
    vec)
  (:method ((vec fset:seq) key val &rest kvs)  
    (if (<= key (fset:size vec))
        (setf vec (fset:with vec key val))
        (error "Index ~d is out of bounds for ASSOC on vector of size ~d"
               key (fset:size vec)))
    (when kvs
      (loop for (k v) on kvs by #'cddr
            do (setf vec (fset:with vec k v))))
    vec)
  (:method ((vec t) key val &rest kvs)
    (declare (ignore key val kvs))
    (error "~s, a ~s, is not associative" vec (type-of vec))))

(defgeneric dissoc (map &rest keys)
  (:documentation "([map] [map key] [map key & ks])
dissoc[iate]. 

Note that Clojure supports `assoc` for vectors, but not `dissoc`,
and that is true of CLJ-COLL vectors as well. For that you're left
with REMOVE, FILTER, TAKE, and DROP, or M function equivalents.

For immutable maps, returns a new map of the same (hashed/sorted) type,
that does not contain a mapping for key(s). The original map is returned
if there are no keys specified.

For mutable hash-tables, returns the input hash-table with the indicate keys removed.

See also: DISJ for set element removals")
  (:method ((map hash-table) &rest keys)
    (check-mutation-barrier map)
    (loop for key in keys
          do (remhash key map))
    map)
  (:method ((map fset:map) &rest keys)
    (loop for key in keys
          do (setf map (fset:less map key))) ;fset:remove is for seqs
    map)
  (:method ((map cl:vector) &rest keys)
    (declare (ignore map keys))
    (error "The DISSOC function does not support vectors."))
  (:method ((map fset:seq) &rest keys)
    (declare (ignore map keys))
    (error "The DISSOC function does not support vectors.")))


(defgeneric get (map key &optional not-found)
  (:documentation "([map key] [map key not-found])
Returns the value mapped to key, not-found or nil if key not present
in associative collection, set, string, array.

Note that any indices beyond the end of vectors (by any amount) are permitted 
(and considered not-found). Unlike assoc-in, indices more than one past the end of
the vector does not signal a bounds errors.

Clojure quirk: returns NIL for unsupported collection types
(or even non-collection arguments) even though an error might be better.")
  (:method ((map hash-table) key &optional not-found)
    (gethash key map not-found))
  (:method ((map fset:map) key &optional not-found)
    ;; Gotcha: `lookup` map return vals are reversed compared to fset set/seq lookup return values
    (multiple-value-bind (val found?)
        (fset:lookup map key)
      (if found?
          val
          not-found)))
  (:method ((map fset:seq) key &optional not-found)
    (multiple-value-bind (val found?)
        (fset:lookup map key)
      (if found?
          val
          not-found)))
  (:method ((map cl:vector) key &optional not-found)
    ;; Clojure allows keys out of range as not-found
    (if (< key (length map))
        (aref map key)
        not-found))
  (:method ((map fset:set) key &optional not-found)
    (if (fset:lookup map key)
        key
        not-found))
  (:method ((map t) key &optional not-found)
    (declare (ignore key not-found))
    nil))

(defun get-in (m ks &optional not-found)
  "([m ks] [m ks not-found])
Returns the value in a nested associative structure
wehere ks is a sequence of keys. Returns nil if the key
is not present, or the not-found value if supplied.

Shares CLojure's quirks in `GET` and may return NIL or signal an error
if a non-associative collection is passed for M."
  (let ((result not-found))
    (doseq (k ks)
      (setf result (get m k not-found))
      (if (equal? result not-found)
          (return-from get-in result)
          (setf m result)))
    result))

(defun assoc-in-aux (map keys value parent-map)
  "Helper routine for assoc-in that passes down parent map for mutability memory."
  (let ((first-key (first keys))
        (more-keys (rest keys))
        (map (if (null map)
                 (if parent-map
                     (if (coll? parent-map)
                         (hash-map)
                         (cl-hash-map))
                     (hash-map))
                 map)))
      (if (seq more-keys)
          (assoc map first-key (assoc-in-aux (get map first-key) more-keys value map))
          (assoc map first-key value))))

(defun assoc-in (map keys value)
  "([map keys value]
Associates a value in a nested associative structure MAP, traversing the path
indicated by keys in KEYS, and finally updating the value found at the last key in KEYS.

Supported associative collections are maps/hash-tables and vectors.

Any mutable collections including and derivable from MAP will be mutated.
Any immutable collections will result in new collections.

Logically a new nested structure is created, and physically that is true if all
elements are immutable, but YMMV if you're mixing mutable and immutable collections.

If any levels do not exist, mutable or immutable hash-maps will be created,
according to the mutability of the immediate parent. If the immediate parent
is NIL, an immutable map is created, consistent with Clojure.

e.g. (assoc-in nil [:a] 1) => {:a 1}
     (assoc-in nil [:a :b] 1) => {:a {:b 1}}"
  (assoc-in-aux map keys value nil))

(defun update (m k f &rest args-to-f)
  "([m k f] [m k f x] [m k f x y] [m k f x y z] [m k f x y z & more])
Updates a value in an associative collection M keyed by K
(a map key or vector index).

F is a function that will take the old value for the K on M, 
and as well as any arguments following F in the call to UPDATE.

If the key K does not exist in M, NIL is passed as the old value to F.

F should return a new value for K on M.

As with ASSOC, indices on vectors should not be greater than the number of elements
in the vector.  If K == the count, the value will be added to the end. If K exceeds
the count, an bounds error is signalled.

RETURNS:

If M is mutable (i.e. (coll? M) is nil), the collection is mutated
and M is returned.

If M is immutable, a new collection with the updated value for K on M
is returned.

See also: CONSTANTLY, which is often useful to generate functions for UPDATE."
  (assoc m k (apply f (get m k) args-to-f)))

(defun update-in (map keys f &rest args-to-f)
  "([map keys f & args
Updates a value in a nested associative structure reachable from map MAP, traversing
the path indicated by keys in KEYS, and finally updating the value found at the last
structure/key in KEYS by calling F.

F is a function that will take the old value for the the last key as well as any
arguments following F in the call to UPDATE-IN.  If the last key does not exist its
parent structure, NIL is passed as the old value to F.  F should return a new value
for the last key.

Supported associative collections are maps/hash-tables and vectors.

As with ASSOC-IN, indices on vectors should not be greater than the number of
elements in the vector.  If a index-as-key == the count of a vector, the value will
be added to the end. If the index-as-key exceeds the count, an bounds error is
signalled.

RETURNS the updated tree rooted by MAP, which may be EQ to MAP if map is mutable.

See also: CONSTANTLY, which is often useful to generate functions for UPDATE."
  (labels ((update-in-aux (parent map keys f args-to-f)
             (let ((first-key (first keys))
                   (more-keys (rest keys))
                   (map (if (null map)
                            (if parent
                                (if (coll? parent)
                                    (hash-map)
                                    (cl-hash-map))
                                (hash-map))
                            map)))
               (if (seq more-keys)
                   (assoc map first-key (update-in-aux map (get map first-key) more-keys f args-to-f))
                   (assoc map first-key (apply f (get map first-key) args-to-f))))))
    (update-in-aux nil map keys f args-to-f)))

(defgeneric keys (map)
  (:documentation "
Returns a non-lazy sequence of the map's keys in the same order as (seq map).
If the map is empty, NIL is returned.

Due to limitations in  CL hash-tables and underlying persistent maps,
these are not managed as iterators on the data structures, we materialize a 
hidden vector of keys and return it as a seq.

See also: MKEYS")
  (:method ((map hash-table))
    (if (= 0 (cl:hash-table-count map))
        nil
        ;; As a hidden array the user shouldn't be manipulating, we allocate a 
        ;; simple-vector for potential SVREF optimizations.
        (make-arrayseq :array (hash-table-keys-as-vector map))))
  (:method ((map fset:map))
    (if (= 0 (fset:size map))
        nil
        (make-arrayseq :array (map-keys-as-vector map))))
  (:method ((map cl:vector))
    (declare (ignore map))
    (error "The KEYS api does not support vectors."))
  (:method ((map fset:seq))
    (declare (ignore map))
    (error "The KEYS api does not support vectors.")))

(defun mkeys (result-type-or-map &optional (map nil mapp))
  "([map] [result-type map])

An eager version of KEYS that returns a CL:LIST(default) or CL:VECTOR
of the map's keys in the same order as (seq map).  Map may be an immutable CLJ-COLL map 
or a CL:HASH-TABLE.

If the map is empty, NIL is returned for CL:LIST returns, and #() is
returned for CL:VECTOR returns.

See also: KEYS"
  (let ((result-type (if mapp result-type-or-map 'cl:list))
        (map (if mapp map result-type-or-map)))
    (when (vectorp map)
      (error "MKEYS does not support vectors."))
    (ecase result-type
      (cl:list 
       (etypecase map
         (cl:hash-table (hash-table-keys-as-list map))
         (fset:map (map-keys-as-list map))))
      (cl:vector
       (if (empty? map)
           #()
           (etypecase map
             (cl:hash-table (hash-table-keys-as-vector map))
             (fset:map (map-keys-as-vector map))))))))

(defgeneric vals (map)
  (:documentation "
Returns a sequence of the map's values in the same order as (seq map).
If the map is empty, NIL is returned.

Due to limitations in  CL hash-tables and underlying persistent maps,
these are not managed as iterators on the data structures, we materialize a 
hidden vector of values and return it as a seq.

See also: MVALS")
  (:method ((map hash-table))
    (if (= 0 (cl:hash-table-count map))
        nil
        ;; As a hidden array the user shouldn't be manipulating, we allocate a 
        ;; simple-vector for potential SVREF optimizations.
        (make-arrayseq :array (hash-table-values-as-vector map))))
  (:method ((map fset:map))
    (if (= 0 (fset:size map))
        nil
        (make-arrayseq :array (map-values-as-vector map))))
  (:method ((map cl:vector))
    (declare (ignore map))
    (error "The VALS api does not support vectors."))
  (:method ((map fset:seq))
    (declare (ignore map))
    (error "The VALS api does not support vectors.")))

(defun mvals (result-type-or-map &optional (map nil mapp))
  "([map] [result-type map])

An eager version of VALS that returns a CL:LIST(default) or CL:VECTOR of the map's
entry values in the same order as (seq map).  Map may be an immutable CLJ-COLL map or
a CL:HASH-TABLE.

If the map is empty, NIL is returned for CL:LIST returns, and #() is
returned for CL:VECTOR returns.

See also: VALS"
  (let ((result-type (if mapp result-type-or-map 'cl:list))
        (map (if mapp map result-type-or-map)))
    (when (vectorp map)
      (error "MVALS does not support vectors."))
    (ecase result-type
      (cl:list 
       (etypecase map
         (cl:hash-table (hash-table-values-as-list map))
         (fset:map (map-values-as-list map))))
      (cl:vector
       (if (empty? map)
           #()
           (etypecase map
             (cl:hash-table (hash-table-values-as-vector map))
             (fset:map (map-values-as-vector map))))))))

(defgeneric last (coll)
  (:documentation
   "Return the last item in coll/seq or NIL if coll is empty.

Time required may be O(n), depending on the nature of the collection.

Unordered collection types attempt a reproducible traversal order
across the implementations of FIRST, SECOND, and LAST so that, barring mutation,
results should be reproducible across calls, and values should be distinct where appropriate.

When invoked on hash-tables/maps, a logical mapentry is returned in the form of a
mutable list for mutable hash-tables and an immutable vector for immutable maps.

Note that this function differs from CL:LAST, which returns the last cons of
a CL:LIST instead of the last element of a collection.")
  (:method ((coll cl:list))
    (first (cl:last coll)))
  (:method ((coll cl:vector))
    (if (empty? coll)
        nil
        (aref coll (- (length coll) 1))))
  (:method ((coll cl:hash-table))
    (if (= 0 (hash-table-count coll))
        nil
        (let (lastk lastv)
          (maphash (lambda (k v) 
                     (setf lastk k lastv v))
                   coll)
          (cl:list lastk lastv))))
  (:method ((coll fset:map))
    (if (= 0 (fset:size coll))
        nil
        (let (lastk lastv)
          (fset:do-map (k v coll)
            (setf lastk k lastv v))
          (vector lastk lastv))))
  (:method ((coll fset:set))
    (if (= 0 (fset:size coll))
        nil
        (let (last)
          (fset:do-set (k coll)
            (setf last k))
          last)))
  (:method ((coll fset:seq))
    (values (fset:lookup coll (- (count coll) 1))))
  (:method ((coll lazyseq))
    (let ((s (seq coll)))
      (and s (last s))))
  (:method ((coll seq))
    (let ((result (first coll)))
      (loop for s = (next coll) then (next s)
            while s
            do (setq result (first s)))
      result)))

(defun contains? (coll key)
  "([coll key]) ([coll key default])
Returns T if key is present in the given collection (but not seqs), otherwise
returns NIL.  Note that for numerically indexed collections like
vectors this tests if the numeric key is within the
range of indexes. 'contains?' operates constant or logarithmic time;
it will not perform a linear search for a value.  See also 'some'."
  (typecase coll
    ;; Note that Clojure contains? works for strings like vectors 
    (cl:vector (< key (cl:length coll))) ;includes strings
    (fset:seq (< key (fset:size coll)))

    ;; Dubious hack.FSET won't let us compare '(:a 1) with [:a 1] when we're searching for mapentry
    ;; representations in sets constructed from CL:HASH-TABLE or FSET:MAP.
    ;; For now we do this hack for convenience mostly in testing.
    (fset:set
     (if *map-entry-comparison-hacks-enabled*
         (if (map-entry-p key)
             (or (fset:contains? coll key)
                 (fset:contains? coll (alternative-map-entry key)))
             (fset:contains? coll key))
         (fset:contains? coll key)))

    (cl:hash-table 
     (let ((v (gethash key coll #1='#:eof)))
       (not (eq v #1#))))
    (fset:map 
     (multiple-value-bind (v success)
         (fset:lookup coll key)
       (declare (ignore v))
       success))
    (t (error "CONTAINS? is not supported on objects of type ~s" (type-of coll)))))

(defun every? (pred coll)
  "([pred coll])
Returns T if (pred x) is logical true for every x in coll, else NIL.
See also: CL:EVERY."
  (let ((pred (alexandria:ensure-function pred)))
    (declare (function pred))
    (run! (lambda (x)
            (unless (funcall pred x)
              (return-from every? nil)))
          coll))
  t)

(defun not-every? (pred coll)
  "([pred coll])
Returns NIL if (pred x) is true for every x in
COLL, else T."
  (let ((pred (alexandria:ensure-function pred)))
    (declare (function pred))
    (run! (lambda (x)
            (unless (funcall pred x)
              (return-from not-every? t)))
          coll))
  nil)


(defun nil? (x)
  "([x])
Returns true if x is nil, false otherwise.

Note that in Clojure `()` would generate an empty immutable list,
for which nil? would return false. However in Common Lisp, () and NIL are
the same and (nil? ()) returns true.

To create or pass an immutable empty list, use `(clj-coll:list)`, for which
NIL? would return false."
  (cl:null x))

(defun some? (x)
  "([x])
Returns T if x is not nil, NIL otherwise.

Clojure quirk note: not in any mapping function that examines multiple elements,
the name is confusing.  Purely a test of non-nullness.

Note that () is NIL in Common Lisp
To pass a non-nil persistent empty list in CLJ-COLL, use `(clj-coll:list)`."
  (not (cl:null x)))


(defun nth (coll index &optional (not-found nil nf-supplied-p))
  "([coll index] [coll index not-found])
Returns the value at the INDEX. NTH signals an error unless NOT-FOUND is supplied. 
NTH works for strings, lists, and vectors.  May be O(n) time, depending
on the collection traversed.

See also: GET for associative/indexed collections, which returns NIL for
out-of-bounds index.

See also: CL:NTH, which is shadowed by CLJ-COLL:NTH. It only accepts lists, takes
arguments in reverse order and returns nil if index is out of bound."
  (declare (fixnum index))
  (typecase coll
    (cl:list                            ;includes null
     (loop for i from 0
           for element in coll
           when (= i index)
             do (return-from nth element)
           finally (if nf-supplied-p 
                       (return not-found)
                       (error "Index ~d is out of bounds on list ~s" index coll))))
    (ipersistentlist
     (loop for i from 0
           for s = (seq coll) then (next s)
           while s
           when (= i index)
             do (return-from nth (first s))
           finally (if nf-supplied-p 
                       (return not-found)
                       (error "Index ~d is out of bounds on persistent list ~s" index coll))))
    (cl:vector
     (if (< index (cl:length coll))
         (aref coll index)
         (if nf-supplied-p
             (return-from nth not-found)
             (error "Index ~d is out of bounds on cl-vector ~s" index coll))))
    (fset:seq
     (if (< index (fset:size coll))
         (values (fset:lookup coll index))
         (if nf-supplied-p
             (return-from nth not-found)
             (error "Index ~d is out of bounds in vector ~s" index coll))))
    (lazyseq
     (alexandria:if-let (s (seq coll))
       (if nf-supplied-p
           (nth s index not-found)
           (nth s index))
       (if nf-supplied-p
           not-found
           (error "Index ~d is out of bounds on lazyseq" index))))
    (seq
     (loop for i from 0 to index        ;N.B. order of these FOR's matters.
           for s = coll then (next s)
           while s
           finally 
              (return 
                (if s
                    (first s)
                    (if nf-supplied-p
                        not-found
                        (error "Index ~d is out of bounds on seq" index))))))
    (t (error "NTH is unsupported on collections of type ~s" (type-of coll)))))


(defun copy-if-mutable (coll)
  "If COLL is a mutable collection, return a copy of COLL.
If COLL is an immutable collection, return it as-is.
If COLL is NIL, return an empty PersistentList
If strings are passed, we'll make a copy, though the caller
may or may not support strings as collections which is rather variable
in Clojure collection APIs."
  (typecase coll
    (cl:null *EMPTY-LIST*)
    (cl:cons (cl:copy-list coll))
    ;; This copy works for strings, and copies attributes such as fill-pointer.
    (cl:vector (alexandria:copy-array coll))
    (cl:hash-table (alexandria:copy-hash-table coll))
    (cl:array (error "Unsupported array type, ~s" (type-of coll)))
    (t coll)))

(defun ninto (&rest args)
  "([] [to] [to from] [to xform from])
Identical to `INTO` however if the TO collection is mutable
it will be mutated, no copy is made.
Named according to Common Lisp mutating-function patterns."
  (let ((nargs (length args)))
    (case nargs
      (0 (vector))
      (1 (first args))
      (2 (reduce #'conj (cl:first args) (cl:second args)))
      (3 (transduce (cl:second args) #'conj (cl:first args) (cl:third args)))
      (t (error "Too many arguments (~d) specified in call to NINTO" (cl:length args))))))

(defun into (&rest args)
  "([] [to] [to from] [to xform from])
With no arguments, returns a new immutable vector.

With one argument, returns the unmodified argument.

Returns a new collection consisting of TO with all of the items of
FROM conjoined (as per `CONJ`). 

Note that 'new collection' for common lisp collections means a full (shallow)
collection copy.

Common Lisp lists, vectors, and hash-tables are supported as well as
immutable CLJ-COLL types.

A transducer (XFORM) may be supplied."
  (let ((nargs (length args)))
    (case nargs
      (0 (vector))                      ;[]
      (1 (cl:first args))               ;[to]
      (2 (ninto (copy-if-mutable (cl:first args)) (cl:second args))) ;[to from]
      (3 (ninto (copy-if-mutable (cl:first args)) (cl:second args) (cl:third args))) ;[to xform from]
      (t (error "Too many arguments (~d) specified in call to INTO" (cl:length args))))))

(defun some (pred coll)
  "([pred coll])
Returns the first non-NIL value of (pred x) for any x in coll, or
NIL if PRED never returns true. Will only traverse as many items
as necessary for a true result."
  (let ((pred (alexandria:ensure-function pred)))
    (declare (function pred))
    (run! (lambda (x)
            (when-let (val (funcall pred x))
              (return-from some val)))
          coll)))

(defun not-any? (pred coll)
  "([pred coll])
Returns NIL if (pred x) is true for any x in coll, T otherwise."
  (not (some pred coll)))

(defgeneric peek (coll)
  (:documentation "([coll])
`first` on a list, `last` on a vector.
Empty collections and NIL return NIL.

Lazyseqs, sets, and seqs in general do not support PEEK.")
  (:method :around (coll)
    (if (empty? coll)
        nil
        (call-next-method)))
  (:method ((coll cl:list)) (cl:first coll))
  (:method ((coll cl:vector)) (last coll))
  (:method ((coll ipersistentlist)) (first coll))
  (:method ((coll fset:seq)) (last coll)))

(defgeneric pop (coll)
  (:documentation "([coll])
Returns a collection minus the first or last item as follows:

For CL:LIST, returns the CDR of the list.
For PersistentList, returns the REST of the list (never nil, alway a list)
For PersistentQueue, returns the tail of the queue (never nil, always a queue)

For a CL:VECTOR, assumes a fill-pointered vector as if created with `cl-vector`
and decreases the fill pointer by one. Does not create new CL:VECTOR instances
and will error out if called on non-fill-pointered vectors. The modified vector
is returned.

For an immutable vector, returns a new vector without the last item.

Clojure compatible quirks:
An error is signalled if COLL is empty, _except_ if COLL is a PersistentQueue.  Pop
of a PersistentQueue yields a PersistentQueue (despite what the Clojure docstring
says).

See also: CL:PUSH, CL:POP.  Note that CONJ is the Clojure equivalent of PUSH.")
  ;; Not supported on persistent conses, same as clojure.
  (:method :around (coll)
    (if (and (empty? coll) (not (queue? coll)))
        (error "POP disallowed on empty collections (except for queues).")
        (call-next-method)))
  (:method ((coll cl:list)) (cl:cdr coll))
  (:method ((coll cl:vector)) 
    (check-mutation-barrier coll)
    (decf (fill-pointer coll))
    coll)
  (:method ((coll persistentqueue)) (pq-pop coll))
  (:method ((coll ipersistentlist)) (rest coll))
  (:method ((coll fset:seq)) (fset:less-last coll)))

(defgeneric index-of (coll item)
  (:documentation "([coll item])

A CLJ-COLL version of `.indexOf`, a Java interop documented for Clojure lists and vectors.

Returns the index of the first occurrence of the specified ITEM in COLL
or -1 if COLL does not contain ITEM.  Equality determined by EQUAL?.

This method is for (immutable or mutable) list and vector types only.

See also CL:POSITION which returns NIL if not found.")
  ;; It could be argued Clojure has punted on useful functionality here like
  ;; doing indexes on seqs/lazyseqs, or haveing CL:POSITION-like :KEY and :TEST
  ;; options.  Ah well.
  (:method ((coll cl:sequence) item) 
    (or (cl:position item coll :test #'equal?) -1))
  (:method ((coll ipersistentlist) item) 
    (loop for s = (seq coll) then (next s)
          for i from 0
          while s
          until (equal? (first s) item)
          finally (if s
                      (return i)
                      (return -1))))
  (:method ((coll fset:seq) item) 
    (or (fset:position item coll :test #'equal?) -1)))
  
(defgeneric last-index-of (coll item)
  (:documentation "([coll item])

A CLJ-COLL version of `.lastIndexOf`, a Java interop documented for Clojure lists and vectors.

Returns the index of the last occurrence of the specified ITEM in COLL
or -1 if COLL does not contain ITEM.  Equality determined by EQUAL?.

This method is for (immutable or mutable) list and vector types only.

See also `(CL:POSITION ... :FROM-END T)` which returns NIL if not found.")
  (:method ((coll cl:sequence) item) 
    (or (cl:position item coll :from-end t :test #'equal?) -1))
  (:method ((coll ipersistentlist) item) 
    (loop with last-pos = -1
          for s = (seq coll) then (next s)
          for i from 0
          while s
          do (when (equal? item (first s))
               (setf last-pos i))
          finally (return last-pos)))
  (:method ((coll fset:seq) item) 
    (or (fset:position item coll :from-end t :test #'equal?) -1)))

(defun subvec (v start &optional end)
  "([v start] [v start end])
Returns a vector of the items in vector V from start (inclusive) to end
(exclusive). If end is not supplied defaults to (count vector).

The intent of this somewhat specialized Clojure API operation is to provide a vector
subsequence in O(~1). This is true for CL:VECTOR inputs, however the current
FSET:SEQ implementation used for CLJ-COLL immutable vectors is a 'log time'
subsequence operation according to
https://gitlab.common-lisp.net/fset/fset/-/wikis/FSet/User-Guide,
and likely not the O(log32 n) time of CLojure's vector implementation.

This operation will accept either CL:VECTOR or immutable vector types,
and will return a vector of like type according to the input.

Unsupported for strings, both here and in Clojure, though the EQ case may allow it.

(subvec v 0) will simply return V. Note that this is not compatible with clojure,
however it seems like a harmless improvement.

CL:VECTOR returns will be displaced arrays on the input
(except for the EQ case previously noted), and are not adjustable as
vectors by most CLJ-COLL APIs would be."
  (if (and (= start 0) (null end))
      v
      (let ((end (or end (count v))))
        (if (> start end)
            (error "start ~s > end ~s not supported" start end)
            (if (cl:vectorp v)
                (make-array (- end start) :displaced-to v :displaced-index-offset start)
                (fset:subseq v start end))))))

(defgeneric find (coll key)
  (:documentation "([coll key])
Returns the map entry for key, or nil if key not present.

Quirky clojure land:
Works on vectors too, in which case keys are indices into the vector,
and if they are not valid indices (i.e. < 0 or > count) nil is returned.
When used on vectors, returns a vector of [key val] such that key is the ordinal
index and val is the vector element at that offset. E.g.

    (find [:a :b :c] 1) => [1 :b]

If you pass a bogus key for vectors, like (find [:a b] :a), returns nil.  Oy.

Note, map/hash-table key checks presently semantics similar to CL:EQUAL
not CLJ-COLL:EQUAL?.

MapEntry representation is as a two element collection, a cl:list
for mutable inputs, an immutable vector for immutable inputs.

Clojure compatibility note:
In Clojure the following example is considered a successful FIND
due to the vagaries of integer representations:

    (find [1 2 3] 4294967296) => [4294967296 1])

In Common Lisp this will not happen, NIL would be returned.
")
  (:method ((coll cl:vector) key)
    (and (cl:integerp key)              ;allow bogus keys, juuuust great
         (and (>= key 0)
              (< key (cl:length coll))
              (cl:list key (aref coll key)))))
  (:method ((coll fset:seq) key)
    (and (cl:integerp key)
         (and (>= key 0)
              (< key (fset:size coll))
              (vector key (fset:lookup coll key)))))
  (:method ((coll cl:hash-table) key)
    (let ((v (cl:gethash key coll #1=':noval)))
      (if (eq v #1#)
          nil
          (cl:list key v))))
  (:method ((coll fset:map) key)
    (multiple-value-bind (val found?)
        (fset:lookup coll key)
      (if found?
          (vector key val)
          nil)))
  (:method ((coll t) key)
    (declare (ignore key))
    (error "FIND is not supported on collections of type ~s" (type-of coll))))

(defun replace (rmap &optional (coll nil collp))
  "([rmap] [rmap coll])

Given a map of replacement pairs (RMAP) and a vector/collection (COLL), returns a
vector or lazy sequence such that any element that is a key in RMAP is replaced with
the value for that key in RMAP.

Returns a vector if COLL is a vector, otherwise the result is a lazy seqence.
Returns a transducer when no collection is provided.

RMAP is an associative collection (map/vector) of replacement key/value pairs.
If RMAP is a vector, the key is the ordinal index corresponding to the vector element.

MUTATION ALERT:
If COLL is a CL:VECTOR it is changed in place and returned, otherwise the usual
immutable semantics apply.

Assumes Clojure's quirky semantics for vectors as 'associative', despite varying and
contradictory documentation in clojure-docs.org (e.g. `find` vs `get` vs `replace`
w.r.t. vectors as logical maps).

E.g. (replace [:zeroth :first] [0 1 2 3 0]) is the same as
     (replace {0 :zeroth 1 :first} [0 1 2 3 0]) 
     => [:zeroth :first 2 3 :zeroth]

Clojure fails to document that the 'seq' returns will in fact be lazy sequences,
and no other kind of seq. This function is compliant as of Clojure version 1.12.

Note that something like:

    (replace {:a 1 :b 2} {:a 9 :b 12}) ;WRONG

isn't going to do what you want, replace is about seqs in and seqs out,
keys in this example would have to be mapentries because the COLL will be treated
as a sequence of MapEntry representations. Use UPDATE or ASSOC
if you want to update keys in a map.

Shadows CL:REPLACE.

See also MREPLACE which is eager and will also mutate input lists, and can return
CL:LISTs instead of lazy sequences."

  (let ((no-value #1=':no-value)) ;(get v|m k no-value)
    (if collp 
        ;; Not a transducer
        (if (vectorp coll)
            ;; Vector result please
            (reduce (lambda (r key)
                      (let ((newval (get rmap key no-value)))
                        (if (eq newval no-value)
                            (conj r key)
                            (conj r newval))))
                    (empty coll)
                    coll)
            ;; Lazyseq result please
            (map (lambda (key)
                   (let ((newval (get rmap key no-value)))
                     (if (eq newval no-value)
                         key
                         newval)))
                 coll))
        ;; Need a transducer
        (transducer (rf result input)
          (let ((newval (get rmap input no-value)))
            (if (eq newval no-value)
                (funcall rf result input)
                (funcall rf result newval)))))))

(defun mreplace (rmap coll)
  "([rmap coll])

Like REPLACE except that:
- it is eager,
- it will return CL:VECTORs where REPLACE might return immutable vectors,
- it will return CL:LISTs where REPLACE would return lazy or other seqs,
- it will return NIL as the empty CL:LIST result 
  (whereas replace would return an empty persistent list),
- it will return #() as the result of empty mutable/immutable vector inputs
- if COLL is a CL:LIST or CL:VECTOR, it is mutated and returned, you may 
  wish to make a copy.

See REPLACE for more details."
  (let* ((no-value #1=':no-value)       ;(get v|m k no-value)
         (vcount (count coll))
         (result (typecase coll
                   (cl:list     
                    (check-mutation-barrier coll)
                    coll)
                   (cl:vector
                    (check-mutation-barrier coll)
                    coll)
                   (fset:seq (make-array vcount :adjustable t :fill-pointer vcount))
                   ;; Everything else becomes a (newly allocated) list
                   (t (make-list (count coll))))))
    ;; Could potentially further optimize by not updating unchanged values when 
    ;; (eq coll result).
    (if (cl:listp result)
        (loop for cons on result
              for s = (seq coll) then (next s) ;fast enough for cl:list colls
              while s
              as key = (first s)
              do (let ((newval (get rmap key no-value)))
                    (if (eq newval no-value)
                        (setf (car cons) key)
                        (setf (car cons) newval))))
        ;; result is cl:vector, try to optimize for cl:vector coll input iteration
        (locally (declare (cl:vector result))
          (if (cl:vectorp coll)
              (loop for i from 0 below vcount
                    for key across coll
                    do (let ((newval (get rmap key no-value)))
                         (if (eq newval no-value)
                             (setf (aref result i) key)
                             (setf (aref result i) newval))))
              ;; coll is immutable vector
              (let ((i 0))
                (declare (fixnum i))
                (run! (lambda (key)
                        (let ((newval (get rmap key no-value)))
                          (if (eq newval no-value)
                              (setf (aref result i) key)
                              (setf (aref result i) newval))
                          (incf i)))
                      coll)))))
    result))

;;;
;;; MAPS
;;;

(defun select-keys (map keys)
  "([map keys])
Returns a map containing only those entries in MAP (or vector because
they're 'associative' in clojure) whose key is in KEYS.

E.g. (select-keys {:a 1 :b 2} [:a]) => {:a 1}
     (select-keys [1 2 3] [0 0 2 4]) => {0 1 2 3} 

If no keys are present in the map, an empty map is returned.

Mutable map/vector as input means mutable map as output, but it will be a copy of MAP
so this is not a destructive function. Keys may be any coll/seq."
  (let ((r (if (coll? map) (hash-map) (cl-hash-map))))
    (run! (lambda (key)
            (when-let (map-entry (find map key))
              (setf r (conj r map-entry))))
          keys)
    r))

(defun frequencies (coll)
  "([coll])
Returns a map from distinct items in coll to the number of times
they appear.

(frequencies nil) returns an empty immutable map for compatibility with Clojure.

Mutable in, mutable out, if COLL is mutable, returned map will be a CL:HASH-TABLE
otherwise it will be an immutable map."
  (if (null coll)
      (hash-map)
      (let ((result
              (if (coll? coll)
                  (hash-map)
                  (cl-hash-map))))
        (doseq (item coll)
          (setf result (update result item (fnil #'inc 0))))
        result)))

(defun group-by (f coll)
  "([f coll])
Returns a map of the elements of COLL keyed by the result of
F on each element. The value at each key will be a collection of
corresponding elements, in the order they appeared in COLL.

(group-by f nil) yields an empty immutable map for clojure compatibility.

Mutable in, mutable out. If COLL is immutable, the resulting map will
be immutable, and map values will be immutable vectors.  If COLL is mutable,
the resulting map will be a CL:HASH-TABLE and the map values will be CL:LISTs."
  (if (null coll)
      (hash-map)
      (let ((f (alexandria:ensure-function f)))
        (declare (function f))
        (if (coll? coll)
            (reduce 
             (lambda (result item)
               (update result (funcall f item)
                       (fnil #'conj (vector)) item))
             (hash-map) coll)
            (let ((m (reduce
                      (lambda (result item)
                        (update result (funcall f item) #'cl-conj item))
                      (cl-hash-map) coll)))
              (maphash (lambda (k v)
                         (setf (gethash k m) (nreverse v)))
                       m)
              m)))))

(defun merge-with (f &rest maps)
  "([f & maps])
Returns a map that consists of the rest of MAPS conj-ed onto
the first.  If a key occurs in more than one map, the mapping(s)
from the latter (left-to-right) will be combined with the mapping in
the result by calling (F val-in-result val-in-latter).

If there are no maps, MERGE-WITH returns nil.

Mutable in, mutable out.  The type of result depends on the mutability
characteristics of the first map. If the first map is mutable, it is
modified and returned, otherwise an immutable map is returned.

The content of the map values will depend on the combining function provided.
Mixing mutable and immutable types is not recommended."
  (when maps
    (let ((result (cl:first maps))
          (f (alexandria:ensure-function f)))
      (doseq (m (cdr maps))
        (setf result
              (reduce (lambda (result mapentry)
                        ;; Gotta do the contains? to know if we're merging with F or
                        ;; just associng without F, therefore can't use update.
                        ;; To bad update doesn't have a 'no-found' option.
                        (let* ((k (first mapentry))
                               (new-val (second mapentry))
                               (old-val (get result k #1=':noval)))
                          (if (eq #1# old-val)
                              (assoc result k new-val)
                              (assoc result k (funcall f old-val new-val)))))
                      result
                      m)))
      result)))

(defun map-invert (m)
  "([m])

Implements clojure.set/map-invert.

Returns a new map with the vals mapped to the keys and vice versa.

(map-invert nil) => empty immutable map.

Mutable-in, mutable-out, if a mutable map is input,
a new mutable map witis returned.

Clojure compatibility: map key equality is not Clojure compatible
and does not use CLJ-COLL:EQUAL?. Key equivalence semantics resemble EQUAL."
  (reduce-kv (lambda (r k v) (assoc r v k)) 
             (or (empty m) (hash-map)) m))

(defun update-keys (m f)
  "([m f])

Given a map M and a function F of one argument, returns a new map whose
keys are the result of applying F to the keys of M, mapped to the
corresponding values of M.

Map entries may be lost if F fails to return a unique key for the enw map.

(update-keys nil f) results in an empty immutable map.

Is NOT mutable-in, mutable-out, if a mutable map is input,
a new mutable map is retuned.

Clojure compatibility: map key equality is not Clojure compatible
and does not use CLJ-COLL:EQUAL?. Key equivalence semantics resemble EQUAL."
  (let ((f (alexandria:ensure-function f)))
    (declare (function f))
    (reduce-kv (lambda (r k v) (assoc r (funcall f k) v))
               (or (empty m) (hash-map)) m)))

(defun update-vals (m f)
  "([m f])

Given a map M and a function F of one argument, returns a new map whose
values are the result of applying F to the values of M, mapped to the
corresponding keys of M.

(update-keys nil f) results in an empty immutable map.

Mutable-in, mutable-out, if a mutable map is input,
a new mutable map is retuned."
  (let ((f (alexandria:ensure-function f)))
    (declare (function f))
    (reduce-kv (lambda (r k v) (assoc r k (funcall f v)))
               (or (empty m) (hash-map)) m)))


;;;
;;; SETS
;;;

(defun require-sets-or-lists (args)
  "Require that all args be either nil, immutable sets, or common lisp lists.
If non-nil lists or sets are present, requir that all non-nil values be of the same type.

Returns the type of sets in args (one of CL:CONS or FSET:WB-SET),
or nil if there weren't any set specifications."
  (loop with type = nil
        for arg in args
        do (when arg 
             (unless type
               (setf type (type-of arg)))
             (unless (typep arg type) 
               (error "Non-nil arguments should be of uniform type, either cl:list or fset:set types.  Received the following non-nil types: ~s"
                      (->> args
                           (mfilter (complement #'null))
                           (mmap #'type-of)))))
           (unless (typep arg '(or fset:set cl:list))
             (error "Unsupported type for set-oriented operation: ~s, ~s"
                    (type-of arg) arg))
        finally (return type)))

(defun disj (set &rest keys)
  "([set] [set key] [set key & ks])
disj[oin]. Returns a new set of the same (hashed/sorted) type, that
does not contain key(s).

(disj nil ...) returns nil.

Clojure incompatibility: set membership tests rely on FSET:EQUAL?, which is
not as liberal as the structural equivalence used by Clojure's `=` function
and CLJ-COLL:EQUAL? (which approximates Clojure's `=`).

See also: DISSOC for map/vector item removals."
  (when set
    (check-type set fset:set))
  (when set
    (if keys
        (loop for key in keys
              do (setf set (fset:less set key))
              finally (return set))
        set)))

(defun union (&rest sets)
  "([] [s1] [s1 s2] [s1 s2 & sets])

Implement clojure.set/union for sets.

Sets may be given as either CL:LIST or immutable set entities, but not both.
Vectors of any type are not supported because of their problematic Clojure history with set
operations.

If CL:LIST inputs are given, we use CL:UNION with a CLJ-COLL:EQUAL? test and return a cl:list.

If immutable set inputs are given, we use a FSET:EQUAL? test and return an immutable set.
Clojure compatibility: FSET:EQUAL? is restrictive compared to Clojure equivalence
semantics.

(union) returns nil
(union nil) returns nil

Nil is treated as the empty set, but non-nil result is returned only if
a non-nil input occurs."
  (let* ((sets (cl:delete-if #'null sets))
         (set-type (require-sets-or-lists sets))
         (set-fun (if (eq set-type 'cl:cons)
                      (lambda (s1 s2)
                        (cl:union s1 s2 :test #'equal?))
                      #'fset:union)))
    (when sets
      (if (length-1? sets)
          (cl:first sets)
          (loop with result = (cl:first sets)
                for other in (cl:cdr sets)
                do (setf result (funcall set-fun result other))
                finally (return result))))))

(defun difference (set &rest sets)
  "([s1] [s1 s2] [s1 s2 & sets])

Implements clojure.set/difference, returning a set or list
that is the first set without elements of the remaining sets.

Sets may be given as either CL:LIST or immutable set entities, but not both.
Vectors of any type are not supported because of their problematic Clojure history with set
operations.

If CL:LIST inputs are given, we use CL:SET-DIFFERENCE with a CLJ-COLL:EQUAL? test
and return a CL:LIST (NIL if empty).

If immutable set inputs are given, we use a FSET:EQUAL? test and return an immutable set.
Clojure compatibility: FSET:EQUAL? is restrictive compared to Clojure equivalence
semantics.

(difference nil) => nil
(difference nil ...) => nil
NIL is treated as the empty set, but non-nil result is returned only if
a non-nil input occurs in a semantically meaningful position.

Clojure incompatibility: set membership tests rely on FSET:EQUAL?, which is
not as liberal as the structural equivalence used by Clojure's `=` function
and CLJ-COLL:EQUAL? (which approximates Clojure's `=`).

See also: CL:SET-DIFFERENCE, CL:NSET-DIFFEREENCE, which operate on cl:lists"
  (let* ((sets (cl:delete-if #'null (cl:list* set sets)))
         (set-type (require-sets-or-lists sets))
         (set-fun (if (eq set-type 'cl:cons)
                      (lambda (s1 s2)
                        (cl:set-difference s1 s2 :test #'equal?))
                      #'fset:set-difference)))
    (when sets
      (if (length-1? sets)
          (cl:first sets)
          (loop with result = (cl:first sets)
                for other in (cl:cdr sets)
                do (setf result (funcall set-fun result other))
                finally (return result))))))

(defun intersection (set &rest sets)
  "([s1] [s1 s2] [s1 s2 & sets])

Implements clojure.set/intersection

Sets may be given as either cl:list or immutable set entities, but not both.
Vectors of any type are not supported because of their problematic Clojure history with set
operations.

If cl:list inputs are given, we use CL:INTERSECTION with a CLJ-COLL:EQUAL? test
and return a cl:list (NIL if empty).

If immutable set inputs are given, we use a FSET:EQUAL? test and return an immutable set.
Clojure compatibility: FSET:EQUAL? is restrictive compared to Clojure equivalence
semantics.

If any argument is nil, intersection returns nil (even for set inputs).
(You decide if this is at odds with other clojure set function semantics regarding nils,
for whatever it's worth we're clojure compatible here).

Good clojure incompatibility: this function rejects Clojure's buggy
allowance of non-set args (like vectors) that result in erroneous
answers (e.g. `(intersection #{1} [1]) => #{}`).  Do it right, or not at all.

Shadows CL:INTERSECTION
See also: CL:INTERSECTION, CL:NINTERSECTION, which operate on cl:lists.
See also CL:SET-EXCLUSIVE-OR and CL:NSET-EXCLUSIVE-OR for XOR semantics
not found in the Clojure API."
  (let* ((sets (cl:list* set sets))
         (set-type (require-sets-or-lists sets))
         (set-fun (if (eq set-type 'cl:cons)
                      (lambda (s1 s2)
                        (cl:intersection s1 s2 :test #'equal?))
                      #'fset:intersection)))
    (if (some #'null sets)
        nil
        (when sets
          (if (length-1? sets)
              (cl:first sets)
              (loop with result = (cl:first sets)
                    for other in (cl:cdr sets)
                    do (setf result (funcall set-fun result other))
                    finally (return result)))))))

(defun subset? (set1 set2)
  "([set1 set2])

Implements clojure.set/subset?

Return T if SET1 is a subset of SET2, NIL otherwise.

Sets may be given as either cl:list or immutable set entities, but not both.
Vectors of any type are not supported because of their problematic Clojure history with set
operations.

If cl:list inputs are given, we use CL:SUBSETP with a CLJ-COLL:EQUAL? test
and return a cl:list (NIL if empty).

If immutable set inputs are given, we use a FSET:EQUAL? test and return an immutable set.
Clojure compatibility: FSET:EQUAL? is restrictive compared to Clojure equivalence
semantics.

NIL is treated as an empty set.

Good clojure incompatibility: this function rejects Clojure's buggy
allowance of non-set args (like vectors) that result in erroneous
answers (e.g. `(subset? #{1} [1]) => nil`).  Do it right, or not at all.

See also: CL:SUBSETP, which operates on CL:LIST types."
  (let* ((sets (cl:list set1 set2))
         (set-type (require-sets-or-lists sets))
         (set-fun (if (eq set-type 'cl:cons)
                      (lambda (s1 s2)
                        (cl:subsetp s1 s2 :test #'equal?))
                      #'fset:subset?)))
    ;; Clojure semantics here
    (if (null set1)
        t
        (if (null set2)
            (empty? set1)               ;both empty sets or nil
            ;; both sets are non-nil
            (funcall set-fun set1 set2)))))
          
(defun superset? (set1 set2)
  "([set1 set2])

Implements clojure.set/superset?

Return T if SET1 is a superset of SET2, NIL otherwise.

NIL is treated as an empty set.

Sets may be given as either cl:list or immutable set entities, but not both.
Vectors of any type are not supported because of their problematic Clojure history with set
operations.

If cl:list inputs are given, we use CL:SUBSETP (in the logically appropriate fashion)
with a CLJ-COLL:EQUAL? test and return a cl:list (NIL if empty).

If immutable set inputs are given, we use a FSET:EQUAL? test and return an immutable set.
Clojure compatibility: FSET:EQUAL? is restrictive compared to Clojure equivalence
semantics."
  (subset? set2 set1))


(defun require-immutable-rel (rel)
  "Ensure that rel and its children are immutable data structures.
Generally REL should be some immutable collection, CLojure seems to allow
lists, sets, and maps, whose children are maps or map-entries.

Clojure documentation shows sets of maps as being rels, but INDEX
will work just fine on a single map (and index the mapentries), or
a list of maps."
  ;; Okay, you're still scratching your head about this restriction.
  ;; Basically if you use cl:hash-tables as values in Fset sets or maps
  ;; it currently only compares them with EQ, and thus will pretty much always
  ;; return the wrong comparison result when used for higher order CLojure operations.
  ;; IIRC...
  (unless (coll? rel)
    (error "Immutable collection expected where a ~s was received" (type-of rel)))
  (doseq (x rel)
    (unless (or (map? x) (map-entry? x))
      (error "Immutable collection expected where a ~s was received" (type-of x)))))

(defun index (rel keys)
  "([rel keys])

This implements clojure.set/index, one of the so-called 'relation' functions. 

A relation (or 'rel') is a collection of maps that each have the same keys such as
might be derived from clojure.java.jdbc queries. It can also be a single map whose
matching mapentries are indexed.

Returns a map of the distinct values of KEYS in the collection/seq REL, mapped to a set of
the maps in REL with the corresponding values of KEYS.

Said another way, INDEX returns a map whose keys are maps of keys k (from keys) with
values v (from maps in rel), and whose values are the subset of maps in REL that had
those k/v pairs.

E.g.
    (defvar *weights* #{{:name 'betsy :weight 1000}
                        {:name 'jake :weight 756}
                        {:name 'shyq :weight 1000}})
    (index *weights* [:weight])
    =>
    {{:weight 756}  #{{:name jake :weight 756}} 
     {:weight 1000} #{{:name shyq :weight 1000} 
                      {:name betsy :weight 1000}}}

Temporary(?) restriction: do not use mutable content in REL
or you are likely to get results which won't compare correctly with EQUAL?
due to hash-table and keyed immutable collection comparison limitations.

See also: PARTITION-BY, GROUP-BY"
  (require-immutable-rel rel)           ;hopefully temporary

  ;; We also need mutable sets (as cl:lists) to do mutable output.
  (let ((empty-set (hash-set)))
    (reduce (lambda (r map)
              ;; Creating map keys in result, not mapentry keys
              ;; thus the use of select-keys vs find
              (let ((m (select-keys map keys)))
                (update r m (fnil #'conj empty-set) map)))
            (hash-map)
            rel)))

(defun rename-keys (map key-map)
  "([map key-map])

This implements clojure.set/rename-keys, because apparently it made sense to put
map functions in the clojure.set namespace and that's not quirky at all.

Returns MAP with the keys in KEY-MAP renamed to the vals in KEY-MAP.

The renamed key will have the value of the old key. If the new key is already
present in the map with some other value, it will have the value associated with
the key that was renamed. I.e. `(rename-keys {:a 1 :b 2} {:a :b}) => {:b 1}`

Keys in KEY-MAP which are not in MAP are ignored.

Keys should be a map, seqs of map-entries are apparently not supported in clojure.

Mutable in, mutable out, if you pass a CL:HASH-TABLE as input, it is modified and returned.
Note restricted CL:EQUAL key semantics for CL:HASH-TABLE and FSET:EQUAL? semantics
for immutable maps."
  (if (cl:hash-table-p map)
      ;; Need old values before we start zapping keys in case of 
      ;; key swaps/collisions, i.e. (rename-keys {:a 1 :b 2} {:a :b :b :a})
      (progn 
        (check-mutation-barrier map)
        (let* ((no-value #1=':no-value)
               (key-map-entries (map-entries-as-list key-map)) ;handles any key map
               (old-keys (cl:mapcar #'cl:first key-map-entries))
               (new-keys (cl:mapcar #'cl:second key-map-entries))
               (old-vals (cl:mapcar (lambda (k) (gethash k map no-value)) old-keys)))
          ;; Do the removals first lest you remove a renamed key later
          (loop for old-key in old-keys do (remhash old-key map))
          (loop for new-key in new-keys
                for old-val in old-vals
                unless (eq old-val no-value)
                  do (setf (gethash new-key map) old-val))
          map))
      ;; This works for persistent types only, otherwise the old value is removed
      ;; before we can access it.
      (if (map? map)
          (reduce 
           (lambda (r mapentry)
             (multiple-value-bind (old-key new-key) (map-entry-values mapentry)
               (if (contains? map old-key)
                   (assoc r new-key (get map old-key))
                   r))) 
           ;; Apply dubious here if we have a lot of keys being renamed.
           (apply #'dissoc map (mkeys key-map))
           key-map)
          (error "rename-keys is unsupported on objects of type ~s" (type-of map)))))

(defun rename (rel keymap)
  "([rel keymap])

This implements clojure.set/rename, one of the so-called 'relation' functions.

A relation (or 'rel') is a collection of maps that each have the same keys such as
might be derived from clojure.java.jdbc queries. It can also be a single map whose
matching mapentries are indexed.

Returns a 'rel' of the maps in REL with the keys in KEYMAP renamed to the vals in KEYMAP.

Mutable in, mutable out, if REL is mutable
the resulting container will be a cl:list, not an immutable set.

Mixing mutable and immutable collections is not recommended and will 
likely lead to unexpected results, but it is only rejected
if REL is immutable."
  (when (coll? rel)
    ;; May seem redundnt with coll? test above, but we want to check children in this case.
    (require-immutable-rel rel))           ;hopefully temporary
  (let ((renamed-maps 
          (mmap (lambda (map) (rename-keys map keymap)) rel)))
    (if (coll? rel)
        (set renamed-maps)
        renamed-maps)))

(defun split-at (n coll)
  "([n coll])
Returns a vector of [(take n coll) (drop n coll)],
which means a 2 element vector of unrealized lazy sequences.

See also: SPLITV-AT, SUBVEC, MSPLIT-AT"
  (vector (take n coll) (drop n coll)))  

(defun splitv-at (n coll)
  "([n coll])
Returns a vector of [(into [] (take n) coll) (drop n coll)]
replacing the first lazy-seq of `split-at` with a vector.

See also: SPLIT-AT, SUBVEC, MSPLIT-AT"
  ;; This seems a really dubious addition to Clojure's core APIs...
  (vector (into (vector) (take n) coll) (drop n coll)))

(defun msplit-at (n coll &key (result-type 'cl:list) (partition-type 'cl:list))
  "Like SPLIT-AT, but with Common Lisp collections as output.

Basically (cl:list (mtake 'cl:list n coll) (mdrop 'cl:list n coll)).

Where the outer cl:list is the effect of RESULT-TYPE and the inner cl:list arguments
to mtake/mdrop are controlled by the PARTITION-TYPE keyword.

Return a 2 element sequence of RESULT-TYPE whose first element is a PARTITION-TYPE
of the first N values of COLL, and whose second element is a PARTITION-TYPE
of the remaining values of COLL.

CL:LIST valued partitions with no members will be NIL.
CL:VECTOR valued partitions with no members will be #().

See also: SPLIT-AT, MTAKE, MDROP"
  (ecase result-type
    (cl:list (cl:list (mtake partition-type n coll) (mdrop partition-type n coll)))
    (cl:vector 
     (let ((result (make-array 2)))
       (setf (svref result 0) (mtake partition-type n coll)
             (svref result 1) (mdrop partition-type n coll))
       result))))

(defun split-with (pred coll)
  "([pred coll])
Returns a vector of [(take-while pred coll) (drop-while pred coll)]

See also: MSPLIT-WITH."
  (vector (take-while pred coll) (drop-while pred coll)))

(defun msplit-with (pred coll &key (result-type 'cl:list) (partition-type 'cl:list))
  "An eager version of SPLIT-WITH returning either CL:LIST or CL:VECTOR results.

Basically (cl:list (mtake-while pred coll) (mdrop-while pred coll))

Where the outer cl:list is the effect of RESULT-TYPE and the inner cl:list arguments
to mtake-while/mdrop-while are controlled by the PARTITION-TYPE keyword.

Return a 2 element sequence of RESULT-TYPE whose first element is a PARTITION-TYPE
of the N values of COLL for which PRED returns true, and whose second element is a PARTITION-TYPE
of the remaining values of COLL.

CL:LIST valued partitions with no members will be NIL.
CL:VECTOR valued partitions with no members will be #().

See also: SPLIT-WITH, MTAKE-WHILE, MDROP-WHILE"
  (ecase result-type
    (cl:list (cl:list (mtake-while partition-type pred coll) 
                      (mdrop-while partition-type pred coll)))
    (cl:vector
     (let ((result (make-array 2)))
       (setf (svref result 0) (mtake-while partition-type pred coll)
             (svref result 1) (mdrop-while partition-type pred coll))
       result))))

(defun mshuffle (coll)
  "([coll])
Return a random permutation of coll as a new cl:vector.

See also: SHUFFLE"
  (-> (convert 'cl:vector coll)
      (serapeum:reshuffle)))

(defun shuffle (coll)
  "([coll])
Return a random permutation of coll as an immutable vector.

See also: MSHUFFLE."
  (vec (mshuffle coll)))

(defun reverse (coll)
  "([coll])
Returns an immutable list of items in COLL in reverse order."
  (reduce #'conj (list) coll))

(defun mreverse (result-type-or-coll &optional (coll nil collp))
  "([coll] [result-type coll])
Returns a CL:LIST of items in COLL in reverse order.

An optional leading RESULT-TYPE argument may be given as
the symbol CL:LIST or CL:VECTOR to specify the desired result type.

If COLL is empty, returns NIL if CL:LIST is the result-type, 
or #() if CL:VECTOR is the result-type."
  (let ((result-type (if collp result-type-or-coll 'cl:list))
        (coll (if collp coll result-type-or-coll)))
    (ecase result-type
      (cl:list (reduce #'cl-conj '() coll))
      (cl:vector
       (let ((size (count coll)))
         (if (zerop size)
             #()
             (let ((v (make-array size :fill-pointer size)))
               (reduce #'vector-conj-reverse v coll)
               (setf (fill-pointer v) size)
               v)))))))

(defun rand-nth (coll)
  "([coll])

Basically `(nth coll (rand-int (count coll)))`

Return a random element of the (sequential) collection. Will have
the same performance characteristics as NTH for the given
collection.

Clojure semantics: 
    (rand-nth nil) => nil
    (rand-nth [])  => IndexOutOfBoundsException

Which we emulate here."
  (if (null coll)
      nil
      (nth coll (rand-int (count coll)))))

(defun max-key (kfn x &rest xs)
  "Returns the X for which (funcall KFN X), a number, is greatest.
If there are multiple such Xs, the last one is returned.

See also: MIN-KEY"
  (let ((k-result (funcall kfn x))
        (x-result x))
    (loop for x in xs
          as k = (funcall kfn x)
          when (>= k k-result)
            do (setf k-result k x-result x))
    x-result))

(defun min-key (kfn x &rest xs)
  "Returns the X for which (funcall KFN X), a number, is least.
If there are multiple such Xs, the last one is returned.

See also: MAX-KEY"
  (let ((k-result (funcall kfn x))
        (x-result x))
    (loop for x in xs
          as k = (funcall kfn x)
          when (<= k k-result)
            do (setf k-result k x-result x))
    x-result))

;;;
;;; For clojure.walk/ and clojure.set/join/project/select
;;; I have not bothered to do much to improve or alter the basic
;;; clojure algorithms.  Pure lazyness to get this thing out.
;;;

(defun walk (inner outer form)
  "([inner outer form])
Traverses FORM, an arbitrary data structure. INNER and OUTER are
FUNCTIONS. 

For each element walked for which COLLP is true, WALK applies INNER to each element
of FORM, building up a data structure of the same type, then applies OUTER to the
result.

If a given element is not a collection as per COLLP, simply calls OUTER on the
object.

Recognizes all supported CLJ-COLL collection/seq types (cl:list, cl:vector,
cl:hash-table, and CLJ-COLL immutable & sequence types).

Consumes seqs as with doall."
  ;; 1) We want order preserved so we can't use INTO on a persistent list,
  ;; 2) Note that the DOALL caveat means outer is called on a 
  ;;    (doall (map inner form))
  ;; 3) 'seqs' become lazyseqs
  (if (collp form)
      (typecase form
        (cl:list (funcall outer (cl-map 'cl:list inner form)))
        (persistentlist (funcall outer (cl-map 'persistentlist inner form)))
        ((or seq lazyseq) (funcall outer (doall (map inner form))))
        (t (funcall outer (into (empty form) (cl-map 'cl:list inner form)))))
      (funcall outer form)))

(defun postwalk (f form)
  "([f form])
Performs a depth-first, post-order traversal of form. Calls F on
each sub-form, uses F's return value in place of the original.
Recognizes all CLJ-COLL supported data structures.
Consumes seqs as with doall."
  (walk (partial #'postwalk f) f form))

(defun prewalk (f form)
  "([f form])
Like postwalk, but does pre-order traversal."
  (walk (partial #'prewalk f) #'identity (funcall f form)))

(defun prewalk-replace (smap form)
  "([smap form])
Recursively transforms form by replacing keys in SMAP with their
values. Similar to REPLACE but works on any data structure. Does
replacement at the root of the tree first."
  (prewalk (lambda (x) (if (contains? smap x) (get smap x) x)) form))

(defun postwalk-replace (smap form)
  "([smap form])
Recursively transforms form by replacing keys in smap with their
values. Similar to REPLACE but works on any data structure. Does
replacement at the leaves of the tree first."
  (postwalk (lambda (x) (if (contains? smap x) (get smap x) x)) form))

(defun postwalk-demo (form)
  "Demonstrates the behavior of postwalk by printing each form as it is
walked. Returns form.

E.g. (postwalk-demo [[1 2] [3 4 [5 6]] [7 8]]) => <lots of output>"
  (postwalk (lambda (x) (format t "Walked: ~s~%" x) x) form))

(defun prewalk-demo (form)
  "Demonstrates the behavior of prewalk by printing each form as it is
walked. Returns form.

E.g. (prewalk-demo [[1 2] [3 4 [5 6]] [7 8]]) => <lots of output>"
  (prewalk (lambda (x) (format t "Walked: ~s~%" x) x) form))


(defun join (xrel yrel &optional (km nil km-p))
  "([xrel yrel] [xrel yrel km])

Implements clojure.set/join

A relation (or 'rel') is a set of maps expected to have the same keys.
In Clojure these might typically arise from clojure.java.jdbc queries.

When passed 2 relations join returns a relation corresponding to the natural
join of the inputs. If a keymap is specified the join takes place on the corresponding
keys.

E.g. (join #{{:a 1} {:a 2}} #{{:b 1} {:b 2}})
=>   #{{:b 1, :a 1} 
       {:b 2, :a 1} 
       {:b 1, :a 2} 
       {:b 2, :a 2}}

Temporary(?) restriction: do not use mutable content in REL
or you are likely to get results which won't compare correctly with EQUAL?
due to hash-table and keyed immutable collection comparison limitations."
  (if km-p
      ;; arbitrary key mapping
      (multiple-value-bind (r s k)
          (if (<= (count xrel) (count yrel))
              (values xrel yrel (map-invert km))
              (values yrel xrel km))
        (let ((idx (index r (vals k)))) ;index signals error if immutable data
          (reduce (lambda (ret x)
                    (let ((found (get idx (rename-keys (select-keys x (keys k)) k))))
                      (if found
                          (reduce (lambda (m1 m2) (conj m1 (merge m2 x))) ret found)
                          ret)))
                (hash-set) s)))
   ;; natural join
   (if (and (seq xrel) (seq yrel))
     (let ((ks (intersection (set (keys (first xrel))) (set (keys (first yrel))))))
       (multiple-value-bind (r s)
           (if (<= (count xrel) (count yrel))
               (values xrel yrel)
               (values yrel xrel))
         (let ((idx (index r ks)))
           (reduce (lambda (ret x)
                     (let ((found (get idx (select-keys x ks))))
                       (if found
                           (reduce (lambda (m1 m2) (conj m1 (merge m2 x))) ret found)
                           ret)))
                   (hash-set) s)))))))

(defun select (pred xset)
  "([pred xset])

Implements clojure.set/select

Returns a set of the elements for which pred is true.
XSET must be a CLJ-COLL Set.

See also: FILTER, MFILTER"
  (let ((pred (alexandria:ensure-function pred)))
    (declare (function pred))
    (reduce (lambda (s k) (if (funcall pred k) s (disj s k)))
            xset xset)))

(defun project (xrel ks)
  "([xrel ks])

Implements clojure.set/project

Returns a 'rel' of the elements of XREL with only the keys in KS.

A relation (or 'rel') is a set of maps expected to have the same keys.
In Clojure these might typically arise from clojure.java.jdbc queries."
  ;; You can pass mutable maps, but it isn't recommended.
  (set (cl-map 'cl:list (lambda (k) (select-keys k ks)) xrel)))
