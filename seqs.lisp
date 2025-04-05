(in-package :clj-coll)

;;;; Lazy sequences and 'seq' pragmatically identical Clojure at the API level, but
;;;; in no ways identical in the implementation, or as performant (no chunking, for
;;;; example).  They're for convenience, if you're doing performance intensive work,
;;;; consider using native common lisp data structures (which, yay, you can use with
;;;; the CLJ-COLL Clojure APIs).
;;;;
;;;; There are a number of low-hanging fruits to improve performance that have not
;;;; yet been done.  The first release focuses on correctness.
;;;;
;;;; Seqs, lazy sequences, and persistent collections should be thread safe, but it has
;;;; not been tested in this regard. Please report bugs if you enounter them.

;;;; Hear ye, hear ye, on the zen of seqs, lest ye be lost.
;;;;
;;;; * The collections you know and love tend to be (logical) sequences.
;;;;   Even unordered collections like maps are 'seqable?' and produce sequences.
;;;; * There are also _things_ called 'seqs', which provide a sequential
;;;;   view onto collections (including maps as a sequence of MapEntry datums),
;;;;   or more abstract things such as lazyseqs.
;;;; * There are different kinds of seqs providing views onto different types of
;;;;   data sources. For a thing to act as a seq it must support a certain protocol,
;;;;   namely first(), next(), and cons() to manipualte seq elements.
;;;;   In clojure things which implement this interface are "ISeq"s.
;;;; * Some object types here are "isa" seqs, e.g. a CLJ-COLL:CONS.
;;;;   Having a pointer to one means you have a seq with at least one value.
;;;;   For these data structures (seq <cons>) => <cons>.
;;;; * Lazyseqs represent a data source where none of the data is available
;;;;   until you "realize" values. To realize and look at values, you need a 'seq'
;;;;   on the lazyseq, i.e. `(seq <lazyseq>)`.
;;;; * CAUTION: Lazyseqs are not seqs, at least not here where anything
;;;;   that "isa" seq must have a value associated with it. Lazyseqs may have
;;;;   zero values, and you won't know until you request a seq on it and attempt
;;;;   to realize a value.
;;;; * The 'seq' function coerces things to non-empty sequences or returns NIL.
;;;;   if the data source or collection has no values.  
;;;; * You can use CLJ-COLL:CONS to glue together different types of `seqable?`
;;;;   entities.  So you can cons onto the head of a lazy sequence, an array seq,
;;;;   and so on, e.g. (cons 1 [2 3]) => a logical persistent list of 1 2 3.
;;;;   Whether that shows up as a PersistentList, some Cons objects, or combinations
;;;;   of the above is nuanced. CLJ-COLL goes to some effort to behave like Clojure
;;;;   in this regard.

;;;; Miscelleneous older notes to myself are in ./seq-notes.txt

;; I could not get a structural constant to work as expected in CCL
;; for +EMPTY-LIST+, despite or because of the combinations of EVAL-WHEN
;; and make-load-form that CCL required. It worked fine on SBCL.
;; The intent was to be able to use EQ on a constant struct instance.
;; So... I gave up, here we are with a DEFVAR.
(defvar *EMPTY-LIST* (make-emptylist)
  "Meant to be a singleton instance of an immutable empty list for use with EQ.")

(defvar *EMPTY-VECTOR* (fset:seq)
  "Meant to be a singleton instance of an immutable empty vector for use with EQ.")

(defvar *EMPTY-QUEUE* (make-persistentqueue)
  "Meant to be a singleton instgance of an immutable empty queue for use with EQ.")

(defun pq-pop (pq)
  "Perform POP on a persistent queue, always returning a (possibly empty) persistent queue.

POP on an empty queue returns an empty queue.

Note that 'pop' and 'first' have different semantics w.r.t. queues, see the
persistentqueue unit test."
  (declare (persistentqueue pq))
  ;; See also: queueseq-next which is slightly similar, but on the seq, not the queue
  (if (persistentqueue-front-seq pq)
      (if-let (n (next (persistentqueue-front-seq pq)))
        (make-persistentqueue :front-seq n :rear-vec (persistentqueue-rear-vec pq)
                              :count (1- (persistentqueue-count pq)))
        (make-persistentqueue :front-seq (seq (persistentqueue-rear-vec pq))
                              :count (1- (persistentqueue-count pq))))
      pq))                              ;queue is empty, pop returns empty queue

(defun pq-cons (x pq)                   ;pq == persistentqueue
  "Return a new PersistentQueue with X added to PersistentQueue PQ for CONJ."
  (if-let (f (persistentqueue-front-seq pq))
      (make-persistentqueue :front-seq f 
                            :rear-vec (conj (or (persistentqueue-rear-vec pq) *EMPTY-VECTOR*) x)
                            :count (1+ (persistentqueue-count pq)))
      (make-persistentqueue :front-seq (list x) :count (1+ (persistentqueue-count pq)))))

(defun queue (&rest xs)
  "Create an immutable FIFO queue as if you had CONJ'd each of the Xs
onto CLJ-COLL:*EMPTY-QUEUE* (which is CLJ-COLL's version of
clojure.lang.PersistentQueue/EMPTY.

The first x in Xs will be at the head of the queue.

So: (queue 1 2 3) == (conj *EMPTY-QUEUE* 1 2 3) => <-(1 2 3)-<

Clojure compatibility: Clojure has no QUEUE function. Hopefully this convenience
won't conflict with future clojure releases.  One wonders why the designers of
Clojure felt compelled to make you type `clojure.lang.PersistentQueue/EMPTY` to
create persistent queues."
  (reduce #'conj *EMPTY-QUEUE* xs))

(defun queue? (pq)
  "Return T if PQ is an immutable FIFO queue, NIL otherwise."
  (persistentqueue-p pq))

(defstruct (Cons
            ;; Clojure cons is also an IPersistentList, not expressed here, handled in `cons`.
            (:include Seq)
            ;; Clojure doesn't have a cons predicate, avoid confusing with cl:consp
            (:predicate cons?)
            (:copier nil))
  "Emulates an immutable Clojure Cons.
Clojure Cons objects are primarily used in the creation of lazy or other sequences.
Note that a Clojure Cons is not a Clojure List, nor is it in any way equivalent to a CL:CONS."
  (first nil :read-only t)    ;Object
  (more  nil :read-only t))   ;ISeq

(defun pcons (x persistentlist)
  "Like cons, but returns a persistent list where 'cons' might return a cons.
Assumes the second arg is a IPersistentList."
  (declare (IPersistentList persistentlist))
  (if (eq persistentlist *EMPTY-LIST*)
      (make-persistentlist :first x :count 1)
      (make-persistentlist :first x :rest persistentlist 
                           :count (1+ (persistentlist-count persistentlist)))))

(defun cons (x seq)
  "([x seq])
Returns a new seq where x is the first element and seq is the rest.
E.g. (cons 1 [2 3 4]) => 1 2 3 4

Do not confuse this with CL:CONS, CLJ-COLL:CONS is primarily used in lazy sequence
construction and prepending values to sequences."
  ;; Clojureisms:
  ;; (cons 1 nil) => (1) as a persistentlist
  ;; (cons 1 (list)) => (1) as a Cons 
  (typecase seq
    (null (make-persistentlist :first x :count 1))
    (EmptyList (make-cons :first x))
    (seq (make-cons :first x :more seq))
    (t (if (seq? seq) ;logically implements ISeq but doesn't subclass `seq?`, e.g. cl:list
           (make-cons :first x :more seq)
           (make-cons :first x :more (seq seq))))))

(defun list-from (items)
  "Auxiliary routine for LIST that will construct a PersistentList in order
of elements in ITEMS so we can avoid (apply list <list>) for some uses,
and generally convert collections to persistent lists with CONVERT."
  (let ((n (count items)))           ;we need N in advance for persistent list counts
    (cond ((= 0 n) *EMPTY-LIST*)
          ((= 1 n) (make-persistentlist :first (first items) :count 1))
          (t
           (loop 
             with head = (make-persistentlist :first (first items) :count n)
             with current = head
             for count from (1- n) downto 1
             for s = (-> items seq next) then (next s)
             as next = (make-persistentlist :first (first s) :count count)
             do (setf (persistentlist-rest current) next
                      current next)
             finally (return head))))))
    
(defun list (&rest items)
  "([& items])
Creates a new persistent list containing the items."
  (list-from items))

(defun list* (&rest items)
  "([args] [a args] [a b args] [a b c args] [a b c d & more])
Creates a new seq with leading items prepended to the 
last which will be treated as a sequence.

(list* <empty-seq>) => NIL.

You might expect a persistent list result, but that would
occur only with persistent list as the sole arg."
  (flet ((ensure-seq (x)
           (unless (seqable? x)
             (error "Expected a sequence, not ~s of type ~s" x (type-of x)))
           x))
    (let ((nargs (cl:length items)))
      (case nargs
        (0 (error "Missing args"))
        (1 (seq (cl:first items)))
        (t
         (loop with items = (nreverse items)
               with coll = (ensure-seq (cl:car items))
               for item in (cdr items)
               do (setf coll (cons item coll))
               finally (return coll)))))))


(defstruct (lazyseq
            (:constructor %lazyseq)
            (:copier nil))
  "Realized lazyseq node content.
THUNK is a function of zero arguments that returns some kind of sequence / lazyseq,
or NIL"
  thunk                                 ;non-nil if unrealized
  seq                                   ;Seq on the thunk return value if realized
  ;; If lock is non-nil, acquire it to examine/mutate other slots, including the lock slot.
  ;; If it is nil, you can assume the thunk has been invoked and values realized.
  ;; TBD: I'm not convinced I need a recursive lock here.
 (lock (bt:make-recursive-lock "lazyseq node lock")))

(defmacro lazy-seq (&body body)
  "Takes a body of expressions that returns a CONS whose CAR
is a sequence value, and whose CDR is the next lazyseq 
node.

The value returned by lazy-seq is a lazyseq object representing
the body expressions which have yet to be invoked.

BODY is executed only once when a suitable operation is invoked on the resulting
lazyseq, e.g. (first <lazyseq>)."
  `(%lazyseq :thunk (lambda () ,@body)))

(defun lazyseq-realize (lazyseq)
  "Realize the value of a lazy sequence node, if necessary.
This function should be called anytime a lazyseq has a lock object associated
with it before examining other slots.

This function is always called for possible side effects.
Returns a seq on the value returned by the thunk, or NIL if the thunk returns nil
or an empty collection.

Note that the thunk can return any seqable collection, including a CL:CONS or even
another lazyseq. The usual (-> thunkval seq first|next) cycles apply.

We also make a special case for dotted conses so that you can call cl:cons just like
you'd call clj-coll:cons so we can decode `(cl:cons n (lazyfun (1- n)))`
as if it were a regular sequence. We turn it into a persistent cons."
  ;; *TBD*, BUG?
  ;; Should have worked turning dottied pair thunkval into a CL:LIST, but didn't.
  ;; Perhaps a mystery for another day. Something about my seq support on CL:LIST?
  (declare (lazyseq lazyseq))
  (alexandria:if-let (lock (lazyseq-lock lazyseq))
    (bt:with-recursive-lock-held (lock)
      (when (lazyseq-lock lazyseq)      ;reexamine under lock
        ;; When we realize a lazyseq, we need to chase the thunk return value until
        ;; we know it's some kind of sequence that will have at least one value, or NIL.
        (let ((thunkval (funcall (lazyseq-thunk lazyseq))))
          (loop while (lazyseq-p thunkval)
                do (setf thunkval (lazyseq-realize thunkval)))
          (when (dotted-pair-p thunkval)
            (setf thunkval (cons (car thunkval) (cdr thunkval))))
          ;; Thunkval should now be something we can seq() on.
          ;; Caution printing lazyseqs while lazyseqs...
          ;;(format t "lazyseq-realize thunkval ~s" thunkval)
          (setf (lazyseq-seq lazyseq) (seq thunkval) ;could signal error
                (lazyseq-thunk lazyseq) nil
                (lazyseq-lock lazyseq) nil)
          (lazyseq-seq lazyseq))))
    (lazyseq-seq lazyseq)))

(defun lazyseq-realized? (lazyseq)
  "Return true if a value has been obtained from thunk."
  (declare (lazyseq lazyseq))
  ;; If there's a lock, you must use it.
  ;; If there's no lock object, this lazyseq is definitely realized.
  (alexandria:when-let (lock (lazyseq-lock lazyseq))
    (bt:with-recursive-lock-held (lock)
      (when (lazyseq-lock lazyseq)      ;reexamine under lock
        (return-from lazyseq-realized? nil))))
  t)

(defgeneric realized? (x)
  (:documentation "([x])
Returns true if a value has been produced for a promise, delay, future or lazy sequence.
CLJ-COLL defines only lazy sequences, for the concurrency operations see CLJ-CON,
which is also in quicklisp.  CLJ-CON and CLJ-COLL are not unified at this time.")
  (:method ((l lazyseq))
    "Return T if a lazy sequence node has called it thunk to produce a value, NIL otherwise."
    (lazyseq-realized? l)))

(defstruct (arrayseq
            (:include seq)
            (:copier nil))
  "A Clojure-style 'seq' on mutable and immutable arrays, which should not be modified once seq'd."
  (array nil :read-only t)              ;CL:VECTOR or FSET:SEQ
  (index 0 :type fixnum :read-only t))

(defstruct (arrayrseq
            (:include seq)
            (:copier nil))
  "A Clojure-style 'seq' on mutable and immutable vectors that iterates in reverse order."
  (array nil :read-only t)              ;CL:VECTOR or FSET:SEQ
  (index 0 :type fixnum :read-only t))  ;current viable index, starts at (1- count)

(defstruct (mapseq
            (:include seq)
            (:copier nil))
  "A Clojure-style 'seq' on mutable and immutable hash-table/map types.
Unfortunately both of those types do not provide their own reliable seqable behavior
so we must materialize keys in for the seq, and will fetch values as needed from
the underlying ht/map.

Like arrayseqs, mapseqs are immutable and thread-safe.
If created on a mutable map, the burden is on the map owner to avoid mutating
the map."
  (map nil :read-only t)                ;CL:HASH-TABLE or FSET:MAP
  (keys #() :type simple-vector :read-only t) ;keys from the map
  (index 0 :type fixnum :read-only t))

(defstruct (queueseq
            (:include seq)
            (:copier nil))
  "A 'seq' on PersistentQueue. Basically a snapshot a persistentqueue."
  (front-seq nil :read-only t) ;seq on persistentqueue front
  (rear-seq nil :read-only t) ;seq on persistenqueue rear vector
  (count 0 :type fixnum :read-only t))   ;for O(1) counts

(defun queueseq-first (queueseq)
  (declare (queueseq queueseq))
  (first (queueseq-front-seq queueseq)))

(defun queueseq-next (pqs)
  (declare (queueseq pqs))
  (if-let (f (next (queueseq-front-seq pqs)))
    (make-queueseq :front-seq f :rear-seq (queueseq-rear-seq pqs)
                   :count (1- (queueseq-count pqs)))
    (when-let (r (queueseq-rear-seq pqs))
      (make-queueseq :front-seq r :count (1- (queueseq-count pqs))))))
  

(defgeneric seq? (x)
  (:documentation
   "([x])
Returns true if x implements what passes for ISeq in Clojure, i.e.
    Object first();
    ISeq next();
    ISeq more();   -- 'more' IS NOT USED BY CLJ-COLL
    ISeq cons(Object o);

Does NOT mean that the entity is a 'seq' (though the entity _may_ be a seq), since
empty collections cannot be seqs. Also does not mean that the entity is immutable.

See also: COLL?")
  ;; Note that in both Clojure and CLJ-COLL, (seq? <vector-type>) is nil/false.
  ;; So seq on list types differs from seq on vector types.
  (:method ((x cl:cons)) t)
  (:method ((x IPersistentList)) t)     ;List, EmptyList, PersistentQueue
  (:method ((x seq)) t)                 ;seq, Cons, arrayseq, mapseq
  (:method ((x lazyseq)) t)             ;for Clojure compatibility, not for CLJ-COLL needs
  (:method ((x t)) nil))

(defgeneric seqable? (x)
  (:documentation
   "([x])
Return true if the SEQ function is supported for x")
  (:method ((x null)) t)                ;(seq x) => NIL
  (:method ((x cl:cons)) t)             ;(seq x) => x
  (:method ((x IPersistentList)) t)     ;List, EmptyList, PersistentQueue
  (:method ((x seq)) t)                 ;seq, Cons, ArraySeq, MapSeq
  (:method ((x lazyseq)) t)
  (:method ((x cl:vector)) t)           ;arrayseq, note that clojure strings are seqable
  (:method ((x fset:seq)) t)            ;also arrayseq
  (:method ((x fset:set)) t)            ;arrayseq on set vals
  (:method ((x cl:hash-table)) t)       ;mapseq 
  (:method ((x fset:map)) t)            ;mapseq 
  (:method ((x t)) nil))

(defgeneric seq (x)
  (:documentation
   "([coll])
Returns a seq on the collection. If the collection is empty, returns nil.  (seq nil)
returns nil. seq also works on Strings and may support any Lisp types that represents
multiple logical values, though support is limited in this first release.

Note that seqs cache values, thus seq should not be used on any object that will be
mutated.")
  (:method ((x null)) nil)
  (:method ((x cl:cons)) x)             ;no dotted conses please
  (:method ((x seq)) x)                 ;seq, Cons
  (:method ((x lazyseq)) (lazyseq-realize x))
  (:method ((x EmptyList)) nil)
  (:method ((x PersistentList)) x)
  (:method ((x PersistentQueue))
    (let ((count (persistentqueue-count x)))
      (if (zerop count) 
          nil
          (make-queueseq :front-seq (persistentqueue-front-seq x)
                         :rear-seq (seq (persistentqueue-rear-vec x))
                         :count count))))
  (:method ((x cl:vector)) 
    (if (empty? x)
        nil
        (make-arrayseq :array x)))
  (:method ((x fset:seq)) 
    (if (empty? x)
        nil
        (make-arrayseq :array x)))
  (:method ((x fset:set))
    (if (empty? x)
        nil
        ;; Won't be adjustable, but that shouldn't be necessary for an arrayseq, in fact
        ;; it should be further optimizable in some cases, e.g. svref on simple-vector.
        (make-arrayseq :array (fset:convert 'cl:vector x))))
  (:method ((x cl:hash-table))
    (if (empty? x)
        nil
        (make-mapseq :map x :keys (hash-table-keys-as-vector x))))
  (:method ((x fset:map))
    (if (empty? x)
        nil
        (make-mapseq :map x :keys (map-keys-as-vector x)))))

(defun rseq (coll)
  "([coll])
Returns, in constant time, a seq of the items in coll in reverse order.
If coll is empty returns nil.

Presently sorted only for mutable and immutable vectors. Is not supported for strings,
per Clojure's quirky semantics."
  (typecase coll
    (cl:string (error "Strings are not reversible? (Clojure's quirky semantics)"))
    ((or fset:seq cl:vector)
     (if (empty? coll)
         nil
         (make-arrayrseq :array coll :index (1- (count coll)))))
    (t (error "Collections of type ~s are not reversible." (type-of coll)))))

(defgeneric first (coll)
  (:documentation "([coll])
Returns the first item in the collection/sequence, realizing a value if necessary for
lazy sequence elements.

If COLL is NIL, returns NIL.
If COLL is empty, returns NIL.

Unlike CL:FIRST, works for collections other than lists including hashtables/maps.
Mutable CL hashtables will return a mutable two element list representing a map entry.
Immutable maps return immutable 2 element vectors.")
  (:method ((coll cl:list)) (car coll)) ;incl null
  (:method ((coll emptylist)) nil)
  (:method ((coll persistentlist)) (persistentlist-first coll))
  (:method ((coll persistentqueue)) (first (persistentqueue-front-seq coll)))
  (:method ((coll queueseq)) (queueseq-first coll))
  (:method ((coll cons)) (cons-first coll))
  (:method ((coll cl:vector)) 
    (if (> (cl:length coll) 0)
        (cl:elt coll 0)
        nil))
  (:method ((coll lazyseq)) (first (lazyseq-realize coll)))
  (:method ((coll fset:seq))
    (if (> (fset:size coll) 0)
        (values (fset:lookup coll 0))   ;2 values -> 1 value
        nil))
  (:method ((coll fset:set))
    (fset:do-set (val coll)
      (return-from first val))
    nil)
  (:method ((coll cl:hash-table))
    (maphash (lambda (k v) (return-from first (cl:list k v))) coll)
    nil)
  (:method ((coll fset:map))
    (fset:do-map (k v coll) (return-from first (vector k v)))
    nil)
  (:method ((coll arrayseq))            ;CL:VECTOR _and_ FSET:SEQ
    ;; Should not ever be out of bounds unless cl:vector colls are mutated
    (let ((array (arrayseq-array coll)))
      (if (cl:vectorp array)
          (aref array (arrayseq-index coll))
          (get array (arrayseq-index coll)))))
  (:method ((coll arrayrseq))           ;sadly identical to arrayseq
    ;; Should not ever be out of bounds unless cl:vector colls are mutated
    (let ((array (arrayrseq-array coll)))
      (if (cl:vectorp array)
          (aref array (arrayrseq-index coll))
          (get array (arrayrseq-index coll)))))
  (:method ((coll mapseq))             ;CL:HASH-TABLE and FSET:MAP
    ;; Return a mapentry consistent with the type of mapseq
    (let ((key (svref (mapseq-keys coll) (mapseq-index coll)))
          (map (mapseq-map coll)))
      (if (cl:hash-table-p map)
          (let ((v (gethash key (the cl:hash-table map) #1='#:eof)))
            (when (eq v #1#)
              (error "Hash-table has mutated such that the key sought for key ~s is no longer available."
                     key))
            (cl:list key v))               ;mapentry for mutable ht
          (vector key (fset:lookup (the fset:map map) key))))));mapentry for immutable map

(defgeneric next (coll)
  (:documentation
   "([coll])
Returns a seq of the items after the first. Behaves as if seq were called on its
argument. If there are no more items, returns nil.")
  (:method ((coll null)) nil)
  (:method ((coll cl:cons)) (cl:cdr coll))
  (:method ((coll emptylist)) nil)
  (:method ((coll persistentlist))
    (if (= 1 (persistentlist-count coll))
        nil
        (persistentlist-rest coll)))
  (:method ((coll queueseq)) (queueseq-next coll))
  (:method ((coll lazyseq)) (next (lazyseq-realize coll)))
  (:method ((coll arrayseq)) 
    (let ((count (count (arrayseq-array coll)))
          (next (1+ (arrayseq-index coll))))
      (if (< next count)
          (make-arrayseq :array (arrayseq-array coll) :index next)
          nil)))
  (:method ((coll arrayrseq)) 
    (let ((next (1- (arrayrseq-index coll))))
      (if (>= next 0)
          (make-arrayrseq :array (arrayrseq-array coll) :index next)
          nil)))
  (:method ((coll mapseq)) 
    (let ((count (cl:length (mapseq-keys coll)))
          (next (1+ (mapseq-index coll))))
      (if (< next count)
          (make-mapseq :map (mapseq-map coll) :keys (mapseq-keys coll) :index next)
          nil)))
  (:method ((coll cons)) (seq (cons-more coll))) ;CLJ-COL:CONS
  (:method ((coll t))                            ;PersistentQueue
    (if (seqable? coll)
        (some-> (seq coll) next)
        (error "~s is not a seqable entity" coll))))

(defun rest (coll)
  "([coll])
Returns a possibly empty sequence (not a seq, CLJ-COLL seqs are never empty)
of the items after the first. Implies a call to seq on its argument.
The only difference between NEXT and REST is that next will return NIL if there
are no more items, while REST will return an empty persistent list.

REST shadows CL:REST, it will work reasonably intuitively on CL:LIST types, but also
on CLJ-COLL persistent lists expressed with persistent conses, lazy sequences, and so on."
  (or (next coll) *EMPTY-LIST*))

(defun nthrest (coll n)
  "Returns the nth `rest` of coll. Returns coll when n is <= 0."
  ;; I guess it's a feature that Clojure's nthrest allows negative N.
  ;; LOOP REPEAT handles that.
  (loop repeat n
        while coll
        do (setf coll (rest coll)))
  coll)

(defun nthnext (coll n)
  "Returns the nth `next` of coll, (seq coll) when n is 0."
  (loop repeat n
        while coll
        do (setf coll (next coll)))
  (seq coll))

(defgeneric second (coll)
  (:documentation "([coll])
Returns the second item in the collection.  

If COLL is NIL, returns NIL.
If COLL is empty, returns NIL.

Unlike CL:SECOND, works for collections other than lists including hashtables/maps.
Mutable CL hashtables will return a mutable two element list representing a map entry.
Immutable maps return immutable 2 element vectors.")
  (:method ((coll cl:list)) (cl:second coll))
  (:method ((coll cl:vector)) 
    (if (> (cl:length coll) 1)
        (cl:elt coll 1)
        nil))
  (:method ((coll fset:seq))
    (if (> (fset:size coll) 1)
        (values (fset:lookup coll 1))   ;2 values -> 1 value
        nil))
  (:method ((coll fset:set))
    (let (skipped)
      (fset:do-set (val coll)
        (if skipped
            (return-from second val)
            (setf skipped t))))
    nil)
  (:method ((coll cl:hash-table))
    (and (> (hash-table-count coll) 1)
         (let (skipped)
           (maphash (lambda (k v) 
                      (if skipped
                          (return-from second (cl:list k v))
                          (setf skipped t)))
                    coll))))
  (:method ((coll fset:map))
    (and (> (fset:size coll) 1)
         (let (skipped)
           (fset:do-map (k v coll)
             (if skipped
                 (return-from second (vector k v))
                 (setf skipped t))))))
  ;; You'll get NoApplicableMethod errors if you try to call second
  ;; on unseqable things.
  (:method ((coll t)) (first (next coll))))

(defun dorun (n-or-coll &optional (coll nil coll-p))
  "Walks through the successive nexts of (seq coll) for side effects.
Does not retain the head and returns nil.

If N is specified, only examines at most N elements of the seq.

As `dorun` accepts no function to map and returns only nil, its
only use is to realize lazyseq nodes."
  (if coll-p
      (loop for n from n-or-coll downto 0
            for s = (seq coll) then (next s)
            while (and s (> n 0)))
      (loop for s = (seq n-or-coll) then (next s)
            while s)))

(defun doall (n-or-coll &optional (coll nil coll-p))
  "([coll] [n coll])

Walks through the successive nexts of (seq coll).
Retains the head and returns it causing the entire
seq to reside in memory at one time.

If N is specified, only examines at most N _nexts_ of the seq.

If you thought (DOALL N SEQ) was going to give you some partial
subset of values from a sequence, you're wrong.  This is about side effects
and just happens to return the input collection.

*TBD* It seems pointless to call doall on any non-lazy-seq entity,
we should probably optimize it to do nothing if coll is not a lazyseq
or a seq on a lazyseq."
  (cond (coll-p (dorun n-or-coll coll)
                coll)
        (t (dorun n-or-coll)
           n-or-coll)))

(defun fnext (x)
  "([x])
Same as (first (next x)).
As if CADR were extended to arbitrary collections."
  (first (next x)))

(defun nnext (x)
  "([x])
Same as (next (next x)).
As if CDDR were extended to arbitrary collections."
  (next (next x)))

(defun ffirst (x)
  "([x])
Same as (first (first x)).
As if CAAR were extended to arbitrary collections."
  (first (first x)))

(defun nfirst (x)
  "([x])
Same as (next (first x)).
As if CDAR were extended to arbitrary collections."
  (next (first x)))

(defmacro when-first ((var-name seq-exp) &body body)
  "Invoke BODY with VAR-NAME bound to the first value of SEQ-EXP.
If there is no first value, BODY is not executed and NIL is returned.

Basically:

    (when-first (s coll)
      body)
=>
    (when-let (s (seq coll))
      (let ((x (first s)))
         body-using-x-and-or-s))"
  (let ((seq-name (gensym "s-")))
    `(when-let (,seq-name (seq ,seq-exp))
       (let ((,var-name (first ,seq-name)))
         ,@body))))
