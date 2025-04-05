(in-package :clj-coll)

;;;;
;;;; Functions related to REDUCE and TRANSDUCE, as well as some transducers.
;;;;

(defun reduce-kv (f init coll)
  "([f init coll])

Reduces an associative collection
(which includes vectors where keys are ordinal index values, but not seqs
on vectors).

F should be a function of 3 arguments (initial-result key value).

F is first applied to (init first-key-in-coll first-value-in-coll).
F is then applied to the last return from F, and successive keys and values in COLL.

If COLL contains no entries, returns INIT and F is not called."
  (let ((f (alexandria:ensure-function f)))
    (declare (function f))
    (if (empty? coll)
        init
        (typecase coll
          (cl:hash-table
           (loop with result = init
                 for k being the hash-keys of coll
                   using (hash-value v)
                 do (setf result (funcall f result k v))
                 finally (return result)))
          (fset:map
           (let ((result init))
             (fset:do-map (k v coll) (setf result (funcall f result k v)))
             result))
          (cl:vector 
           (loop with result = init
                 for v across coll
                 for k from 0
                 do (setf result (funcall f result k v))
                 finally (return result)))
          (fset:seq
           (let ((result init))
             (fset:do-seq (v coll :index k) 
               (setf result (funcall f result k v)))
             result))
          (t (error "~s is not a supported assocative collection." (type-of coll)))))))

;;;
;;; "reduced" support, because god forbid FP coders use something more immediate like
;;; return-from. Let's inspect every funcall result instead. Pfft.
;;;

(defstruct (reduced 
            (:constructor reduced (value))
            (:predicate reduced?)
            (:copier nil))
  "Instances of this structure wrap a reduced value."
  (value nil :read-only t))

(setf (documentation 'reduced 'function)
      "Wraps x in a way such that a `reduce` will terminate with the value x"
      (documentation 'reduced? 'function)
      "Returns true if x is the result of a call to `(reduced x)`")

(defun deref (object)
  "Obtain the value of a `reduced` object."
  (if (reduced? object)
      (reduced-value object)
      (error "~a is not a reduced object." object)))

(defun unreduced (x)
  "([x])
  If x is reduced?, returns (deref x), else returns x"
  (if (reduced? x)
      (deref x)
      x))

(defun ensure-reduced (x)
  "([x])
  If x is already reduced?, returns it, else returns (reduced x)"
  (if (reduced? x)
      x
      (reduced x)))

(defgeneric length-1? (coll)
  (:documentation "We want to know if a collection has exactly one element, but avoid
    any O(n) behavior on list length calls. Used for internal stuff.
    Will attempt to realize 2 lazyseq nodes if given a lazyseq.")
  (:method ((coll cl:list))
    (null (cdr coll)))
  (:method ((coll lazyseq))
    (loop for i from 0 below 3
          as s = (seq coll) then (next s)
          while s
          finally (return (= i 1))))
  (:method ((coll t))
    (= 1 (count coll)))) ; *FIXME* bad for seqs

(defun paranoid-gethash (key ht)
  "A function to perform a GETHASH on a HASH-TABLE and signal an error
if the KEY is not present. Used by mapseqs and related logic in which
the hashtable is not expected to mutate while being used for a particular purpose."
  (declare (hash-table ht))
  (let ((val (gethash key ht #1='#:eof)))
    (when (eq val #1#)
      (error "A search for key ~s in a hash-table was not found where one was expected.
Note that hash-table use in CLJ-COLL 'seqs' should not be mutated while seqs are being used."
             key))
    val))

(defun fset-map-gethash (key map &optional default)
  "A function to perform a the equivalent of GETHASH on FSET:MAP types.
Matches the (GETHASH key map) argument order."
  (declare (fset:map map))
  (multiple-value-bind (value successp)
      (fset:lookup map key)
    (if successp
        value
        default)))

(defun reduce2 (f coll &optional (init nil init?))
  "Behave like REDUCE, knowing that the collection has 2+ elements and
open-coded collection traversal for hopefully optimal iteration.

F must be a FUNCTION, not a symbol.

F is a wrapper that returns early from reduce if it receives a reduced value
from the original user function passed to REDUCE. REDUCE2 need not worry about
reduced values, just iterating over COLL with successive values from F."
  (declare (function f))
  (etypecase coll
    (cl:list 
     (loop with x = (if init? init (car coll))
           for y in (if init? coll (cdr coll))
           do (setf x (funcall f x y))
           finally (return x)))
    (cl:vector
     (loop with x = (if init? init (aref coll 0))
           for idx from (if init? 0 1) below (length coll)
           as y = (aref coll idx)
           do (setf x (funcall f x y))
           finally (return x)))
    (fset:seq
     (let ((did-init? (not init?))
           (did-last-val? nil)
           (last-val nil))
       (fset:do-seq (val coll) ;awkward this way, but hopefully most efficient traversal
         (if did-init?
             (if did-last-val?
                 (setf last-val (funcall f last-val val))
                 (setf last-val val did-last-val? t))
             (setf did-init? t
                   did-last-val? t
                   last-val (funcall f init val))))
       last-val))
    (fset:set
     (let ((did-init? (not init?))
           (did-last-val? nil)
           (last-val nil))
       (fset:do-set (val coll)
         (if did-init?
             (if did-last-val?
                 (setf last-val (funcall f last-val val))
                 (setf last-val val did-last-val? t))
             (setf did-init? t
                   did-last-val? t
                   last-val (funcall f init val))))
       last-val))
    ;; hashtables/maps as collections yield appropriate mapentry representations
    (cl:hash-table
     (let ((did-init? (not init?))
           (did-last-val? nil)
           (last-val nil))
       (maphash 
        (lambda (k v)
          (let ((val (cl:list k v)))
            (if did-init?
                (if did-last-val?
                    (setf last-val (funcall f last-val val))
                    (setf last-val val did-last-val? t))
                (setf did-init? t
                      did-last-val? t
                      last-val (funcall f init val)))))
        coll)
       last-val))
    (fset:map
     (let ((did-init? (not init?))
           (did-last-val? nil)
           (last-val nil))
       (fset:do-map (k v coll)
         (let ((val (vector k v)))
           (if did-init?
               (if did-last-val?
                   (setf last-val (funcall f last-val val))
                   (setf last-val val did-last-val? t))
               (setf did-init? t
                     did-last-val? t
                     last-val (funcall f init val)))))
       last-val))

    (arrayseq 
     ;; The 'seq' typecase could do this too, but here we're trying to be more
     ;; efficient on arrays and avoid consing up a bunch of arrayseq objects for each index
     (let ((array (arrayseq-array coll)) ;fset:seq or cl:vector
           (index (arrayseq-index coll)))
       ;; As a seq, by definition the value at index exists unless someone mucked with
       ;; mutable array, which we are nice about and check for here.
       (if (cl:vectorp array)
           (let ((arraylen (cl:length array)))
             (unless (< index arraylen)
               (error "The array on which ~s is based has been modified and no longer contains
the expected number of values." coll))
             ;; Loop copied from the cl:vector typecase above, adapted for starting index
             (loop with x = (if init? init (aref array index))
                   for idx from (if init? index (1+ index)) below arraylen
                   as y = (aref array idx)
                   do (setf x (funcall f x y))
                   finally (return x)))
           ;; FSET:SEQ, use of :start is only difference from fset:seq handling above.
           ;; Would be nice to refactor.
           (let ((did-init? (not init?))
                 (did-last-val? nil)
                 (last-val nil))
             (fset:do-seq (val array :start index)
               (if did-init?
                   (if did-last-val?
                       (setf last-val (funcall f last-val val))
                       (setf last-val val did-last-val? t))
                   (setf did-init? t
                         did-last-val? t
                         last-val (funcall f init val))))
             last-val))))

    (arrayrseq                           ;same comments/caveats/checks as arrayseq
     (let ((array (arrayrseq-array coll)) ;fset:seq or cl:vector
           (index (arrayrseq-index coll)))
       (if (cl:vectorp array)
           (let ((arraylen (cl:length array)))
             (unless (< index arraylen)
               (error "The array on which ~s is based has been modified and no longer contains
the expected number of values." coll))
             (loop with x = (if init? init (aref array index))
                   for idx from (if init? index (1- index)) to 0
                   as y = (aref array idx)
                   do (setf x (funcall f x y))
                   finally (return x)))
           (let ((did-init? (not init?))
                 (did-last-val? nil)
                 (last-val nil))
             (fset:do-seq (val array :start index :from-end? t)
               (if did-init?
                   (if did-last-val?
                       (setf last-val (funcall f last-val val))
                       (setf last-val val did-last-val? t))
                   (setf did-init? t
                         did-last-val? t
                         last-val (funcall f init val))))
             last-val))))

    (mapseq 
     (let* ((keys (mapseq-keys coll))   ;simple-vector
            (keys-index (mapseq-index coll))
            (map (mapseq-map coll))
            (last-val nil)
            (getfn (if (cl:hash-table-p map) #'paranoid-gethash #'fset-map-gethash))
            (mapentry-fn (if (cl:hash-table-p map) #'cl:list #'vector)))
       (declare (function getfn mapentry-fn) (simple-vector keys) (fixnum keys-index))
       (if init?
           (let* ((key (svref keys keys-index))
                  (val (funcall getfn key map)))
             (setf last-val (funcall f init (funcall mapentry-fn key val)))
             (incf keys-index))
           (let* ((k1 (svref keys keys-index))
                  (k2 (svref keys (1+ keys-index)))
                  (v1 (funcall getfn k1 map))
                  (v2 (funcall getfn k2 map)))
             (setf last-val (funcall f (funcall mapentry-fn k1 v1) (funcall mapentry-fn k1 v2)))
             (incf keys-index 2)))
       (loop for keys-index from keys-index below (cl:length keys)
             as  key = (svref keys keys-index)
             as  val = (funcall getfn key map)
             do (setf last-val (funcall f last-val (funcall mapentry-fn key val))))
       last-val))

    (lazyseq 
     (if init?
         (reduce2 f (seq coll) init)
         (reduce2 f (seq coll))))
    (ipersistentlist ;is no longer a seq, but returns itself for seq, don't recurse with (seq x)
     (loop with x = (if init? init (first coll))
           for s = (if init? coll (next coll)) then (next s)
           while s
           do (setf x (funcall f x (first s)))
           finally (return x)))
    (seq
     (loop with x = (if init? init (first coll))
           for s = (if init? coll (next coll)) then (next s)
           while s
           do (setf x (funcall f x (first s)))
           finally (return x)))))

(defun reduce (f val-or-coll &optional (coll nil init-supplied?))
 "[f coll] or [f val coll]

f should be a function of 2 arguments. If val is not supplied,
returns the result of applying f to the first 2 items in coll, then
applying f to that result and the 3rd item, etc. If coll contains no
items, f must accept no arguments as well, and reduce returns the
result of calling f with no arguments.  If coll has only 1 item, it
is returned and f is not called.  If val is supplied, returns the
result of applying f to val and the first item in coll, then
applying f to that result and the 2nd item, etc. If coll contains no
items, returns val and f is not called.

Perhaps more readably for function f and initial value i:

    (reduce f [])      => f()
    (reduce f [1])     => 1
    (reduce f [1 2])   => f(1 2)

    (reduce f i [])    => i
    (reduce f i [1])   => f(i 1)
    (reduce f i [1 2]) => f(i 1) ...

Any one-value scenario returns 1 value without calling f, otherwise f is called.

`reduced` values:

You can blame Clojure for what follows, re: 'reduced', whose edge cases
are not well documented but were determined by playing in the repl.

Early termination occurs if `f` returns a `reduced?` value, in which case the value
is returned (unreduced). 

If the result of `reduce` is either the _only_ element of coll (with no initval)
or the result of calling f with zero arguments, 
reduced values will remain reduced when returned.  The moral of the story
being that reduced values should only occur in the 2nd+ elements of coll
unless you're prepared to deal with them as the return value of reduce.

Compatibility notes w.r.t. cl:reduce:

Clojure and CL reduce semantics are identical with respect to
empty collections, initial values, and function arities.  CL's reduce is
likely much more efficient if you can use it directly on native CL sequence
inputs. However CL:REDUCE won't deal with `reduced` early termination or
CLJ-COLL-specific sequences/collections"
  (declare ((or symbol function) f))
  (let ((coll (if init-supplied? coll val-or-coll))
        (f (alexandria:ensure-function f)))
    (declare (function f))
    ;; coll-supplied? implies an initial value was supplied
    (cond ((empty? coll)
           (if init-supplied?
               val-or-coll
               (unreduced (funcall f))))
          ((length-1? coll)
           (if init-supplied?
               (unreduced (funcall f val-or-coll (first coll)))
               (first coll)))
          (t                            ;2+ elements in coll
           (flet ((looking-for-reduced-values (a b)
                    (let ((result (funcall f a b)))
                      (if (reduced? result)
                          (return-from reduce (reduced-value result))
                          result))))
             (if init-supplied?
                 (reduce2 #'looking-for-reduced-values coll val-or-coll)
                 (reduce2 #'looking-for-reduced-values coll)))))))

(defun transduce (xf rf init-or-coll &optional (coll nil coll-p))
  "([xf rf coll] [xf rf init coll])

Reduce with a 'transformation' ('xform') of (xf rf).  An xform is basically the
step ([result input]) arity of a transducer.  Transduce turns a transducer
into a function that can be used with `reduce`.

If init is not supplied, (rf) will be called to supply it, e.g. `(+)` => 0.

rf should be a reducing function that accepts 1 and 2 arguments.
If it accepts only 2 arguments, you can augment it by calling `(completing rf)`.

Returns the result of applying the transformation to init and the first item
in coll, then applying the transformation to that result and the second
item in coll, and so on.

If coll contains no items returns init and rf is not called.

Note that some transformations can delete (by not calling rf), or inject
(by calling rf multiple times) items.

Also note that transduce, as an application of reduce, is eager, not lazy.
Transduce will immediately (not lazily) reduce over coll with the transducer xf(orm)
applied to the reducing function rf."

  (let* ((transform (funcall xf rf))
         (result
           (if coll-p
               ;; An init was supplied, init-or-coll is the init
               (reduce transform init-or-coll coll)
               ;; No init was supplied, init-or-coll is a collection
               (let ((init (funcall rf)))
                 (reduce transform init init-or-coll)))))
    (funcall transform result)))        ;the completion step for stateful transducers

(defun cat (rf)
  "[rf] 
Transducer which concatenates the contents of each input (which must be a
collection) into the reduction.

Examples:

(transduce cat conj (cl:list [1 2] [3 4]))
[1 2 3 4]
clj-foo.core> (transduce cat conj #{} (cl:list [1 2] [3 4]))
#{1 4 3 2}"
  ;; Note that clojure has this reduced? preserving return value logic 
  ;; I don't understand and haven't used because it looks like a NO-OP to me.
  ;; See 'preserving-reduced' in src/clj/clojure/core.clj
  ;; Perhaps it applies to https://clojure.org/reference/transducers#_creating_transducible_processes
  ;; and I'm just not grokking it yet.
  (lambda (&rest args)                  ;[], [result], or [result input] args
    (cond ((null args) (vector))
          ((length-1? args) (funcall rf (cl:first args)))
          (t (reduce rf (cl:first args) (cl:second args))))))

(defun halt-when (pred &optional retf)
  "([pred] [pred retf])

Returns a transducer that ends transduction when PRED returns true
for an input. 

When RETF is supplied it must be a function of 2 arguments,
the (completed) result so far and the input that
triggered the predicate. Its return value (if it does not throw
an exception) will be the return value of the transducer.

If RETF is not supplied, the input that triggered the predicate will be
returned. If the predicate never returns true the transduction is
unaffected."
  (stateful-transducer      ;not really stateful, just convenient for completion step
      (rf result input ((eof #1='clj-coll::halt))
       :completion-body
       (if (and (cl:consp result) 
                (eq (car result) eof))
           (cdr result)
           (funcall rf result)))
    ;; [result input]
    (if (funcall pred input)
        (reduced (cl:cons eof (if retf
                                  (funcall retf (funcall rf result) input)
                                  input)))
        (funcall rf result input))))

(defun completing (f &optional cf)
  "Takes a reducing function F of zero or 2 args and returns a function suitable for
TRANSDUCE by adding an arity-1 signature that calls CF (default - IDENTITY) on the
result argument."
  (if cf
     (lambda (&rest args) ;[] [result] [result input]
       (ecase (cl:length args)
         (0 (funcall f))
         (1 (funcall cf (car args)))
         (2 (funcall f (cl:first args) (cl:second args)))))
     (completing f #'identity)))
