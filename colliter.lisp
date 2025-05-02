(in-package :clj-coll)

;;;; Collector and iterator support specific to CLJ-COLL types.
;;;; Both endeavor to be relatively efficient for value accumulation/iteration.
;;;; So we eschew optional arguments were we can but may use an extra piece of state
;;;; data in the process.  Nothing really optimized here though, just a first pass.
;;;; Where possible we try to avoid collectors and iterators entirely, but sometimes
;;;; they're too convenient to pass up when we're worried about other things.

(defun missing-collector-function (&rest vals)
  (declare (ignore vals))
  (error "Collector was missing a function where one was expected."))

(defstruct (collector 
            (:conc-name %coll-)
            (:constructor %collector (collection collector grabber &optional collector-kv)))
  "A collector is a thing which accumulates values in different ways according to its
underlying collection."
  collection
  (collector #'missing-collector-function :type function) ;fn to store new value
  (collector-kv #'missing-collector-function :type function) ;fn to store new key+value
  (grabber #'missing-collector-function :type function) ;fn to retrieve value with intended datatype
  tail)                                  ;if needed, for cl:list types

(defun %collect-list (collector x)
  "Helper to accummulate X in collector's list/tail.  Return value N/A"
  (declare (collector collector))
  (let ((new (cl:cons x nil))
        (old (%coll-tail collector)))
    (if (null old)
        (setf (%coll-collection collector) new
              (%coll-tail collector) new)
        (setf (cdr old) new
              (%coll-tail collector) new))))

(defun %collect-list-prepending (collector x)
  "Helper to accummulate X in collector's list by customary prepending.  Return value N/A"
  (declare (collector collector))
  (push x (%coll-collection collector)))

(defun %collect-vector (collector x)
  "Helper to accummulate X in collector's vector.  Return value N/A"
  (declare (collector collector))
  (vector-push-extend x (%coll-collection collector)))

(defun %collect-fset-seq (collector x)
  "Helper to accummulate X in collector's FSET:SEQ.  Return value N/A"
  (declare (collector collector))
  (setf (%coll-collection collector)
        (fset:with-last (%coll-collection collector) x)))

(defun %collect-fset-set (collector x)
  "Helper to accummulate X in collector's FSET:SET.  Return value N/A"
  (declare (collector collector))
  (setf (%coll-collection collector)
        (fset:with (%coll-collection collector) x)))

(defun %collect-hash-table-map-entry (collector mapentry)
  "Helper to accumulate MapEntry equivalents in CL hash-table collectors.
The mapentry should be a 2 element sequence of some kind,
from which we'll take the first and second values.

Return value is N/A."
  (declare (collector collector))
  (setf (gethash (first mapentry) (%coll-collection collector))
        (second mapentry)))

(defun %collect-hash-table-kv (collector key value)
  "Helper to accumulate key/value data in CL hash-table collectors.
Likely more efficient than using the MapEntry flavor of this function.
Return value is N/A."
  (declare (collector collector))
  (setf (gethash key (%coll-collection collector)) value))
  
(defun %collect-map-map-entry (collector mapentry)
  "Helper to accumulate MapEntry equivalents in CLJ-COLL immutable map collectors.
The mapentry should be a 2 element sequence of some kind,
from which we'll take the first and second values.

Return value is N/A."
  (declare (collector collector))
  (setf (%coll-collection collector)
        (fset:with (%coll-collection collector) 
                   (first mapentry) (second mapentry))))

(defun %collect-map-kv (collector key value)
  "Helper to accumulate key/value data in CLJ-COLL immutable map collectors.
Likely more efficient than using the MapEntry flavor of this function.
Return value is N/A."
  (declare (collector collector))
  (setf (%coll-collection collector)
        (fset:with (%coll-collection collector) key value)))

(defun %grab-persistentlist (collector)
  "Helper function called by GRAB on a collection to convert data accumulated in a CL:LIST
into a PersistentList."
  (declare (collector collector))
  (list-from (%coll-collection collector)))
  
(defun make-collector (type-or-obj &key (size 8) prepend)
  "Create a COLLECTOR with a collection of type TYPE-OR-OBJ, 
and optionally the size of the collector which is only used for type CL:VECTOR. 

TYPE-OR-OBJ has several supported flavors:

1. It can be a symbol specifying the type of collector to allocate. 

   Valid _symbols_ are: 
   NIL, CL:LIST, CL:VECTOR, CL:HASH-TABLE, (the mutable collections)
   PERSISTENTLIST, FSET:SEQ, FSET:SET, FSET:MAP, (the immutable collections)
   SEQ, and LAZYSEQ (seqs on things, mutable or immutable - we can't necessarily tell)

   NIL is equivalent to CL:LIST.

   SEQ/LAZYSEQ types will result in a PERSISTENTLIST collector.

   (*TBD*: perhaps we could allow ARRAYSEQ and some clue about whether to use 
   mutable or immutable vector, though that can't be told from a symbol, 
   only from an ArraySeq _object_).

2. It can be a collection supported by CLJ-COLL (common lisp collections, 
   persistent collections), in which case a collection of like kind is allocated.

3. It can be a seq on a collection, or a lazyseq, in which case we allocate
   a persistent list (at GRAB time), based on a CL:LIST within the collector.

Once you have a collector you can:

- Add values to the collector using (COLLECT <collector> <value).
  If you're using a hash-table or map collector, you can use COLLECT or COLLECT-KV
  as needed.
- Access the values collected so far with (GRAB <collector).
- Reset the collector using (RESET <collector>).

The :PREPEND option may be used with any collector that collects for a CL:LIST
or CL:PERSISTENT-LIST (namely those two types, seqs, and lazyseqs).
:PREPEND NIL (the default) will append to lists (using O(1) logic).
:PREPEND T will prepend to lists.
"
  (unless (symbolp type-or-obj)
    ;; Produce a type for the `ecase` below.
    (setf type-or-obj
          (etypecase type-or-obj
            (cl:vector       'cl:vector)
            (cl:list         'cl:list)  ;includes NIL
            (cl:hash-table   'cl:hash-table)
            (ipersistentlist 'persistentlist)
            (fset:set        'fset:set)
            (fset:seq        'fset:seq)
            (fset:map        'fset:map)
            (seq             'persistentlist)
            (lazyseq         'persistentlist))))
  (ecase type-or-obj
    (cl:vector     (%collector (make-array size :adjustable t :fill-pointer 0)
                               #'%collect-vector #'%coll-collection))
    ((nil cl:list) (%collector nil 
                               (if prepend #'%collect-list-prepending #'%collect-list)
                               #'%coll-collection))
    (cl:hash-table (%collector (cl-hash-map) 
                               #'%collect-hash-table-map-entry
                               #'%coll-collection
                               #'%collect-hash-table-kv))
    ;; Collect persistentlist values as cl:list, grab converting to persistentlist
    ((seq lazyseq persistentlist) (%collector 
                                   nil 
                                   (if prepend #'%collect-list-prepending #'%collect-list)
                                   #'%grab-persistentlist))
    (fset:set      (%collector (fset:set) #'%collect-fset-set #'%coll-collection))
    (fset:seq      (%collector (fset:seq) #'%collect-fset-seq #'%coll-collection))
    (fset:map      (%collector (hash-map) 
                               #'%collect-map-map-entry
                               #'%coll-collection
                               #'%collect-map-kv))))

(defun collect (collector value)
  "Adds a value to the collection in a Collector.

If the collector is for hash-table or map types (k/v pairs)
you can use COLLECT and specify a logical mapentry as the value,
or you can use COLLECT-KV and specify the key and value separately.

Returns COLLECTOR."
  (declare (collector collector) (optimize (speed 3) (safety 1)))
  (funcall (%coll-collector collector) collector value)
  collector)

(defun collect-kv (collector key value)
  "Adds a key/value pair to a hash-table and/or persistent map collector.

You can also use COLLECT with logical map-entry values, whichever
suits the structure of your incoming data to be collected.

Returns COLLECTOR."
  (declare (collector collector) (optimize (speed 3) (safety 1)))
  (funcall (%coll-collector-kv collector) collector key value)
  collector)

(defun grab (collector)
  "Materialize current collector state into a collection of the type
indicated when the collector was created with MAKE-COLLECTOR.

Caution: multiple GRAB calls on a collector may share state if the collector target
is a mutable type, you may wish to make a copy if you're calling GRAB more than once.
Immutable collector types are safe from subsequent mutation of the collector after
GRAB calls.

See also GRAB-SAFE for grab copy only if the accumulator is a CL-VECTOR."
  (declare (collector collector))
  (funcall (%coll-grabber collector) collector))

(defun grab-safe (collector)
  "Like GRAB, only iff the collector is using a CL VECTOR or HASHTABLE for
accumulation we make a copy of the accummulated collection.

Call this if you intend to RESET the collector and need the GRAB content after the
RESET.

Note that you don't usually need to copy CL:LIST results because the
collector isn't going to overwrite a previously allocated list."
  (declare (collector collector))
  (let ((result (grab collector)))
    (if (or (cl:vectorp result) (cl:hash-table-p result))
        (copy result)
        result)))

(defun reset (collector)
  "Reset the collector to its initial state, discarding any accummulated content.

You may wish to make a copy of the prior content by accessing (%coll-collection <collector)
before resetting it.

Returns COLLECTOR."
  (declare (collector collector))
  (etypecase (%coll-collection collector)
    (cl:null)                       ;do nothing, empty list/persistentlist collector 
    (cl:cons (setf (%coll-collection collector) nil
                   (%coll-tail collector) nil))
    (cl:vector (setf (fill-pointer (%coll-collection collector)) 0))
    (cl:hash-table (cl:clrhash (%coll-collection collector)))
    (fset:set (setf (%coll-collection collector) (fset:set)))
    (fset:seq (setf (%coll-collection collector) (fset:seq)))
    (fset:map (setf (%coll-collection collector) (fset:map)))
    )
  collector)


;;;
;;; Functions that return stateful iterator functions on collections or EOF if the collection
;;; is exhausted.
;;;

(defun null-iterator (eof) "Iterator fn for NIL collection." (lambda () eof))
(defun cl-vector-iterator (coll eof) "Iterator fn for CL:VECTOR collection."
  (declare (cl:vector coll))
  (let ((index 0))
    (declare (fixnum index))
    (lambda ()
      (if (< index (cl:length coll))
          (prog1 (aref coll index)
            (incf index))
          eof))))
(defun cl-list-iterator (coll eof) "Iterator fn for CL:LIST collection."
  (declare (cl:list coll))
  (let ((cons coll))
    (lambda ()
      (if (null cons)
          eof
          (prog1 (car cons)
            (setf cons (cdr cons)))))))
(defun hash-table-iterator (coll eof) 
  "Iterator fn for CL:HASH-TABLE collection. Returns MapEntries as mutable lists of two elements."
  (declare (cl:hash-table coll))
  ;; Materialize keys, generate (k v) lists when values are fetched.
  (let* ((keys (hash-table-keys-as-vector coll)) ;also used as unique value
         (n-keys (cl:length keys))
         (index 0))
    (declare (simple-vector keys) (fixnum index n-keys))
    (lambda ()
      (if (< index n-keys)
          (let* ((key (svref keys index))
                 (val (gethash key coll keys)))
            (when (eq keys val)
              (error "Hashtable has mutated and key is no longer present for iterator: ~s" key))
            (incf index)
            (cl:list key val))
          eof))))
(defun fset-map-iterator (coll eof) 
  "Iterator fn for FSET:MAP collections, returns MapEntrys as persistent vectors of two elements."
  (declare (fset:map coll))
  (let ((fset-iterator (fset:iterator coll)))
    (lambda ()
       (multiple-value-bind (key value successp)
           (funcall fset-iterator :get)
         (if successp
             (fset:seq key value)
             eof)))))
(defun fset-seq-set-iterator (coll eof) "Iterator fn for FSET:SEQ and FSET:SET collections."
  (let ((fset-iterator (fset:iterator coll)))
    (lambda ()
      (multiple-value-bind (value successp)
          (funcall fset-iterator :get)
        (if successp
            value
            eof)))))
(defun seq-iterator (coll eof) "Iterator for non-null seqs."
  (declare ((or seq ipersistentlist) coll))
  ;; When iterator is called for the first time,
  ;; we know s is a seq (not nil) whose first we have not returned.
  ;; We need to be careful not to invoke (next) and realize the next node in the lazyseq
  ;; prematurely.
  (let ((s coll)
        (fetch? nil))
    (lambda ()
      (if fetch?
          ;; s's value has already been returned, fetch next
          (if s
              (progn (setf s (next s)) (if s (first s) eof))
              eof)
          ;; return current seqnode value, DO NOT FETCH NEXT YET
          (progn (setf fetch? t)
                 (first s))))))
      

(declaim (ftype (function (t t) (values function &optional)) iterator))
(defun iterator (coll eof)
  "Return a stateful function of no arguments that returns successive values of COLL each time
it's called, or EOF if there are no more values."
  (etypecase coll
    (null (null-iterator eof))
    (cl:vector (cl-vector-iterator coll eof))
    (cl:list   (cl-list-iterator coll eof))
    (cl:hash-table (hash-table-iterator coll eof))
    (fset:map (fset-map-iterator coll eof))
    (fset:collection (fset-seq-set-iterator coll eof))
    (seq (seq-iterator coll eof))
    ((or ipersistentlist persistentqueue)
     (if-let (s (seq coll))
       (seq-iterator s eof)
       (null-iterator eof)))
    (lazyseq (alexandria:if-let (s (seq coll))
               (seq-iterator s eof)
               (null-iterator eof)))))

(defun iterator-skip (iterator n eof)
  "Read N values from iterator, returning the last value
or EOF if the iterator was exhausted.  EOF should be
the value specified on the call to the `iterator` constructor.

N must be >= 1."
  (loop repeat n
        as val = (funcall iterator)
        until (eq val eof)
        finally (return val)))
