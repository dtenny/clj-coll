(in-package :clj-coll)

(defun cl-symbol-names (&key exclude)
  "This is an exported utility function for use in CLJ-COLL client DEFPACKAGE
contexts. It returns a list of strings naming all symbols exported by the
Common-Lisp package that are also shadowed by CLJ-COLL.

You can use it a variety of ways, the most flexible of which is probably in
the DEFPACKAGE form for some package that wants to use CLJ-COLL. E.g.

    (defpackage my-package
      (:use :cl :clj-coll)
      (:shadowing-import-from :clj-coll . #.(clj-coll:cl-symbol-names))
      ...)

If EXCLUDE is specified, it should be a list of string designators
indicating symbols that should NOT appear in the output of this function.
E.g. 

    (cl-symbol-names :exclude '(cons list list*))

Would return the CL symbols shadowed by CLJ-COLL except for the ones
listed as exclusions, so that the calling DEFPACKAGE or other
package operation might favor CL or other providers of the symbols
instead of CLJ-CON's versions of those symbols.

SEE ALSO: NON-CL-SYMBOL-NAMES, CL-COMPATIBLE-SYMBOL-NAMES."
  (loop with exclusions = (cl:mapcar #'string exclude)
        for shadow in (package-shadowing-symbols :clj-coll)
        as name = (string shadow)
        unless (member name exclusions :test #'string=)
          collect name))

(defun non-cl-symbol-names (&key exclude)
  "This is an exported utility function for use in CLJ-COLL client DEFPACKAGE
contexts. It returns a list of strings naming all symbols exported by the
CLJ-COLL package that NOT exported by the COMMON-LISP package (hence don't need
to be shadowed if you want to use them in a package you're defining).

This function is probably useful only if you're not going to 'USE'
the CLJ-COLL package in some package you are creating.

    (defpackage my-package
      (:use :cl)
      ;; symbol names that don't need to be shadowed
      (:import-from :clj-coll . #.(clj-coll:non-cl-symbol-names))
      ;; symbol names that conflict with CL symbols
      (:shadowing-import-from :clj-coll . #.(clj-coll:cl-symbol-names))
      ...)

If EXCLUDE is specified, it should be a list of string designators
indicating symbols that should NOT appear in the output of this function.
E.g. 

    (non-cl-symbol-names :exclude '(:non-cl-symbol-names :cl-symbol-names))

Would return the CLJ-COLL symbols that don't shadow CL symbols except for the ones
listed as exclusions.

SEE ALSO: CL-SYMBOL-NAMES, CL-COMPATIBLE-SYMBOL-NAMES."
  (loop with exclusions = (cl:union (cl:mapcar #'string exclude)
                                    (cl-symbol-names)
                                    :test #'string=)
        for symbol being the external-symbol of :clj-coll
        as name = (string symbol)
        unless (member name exclusions :test #'string=)
          collect name))

(defparameter +m-function-symbol-names+ 
  (cl:mapcar 
   #'string
   '(:mbutlast :mconcat :mcycle :mdedupe :mdistinct :mdrop :mdrop-last :mdrop-while
     :mfilter :mflatten :minterleave :minterpose :mjuxt :mkeep :mkeep-indexed
     :mkeys :mmap :mmapcat :mpartition :mpartition-all :mpartition-by
     :mremove :mrandom-sample :mrange :mrepeat :mrepeatedly :mreplace :mreverse
     :mshuffle :msplit-at :msplit-with :mtake :mtake-last :mtake-while :mvals))
  "Symbol names of so-called 'M functions'. See README.md.
Access with `CL-COMPATIBLE-SYMBOL-NAMES`.")

(defparameter +cl-compatible-symbol-names+
  (cl:mapcar
   #'string
   '(:any? :assoc-in :bounded-count :cl-conj :conj :contains? :count-while
     :difference :disj :dissoc :distinct? :doseq :empty :empty? :every? 
     :frequencies :get-in :group-by :index :index-of :join :map-invert
     :merge-with :not-any? :not-empty :not-every? :last-index-of :peek
     :postwalk :prewalk :postwalk-replace :prewalk-replace :project
     :rand-nth :reduce-kv :rename :rename-keys :select :select-keys
     :subset? :superset? :subvec :update :update-in :update-keys
     :update-vals :walk

     :coll? :collp :cons? :list? :map-entry? :map? :mapp :queue :queue?
     :set? :string? :vector? :associative? :counted? :reversible? :seq?
     :seqable? :sequential?

     :cat :completing :deref :ensure-reduced :halt-when :unreduced
     :reduced? :into :transduce
    
     :doall :dorun :lazy-cat :lazy-seq :nthnext :nthrest :realized? :seq :rseq))
  "Symbol names of CLJ-COLL functions which return pure CL compatible data types,
as opposed to types CL functions will not understand, like immutable collections.

Access with `CL-COMPATIBLE-SYMBOL-NAMES`, typically in a `defpackage` form.
See README.md.")

(defun cl-compatible-symbol-names (&key exclude)
  "This is an exported utility function for use in CLJ-COLL client DEFPACKAGE
contexts. It returns a list of strings naming all symbols exported by the
CLJ-COLL package that (a) do not shadow CL package symbols and (b) return only
Common Lisp compatible data types (i.e. things that pure CL code would understand, 
and not the CLJ-COLL immutable data structures).

This function is probably useful of you want to import just the above subset of
CLJ-CON symbols.  For example

    (defpackage my-package
      (:use :cl)
      (:import-from :clj-coll . #.(clj-coll:cl-compatible-symbol-names))
      ...)

If EXCLUDE is specified, it should be a list of string designators
indicating symbols that should NOT appear in the output of this function.
E.g. `(cl-compatible-symbol-names :exclude '(:peek :pop :empty))`.

SEE ALSO: CL-SYMBOL-NAMES, NON-CL-SYMBOL-NAMES."
  (-> (cl:union +m-function-symbol-names+ +cl-compatible-symbol-names+
                :test #'string=)
      (cl:set-difference (mapcar #'string exclude) :test #'string=)))

;; Probable temporary hacks to debug transducers, which proved difficult in SBCL.
;; TODO: make call-invocation-id correlated to the call-site-id ???
;; (HT, keyed by call site, valued by invocation-id).
;; Also make it thread safe.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *xpr-call-site-id* 0 "Reflects a XPR macro call site")
  (defparameter *xpr-call-invocation-id* 0 "Reflects an XPR expansion execution"))
(defmacro xpr ((label &rest misc) sexp)
  "Debugging macro, print a form being evaluated, it's result, and return the result.

Use like (xpr (\"some-label\" arg1 arg2) (some-form-to-execute))

Note that the 'id' in square brakets reflects a compiled code site, and not an instance
of execution of the compiled code site."
  (let* ((v (gensym "xpr-"))
         (inv (gensym "xpr-invocation-"))
         (id (incf *xpr-call-site-id*))
         (printme (if misc 
                      `(format t "~%[xpr site-~d / call-~d] ~s ~s ~s:~%" 
                               ,id ,inv ,label ',misc (cl:list ,@misc))
                      `(format t "~%[xpr site~d / call-~d] ~s =>~%" ,id ,inv ,label))))
    `(let ((,inv (incf *xpr-call-invocation-id*)))
       ,printme
       (let ((,v ,sexp))                ;eval AFTER printing header material
         (format t "[xpr site-~d / call-~d] => ~s~%" ,id ,inv ,v)
         ,v))))

#+NIL
(defun vector-conj (vector item)
  "A reducing function of two arguments that it will set the element of VECTOR
designated by its fill pointer to ITEM and increment the fill pointer by one.

This function always APPENDS to a vector.

Returns VECTOR.

This function is similar to a cross between CL:VECTOR-PUSH and CL:VECTOR-PUSH-EXTEND.
However the argument order is reversed for use with REDUCE, and it may allocate
and return a new adjustable fill-pointered array if the input array doesn't support
fill pointers, or if it can't be adjusted when the need arises."
  (print "*FINISH* VECTOR-CONJ"))

(declaim (ftype (function (cl:vector t) (values cl:vector &optional)) vector-conj-reverse))
(defun vector-conj-reverse (vector item)
  "This function serves as a reducer that will fill a vector in reverse order,
first decrementing the fill-pointer of the vector, then storing ITEM at that location.
The fill pointer should initially equal to capacity of the vector.

An error is signalled if the fill pointer goes below zero.

The return value of this function is always the input vector.

THIS FUNCTION REQUIRES CAREFUL USE BY THE CALLER.
The vector must have a fill pointer, and should be used only when you know exactly
how many elements you're dealing with.

THE CALLER SHOULD SET THE FILL-POINTER to the array capacity when REDUCE
or other operation is complete, otherwise it will appear empty to other
users of the vector.

It 'is an error' if the vector does not have a fill pointer."
  (let ((idx (1- (fill-pointer vector))))
    (if (< idx 0)
        (error "Negative fill pointer")
        (setf (fill-pointer vector) idx))
    (setf (aref vector idx) item)
    vector))

(defun non-keyword-symbol-p (x)
  "Return true if X is a symbol but not a keyword."
  (and (symbolp x)
       (not (keywordp x))))

(defun atom-cdr-p (x)
  "When you know X is cons, this function returns true if it's a dotted pair
(i.e. the CDR is an ATOM - not a cons), NIL otherwise."
  (declare (cl:cons x) (optimize (speed 3) (safety 1)))
  (and (cdr x)
       (cl:atom (cdr x))))

(defun dotted-pair-p (x)
  "True if x is a cons whose cdr is not nil and not a cons.

Use this when you don't know if X is a cons. If you know X is a cons, use atom-cdr-p."
  (declare (optimize (speed 3) (safety 1)))
  (and (cl:consp x)
       ;; Because I goof on this all the time, the following comment exists.
       (cdr x)                  ;atom is "not cons", not "not nil". Nil isn't a cons.
       (cl:atom (cdr x))))

;;;
;;; Before you write some kind of hashtable|map-map-entries-as-list
;;; just use alexandria:hash-table-plist, eh?  That said, you may need some plist tools.
;;; Also, alexandria doesn't understand our immutable maps.
;;;

#+NIL
(defun plist-keys (plist)
  "Retrieve a cl:list of keys in plist, nil if PLIST is empty"
  (loop for k in plist by #'cddr
        collect k))

#+NIL
(defun plist-vals (plist)
  "Retrieve a cl:list of values in plist, nil if PLIST is empty."
  (loop for v in (cdr plist) by #'cddr
        collect v))

(defgeneric map-entries-as-list (map)
  (:documentation "Retrieve all key/val pairs in the CL:HASHTABLE or FSET:MAP
as a cl:list of 2-element cl:lists.  I.e.

    (map-entries-as-list {:a 1 :b 2}) => ((:a 1) (:b 2))

Entries are returned in the same order as (seq map).

For those times you want something eager and don't want the abstraction
overhead of mapping over seqs.

See also MKEYS and MVALS")
  (:method ((map cl:hash-table))
    (loop for k being the hash-keys of map
          using (hash-value v)
          collect (cl:list k v)))
  (:method ((map fset:map))
    (serapeum:with-collector (result)
      (fset:do-map (k v map)
        (result (cl:list k v))))))

(defun map-entries-as-plist (map)
  "Given an FSET:MAP or CL:HASHTABLE, return a plist of key/value
pairs suitable for use via APPLY. Of course for it to work
with APPLY the keys must be keywords, and valid for the function being applied,
but that's up to the caller."
  (loop for (k v) in (map-entries-as-list map)
        collect k collect v))

(declaim (ftype (function (cl:hash-table) (values cl:vector &optional)) hash-table-keys-as-vector))
(defun hash-table-keys-as-vector (ht)
  "Retrieve keys for a lisp hash table, return as a simple vector."
  (declare (cl:hash-table ht))
  (let* ((n (cl:hash-table-count ht))
         (vec (cl:make-array n))        ;a simple-vector
         (idx 0))
    (declare (fixnum idx) (simple-vector vec))
    (maphash (lambda (k val) 
               (declare (ignore val))
               (setf (svref vec idx) k)
               (incf idx))
             ht)
    vec))

(declaim (ftype (function (fset:map) (values cl:vector &optional)) map-keys-as-vector))
(defun map-keys-as-vector (map)
  "Retrieve keys for a fset:map, return as a simple vector."
  (declare (fset:map map))
  (let* ((n (fset:size map))
         (vec (cl:make-array n))          ;a simple-vector
         (idx 0))
    (declare (fixnum idx) (simple-vector vec))
    (fset:do-map-domain (key map)
      (setf (svref vec idx) key)
      (incf idx))
    vec))

(declaim (ftype (function (cl:hash-table) (values cl:list &optional)) hash-table-keys-as-list))
(defun hash-table-keys-as-list (ht)
  "Retrieve keys for a lisp hash table, return as a CL:LIST."
  (declare (cl:hash-table ht))
  (serapeum:with-collector (result)
    (maphash (lambda (k val) 
               (declare (ignore val))
               (result k))
             ht)))

(declaim (ftype (function (fset:map) (values cl:list &optional)) map-keys-as-list))
(defun map-keys-as-list (map)
  "Retrieve keys for a fset:map, return as a CL:LIST."
  (declare (fset:map map))
  (serapeum:with-collector (result)
    (fset:do-map-domain (key map)
      (result key))))

(declaim (ftype (function (cl:hash-table) (values cl:vector &optional)) hash-table-values-as-vector))
(defun hash-table-values-as-vector (ht)
  "Retrieve values for a lisp hash table, return as a simple vector."
  (declare (cl:hash-table ht))
  (let* ((n (cl:hash-table-count ht))
         (vec (cl:make-array n))        ;a simple-vector
         (idx 0))
    (declare (fixnum idx) (simple-vector vec))
    (maphash (lambda (k val) 
               (declare (ignore k))
               (setf (svref vec idx) val)
               (incf idx))
             ht)
    vec))

(declaim (ftype (function (fset:map) (values cl:vector &optional)) map-values-as-vector))
(defun map-values-as-vector (map)
  "Retrieve values for a fset:map, return as a simple vector."
  (declare (fset:map map))
  (let* ((n (fset:size map))
         (vec (cl:make-array n))          ;a simple-vector
         (idx 0))
    (declare (fixnum idx) (simple-vector vec))
    (fset:do-map (_ val map)
      (declare (ignore _))
      (setf (svref vec idx) val)
      (incf idx))
    vec))

(declaim (ftype (function (cl:hash-table) (values cl:list &optional)) hash-table-values-as-list))
(defun hash-table-values-as-list (ht)
  "Retrieve values for a lisp hash table, return as a cl:list."
  (declare (cl:hash-table ht))
  (serapeum:with-collector (result)
    (maphash (lambda (k val) 
               (declare (ignore k))
               (result val))
             ht)))

(declaim (ftype (function (fset:map) (values cl:list &optional)) map-values-as-list))
(defun map-values-as-list (map)
  "Retrieve values for a fset:map, return as a cl:list."
  (declare (fset:map map))
  (serapeum:with-collector (result)
    (fset:do-map (_ val map)
      (declare (ignore _))
      (result val))))

(defun make-v (coll &key size)
  "Utility for all the 'v' suffixed sequence functions (e.g. `filterv`) that return a vector.
If COLL is a mutable type, return an empty extensible CL:VECTOR,
if COLL is an immutable type, return an immutable vector.

SIZE, if specified and if COLL is mutable, will be used for the initial
array allocation of the returned vector (whose fill-pointer is always zero).
It defaults to 8 or the size of COLL, whichever is smaller."
  (typecase coll
    ((or cl:sequence cl:hash-table)
     (let ((size (or size (min 8 (count coll)))))
       (make-array size :adjustable t :fill-pointer 0)))
    (t (vector))))

(defmacro make-pusher (place)
  "WARNING: IMPROPERLY IMPLEMENTED MACRO, PLACE WILL BE ABUSED.

Works for simple vars. Ought to use GET-SETF-EXPANSION instead.

Return a function which will push a value onto place and return the
updated place (which might be mutable or immutable, it's a new place
if immutable, it's not a new place if mutable.  Meant to be a most-efficient
abstraction when adding values to mutable and immutable vectors, typically
returned by `make-v`.

E.g. 
    (let* ((vec (make-v coll))
           (pusher (make-pusher vec)))       
      (pusher 1)
      (pusher 2)
      vec) ;the updated cl:vector in vec, or updated vec with new immutable data
"
  (let ((val (gensym "val-")))
    `(if (cl:vectorp ,place)
         (lambda (,val) (vector-push-extend ,val ,place) ,place)
         (lambda (,val) (setf ,place (fset:with-last ,place ,val))))))


;;;
;;; Tools to prevent mutation where we don't want it.
;;;

(defvar *mutation-considered-harmful* nil
  "Binding this special variable to a non-nil value will cause the system
to signal a condition of type MUTATION-ERROR when a mutable collection
is passed to a mutating CLJ-COLL API.

Note that it is not a requirement that the mutating operation attempt a mutation,
only that it is called with a mutable collection when this special variable is non-NIL.
This ensures we signal an exception that is not sensitive to additional argument values
in a given potential change context.")

(define-condition mutation-error (error)
  ((collection :reader collection :initarg :collection))
  (:documentation "Signalled when *MUTATION-CONSIDERD-HARMFUL* is non-NIL and
a mutating CLJ-COLL API is called on a mutable collection.")
  (:report 
   (lambda (condition stream)
     (format stream "Mutating API called on collection of type ~s while *MUTATION-CONSIDERED-HARMFUL* was enabled."
             (type-of (collection condition))))))

(defmacro check-mutation-barrier (coll)
  "If *MUTATION-CONSIDERED-HARMFUL* is enabled, signal a MUTATION-ERROR on COLL
because the caller already knows COLL is mutable."
  `(when *mutation-considered-harmful*
     (error 'mutation-error :collection ,coll)))

(defun realloc-as-adjustable  (vector &optional (n-additional 1))
  "Allocates a new vector that is fill-pointered
and adjustable and contains the same contents as vector.

It is allocated so that it can hold at least N-ADDITIONAL elements as well as what's
already VECTOR.

If VECTOR is already adjustable and fill-pointered then we're basically making 
a copy of the vector with a capacity that's N-ADDITIONAL more than VECTOR's.
(Which is not the intended use-case for this function, but whatever...)"
  (declare ((cl:vector t *) vector) (fixnum n-additional)
           (optimize (speed 3) (safety 0)))
  (let* ((capacity (array-dimension vector 0))
         (n-elements (cl:length vector)) ; observes fill-pointer if appropriate
         (new-size (+ capacity n-additional))
         (result (make-array new-size :adjustable t :fill-pointer n-elements)))
    (declare ((cl:vector t *) result))
    (if (simple-vector-p vector)
        (loop for i from 0 below n-elements
              do (setf (aref result i) 
                       (svref (the simple-vector vector) i)))
        (loop for i from 0 below n-elements
              do (setf (aref result i) (aref vector i))))
    result))

#+(OR)
(defun realloc-if-not-adjustable (vector &optional (n-additional 1))
  "Allocates a new vector that is fill-pointered
and adjustable and contains the same contents as vector, only if
the vector is not already such a thing.

If a new vector is created, it is allocated so that it can hold at least N-ADDITIONAL
elements as well as what's already VECTOR."
  (declare ((cl:vector t *) vector)
           (optimize (speed 3) (safety 0)))
  (if (and (adjustable-array-p vector)
           (array-has-fill-pointer-p vector))
      vector
      (realloc-as-adjustable vector n-additional)))

(defun safe-vector-push-extend (item vec)
  "A wrapper for VECTOR-PUSH-EXTEND
which will trap errors if VEC is not an adjustable or fill-pointered array,
allocate a new vector which is adjustable and fill-pointered,
copying the old vec contents and adding ITEM.

If no reallocation occurs, returns NIL.
Otherwise the newly allocated, initialized, item-added vector. 
NOTE: This is different than the return value of VECTOR-PUSH-EXTEND."
  (declare ((cl:vector t *) vec) (optimize (speed 3) (safety 1)))
  ;; Adds 25-30% overhead compared to just `vector-push-extend`
  ;; on a test where the vector was never extended.  So that's all
  ;; catch/handler-bind activity, not super friendly in a tight 
  ;;[safe-]vector-push-extend loop.  ~0.16 secs vs ~0.12 secs
  ;; 37m processor cycles vs 28m
  #| ;1M calls to vector-push-extend
  (let ((v (make-array 100000 :fill-pointer 0 :adjustable t))
                (l '(1)))
            (time (dotimes (i 10) (setf (fill-pointer v) 0)
                    (dotimes (i 100000) (conj-aux v l)))))
  |#
  (let (retry)
    (block oops
      (handler-bind 
          ((error (lambda (condition)
                    (declare (ignore condition))
                    (setf vec (realloc-as-adjustable vec) retry t)
                    (return-from oops))))
        (vector-push-extend item vec)))
    ;; NIL if not upgraded, new vector if upgraded
    (when retry
      (vector-push-extend item vec)
      vec)))

#+(OR)
;; THere's a unit test too for this function, also commented out.  Delete eventually if unused.
(defun realloc-if-necessary (vector &optional (n-additional 1))
  "Examine VECTOR to see if it has capacity or can be adjusted to fit N-ADDITIONAL elements.

If it is adjustable and fill-pointered, do nothing and return VECTOR.
If it is fill-pointered and has space for N-ADDITIONAL elements,
do nothing and return VECTOR.

Otherwise create and return a new fill-pointered adjustable array initialized with
the contents of VECTOR, with empty space for N-ADDITIONAL elements."
  (declare ((cl:vector t *) vector) (fixnum n-additional)
           #+NIL
           (optimize (speed 3) (safety 0)))
  (check-type vector (cl:vector t *))
  (check-type n-additional fixnum)
  ;; SBCL MAKE-ARRAY makes them adjustable even if you don't request it when
  ;; you request a fill-pointer. I guess it just isn't "expressly adjustable"
  (let ((fp (and (array-has-fill-pointer-p vector) (fill-pointer vector))))
    (if (and fp (adjustable-array-p vector))
        vector
        (let ((capacity (array-dimension vector 0)))
          (declare (fixnum capacity))
          (if (and fp (> (- capacity fp) n-additional))
              vector
              (let* ((new-size (+ capacity n-additional))
                     (n-elements (or fp capacity))
                     (result (make-array new-size :adjustable t 
                                                  :fill-pointer n-elements)))
                (declare ((cl:vector t *) result))
                (if (simple-vector-p vector)
                    (loop for i from 0 below n-elements
                          do (setf (aref result i) 
                                   (svref (the simple-vector vector) i)))
                    (loop for i from 0 below n-elements
                          do (setf (aref result i) (aref vector i))))
                result))))))

#+(OR) ;haven't needed this yet
(defmacro check-mutation-barrier-and-coll (coll)
  "If *MUTATION-CONSIDERED-HARMFUL* is enabled, check to see if COLL is mutable,
and if it is, signal a MUTATION-ERROR.  Returns NIL otherwise."
  (let ((v (gensym)))
    `(when (and *mutation-considered-harmful*
                (let ((,v ,coll))
                  (or (typep ,v 'cl:sequence)
                      (typep ,v 'cl:hash-table))))
       (error 'mutation-error :collection coll))))

;;; Macroexpansion time tools for `defn-ish`
(eval-when (:compile-toplevel :load-toplevel :execute)

(defun defn-syntax (defn-arity-body-list)
  "Tool for `defn-ish`.
e.g. (defun-syntax (((pred) ...) ((pred coll) ...))
=> \"[pred] [pred coll]\""
  (flet ((arglist-str (arglist)
           (if arglist
               (format nil "[~{~a~^ ~}]" arglist)
               "[]")))
    (format nil "~{~a~^ ~}" 
            (loop for ab in defn-arity-body-list
                  collect (arglist-str (first ab))))))

(defun arglist-info (arglist)
  "Return information about an arglist in the form of a plist as follows:

    :arity n                ; number of formal parameters
    :parameter-names <list> ; name of each parameter
    :variadic-p t/nil       ; true if the list is variadic, i.e. contains & or &rest.

Validate the list as well, signal an error if there are unsupported/invalid constructs.
Only the last argument may be optional/&rest."

  (loop for token in arglist
        if (or (string= token "&")
               (string= token "&rest"))
          collect token into lambda-keywords
        else
          collect token into parameters
        finally 
           (progn (when (> (cl:length lambda-keywords) 1)
                    (error "Too many or unsupported lambda list keywords in arglist ~s" 
                           arglist))
                  (return (cl:list :arity (cl:length parameters)
                                   :parameter-names parameters
                                   :variadic-p lambda-keywords)))))

(defun defn-ish-args (args)
  "Parse docstring, rest-arg-name, and arity-body-list, and return these three
things as distinct values, with NIL if they aren't specified."
  ;; We do this so that we can express the intent of the defn-ish macro with &optionals
  ;; but it's a pain.  Note that we may have a trailing nil in args 

  (let* ((docstring (when (stringp (cl:first args)) (cl:first args)))
         (args (if docstring (cdr args) args))
         (rest-arg-name (when (symbolp (cl:first args)) (cl:first args)))
         (args (if rest-arg-name (cdr args) args)))
    (values docstring rest-arg-name args)))
)                                       ;eval-when

#|
On DEFN-ISH and DEFN:
*TODO* experiment with vector-enabled syntax for a more accurate `defn` syntax.
There are potential issues with (vector x y z) or [x y z] for parameter lists
and the interaction with reader macros, and perhaps whether CL will even allow
lambda list keywords exist anythere other than a defun/defgeneric lambda list.
E.g. [a b &rest c]. Or would we want to emulate clojure with [a b & c] ?
More potential issues.  Anyway, a bit of play to see what we can do 
with CL ANSI conformant behaviors to make something more closely resemble DEFN.

Note that right now, the CLJ-COLL package does NOT have augmented reader syntax
enabled.
|#

;;; NOTE:  defn-ish may not be CALLED in this utils.lisp module, it uses things
;;; at compile time which haven't been defined yet, like CLJ-COLL:FIRST.

(defmacro defn-ish (name &optional doc-string args-name &body arity-body-list)
  "A multi-arity-only flavor of Clojure's DEFN with the following caveats:

1. Parameter notation is cl:list based, not vector based.

2. Without vector syntax, we can't tell if DEFN is being used
   for a single function definition, or whether it's expressing multiple
   arities for the function, thus DEFN is presently restricted to
   parenthesized arity definitions, even if it's only one.  I.e.

    (defn-ish foo ((x) ...)) ; okay
    (defn-ish foo (x) ...)   ; not okay, use defun

3. A &rest arg signature is permitted so long as the number of
   parameters is distinct from other signatures. I.e.g

   (a b) and (a & b) - not permitted
   (a) (a & b)       - permitted

   '&' and '&rest' are are supported and are synonymous.
   Only one signature may be variadic (consistent with Clojure)

4. There is no argument destructuring. 

5. defn-ish allows an optional symbol between docstring and arity-body-list which,
   if present, is used as an alias for the &rest args declared for the macroexpanded
   function.  This is useful if you need access to all args as one list
   (which already exists as the &rest arg), allows you to avoid reconstituting
   the rest list in some functions.

Each member of arity-body-list is expected to be a cl:list whose car is a possibly
empty sublist naming parameters, and whose cdr is the body of the function for that
arity/parameter-list. the body is executed in an implicit progn, and if there are
parameters, a let block is also implicit such that your body may include
declarations.

for example:

    (defn-ish nums 
      \"return a lazy sequence of numbers. 
        (nums) returns an empty sequence. (nums n) returns n numbers from n downto 1.\"
      (()  (nums 0))
      ((n) (lazy-seq (if (< n 1) nil (cons n (nums (1- n)))))))

    (nums)   => empty lazy sequence
    (nums 3) => lazy seq producing three nums

    (defn-ish add
      ((a) a)
      ((a b) (declare (fixnum a b)) (+ a b)))

    (defn-ish add
      args                     ;alias for &rest args of generated fn
      ((a b & xs) 
       (declare (ignore a b xs))
       (apply #'+ args))) ; rather than (cl:list* a b xs)

See source code comment above defn-ish for notes on making a clojure compatible defn
that behaves as a conformant common lisp macro. 
"
  ;; If there's a variadic signature, we emit a COND for dispatching to correct arity.
  ;; Otherwise we emit a CASE dispatching on each arity.
  ;; No awards for pretty here, kind of evolved as I needed it, isn't public anyway.
  (multiple-value-bind (doc-string args-name arity-body-list)
      (defn-ish-args (cl:list* doc-string args-name arity-body-list))
    (let* (;; Name the the symbol generated for DEFUN &rest parameter
           ;; so that it gives a hint about the real function args.
           ;; The swank prompt when typing in a call to the defn-ish function will resemble
           ;; (filter &rest [pred] [pred coll]), which is very helpful.
           (args (make-symbol (defn-syntax arity-body-list))) ;vs gensym - no counter desired
           (argndoc (if doc-string 
                        `((&rest ,args) ,doc-string)
                        `((&rest ,args))))
           (arglist-infos (mapcar #'arglist-info (mapcar #'cl:first arity-body-list)))
           (variadic-p (cl:some (lambda (info) (getf info :variadic-p)) arglist-infos))
           (narg-var (gensym "defn-nargs-"))
           (rest-arg-alias (when args-name `((,args-name ,args)))))
      (flet ((arity-binding (arglist-info body)
               ;;  arglist => (let ((,arg (cl:first ,args)) ...) ,@body)
               ;;  with support for &rest behavior on last parameter
               (let ((let-bindings
                       (loop with variadic-p = (cl:getf arglist-info :variadic-p)
                             with nargs = (cl:getf arglist-info :arity)
                             for argno from 0
                             for parametername in (cl:getf arglist-info :parameter-names)
                             if (and variadic-p (= argno (1- nargs)))
                               collect `(,parametername (cl:nthcdr ,argno ,args))
                             else
                               collect `(,parametername (cl:nth ,argno ,args)))))
                 `(let (,@let-bindings) ,@body))))
        (let* ((arity-cases 
                 (loop with arities-done = nil
                       for ab in arity-body-list
                       as arglist = (cl:first ab) 
                       as abody = (cl:rest ab)
                       for arglist-info in arglist-infos
                       as arity = (getf arglist-info :arity)
                       as this-sig-variadic-p = (getf arglist-info :variadic-p)
                       ;; receiver is cond test or case label
                       as receiver = (if variadic-p
                                         (if this-sig-variadic-p
                                             `(cl:>= ,narg-var ,arity) 
                                             `(cl:= ,narg-var ,arity))
                                         arity)
                       if (cl:find arity arities-done :test #'cl:=)
                         do (error "More than one signature had arity ~d" arity)
                       else
                         collect 
                         (if arglist
                             `(,receiver ,(arity-binding arglist-info abody))
                             `(,receiver ,@abody))
                       end
                       do (push arity arities-done)))
               ;; Add a check if there were no arith matches for the specified number of args.
               ;; Watch for variadic signatures.
               (error `(error "function ~s has no arity for ~d arguments"
                              ',name ,narg-var))
               (arity-cases (cl:nconc arity-cases `((t ,error))))
               (case-or-cond
                 (if variadic-p
                     `(cond ,@arity-cases)
                     `(case ,narg-var ,@arity-cases))))
          `(defun ,name ,@argndoc 
             (let* ((,narg-var (cl:length ,args))
                    ,@rest-arg-alias)   ;only if args-name is specified, aliases narg-var
               ,case-or-cond)))))))

;;;
;;; Functions that support treating sets/maps/keywords as functions.
;;; See README.md section on "datatypes as functions".
;;;

(declaim (ftype (function (t &optional t) (values function &optional)) coll-fun))
(defun coll-fun (coll-exp &optional not-found) 
  "Always returns a function as follows:


If COLL-EXP is a non-keyword symbol, we treat it as a function designator and return
its function definition.

If COLL-EXP is a function, we return it as is.

If COLL-EXP is a keyed collection (set, map, cl:hash-table), we return a function
parameterized by 'item' which does an item lookup on the collection, i.e.

    (lambda (item) (get coll-exp item not-found))

If COLL-EXP is a keyword, we return a function parameterized by 'coll'
that looks up the keyword in the collection, i.e.

    (lambda (coll) (get coll coll-exp not-found))

You can then do things like the following

    (filter (coll-fun #{1 2 3}) '(... 2 ...)) => (2)
    (filter (coll-fun {:a 1 :b 2}) #{:a :b :c}) => '(:a :b)
    (map (coll-fun {:a 1 :b 2} :fred) [:a :b :c]) => (1 2 :fred)   ;(*)
    (filter (coll-fun 'identity) '(1 2)) => (1 2))                 ;(**)
    (filter (coll-fun :a) [{:a 1} {:b 2} {:a 3}]) => ({:a 1} {:a 3})
    (filter (coll-fun (lambda (x) x)) '(1 2)) => (1 2)             ;(***)

(*) Note that the use of :fred is supplying a optional not-found value
(**) When passed 'identity, all coll-fun is doing is returning the fdefinition of identity.
(***) As lambda returns a function, COLL-FUN here is a NO-OP.

Any other argument type 'is an error' that will not be detected until the resulting
function is invoked.

This function is exported in the event you find it useful, but note that
all CLJ-COLL APIs which can benefit from it already use it and you shouldn't need to 
call it for CLJ-COLL interactions."
  (declare (optimize (speed 3) (safety 1)))
  (if (symbolp coll-exp)
      (if (keywordp coll-exp)
          (lambda (coll) (get coll coll-exp not-found))
          (fdefinition coll-exp))
      (if (functionp coll-exp)
          coll-exp
          (lambda (item) (get coll-exp item not-found)))))

;;;
;;; Miscellaneous convenience for clojure compatibility and/or FP
;;; (Minus definitions that want to use `defn-ish`.
;;;

(defun comp (&rest fns)
  "([] [f] [f g] [f g & fs])
Takes a set of functions and returns a function that is the composition
of those functions.  The returned function takes a variable number of args(*)
and applies the rightmost of the functions to the args, then the next
(right-to-left) function to the result, and so on.

(*) If no fns are specified, the resulting function is IDENTITY, which accepts
only one argument.  This is for clojure compatibility."
  (if fns
      (apply #'alexandria:compose fns)
      #'identity))

(declaim (ftype (function (t &rest t) (values function &rest t)) juxt))
(defun juxt (f &rest fns)            ;declared to allow compile check of missing args
  "([f] [f & fns]
Takes one or more functions and returns a function
that accepts any number of args, and returns an immutable vector
containing the result of applying each function to the args 
(functions applied left to right in order of arguments to JUXT).

(funcall (juxt #'a #'b #'c) x) => [(a x) (b x) (c x)]

For a version of juxt that returns a CL:LIST of results, see MJUXT."
  (let ((all-fns (cl:list* f fns)))
    (lambda (&rest args)
      (reduce (lambda (r fn)
                (conj r (cl:apply fn args)))
              (vector) all-fns))))

(defun mjuxt (f &rest fns)
  "Like JUXT, only the returned function returns a cl:list result of values
instead of an immutable vector."
  (apply #'serapeum:juxt f fns))

(declaim (ftype (function (t &rest t) (values function &rest t)) partial))
(defun partial (f &rest args)
  "Implements clojure.core/partial.

Takes a function F and some number of arguments for F which fewer than F might
normally require, and returns a function that takes a variable number of additional
args. When called, the returned function calls F with args + additional args.

E.g. (funcall (partial #'* 10) 3) => 30

For an inlined version of PARTIAL use SERAPEUM:PARTIAL directly.
This is just a wrapper for that with a useful docstring."
  (apply #'serapeum:partial f args))


(declaim (ftype (function (t &rest t) (values function &rest t)) every-pred))
(defun every-pred (pred &rest preds)
  "([pred & preds])
Returns a function that accepts any number of arguments and returns true if all
predicates return true for when invoked against each arguments, and NIL otherwise.

The predicates are called with one argument at a time, and processing
stops as soon as any predicate returns NIL.  Predicates are called
in the order supplied, so that failure of one on the left means one to the right
of it will not be called.

If the returned function is invoked on zero arguments it will
always return T, e.g. (funcall (every-pred (constantly nil))) => T.

E.g.: (funcall (every-pred #'plusp #'oddp) -1 0 1) => NIL
      (funcall (every-pred #'plusp #'oddp) 1 3 5)  => T

See also: some-fn and every?"
  (let ((preds (cl:list* pred preds)))
    (lambda (&rest args)
      (block every-pred-fn
        (loop for pred in preds
              do (loop for arg in args
                       unless (funcall pred arg)
                         do (return-from every-pred-fn nil)))
        t))))

(declaim (ftype (function (t &rest t) (values function &rest t)) some-fn))
(defun some-fn (pred &rest preds)
  "([pred & preds])
Returns a function that accepts any number of arguments and the first non-NIL
value a predicate applied to an argument.

The predicates are called with one argument at a time, and processing
stops as soon as any predicate returns non-NIL.  Predicates are called
in the order supplied, so that success of one on the left means one to the right
will not be called.  For two predicates P1 and P2, P1 is tried on all args
before P2, which is why '1' is returned in the example below.

If the returned function is invoked on zero arguments it will
always return NIL, e.g. (funcall (some-fn (constantly t))) => NIL.

Example:
(defun plusval (x) (and (plusp x) x))
(defun oddval (x) (and (oddp x) x))
(funcall (some-fn #'plusval #'oddval) -1 0 1) => 1
(funcall (some-fn #'plusval #'oddval) -2 -4 -5)  => -5

See also: every-pred, some, some-> some->>."
  (let ((preds (cl:list* pred preds)))
    (lambda (&rest args)
      (block some-fn-aux
        (loop for pred in preds
              do (loop for arg in args
                       as val = (funcall pred arg)
                       when val
                         do (return-from some-fn-aux val)))
        nil))))

(defun dec (n) 
  "Returns a number one less than N.
Consider using CL:1-."
  (1- n))

(defun inc (n)
  "Returns a number one greater than N.
Consider using CL:1+."
  (1+ n))

(defun identical? (x y) 
  "([x y])
Tests if 2 arguments are the same object, i.e. EQ."
  (eq x y))

(defun not= (x y)
  "(not (equal? x y))"
  (not (equal? x y)))

(defun rand (&optional n)
  "([] [n])
Returns a random double-precision floating point number between 0 (inclusive) and
n (default 1) (exclusive).

To ensure thread safety if you are calling rand concurrently, 
consider establishing distinct per-thread *RANDOM-STATE* values.

See also: CL:RANDOM, CL:MAKE-RANDOM-STATE, CL:*RANDOM-STATE*"
  (if n
      (* n (cl:random 1.0d0))
      (cl:random 1.0d0)))

(defun rand-int (n)
  "Returns a random integer between 0 (inclusive) and n (exclusive).

N must be a positive value. 

If given a non-integer number, note that Clojure/Java would do an integer cast which
might round OR truncate depending on the value. RAND-INT will truncate
non-integer values. If you don't want that pass an integer."
  (cl:random (if (integerp n) n (truncate n))))

;;;
;;; Predicates on numbers. Not very collection-y, but solves the odd reference to 'pos?' and such.
;;; *TODO*: declaim these inline? Some tests might be good too, these were untested quickies.
;;;

(defun even? (n)
  "Return true if N is even. See also CL:EVENP."
  (evenp n))

(defun odd? (n)
  "Return true if N is odd. See also CL:ODDP."
  (oddp n))

(defun neg? (n)
  "Returne true if N is negative. See also CL:MINUSP"
  (cl:minusp n))

(defun pos? (n)
  "Return true if N is positive. See also CL:PLUSP"
  (cl:plusp n))

(defun zero? (n)
  "Return true if N is zero. See also CL:ZEROP."
  (zerop n))

(defun number? (n)
  "Return true if N is a number. See also CL:NUMBERP."
  (cl:numberp n))

(defun integer? (n)
  "Return true if N is an integer. See also CL:INTEGERP."
  (cl:integerp n))

(defun rational? (n)
  "Return true if N is a rational number. See also CL:RATIONALP, CL:RATIONALIZE.
Note that integers and ratios are disjoint subtypes of rationals.
https://www.lispworks.com/documentation/HyperSpec/Body/t_ration.htm#rational"
  (cl:rationalp n))

(defun ratio? (n)
  "Return true if N is a ratio. See also RATIONAL?.
Ratios are a subtype of rational numbers.
https://www.lispworks.com/documentation/HyperSpec/Body/t_ratio.htm#ratio"
  (typep n 'cl:ratio))

(defun decimal? (n)
  "Return true if N is a java.util.BigDecimal.  Always returns false.
Java's BigDecimal is an immutable, arbitrary-precision signed decimal numbers., and
Common Lisp doesn't natively support such a type, though perhaps there is a library to do this
somewhere.

Perhaps `(typep n 'cl:bignum)` will help?"
  (declare (ignore n))
  nil)

(defun float? (n)
  "Return true if N is a floating point number."
  (cl:floatp n))

(defun double? (n)
  "Return true if N is a double precision floating point number."
  (typep n 'cl:double-float))

;;; If we care, the following need definitions: int? nat-int? neg-int? pos-int? NaN? infinite?

(defun string? (x)
  "Return true if X is a string."
  (cl:stringp x))

(defun keyword? (x)
  "Return true if X is a keyword"
  (cl:keywordp x))

(defun symbol? (x)
  "Return true if X is Clojure's idea of a symbol, NIL otherwise.
Note that in Clojure keywords are NOT symbols, and this function
returns nil if given a keyword."
  (and (cl:symbolp x)
       (not (cl:keywordp x))))
