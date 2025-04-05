(in-package :clj-coll)

;;;; Clojure APIs that mostly return lazy seqs, and M function equivalents.
;;;; See also seq-apis2.lisp for stuff which requires doseq.
;;;; M functions (see README.md) that correspond to lazy functions are placed in
;;;; proximity to their namesakes.

(defvar *debug-transducers* nil
  "If non-nil, break on entry to transducers")

;; Debugging crap
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Because I really don't want to pass the name of the function in which transducer macros
  ;; are called just for debugging I should never need once I get things working.
  (defun env-function-name (env)
    "Try to ascertain the name of the function in which &environment ENV exists.
:cl-environment doesn't have a thing for this that I know of, whatever
is done here is surely unportable.

STRICTLY A SHORT TERM DEBUGGING TOOL FOR SBCL + TRANSDUCER MACROS

Returns \"<UNKNOWN-FUNCTION>\" if it can't figure it out."
    (declare (ignorable env))
    (let ((unknown "<ENV-FUNCTION-NAME UNKNOWN>"))
      #+sbcl                              ;#<SB-KERNEL:LEXENV> structure object
      (if env 
          (-> (slot-value env 'sb-c::blocks) caar symbol-name)
          unknown)
      #-sbcl
      unknown)))

(defmacro transducer ((rf result input) &body body &environment env)
  "emit code that defines a transducer, taking care of the multiple-arity
function skeleton, using parameter names indicated by rf, result, and input.

the expansion has to the basic clojure transducer shape (as equivalent common lisp):

    (fn [rf]
      (fn ([] (rf))                     ;arity0 - init
          ([result] (rf result))        ;arity1 - completion for some stateful transducers
          ([result input] <body>)))     ;arity2 - so-called 'step' function, the meaningful bits

body is used as the body of the arity2 function signature.

If you need to specify a greater arity than 2, because your logic requires multiple
inputs (e.g. `map`), you'll have to hand code it, or this macro would have to be made to
accommodate multiple bodies for the different arities.

See the single-argument FILTER signature for an example use of this macro.
See also: STATEFUL-TRANSDUCER.
"
  (let ((xfargs (gensym "xfargs-"))
        (function-name (env-function-name env)))
    `(lambda (,rf)
       (lambda (&rest ,xfargs)
         (when *debug-transducers*
           (format t "~%~a transducer args: ~s~%" ,function-name ,xfargs))
         (ecase (cl:length ,xfargs)
           (0 (funcall ,rf))                       ;init
           (1 (let ((,result (cl:first ,xfargs)))  ;completion
                (funcall ,rf ,result)))
           (2 (let ((,result (cl:first ,xfargs))   ;step
                    (,input  (cl:second ,xfargs)))
                ,@body)))))))

(defmacro stateful-transducer ((rf result input state-vars
                                &key init-body completion-body)
                               &body step-body &environment env)
  ;; This macro may or may not be better than simply typing all the nested lambdas.
  ;; But the arity-expression is still better.
  "Like 'transducer' except it provides parameters for state variable declaration,
initialization code, and completion code, as well as the customary step body.

STATE-VARS should be a list of variable names or variable/init sublists as if for LET. 

    (fn [rf]
      (let ((<state1> <init1>)                       ;state vars
            ...)
        (fn ([] (rf)|<init-body>)                    ;arity0 - initialization
            ([result] (rf result)|<completion-body>) ;arity1 - completion
            ([result input] <step-body>))))          ;arity2 - step

INIT-BODY and COMPLETION-BODY are optional and default to the behavior of the
`TRANSDUCER` macro if unspecified.
"
  (let ((xfargs (gensym "xfargs-"))
        (init-body (or init-body `(funcall ,rf)))
        (completion-body (or completion-body `(funcall ,rf ,result)))
        (function-name (env-function-name env)))
    `(lambda (,rf)
       (let (,@state-vars)
         (lambda (&rest ,xfargs)
           (when *debug-transducers*
             (format t "~%~a stateful transducer args: ~s~%" ,function-name ,xfargs))
           (case (cl:length ,xfargs)
             (0 ,init-body)
             (1 (let ((,result (cl:first ,xfargs)))
                  ,completion-body))
             (2 (let ((,result (cl:first ,xfargs))
                      (,input  (cl:second ,xfargs)))
                  ,@step-body))))))))

(defun count-while (pred coll)
  "Return the count of leading items in COLL for which (PRED item) returns true.
Don't confuse with COUNT-IF which would continue searching and possibly counting
after a NIL value from PRED."
  (let ((c 0)
        (pred (alexandria:ensure-function pred)))
    (declare (function pred))
    (run! (lambda (item)
            (if (funcall pred item)
                (incf c)
                (return-from count-while c)))
          coll)
    c))

(defun mfilter (pred coll)
  "An eager version of FILTER that returns a CL:LIST
of all items in COLL for which (PRED item) returns true.

Will realize lazy sequences, don't pass an infinite sequence!

See also CL:REMOVE-IF-NOT."
  (let ((pred (alexandria:ensure-function pred)))
    (declare (function pred))
    (if (cl:listp coll)
        (cl:remove-if-not pred coll)
        (serapeum:with-collector (collect)
          (run! (lambda (item)
                  (when (funcall pred item)
                    (collect item)))
                coll)))))

(defn-ish filter
  "([pred] [pred coll])
Returns a lazy sequence of the items in COLL for which
(PRED item) returns true. 

Phrased another way: (REMOVE-IF-NOT PRED COLL).

PRED should be free of side-effects.
Returns a transducer when no collection is provided.

See also: MFILTER, REMOVE, CL:REMOVE-IF-NOT."
  ((pred)
   (transducer (rf result input)
     (if (funcall pred input)
         (funcall rf result input)
         result)))
  ((pred coll)
   (lazy-seq
     (when-let (s (seq coll))
       (let ((first (first s)))
         (if (funcall pred first)
             (cons first (filter pred (rest s)))
             (filter pred (rest s))))))))
   
(defun filterv (pred coll)
  "([pred coll])
Returns a persistent or mutable vector having the same mutability trait as COLL,
consisting of items in COLL for which (PRED item) returns true.  PRED should be free
of side-effects."
  (let* ((result (make-v coll))
         (pusher (make-pusher result))
         (pred (alexandria:ensure-function pred)))
    (declare (function pred))
    (run! (lambda (val) 
            (when (funcall pred val)
              (funcall pusher val)))
          coll)
    result))

(defn-ish mtake
  "([n coll] [result-type n coll])

An eager version of TAKE that will return a CL:LIST(default) or
CL:VECTOR of N elements from COLL (which may be a seq), returning all items in coll
if there are fewer than N.

The optional leading RESULT-TYPE parameter may be one of the symbols CL:LIST or
CL:VECTOR, and behaves as if CL:LIST were specified if RESULT-TYPE is omitted.

No structure is shared with COLL.

(MTAKE 0) returns NIL for CL:LIST returns and #() for CL:VECTOR returns."
  ((n coll) (mtake 'cl:list n coll))
  ((result-type n coll)
   (ecase result-type
     (cl:list
      (loop repeat n
            for s = (seq coll) then (next s)
            while s
            collect (first s)))
     (cl:vector 
      (if (<= n 0)
          #()
          (loop with result = (make-array n :fill-pointer 0)
                repeat n
                for s = (seq coll) then (next s)
                while s
                do (vector-push (first s) result)
                finally (return result)))))))

(defn-ish mtake-while
  "([pred coll] [result-type pred coll])

An eager version of TAKE that will return a CL:LIST(defualt) or CL:VECTOR 
of leading elements from COLL (which may be a seq) for which (PRED item) returns true.

If there are no items matched by PRED or COLL is empty, CL:LIST result will be NIL
and CL:VECTOR result will be #().

Results may share structure (or be EQ) with CL:SEQUENCE inputs."
  ((pred coll) (mtake-while 'cl:list pred coll))
  ((result-type pred coll)
   (let ((pred (alexandria:ensure-function pred)))
     (declare (function pred))
     (ecase result-type
       (cl:list 
        ;; No attempt at sharing a fully matched input CL:LIST
        (loop for s = (seq coll) then (next s)
              as  val = (first s)
              while (and s (funcall pred val))
              collect val))
       (cl:vector 
        (let ((n-matched (count-while pred coll)))
          (if (zerop n-matched)
              #()
              (let* ((sharing (cl:vectorp coll))
                     (result (if sharing
                                 (if (= n-matched (cl:length coll))
                                     coll ;EQ result
                                     (make-array n-matched :displaced-to coll))
                                 (make-array n-matched))))
                (if sharing
                    result              ;no need to copy vals
                    (loop for i from 0 
                          for s = (seq coll) then (next s)
                          as  val = (first s)
                          while (and s (funcall pred val))
                          do (setf (svref result i) val)
                          finally (return result)))))))))))
                           

(defn-ish take
  "([n] [n coll])
Returns a lazy sequence of the first n items in coll, or all items if
there are fewer than n.  Returns a stateful transducer when
no collection is provided.  The transducer is not thread-safe.

See also: MTAKE"
  ((n)
   (transducer (rf result input)
     (let ((result (if (plusp n) 
                       (funcall rf result input)
                       result)))
       (if (not (plusp (decf n)))
           (ensure-reduced result)
           result))))
  ((n coll)
   (lazy-seq
     (when (plusp n) 
       (when-let (s (seq coll))
         (cons (first s) (take (1- n) (rest s))))))))

(defn-ish take-while
  "([pred] [pred coll])
Returns a lazy sequence of successive items from coll while
(pred item) returns true. pred must be free of side-effects.
Returns a transducer when no collection is provided."
  ((pred)
   (transducer (rf result input)
     (if (funcall pred input)
         (funcall rf result input)
         (ensure-reduced result))))
  ((pred coll)
   (lazy-seq
     (when-let (s (seq coll))
       (let ((item (first s)))
         (when (funcall pred item)
           (cons item (take-while pred (rest s)))))))))

(defn-ish take-nth
  "([n] [n coll])
Returns a lazy seq of every item in COLL whose ordinal sequence position is
such that (= 0 (REM I N)) for some position I.  This means the first item
is always returned.  

E.g. 
(take-nth 2 (range 6)) => (0 2 4)
(take-nth 3 (range 6)) => (0 3)

Returns a stateful transducer when no collection is provided.

For the [N COLL] arity, values of N <= 0 will result in an infinite sequence.
For the transducer, values of N <= 0 will signal an error.

See also MTAKE-NTH. There is no DROP-NTH ... because regularly occurring
values of a collection are only interesting, never uninteresting?"
  ((n)
   (when (<= n 0)
     (error "Values of N <= 0 are not valid for the TAKE-NTH transducer."))
   (stateful-transducer (rf result input ((i 0)))
     (prog1
         (if (= 0 (rem i n))
             (funcall rf result input)
             result)
       (incf i))))
  ((n coll)
   (lazy-seq
     (when-let (s (seq coll))
       (cons (first s) (take-nth n (drop n s)))))))

(defun mtake-nth (n coll)
  "An eager, CL:LIST returning version of TAKE-NTH.
N must be positive."
  (if (<= n 0)
      (error "Values of N <= 0 are not valid for MTAKE-NTH.")
      (loop for i from 0
            for s = (seq coll) then (next s)
            while s
            when (= 0 (rem i n))
              collect (first s))))

(defn-ish mdrop
  "([n coll] [result-type n coll])

An eager version of DROP that returns CL:LIST(default) or CL:VECTOR, and may share
structure if COLL is a compatible type.

You may optionally specify a RESULT-TYPE leading argument
to control the returned type.  It should be a symbol which is one of
CL:LIST, CL:VECTOR, or :BEST.  The first two specify the
type to be returned. 

If :BEST is given, MDROP will select for the
most efficient of the CL:LIST and CL:VECTOR types and should be
treated by the caller as returning a CL:SEQUENCE.

If N is zero, returns the whole collection with the indicated RESULT-TYPE.
The return value may be EQ the input value if result and collection types are compatible.

If N exceeds the number of elements in COLL, return NIL for CL:LIST RESULT-TYPE, and
an empty CL:VECTOR otherwise.

Do not pass an infinite sequence!

See also: DOSEQ (for polymorphic processing of CL:SEQUENCE types)."
  ((n coll) (mdrop 'cl:list n coll))
  ((result-type n coll)
   (check-type n integer)
   (when (< n 0)
     (setf n 0))
   (ecase result-type
     (:best
      (if (cl:vectorp coll)
          (mdrop 'cl:vector n coll)
          (if (cl:listp coll)
              (mdrop 'cl:list n coll)
              (if (counted? coll)
                  (mdrop 'cl:vector n coll)
                  ;; We could count here on an O(n) coll
                  ;; If we ask for cl:vector it will count _again_
                  ;; (unless we pass it in with special variable)
                  ;; but may be better to use for large collection
                  ;; For now we go with a list.
                  (mdrop 'cl:list n coll)))))
     (cl:list                           ;wants a list
      (typecase coll
        (cl:list (cl:nthcdr n coll))
        (cl:vector 
         (let ((size (- (cl:length coll) n)))
           (if (> size 0)
               (loop for j from n below (cl:length coll)
                     collect (aref coll j))
               nil)))
        (t                              ;suppose we could mconcat...
         (loop for i from 0
               for s = (seq coll) then (next s)
               while s 
               when (>= i n)
               collect (first s)))))
     (cl:vector                         ;wants a vector
      (typecase coll
        (cl:vector
         (let ((size (- (cl:length coll) n)))
           (if (> size 0)
               (if (= n 0)
                   coll
                   (make-array size :displaced-to coll :displaced-index-offset n))
               #())))
        (cl:list 
         (let ((size (- (cl:length coll) n)))
           (if (> size 0)
               (loop with result = (make-array size)
                     for i from 0
                     for val in (cl:nthcdr n coll)
                     do (setf (svref result i) val)
                     finally (return result))
               #())))
        (t
         ;; Could be smarter avoiding seqs on fset:seq here, perhaps another day
         (if-let (s (nthnext (seq coll) n))
           (mconcat 'cl:vector s)
           #())))))))


(defn-ish mdrop-while
  "([pred coll] [result-type pred coll])

An eager CL-collection-returning version of DROP-WHILE.

Returns items in COLL following the leading items for which (PRED item) returns true, 
returns all items if PRED always returns false.

Returns either a CL:LIST(default) or CL:VECTOR, depending on the
presence of the optional RESULT-TYPE argument, which must be one of 'CL:LIST
or 'CL:VECTOR.

If PRED matches all items, returns NIL for a CL:LIST result, or #() for a CL:VECTOR
result.

Results may share data with CL:LIST and CL:VECTOR COLLs, including
being EQ with coll if PRED filters no items.

Lazy seqs are fully realized, don't pass infinite sequences."
  ((pred coll) (mdrop-while 'cl:list pred coll))
  ((result-type pred coll)
   (let ((pred (alexandria:ensure-function pred)))
     (declare (function pred))
     (flet ((generic-mdrop ()
              (loop for s = (seq coll) then (next s)
                    while (and s (funcall pred (first s)))
                    finally (return (if s
                                        (mconcat result-type s)
                                        (if (eq result-type 'cl:list)
                                            nil
                                            #()))))))              
       (ecase result-type
         (cl:list
          (if (cl:listp coll)           ;we can share list structure
              (loop for cons on coll
                    while (funcall pred (car cons))
                    finally (return cons))
              (generic-mdrop)))
         (cl:vector
          (if (cl:vectorp coll)         ;we can share vector structure
              (loop with len = (cl:length coll)
                    for i from 0 below len
                    while (funcall pred (aref coll i))
                    finally
                       (return
                         (if (= i len)
                             #()
                             (if (= i 0)
                                 coll
                                 (make-array (- len i) 
                                             :displaced-to coll
                                             :displaced-index-offset i)))))
              (generic-mdrop))))))))

(defn-ish drop
  "([n] [n coll])
Returns a laziness-preserving sequence of all but the first n items in coll.
Returns a stateful transducer when no collection is provided.

The returned transducer is not safe for concurrent use by multiple threads."
  ((n)
   (transducer (rf result input)
     (if (plusp n)
         (progn (decf n)
                result)                 ;ignoring input, changing state
         (funcall rf result input))))
  ((n coll)
   (lazy-seq
     (loop for s = (seq coll) then (next s)
           for i from n above 0
           while s
           finally (return s)))))

(defn-ish drop-while
  "([pred] [pred coll])
Returns a lazy sequence of the items in coll following leading items
for which (pred item) returns true. Returns a
stateful transducer when no collection is provided.

The returned transducer is not safe for concurrent use by multiple threads."
  ((pred)
   (stateful-transducer (rf result input 
                         ((dropping? t)))
     (if dropping?
         (if (funcall pred input)
             result                               ;drop input
             (progn (setf dropping? nil)          ;no longer dropping
                    (funcall rf result input)))   ;include input
         (funcall rf result input))))
  ((pred coll)
   (lazy-seq
     (loop for s = (seq coll) then (next s)
           while (and s (funcall pred (first s)))
           finally (return (or s *EMPTY-LIST*))))))

(defun applycat (coll colls)
  "Helper for concat, takes one collection/seq, and possibly empty list of collections/seqs"
  (lazy-seq
    (if-let (s (seq coll))
      (cons (first s) (applycat (next s) colls)) ;iterating through first coll
      (when-let (coll (first colls))             ;ready for next coll
        (when-let (s (seq coll))
          (cons (first s) (applycat (next s) (rest colls))))))))

(defn-ish concat
  "([] [coll] [coll1 coll2] [coll1 coll2 & colls])
Return a lazy seq representing the concatenation of the elements in the supplied colls."
  (() 
   (lazy-seq nil))
  ((coll)
   (lazy-seq coll))
  ((coll1 coll2)
   (lazy-seq
     (if-let (s (seq coll1))
       (cons (first s) (concat (rest s) coll2))
       coll2)))
  ((coll1 coll2 & colls)
   (applycat (concat coll1 coll2) colls)))

(defun clj-apply (f &rest args) 
  "A substitute for CL:APPLY if your last argument is not a CL:LIST.
Converts the last argument, which must be some kind of CLJ-COLL supported collection,
to a CL:LIST if necessary, then invokes CL:APPLY.

Tip: MCONCAT will convert your non-cl:list collection into a cl:list.

Design decision: don't shadow CL:APPLY, we don't really want to encourage
use of this more expensive apply. Thus the name CLJ-APPLY.
The need arises only rarely because all CL functions already have CL:LIST
&rest lists.  Note that you can also use M functions
to give you a CL:LIST result in many cases."
  ;; Unfortunately, by this (CLJ-APPLY) indirection on apply
  ;; can't use the args list even if the last element was a cl:list.
  ;; 'args' becomes our new arglist and tail collection must be
  ;; flattened into 'args'.
  (let* ((lastcons (cl:last args))
         (lastarg  (first lastcons))
         (args (cl:butlast args)))
    (if (null lastarg)
        (cl:apply f args)
        (if (collp lastarg)
            (apply f (mconcat 'cl:list args (convert 'cl:list lastarg)))
            (error "Last argument ~s must be a collection." lastarg)))))

(defun mapcat (f &rest colls)
  "([f] [f & colls])

Applies CONCAT to the result of (MAP F COLLS).
Return the lazy result.

F should return a collection. 

Example:
    (flet ((add1 (&rest nums) (mapv #'1+ nums)))
      (mapcat #'add1 [1 2 3] [4 5 6]))
     => (2 5 3 6 4 7)

Returns a transducer when no collections are provided."
  (if colls
      (clj-apply #'concat (apply #'map f colls))
      ;; Remember comp is effectively executed left to right when
      ;; given transducers.  Too clever by half, credit to Clojure.
      (comp (map f) #'cat)))

(defun mmapcat (&rest args)
  "([f & colls]) [result-type f & colls])

An eager version of MAPCAT that returns a CL:LIST(default) or CL:VECTOR
as indicated by the optional RESULT-TYPE which must be one of those symbols.

If the result is an empty collection, returns NIL for CL:LIST RESULT-TYPE
or #() for CL:VECTOR result-type.

Applies MCONCAT to the result of (MMAP F COLLS).

F should return a collection. 

Example:
    (flet ((add1 (&rest nums) (mapv #'1+ nums)))
      (mmapcat #'add1 [1 2 3] [4 5 6]))
     => (2 5 3 6 4 7)

There is no transducer arity, use MAPCAT instead."
  (let* ((nargs (cl:length args))
         (first (cl:first args))
         (second (cl:second args))
         (result-type-p (and (>= nargs 2)
                             first (symbolp first)
                             second (or (symbolp second)
                                        (functionp second))))
         (result-type (if result-type-p first 'cl:list))
         (f (if result-type-p second first))
         (colls (if result-type-p (cddr args) (cdr args))))
    (if colls
        (apply #'mconcat result-type
               ;; Has to be lists for apply unless we get smarter about it
               (apply #'mmap 'cl:list f colls))
        (if (eq 'cl:list result-type)
            nil
            #()))))
  

(declaim (ftype (function (t t) (values cl:vector &optional)) copyseq-to-vector))
(defun copyseq-to-vector (type colls)
  "Helper routine for mconcat to copy colls to a vector type."
  (let* ((n (loop for coll in colls summing (count coll)))
         (result (ecase type
                   (cl:string (make-string n))
                   (cl:simple-vector (make-array n))
                   (cl:vector (make-array n :adjustable t :fill-pointer n))))
         (index 0))
    (declare (fixnum index))
    ;; We're not really taking advantage of optimizations on RESULT type
    ;; We _are_ avoiding arrayseq consing.
    (loop for coll in colls
          do (typecase coll
               (cl:vector 
                (loop for item across coll
                      do (setf (aref result index) item)
                         (incf index)))
               (cl:list
                (loop for item in coll
                      do (setf (aref result index) item)
                         (incf index)))
               (fset:seq
                (fset:do-seq (item coll)
                  (setf (aref result index) item)
                  (incf index)))
               (t 
                (loop for s = (seq coll) then (next s)
                      while s
                      do (setf (aref result index) (first s))
                         (incf index)))))
    result))

(declaim (ftype (function (t) (values cl:list &optional)) copyseq-to-list))
(defun copyseq-to-list (colls)
  "Helper routine for mconcat to copy colls to a list."
  (serapeum:with-collector (result)
    (loop for coll in colls
          do (typecase coll
               (cl:vector 
                (loop for item across coll
                      do (result item)))
               (cl:list
                (loop for item in coll
                      do (result item)))
               (t 
                (loop for s = (seq coll) then (next s)
                      while s
                      do (result (first s))))))))

(defun mconcat (&rest optional-result-type-and-colls)
  "(result-type? colls*) ; pattern syntax for illustration

Concatenate zero or more colls, with an optional RESULT-TYPE symbol as the first
argument. An eager alternative to CONCAT that produces only CL:LIST(default) or
CL:VECTOR results (while still being able to traverse all CLJ-COLL collections &
seqs).

If there are no elements in collections, returns NIL for CL:LIST results,
or an empty CL:VECTOR if CL:VECTOR was the requested return types.

This is a brute force concatenation, ALL collections are copied,
even if you only pass one an it's already of the desired result type.
Thus the result is safe to modify without destroying any inputs.

This function is similar to a CLJ-COLL-aware CL:CONCATENATE, though
there are some interpretations of RESULT-TYPE that differ, including no regard
for some possible subtypes of CL:LIST and CL:VECTOR.

The supported types and semantics here are:
- CL:LIST          - self evident
- CL:VECTOR        - a fill-pointered adjustable vector
- CL:SIMPLE-VECTOR - a simple vector (no fill-pointer, not adjustable,
                      elements of type T)
- CL:STRING        - a string as with MAKE-STRING, in which case colls
                     had better be strings or contain characters.

Unlike CL:CONCATENATE RESULT-TYPE is optional.
Unlike CONCAT, an optional RESULT-TYPE is permitted."
  (let* ((type (when-let (x (first optional-result-type-and-colls))
                 (and (symbolp x) x)))
         (colls (if type
                    (cl:rest optional-result-type-and-colls)
                    optional-result-type-and-colls))
         (type (or type 'cl:list)))
    (if (subtypep type 'cl:vector)
        (copyseq-to-vector type colls)
        (if (eq type 'cl:list)
            (copyseq-to-list colls)
            (error "Unsupported result type ~s" type)))))

(defn-ish range
  "([] [start] [start end] [start end step])
Returns a lazy seq of nums from START (inclusive) to END
(exclusive), by STEP, where START defaults to 0, STEP to 1, and END to
infinity. When STEP is equal to 0, returns an infinite sequence of
START. When START is equal to END, returns an empty list.

Clojure compatibility note: Common Lisp doesn't really have a representation
for numeric infinity. An end value of NIL will be treated as infinity
in whatever direction STEP moves START."
  ;; Note that CLojure's range could return clojure.lang.Repeat objects.
  ;; We have no intention of emulating such artifacts.
  (()
   (range 0 nil 1))
  ((end)
   (range 0 end 1))
  ((start end)
   (range start end 1))
  ((start end step)
   (flet ((done? ()
            (if end
                (if (< step 0)
                    (<= start end)
                    (>= start end))
                nil)))
     (lazy-seq 
       (if (done?)
           nil
           (cons start (range (+ start step) end step)))))))

(defun mrange (&rest args)
  "([result-type* start end] [result-type* start end step])

An eager version of RANGE returning a CL:LIST(default) or CL:VECTOR
of numbers from START (inclusive) to END (exclusive), 
by the optional STEP which defaults to 1.

An optional leading RESULT-TYPE argument may be one of the symbols 
CL:LIST or CL:VECTOR to specify the desired return type.

MRANGE does not permit obvious attempts at returning infinite
sequences. END is a required argument, and error is signalled if STEP is zero.

When START is equal to END, returns nil for CL:LIST return types,
and #() for CL:VECTOR return types."
  ;; I hated to mess up this arg list by adding optional leading result-type, and do
  ;; this only for consistency with other M functions, but we lose compile-time
  ;; checks and and there's no good valid-lambda-list expression of this that reads
  ;; well.
  (when (< (cl:length args) 2)
    (error "(mrange result-type* start end &optional step), missing args"))
  (let* ((result-type-p (and (cl:first args) (symbolp (cl:first args))))
         (result-type (if result-type-p (cl:first args) 'cl:list))
         (args (if result-type-p (cdr args) args))
         (start (cl:first args))
         (end (cl:second args))
         (step (or (cl:third args) 1)))
    (unless (member result-type '(cl:list cl:vector))
      (error "Invalid result-type specification: ~s, must be cl:list or cl:vector." 
             result-type))
    (when (zerop step)
      (error "Step must not be zero. For infinite sequences use RANGE."))
    (let ((result (make-collector result-type)))
      (if (< step 0)
          ;; Couldn't figure out how to loop BY a negative step
          ;; without a sbcl type warning.
          (do ((i start (+ i step)))
              ((<= i end))
            (collect result i))
          (loop for i from start below end by step
                do (collect result i)))
      (grab result))))

(defun mpartition (n &rest args)
  "([n coll & kwargs] [n step coll & kwargs] [n step pad coll & kwargs])

This is an eager version of PARTITION that returns a CL:LIST(default)
or CL:VECTOR with options for both the top level result type
and the partition-type.

Differences from PARTITION:
- This performs O(1n) iterations on the input, not O(4n) (assuming STEP = N).
- All returned collection types are CL collections.
- A NIL/empty collection results in NIL (not an empty lazy-seq).
- STEP and N must be >= 1 to avoid infinite sequences.
- NIL is a valid empty PAD collection.
- NIL is a valid empty COLL.
- COLL should not be an infinite sequence, or anything else that
  would blow out memory if all its elements were materialized.
- The type of result and the type of the partitions may be specified.

This function has the same argument signature as PARTITION except that
for any arity you may specify optional kwargs to control
the type of sequences returned.  The valid key/vey pairs are:

:result-type      CL:LIST|CL:VECTOR
:partition-type   CL:LIST|CL:VECTOR

The default for both collection types is CL:LIST.

See also: PARTITION, PARTITIONV, PARTITION-ALL, PARTITIONV-ALL"
  ;; Grand experiment, this was a real pain in the ass to write
  ;; compared to one using first/next/mtake/mdrop, but does pay dividends
  ;; if the input is an array or some type that 'seq'ing on would cons a lot.
  (let* ((step-p (integerp (cl:first args)))
         (step (if step-p (cl:first args) n))
         (args (if step-p (cdr args) args))
         (pad-p (and (>= (cl:length args) 2) ;for pad and coll
                     (not (keywordp (cl:first args)))
                     (not (keywordp (cl:second args)))))
         (pad (when pad-p (cl:first args)))
         (args (if pad-p (cdr args) args))
         ;; There must be a coll in PARTITION, no transdcuer arity
         (coll-p (and (>= (cl:length args) 1)
                      ;; colls, strings, nil all seqable, but strings aren't collp or coll?
                      (seqable? (cl:first args))))
         (coll (when coll-p (cl:first args)))
         (kwargs (if coll-p (cdr args) args))
         (result-type (cl:getf kwargs :result-type 'cl:list))
         (partition-type (cl:getf kwargs :partition-type 'cl:list)))
    (unless coll-p
      (error "Missing collection argument"))
    (when (<= step 0)
      (error "Step must be greater than zero, not ~s" step))
    (when (<= n 0)
      (error "N must be greater than zero, not ~s" n))
    (check-list-or-vector-result-type :result-type result-type)
    (check-list-or-vector-result-type :partition-type partition-type)
    (when (empty? coll)
      (return-from mpartition nil))
    ;; And away we go.  COLL is not empty. Must respect pad-p to know whether
    ;; NIL is an empty PAD collection or just a stub for a no-pad-supplied invocation

    ;; Minor *PERFORMANCE*: allow non-adjustable vectors with make-collector, use
    ;; svref on them. Requires we do the reallocations on growth-oriented APIs like
    ;; ASSOC/CONJ

    ;; Here we use iterators instead of first/next to avoid seq consing
    ;; Much tricker to write than using seqs and mtake/mdrop, particularly for the 
    ;; step<N case.

    ;; Although if COLL is a seq we haven't optimized anything.
    ;; All this work is only optimal if COLL is a _collection_ to be 
    ;; traversed by the iterator code (which optimizes for collection type).
    (let* ((result-collector (make-collector result-type :size n))
           (partition-collector (make-collector partition-type :size n))
           (eof #1=':eof)
           (colliter (iterator coll eof))
           (partition-item-count 0)
           ;; if skip < n, buffer values we'll need to re-read
           (step-delta (max (- n step) 0))
           (buffered (1+ step-delta))
           (step-read-count 0)
           ;; ringqueue holes exactly step-delta elements
           ;; But needs +1 because of how we read and trigger rereads
           (rq (when (plusp step-delta) (ring-queue buffered))))
      (declare (fixnum step-read-count partition-item-count step-delta buffered))
      (labels ((next-value () 
                 (if (plusp step-read-count)
                     (progn (decf step-read-count) (rq-get rq))
                     (funcall colliter))))
        (do ((item (next-value)))
            ((eq item eof))
          (if (< partition-item-count n)
              (progn (collect partition-collector item)
                     (incf partition-item-count)
                     ;; Each element in partition goes into ringqueue
                     ;; which is sized to discard elements we won't reuse for next partition
                     (when rq (rq-put rq item))
                     (setf item (next-value)))
              ;; Partition complete
              (progn (collect result-collector (grab-safe partition-collector))
                     (reset partition-collector)
                     (setf partition-item-count 0)
                     ;; Does this item begin next partition, or do we need
                     ;; to STEP forward/backward?
                     (cond ((= step n)) ;NOOP: re-execute loop with currently pending item
                           ((> step n)  ;discard item, step forward to next
                            (setf item (iterator-skip colliter (- step n) eof)))
                           (t ;step back, reread buffer, re-execute loop with reused item
                            (setf step-read-count buffered)
                            (rq-put rq item)
                            (setf item (next-value)))))))
        ;; collect final (possibly incomplete) partition
        (if (= partition-item-count n)
            (collect result-collector (grab partition-collector))
            (when (and (> partition-item-count 0) pad-p)
              (loop repeat (- n partition-item-count)
                    for s = (seq pad) then (next s)
                    while s
                    while (< partition-item-count n)
                    do (collect partition-collector (first s))
                       (incf partition-item-count)
                    finally 
                       ;; partial partition possible because we have pad
                       (if (>= partition-item-count 1)
                           (collect result-collector (grab partition-collector))))))
        (grab result-collector)))))

#+(OR)
;; This one really wasn't too bad, but it is 2-3x slower on consing arrayseqs
;; Anyway, just a comparison for my notes.
(defn-ish mpartition
  "([n coll] [n step coll] [n step pad coll])

Like PARTITION, only all partitions are eagerly computed, and the resulting top level
list and all partition subsequences are CL:LISTs.

A STEP <= 0 is will return only the first partition, unlike the infinite sequence
that would result from PARTITION.

N <= 0 will return NIL, unlike PARTITION which returns an infinite sequence."
  ;; Must use seqs instead of iterator because STEP values may require backtracking if
  ;; smaller than partition size N. To use iterators would require caching materialized
  ;; values and indexing them.
  ((n coll) (mpartition n n coll))
  ((n step coll)
   (when (<= n 0)
     (return-from mpartition nil))
   (if (<= step 0)
       (cl:list (mtake n coll))
       (loop for s = (seq coll) then (nthrest s step)
             as partition = (mtake n s)
             while (= n (cl:length partition))
             collect partition)))
  ((n step pad coll)
   (when (<= n 0)
     (return-from mpartition nil))
   (if (<= step 0)
       (cl:list (mtake n coll))
       (loop for s = coll then (nthnext s step)
             while s
             as partition = (mtake n s)
             if (= n (cl:length partition))
               collect partition
             else
               collect (mtake n (mconcat partition pad))))))

(defn-ish partition
  "([n coll] [n step coll] [n step pad coll])

Returns a lazy sequence of lists of N items each,
drawn from elements of COLL at OFFSETS items apart.

If STEP is not supplied, defaults to N, and the partitions
do not overlap. If a PAD collection is supplied, use its elements as
necessary to complete last partition upto n items. If there are 
not enough padding elements, return a partition with fewer than N items.

Note that if _no_ PAD is supplied, there is no case where partitions
with fewer than N are returned.

Coded for Clojure compatibility, with up to 4N seq traversals. Ouch.

See also: MPARTITION (1N), PARTITIONV (2N)."
  ;; Pretty much Clojure's algorithm to give you 
  ;; realized lazy-seq partitions for semantic compatibility.
  ;; with all the associated inefficiencies.
  ((n coll)
   (partition n n coll))
  ((n step coll)
   (lazy-seq
     (when-let (s (seq coll))
       ;; doall for clojure semantics, partitions should be realized
       (let ((partition (doall (take n s)))) ;kerching, 2N, and lazy to boot
         (when (= n (count partition))  ;kerching 3N
           (cons partition (partition n step (nthrest s step)))))))) ;kerching 4N
  ((n step pad coll)
   (lazy-seq
     (when-let (s (seq coll))
       (let ((partition (take n s)))
         (if (= n (count partition))
             (cons partition (partition n step pad (nthrest s step)))
             (list (take n (concat partition pad)))))))))

(defn-ish partitionv
  "([n coll] [n step coll] [n step pad coll])

Basically the same as PARTITION except that the partitions are vectors
instead of realized lazy sequences.

Returns a lazy sequence of vectors of n items each, at offsets step
apart. If step is not supplied, defaults to n, i.e. the partitions
do not overlap. If a pad collection is supplied, use its elements as
necessary to complete last partition upto n items. In case there are
not enough padding elements, return a partition with less than n items.

See also: MPARTITION."
  ((n coll)
   (partitionv n n coll))
  ((n step coll)
   (lazy-seq
     (when-let (s (seq coll))
       (let ((partition (into (vector) (take n) s)))
         (when (= n (count partition))
           (cons partition (partitionv n step (nthrest s step))))))))
  ((n step pad coll)
   (lazy-seq
     (when-let (s (seq coll))
       (let ((partition (into (vector) (take n) s)))
         (if (= n (count partition))
             (cons partition (partitionv n step pad (nthrest s step)))
             (list (into (vector) (take n) (concat partition pad)))))))))


(defn-ish partitionv-all
  "([n] [n coll] [n step coll])

This is the same as PARTITION-ALL except that the partitions are vectors instead
of realized lazy sequences.

See also: PARTITION-ALL, MPARTITION-ALL."
  ;; Curious if the Clojure's use of 'coll' arg to 'into' was deliberate, or if 
  ;; it's meant to afford into some optimization of traversing a
  ;; collection vs a seq. Only ... it would only be a coll on the first
  ;; lazyseq node, the other ones are a seq after the call to drop.
  ;; I think someone was just pretending not to copy partition-all.
  ;; Meanwhile, this code differs a tiny bit from Clojure's.
  ((n)
   (partition-all n))
  ((n coll)
   (partitionv-all n n coll))
  ((n step coll)
   (lazy-seq
     (when-let (s (seq coll))
       (let ((partition (into (vector) (take n) s)))
         (cons partition (partitionv-all n step (nthrest s step))))))))

(defn-ish partition-all
  "([n] [n coll] [n step coll])
Returns a lazy sequence of partitions (as realized lazy sequences) like partition,
but may include partitions with fewer than n items at the end.

--- PERFORMANCE NOTE ---

PARTITION-ALL is an unfortunately implemented function with O(4n) behavior.
We have implemented its semantics to be compatible with Clojure's,
but that brings along some of the performance baggage.

Apparently the use of realized lazy-sequences as partitions is an
undocumented and expected part of the PARTITION-ALL behavior (as casually mentioned
here https://insideclojure.org/2022/06/15/partitioning/).  The link suggests
mitigating alternatives, as well as new PARTITIONV, PARTITIONV-ALL, and SPLITV-AT
functions in Clojure 1.12.

---

Returns a stateful transducer when no collection is provided, the transducer
is not thread-safe. The transducer produces persistent vectors for each partition.

See also: PARTITION, PARTITION-BY, MPARTITION-ALL,
PARTITIONV, and PARTITIONV-ALL."
  ;; On the use of unreduced, Clojure protocol for 'completing' arities
  ;; of the xform don't return reduced values. See this link for discussion:
  ;; https://clojuredocs.org/clojure.core/unreduced
  ((n)
   (when (< n 0)
     (error "N must be >= 0, not ~s" n)) ; for clojure compatibility
   (stateful-transducer
       (rf result input ((accum (vector)))
        :completion-body                ;[result]
        (let ((result
                (if (empty? accum)
                    result
                    ;; May add additional rows, unreduced required
                    (unreduced (funcall rf result accum)))))
          (setf accum (vector))
          (funcall rf result)))         ;propagate completion step
     ;; [result input]
     (setf accum (conj accum input))
     (if (= n (count accum))
         (prog1 (funcall rf result accum)
           (setf accum (vector)))
         result)))
  ((n coll)
   (partition-all n n coll))
  ((n step coll)
   (lazy-seq
     (when-let (s (seq coll))
       (let ((partition (doall (take n s))))
         (cons partition (partition-all n step (nthrest s step))))))))

(defun check-list-or-vector-result-type (name x)
  "Signal an error if X is not 'cl:list or 'cl:vector, with name to label X in some way"
  (unless (or (eq x 'cl:list)
              (eq x 'cl:vector))
    (error "~a should be one of 'cl:list or 'cl:vector, not ~s" name x)))

(defun partition-all-aux-coll (n step coll 
                               &key (result-type 'cl:list) (partition-type 'cl:list))
  "Does the work of PARTITION-ALL when a non-nil, non-empty coll is supplied."
  (check-list-or-vector-result-type :result-type result-type)
  (check-list-or-vector-result-type :partition-type partition-type)
  ;; *PERFORMANCE* allow non-adjustable vectors with make-collector, use svref on them
  ;; requires we do the reallocations on growth-oriented APIs like ASSOC/CONJ
  (loop with result-collector = (make-collector result-type :size n)
        with partition-collector = (make-collector partition-type :size n)
        with s = (seq coll)
        while s                         ;tracks STEP intervals
        do (loop for s2 = s then (next s2)
                 while s2
                 repeat n ;this must be after the for-while test or s isn't incremented on n=1
                 do (collect partition-collector (first s2))
                 finally (progn (collect result-collector (grab-safe partition-collector))
                                (reset partition-collector)
                                ;; Avoid some or all retraversals with `next` if we can
                                (if (<= step n)
                                    (if (< step n)
                                        (setf s (nthnext s step))
                                        (setf s s2)) ;n==step
                                    (setf s (nthnext s2 (- step n)))))) ;step>n
        finally (return (grab result-collector))))

(defun partition-all-aux-xf (n &key (partition-type 'cl:list) result-type)
  "Transducer creation for MPARTITION-ALL.
While mutable transducers are normally not needed to get mutable
output of clojure transducers, the partition functions with their secondary
collection allocation don't offer that control.

Result-type is ignored for transducers, the call to transduce
drives the result type.

Though, TODO, maybe, we could offer kwargs to control it on PARTITION-ALL
like we do for MPARTITION-ALL if we had some time to spare."
  (declare (ignore result-type))
  (check-list-or-vector-result-type :partition-type partition-type)
  (stateful-transducer 
      (rf result input ((accum (make-collector partition-type :size n))
                        (n-accum 0))
       :init-body 
       (progn (reset accum) (setf n-accum 0) (funcall rf))
       :completion-body                 ;[result]
       (let ((result 
               (if (zerop n-accum)
                   result
                   ;; If additional rows are added, unreduced is required
                   (unreduced (funcall rf result (grab-safe accum))))))
         (reset accum)
         (setf n-accum 0)
         (funcall rf result)))          ;propagate completion step
    ;; [result input]
    (collect accum input) (incf n-accum)
    (if (= n n-accum)
        (prog1 (funcall rf result (grab-safe accum))
          (reset accum) (setf n-accum 0))
        result)))

(defun mpartition-all (n &rest args)
  "([n & kwargs] [n coll & kwargs] [n step coll & kwargs])

This is an eager version of PARTITION-ALL that returns a CL:SEQUENCE subtype
and has optional additional collection-type behaviors
(if you'll forgive the liberties taken with the invalid Clojure variadic expressions above).

Differences from PARTITION-ALL:
- This performs O(1n) iterations on the input, not O(4n) (assuming STEP = N).
- All returned collection types are CL collections.
- A NIL/empty collection results in NIL (not an empty lazy-seq).
- STEP and N must be >= 1 to avoid infinite sequences.
- COLL should not be an infinite sequence, or anything else that
  would blow out memory if all its elements were materialized.
- The type of result and the type of the partitions may be specified.

This function has the same argument signature as PARTITION-ALL except that
for any arity you can specify optional kwargs to control
the type of sequences returned.  The valid key/vey pairs are:

:result-type      CL:LIST|CL:VECTOR     ;(ignored for transducer arities)
:partition-type   CL:LIST|CL:VECTOR

The default for both collection types is CL:LIST.

See also: PARTITION-ALL"
  (let* ((step-p (integerp (cl:first args)))
         (step (if step-p (cl:first args) n))
         (args (if step-p (cdr args) args))
         (coll-p (and (>= (cl:length args) 1)
                      (seqable? (cl:first args)))) ;colls, strings, nil all seqable, but strings aren't collp or coll?
         (coll (when coll-p (cl:first args)))
         (kwargs (if coll-p (cdr args) args)))
    ;;(format t "~%step-p ~s step ~s coll-p ~s coll ~s kwargs ~s~%" step-p step coll-p coll kwargs)
    (when (<= step 0)
      (error "Step must be greater than zero, not ~s" step))
    (when (<= n 0)
      (error "N must be greater than zero, not ~s" n))
    (if coll-p
        (if (empty? coll)               ;incl nil
            nil
            (apply #'partition-all-aux-coll n step coll kwargs))
        (apply #'partition-all-aux-xf n kwargs))))
         
(defun mpartition-by (f coll)
  "A flavor of PARTITION-BY for mutable Common Lisp collections as a 
CL:LIST of CL:LISTs. 

Hash table content is treated as 2-element MapEntry representations.

Partition content appears in the same order as elements of COLL."
  ;; *TBD* / *TODO* - allow result-type and partition-type
  ;; keywords like MPARTITION-ALL.
  (loop with no-more-vals = #1=':no-more-vals
        with f function = (alexandria:ensure-function f)
        with iterator function = (iterator coll no-more-vals)
        with result-collector = (make-collector 'cl:list) ;all partition results
        with partition-collector = (make-collector 'cl:list) ;one partition in progress
        with current-f-key = no-more-vals
        as item = (funcall iterator)
        until (eq item no-more-vals)
        as  f-key = (funcall f item)
        do (cond ((eq current-f-key no-more-vals)   ;first partition
                  (collect partition-collector item)
                  (setf current-f-key f-key))
                 ((equal? f-key current-f-key)    ;continue current partition
                  (collect partition-collector item))
                 (t                                   ;new partition starting
                  (collect result-collector (grab partition-collector)) ;completed partition
                  (reset partition-collector)
                  (collect partition-collector item) ;new partition first element
                  (setf current-f-key f-key)))
         finally 
            (when-let (last-partition (grab partition-collector))
              (collect result-collector last-partition))
            (return (grab result-collector))))

(defn-ish partition-by
  "([f] [f coll])
Applies F to each value in COLL, splitting it each time F returns a
new value (i.e. not EQUAL?).  Returns a lazy seq of partitions.

Returns a stateful transducer (which is not thread-safe) when no collection is provided.

The Clojure PARTITION-BY API gives you no control over the partition collection types.
MPARTITION-BY will let you create CL lists or vectors for partitions.

See also: MPARTITION-BY.

Note: the predicate F will be called twice on any object beginning a new partition."
  ((f)
   (stateful-transducer 
       (rf result input 
        ((accum (vector))
         (no-key #1=':no-key)
         (key #1#))
        :completion-body
        (funcall rf                     ;completion step of xform
                 (if (empty? accum)
                     result
                     ;; see 'unreduced' clojure-docs.org
                     (prog1 (unreduced (funcall rf result accum))
                       (setf accum (vector) key no-key)))))
     ;; [result input]
     (let ((k (funcall f input)))
       (cond ((eq no-key key)           ;start of first partition
              (setf key k accum (conj accum input))
              result)
             ((equal? key k)            ;continue existing partition
              (setf accum (conj accum input))
              result)
             ;; New partition
             (t 
              (let ((result (funcall rf result accum)))
                (setf key k
                      accum (if (reduced? result)
                                (vector)
                                (vector input)))
                result))))))
   ((f coll)
    ;; Deliberately stupid to match CLojure's partitions of type lazy-sequence
    ;; Originally written to return non-lazy partitions and avoid re-traversals
    ;; with take-while/drop pair.
    (lazy-seq
      (when-let (s (seq coll))
        (let* ((val (first s))
               (key (funcall f val))
               (partition (cons val (take-while (lambda (item) 
                                                  (equal? key (funcall f item)))
                                                (next s)))))
          (cons partition (partition-by f (lazy-seq (drop (count partition) s)))))))))


(defn-ish distinct 
  "([] [coll])
Returns a lazy sequence of the elements of coll with duplicates removed.
Returns a stateful transducer when no collection is provided.

Clojure incompatibility: the set membership test is FSET:EQUAL?, which is
not as encompassing as CLJ-COLL:EQUAL?

See also: DEDUPE, and MDISTINCT which will apply EQUAL? vs FSET:EQUAL? semantics."
  (()
   (stateful-transducer
       (rf result input ((set (hash-set))))
     (if (contains? set input)
         result
         (progn (setf set (conj set input))
                (funcall rf result input)))))
  ((coll)
   (labels ((distinct (coll set)
              (lazy-seq
                (multiple-value-bind (s f)
                    (loop for s = (seq coll) then (next s)
                          as  f = (first s)
                          while (and s (contains? set f))
                          finally (return (values s f)))
                  (when s
                      (cons f (distinct (rest s) (conj set f))))))))
     (distinct coll (hash-set)))))

(defun mdistinct (coll-or-result-type &optional (coll nil collp))
  "Return an eager cl:sequence of elements of COLL with
duplicates (as per EQUAL?) removed.

Caution: this is an O(n^2) algorithm until such time as EQUAL? semantics are supported
by sets/maps and hash-tables (which makes `mdistinct` unnecessary).
Right now we do not have a hash set that supports EQUAL?.
Note that if there are two+ distinct values for EQUAL? keys of a set/map input
the value selected is not specified.

COLL is not modified, but the result may share structure with (or be EQ to) COLL.

The returned type is either a CL:VECTOR or CL:LIST(default), either symbol may be
given for the optional result type.

If COLL is empty, NIL is returned or a CL:LIST result will be NIL, and a CL:VECTOR
result will be an empty vector.

Note that if COLL is `counted?`, specifying a CL:VECTOR return type is more
efficient.

Works on string characters without a seq, as clojure's `(distinct <string>)` also does.

The order of returned elements (for `sequential?` inputs) is the order
in which they appear in coll, however note that when duplicates are removed the
position of the item with duplicates will be that of the last value, not the first
(which, btw, is confusing in the CL:REMOVE-DUPLICATES spec and counterintuitive)
I.e. (distinct '(nil 1 2 3 1 nil)) => (2 3 1 nil)

See also: CL:REMOVE-DUPLICATES, CL:DELETE-DUPLICATES"
  (let ((result-type (if collp coll-or-result-type 'cl:list))
        (coll (if collp coll coll-or-result-type)))

    ;; Check result type, return appropriate empty coll values
    (ecase result-type
      (cl:list 
       (when (empty? coll)
         (return-from mdistinct nil)))
      (cl:vector
       (when (empty? coll)
         (return-from mdistinct #()))))

    (typecase coll
      ((or cl:list cl:vector)
       (if (typep coll result-type)
           (cl:remove-duplicates coll :test #'equal?)
           (cl:remove-duplicates (coerce coll result-type) :test #'equal?)))
      ((or fset:map cl:hash-table)
       (cl:delete-duplicates (convert result-type coll) :test #'equal? :key #'first))
      (t 
       (if (typep coll result-type)  ;convert returns coll if no conversion necessary
           (cl:remove-duplicates coll :test #'equal?)
           (cl:delete-duplicates (convert result-type coll) :test #'equal?))))))

(defun map-lazy (f seqs)
  "Helper routine to do lazy map on collections."
  (declare (function f) (cl:list seqs))
  (lazy-seq 
    (if (some #'null seqs)
        nil                             ;reached end of some collection
        (cons (apply f (cl:mapcar #'first seqs))
              (map-lazy f (cl:mapcar #'next seqs))))))

(defun map (f &rest colls)
  "Returns a lazy sequence consisting of the result of applying f to
the set of first items of each coll, followed by applying f to the
set of second items in each coll, and so on until any one of the colls is
exhausted. Any remaining items in other colls are ignored.

F should accept number-of-colls arguments. 

Returns a transducer when no collection is provided.

TIP: `map` is the only Clojure transducer supporting a fourth arity and you might
wonder how to invoke it given that `transduce`, `into`, and `eduction`
will only invoke the transducer with a single input.  The answer is
the `sequence` function. E.g.
    (sequence (comp (map #'+) (filter #'even?)) (range 5) (range 5 10) (range 10 15)
    => (18 24)

Tip courtesy of this guy who figured it out:
https://gist.github.com/pjstadig/c27366b1cba7a820d07ffdc957fc86c0"
  (if colls
      ;; No transducer, process collections
      (map-lazy (alexandria:ensure-function f)
                (cl:mapcar #'seq colls))
      ;; Transducer desired
      (lambda (rf)
       (lambda (&rest args)
         (case (cl:length args)
           (0 (funcall rf))
           (1 (funcall rf (cl:first args)))
           (2 (let ((result (cl:first args))
                    (input  (cl:second args)))
                (funcall rf result (funcall f input))))
           (t (let ((result (cl:first args))
                    (input  (cl:second args))
                    (inputs (cl:cddr args)))
                (funcall rf result (apply f input inputs)))))))))

(defun mmap (&rest args)
  "([f & colls] [result-type f &colls])

An eager version of CLJ-COLL:MAP that produces either CL:LIST(default) or CL:VECTOR
results, as specified by the optional RESULT-TYPE argument which should name one of 
those symbols. 

Does not provide a transducer. You could approximate MMAP using the MAP transducer
but it would not be as efficient.

Overlaps a bit with:
- CL:MAPCAR but accepts all CL and CLJ-COLL collection and seqs.
- CLJ-COLL::MAPV except we return a mutable list, not an immutable vector.
- CLJ-COLL::CL-MAP, however MMAP is optimized for CL:LIST/CL:VECTOR return types
  and single-collection traversal.  It also exists for consistency/continuity of
  M functions on sequence returning Clojure APIs.

If you're just using CL:SEQUENCE inputs, consider CL:MAP or CL:MAPCAR for more efficiency."
  ;; Beware NIL as a COLL, not to be confused with RESULT-TYPE
  (let* ((result-type-p (and (>= (cl:length args) 2)
                             (symbolp (cl:first args))
                             (or (symbolp (cl:second args))
                                 (functionp (cl:second args)))))
         (result-type (if result-type-p (cl:first args) 'cl:list))
         (args (if result-type-p (cdr args) args))
         (f-p (or (symbolp (cl:first args))
                  (functionp (cl:first args))))
         (f (if f-p 
                (alexandria:ensure-function (cl:first args))
                (error "Missing function designator in MMAP args: ~s" args)))
         (colls (cdr args))
         (eof '#:eof))
    (ecase result-type
      (cl:list
       (case (cl:length colls)
         (0 nil)                        ;no colls to map
         (1                             ;one coll to map
          (loop with iterator = (iterator (first colls) eof)
                as val = (funcall iterator)
                until (eq val eof)
                collect (funcall f val)))
         (t                             ;N>1 colls to map
          (loop with iterators = (cl:mapcar (lambda (coll) (iterator coll eof)) colls)
                with done = nil
                ;; Inline instead of mapcar to avoid additional level of function calls.
                ;; and to check for early termination if there are mismatched sized lists
                as vals = (loop for iterator function in iterators 
                                as val = (funcall iterator)
                                when (eq val eof)
                                  do (setf done t)
                                collect val)
                until done
                collect (apply f vals)))))
      (cl:vector
       (case (cl:length colls)
         (0 #())                        ;no colls to map
         (1                             ;one coll to map
          (loop with coll = (first colls)
                with iterator = (iterator (first colls) eof)
                with size = (count coll)
                with result = (make-array size)
                for i from 0 
                as val = (funcall iterator)
                until (eq val eof)
                do (setf (svref result i) (funcall f val))
                finally (return result)))
         (t                             ;N>1 colls to map
          (loop with iterators = (cl:mapcar (lambda (coll) (iterator coll eof)) colls)
                with done = nil
                with size = (loop for coll in colls minimize (count coll))
                with result = (make-array size)
                for i from 0 
                as vals = (loop for iterator function in iterators 
                                as val = (funcall iterator)
                                when (eq val eof)
                                  do (setf done t)
                                collect val)
                until done
                do (setf (svref result i) (apply f vals))
                finally (return result))))))))

(defn-ish sequence-rf
  "A reducing function for transforms which puts
items into a (mutable) queue

This is necessary as some transforms may inject
multiple values, or no values for a given sequence item."
  ((queue) queue) ; RF completion arity - required for transducer use
  ((queue item)   ; one input
   (locally (declare (serapeum:queue queue))
     (serapeum:enq item queue)
     queue))
  ((queue item & items) ;N inputs
   (locally (declare (serapeum:queue queue))
     (serapeum:enq item queue)
     (loop for item in items
           do (serapeum:enq item queue))
     queue)))

(defun sequence-xform-completion (xform queue)
  "Handle the completion step on the transform for SEQUENCE drivers.

This should be called ONCE AND ONLY ONCE for any SEQUENCE xform processing.
It may generate one or more values into QUEUE based on the xform completion step.

Returns T if the completion step produced values, NIL if it did not."
  (funcall xform queue)
  (not (serapeum:queue-empty-p queue)))

(defun sequence-xform-1 (xform queue seq completed?)
  "Helper for SEQUENCE, returns a lazy seq of applications of xform to
elements of COLL.

If COMPLETED? has true, it means we've already invoked the completion
step on the transform and anything in the queue was produced by the completion step
(and we don't want to invoke the completion step a second time)."
  (declare (serapeum:queue queue) (function xform))
  (lazy-seq 
    ;; If we have buffered values, return one
    ;; Otherwise Process seq elements until we get at least one 
    ;; element in our queue to return as the lazy seq value
    (loop while (and seq (serapeum:queue-empty-p queue))
          as step-result = (funcall xform queue (first seq))
          do (if (reduced? step-result)
                 (setf seq nil)
                 (setf seq (next seq))))
    (if (serapeum:queue-empty-p queue)
        (if completed?
            nil
            ;; Invoke xform completeion step
            (if (sequence-xform-completion xform queue)
                ;; Produced values
                (cons (serapeum:deq queue)
                      (sequence-xform-1 xform queue seq :completed))
                ;; No additional completion step values, we're done
                nil))
        (cons (serapeum:deq queue)
              (sequence-xform-1 xform queue seq completed?)))))

(defun sequence-xform-n (xform queue seqs completed?)
  "Helper for SEQUENCE, returns a lazy seq of applications of xform to
elements of SEQS, a list of one or more seqs. This is one of the rare
places an xform is called with more than one input argument.

If COMPLETED? has true, it means we've already invoked the completion
step on the transform and anything in the queue was produced by the completion step
(and we don't want to invoke the completion step a second time)."
  (declare (serapeum:queue queue) (function xform) (cl:list seqs))
  ;; Performance: Could reuse seqs list, and inline or shortcircuit EVERY? logic
  ;; when maintaining seqs list.
  (lazy-seq 
    (loop while (and (every? #'identity seqs)
                     (serapeum:queue-empty-p queue))
          as step-result = (apply xform queue (cl:mapcar #'first seqs))
          do (if (reduced? step-result)
                 (setf seqs (cl:mapcar (constantly nil) seqs)) ;done pulling values from seqs
                 (setf seqs (cl:mapcar #'next seqs))))
    (if (serapeum:queue-empty-p queue)
        (if completed?
            nil
            ;; Invoke xform completion step
            (if (sequence-xform-completion xform queue)
                ;; Produced values
                (cons (serapeum:deq queue)
                      (sequence-xform-n xform queue seqs :completed))
                ;; No completion step values, we're done
                nil))
        (cons (serapeum:deq queue)
              (sequence-xform-n xform queue seqs completed?)))))

(defn-ish sequence
  "([coll] [xform coll] [xform coll & colls])

In arity-1 with just a collection, coerces COLL to be a (possibly empty) non-lazy
sequence or returns COLL if it is already a sequence.

All other arities require a tranform which will be applied like MAP to the first
element of each collection, then the next element of each collection, and so on until
a collection is exhausted, returning a lazy sequences of transformed elements.

Remaining items in other collections are ignored if the collections are not the same
length.

The transform should accept number-of-colls arguments. Note that as of Clojure 1.11
the only native transform that handles more than one input is MAP.

Clojure notes; This interface is weird for two reasons, it may as well be
named 'up-is-down-and-down-is-up' (smells like DWIM!):

1) Unlike many Clojure APIs that return 'seq' and mean lazy seq, this API returns a
specifically non-lazy seq for arity-one unless the collection is already a lazy seq.
2. Unlike TRANSDUCE applied transforms which are always eager, SEQUENCE turns those
transforms into lazy sequences.

See also MAP, the only Clojure native transducer which has an an xform with arity > 2
(accepting multiple inputs).

Note that there is no MSEQUENCE function as is often the case with seq returning
functions, the point of M functions is kind of opposite the whole reason 
for the existence of the SEQUENCE function.  However it may be that using
CL-CONJ as reducing function will scratch that itch if you have it."
  ;; The implementation trick to realize with `sequence` for transforms
  ;; is that a transform can produce zero to many values for a given seq element.
  ;; So we give it a reducer that will buffer the results, and our
  ;; then apply the xform in a little dance checking both seq and buffer status.
  ;; In clojure this is done with a TransformerIterator.
  ;; In our case we use a simple list based queue.
  ;;
  ;; *PERFORMANCE*: could use an array for a buffer instead of a Serapeum:Queue
  ;; and likely cons only once for many uses of SEQUENCE, vs one cons per queue
  ;; element.  Doesn't seem worth worrying about.
  ((coll)
   (if (seq? coll) 
       coll 
       (or (seq coll) *EMPTY-LIST*)))
  ((xform coll) 
   (sequence-xform-1 (funcall xform #'sequence-rf) (serapeum:queue) (seq coll) nil))
  ((xform coll & colls)
   (sequence-xform-n (funcall xform #'sequence-rf) (serapeum:queue) 
                     (cl:mapcar #'seq (cl:list* coll colls)) nil)))

(defn-ish repeatedly
  "([f] [n f])
Takes a function of no args, presumably with side effects, and
returns an infinite (or length n if supplied) lazy sequence of calls
to it.

See also REPEAT which repeats a value, whereas REPEATEDLY calculates
a potentially new value each time.

See also MREPEATEDLY for an eager CL:LIST generating version."
  ((f) 
   (lazy-seq
     (cons (funcall f) (repeatedly f))))
  ((n f)
   (lazy-seq
     (when (> n 0)
       (cons (funcall f) (repeatedly (1- n) f))))))

(defun mrepeatedly (n f)
  "Eagerly creates a CL:LIST of N values returned by N calls to F.
N must be >= 0.

See also: REPEATEDLY for the lazy version.
See REPEAT/MREPEAT for a repeated constant value."
  (let ((f (alexandria:ensure-function f)))
    (declare (function f))
    (loop repeat n collect (funcall f))))

(defn-ish repeat
  "([x] [n x])
Returns a lazy sequence of length N repetitions of X.
If N is omitted, the resulting sequence is infinite.

See also REPEATEDLY which makes repeated calls to a function, whereas REPEAT 
simply reproduces the same value repeatedly.

See also MREPEAT for an eager CL:LIST generating version."
  ((x) 
   (lazy-seq
     (cons x (repeat x))))
  ((n x)
   (lazy-seq
     (when (> n 0)
       (cons x (repeat (1- n) x))))))

(defun mrepeat (n x)
  "Eagerly creates a CL:LIST of N instances of X.  N must be >= 0.

See also: REPEAT for a lazy sequence version allowing infinite sequences.
See also: REPEATEDLY, MREPEATEDLY."
  (loop repeat n collect x))

(defun iterate (f x)
  "([f x])
Returns an INFINITE lazy sequence of x, (f x), (f (f x)) etc.
F must be free of side-effects.

See also: REPEATEDLY, REPEAT, CYCLE"
  (lazy-seq
    (cons x (iterate f (funcall f x)))))

(defun line-seq (stream)
  "Returns lines of text from stream as a lazy sequence of strings.
Closing the stream is the responsibility of the caller.

Lines are read via the READ-LINE-CRLF package, which is like READ-LINE
except that it splits lines based on a cross-platform interpretation of line
terminators that includes CR, CRLF, LF.

Example:
(with-open-file (stream \"~/.bashrc\" :direction :input)
  (doseq (line (take 5 (line-seq stream)))
    (print line)))

See also: SLURP
"
  (let ((buffer (make-string 1024))
        (eof #1=':eof))
    (labels ((line-seq-aux (stream)
               (lazy-seq
                 (let ((l (read-line-crlf:read-line-crlf stream nil eof nil buffer)))
                   (if (eq eof l)
                       nil
                       (cons l (line-seq-aux stream)))))))
      (line-seq-aux stream))))

(defn-ish keep 
  "([f] [f coll])
Returns a lazy sequence of the non-nil results of calling (F item). F must be free of
side-effects.  Returns a transducer when no collection is provided.

See also: FILTER, FILTERV, MKEEPP"
  ((f)
   (transducer (rf result input)
     (if-let (val (funcall f input))
       (funcall rf result val)
       result)))
  ((f coll)
   (labels ((keep-aux (f seq)
              (lazy-seq 
                (loop for s = seq then (next s)
                      while s
                      as val = (funcall f (first s))
                      until val
                      finally
                         (when val 
                           (return (cons val (keep-aux f (next s)))))))))
     (keep-aux f (seq coll)))))

(defun mkeep (f coll)
  "An eager version of KEEP returning a CL:LIST of non-nil results
of calling (F item) on COLL."
  (let ((f (alexandria:ensure-function f)))
    (serapeum:with-collector (result)
      (run! (lambda (item)              ;because we know run! optimizes seq/coll traversal
              (if-let (val (funcall f item))
                (result val)))
            coll))))

(defn-ish keep-indexed
  "([f] [f coll])
Returns a lazy sequence which yields the non-nil results of calling (f <index> item)
for each item in coll.  The <index> argument is the ordinal offset into the sequential
collection, and starts with zero.  F must take two arguments.

Coll can be any type of seq or (clj-coll supported) collection.

Example:
(keep-indexed (lambda (index item)
                (when (pos? item)
                  index))
  [-3 29 -7 45 3 -8])
=>
(1 3 4)

F must be free of side-effects. Returns a stateful transducer when no collection is
provided, the transducer is not thread-safe.

See also MKEEP-INDEXED."
  ((f)
   (stateful-transducer (rf result input ((index 0)))
     (prog1
         (if-let (val (funcall f index input))
           (funcall rf result val)
           result)
       (incf index))))
  ((f coll)
   (labels ((keep-indexed-aux (f index seq)
              (lazy-seq 
                (loop for s = seq then (next s)
                      for i from index
                      while s
                      as val = (funcall f i (first s))
                      until val
                      finally
                         (when val 
                           (return (cons val (keep-indexed-aux f (1+ i) (next s)))))))))
     (keep-indexed-aux f 0 (seq coll)))))

(defun mkeep-indexed (f coll)
  "An eager version of keep-index that returns a cl:list."
  (loop with f = (alexandria:ensure-function f)
        for s = (seq coll) then (next s)
        for i from 0
        while s
        as val = (funcall f i (first s))
        when val
          collect val))

(defn-ish map-indexed
  "([f] [f coll])
Returns a lazy sequence consisting of the result of applying F to 0
and the first item of COLL, followed by applying F to 1 and the second
item in COLL, and so on until COLL is exhausted. Thus function F should
accept 2 arguments, INDEX and ITEM. 

Returns a stateful transducer when no collection is provided."
  ((f)
   (stateful-transducer (rf result input ((index -1)))
     (funcall rf result (funcall f (incf index) input))))
  ((f coll)
   (labels ((map-indexed-aux (f index coll)
              (lazy-seq 
                (when-let (s (seq coll))
                  (cons (funcall f index (first s))
                        (map-indexed-aux f (1+ index) (next s)))))))
     (map-indexed-aux f 0 coll))))

(defn-ish remove
  "([pred] [pred coll])
Returns a lazy sequence of the items in COLL for which
(PRED item) returns NIL. 

Phrased another way: (REMOVE-IF PRED COLL).

PRED must be free of side-effects.
Returns a transducer when no collection is provided.

See also: FILTER, MREMOVE, CL:REMOVE-IF"
  ((pred)
   (transducer (rf result input)
     (if (funcall pred input)
         result
         (funcall rf result input))))
  ((pred coll)
   (labels ((remove-aux (pred seq)
              (declare (function pred))
              (lazy-seq
                (loop for s = seq then (next s)
                      while s
                      as item = (first s)
                      as discard? = (funcall pred item)
                      while discard?
                      finally
                         (when s
                           (return (cons item (remove-aux pred (next s)))))))))
     (remove-aux (alexandria:ensure-function pred) (seq coll)))))

(defun mremove (pred coll)
  "([pred coll])

An eager version of REMOVE returning a CL:LIST."
  (loop for s = (seq coll) then (next s)
        while s
        as item = (first s)
        unless (funcall pred item)
          collect item))

(defun dedupe (&optional (coll nil coll-p))
  "([] [coll])
Returns a lazy sequence removing consecutive duplicates (as determined by EQUAL?)
in coll.

Returns a stateful transducer when no collection is provided.

See also: DISTINCT"
  ;; There's some DEDUPE/MDEDUPE performance tests in ./dedupe-notes.txt
  (if coll-p
      ;; This SEQUENCE call is how CLojure does it.  It's much more elegant and nice
      ;; to not code two different ways for the same API (one with xform, one
      ;; without).  But be aware that there's there's some added consing with
      ;; SEQUENCE's use of SERAPEUM:QUEUE, and there's additional code executed to manage
      ;; the queue and completion step logic of SEQUENCE.  And of course the whole
      ;; point of transducers is to be _more_ efficient, not less :-)

      ;; Use this stmt or the labels stmt, not both, for illustrative purposes
      ;; Timing data follows this function definition.
      #+(OR)
      (sequence (dedupe) coll)
      ;; Filter first, emit later.
      #+(OR)
      (labels ((dedupe-aux (seq last)   ;last has not been emitted
                 (lazy-seq
                   (loop for s = seq then (next s)
                         while s
                         as value = (first s)
                         while (equal? last value)
                         finally
                            (return
                              (if s
                                  ;; new value encountered
                                  (cons last (dedupe-aux (next s) value))
                                  ;; no more values, return last
                                  (cons last nil)))))))
        (lazy-seq
          (when-let (s (seq coll))
            (dedupe-aux (next s) (first s)))))
      ;; Emit first, filter later
      (labels ((dedupe-aux (seq last)   ;last has been emitted
                 (lazy-seq
                   (loop for s = seq then (next s)
                         while s
                         as value = (first s)
                         while (equal? last value)
                         finally
                            (return
                              (when s
                                ;; new value encountered, emit proactively
                                (cons value (dedupe-aux (next s) value))))))))
        (lazy-seq
          (when-let (s (seq coll))
            (let ((value (first s)))
              (cons value (dedupe-aux (next s) value))))))
      (stateful-transducer 
          (rf result input (seen? last)
           :completion-body 
           (funcall rf                  ;propagate completion step
                    (if seen?
                        ;; Potentially adding row in completion, unreduced required
                        (unreduced (funcall rf result last))
                        result)))
        ;; [result input] - step
        (if seen?
            (if (equal? last input)
                result
                (prog1 (funcall rf result last)
                  (setf last input)))
            (progn (setf seen? t last input)
                   result)))))
            
(defun mdedupe (type-or-coll &optional coll)
  "([coll] [result-type coll])
An eager version of DEDUPE that returns a CL:LIST (default) or CL:VECTOR.

If RESULT-TYPE is omitted, returns a CL:LIST.

If RESULT-TYPE is specified it must be one of 'CL:LIST or 'CL:VECTOR.

A CL:VECTOR return type will be fill pointered and adjustable, allocated once
after a call to (count coll)."
  (let* ((no-value #1=':no-value)
         (coll (if (symbolp type-or-coll) coll type-or-coll))
         (result-type (if (symbolp type-or-coll) type-or-coll 'cl:list))
         (iter (clj-coll::iterator coll no-value))
         (last (funcall iter)))
    (declare (function iter))
    (ecase result-type
      (cl:list
         (unless (eq last no-value)
           (serapeum:with-collector (result)
             (result last)
             (loop as value = (funcall iter)
                   until (eq value no-value)
                   unless (equal? last value)
                     do (result (setf last value))))))
      (cl:vector
       (if (eq last no-value)
           (cl-vector)                  ;empty vector
           (let ((result (make-array (count coll) :adjustable t :fill-pointer 0)))
             (vector-push last result)
             (loop as value = (funcall iter)
                   until (eq value no-value)
                   unless (equal? last value)
                     do (vector-push (setf last value) result))
             result))))))

(defun random-sample (prob &optional (coll nil coll-p))
  "([prob] [prob coll])

Returns a lazy sequence of items from COLL with random probability of PROB 
(0.0 inclusive - 1.0 exclusive).

Returns a transducer when no collection is provided."
  (if coll-p
      (filter (lambda (_) (declare (ignore _)) (< (rand) prob)) coll)
      (filter (lambda (_) (declare (ignore _)) (< (rand) prob)))))

(defun mrandom-sample (prob coll)
  "An eager version of RANDOM-SAMPLE that produces a cl:list."
  ;; Can't imagine this is really worthwhile, but for completeness here it is.
  ;; Unfortunately (transduce (random-sample prob) #'cl-conj nil coll)
  ;; would reverse the order of accepted items.
  (mfilter (lambda (_) (declare (ignore _)) (< (rand) prob)) coll))

(defmacro lazy-cat (&rest colls)
  "([& colls])
Macro that expands to code which yields a lazy sequence of the concatenation
of the lazy traversal of each of the supplied colls.

(lazy-cat xs ys zs) is equivalent to
(concat (lazy-seq xs) (lazy-seq ys) (lazy-seq zs))"
  `(concat ,@(mmap (lambda (x) (cl:list `lazy-seq x)) colls)))

(defun cycle (coll)
  "([])
Returns an infinite lazy sequence of repetitions of the items in COLL.

If COLL is empty, the result of cycle is an empty sequence.

Useful for shoring up short lists for `map` and friends, among other things.

E.g. (take 10 (cycle [1 2 3])) => (1 2 3 1 2 3 1 2 3 1)"
  (if-let (s (seq coll))
    (let ((head-seq s))
      (labels ((cycle-aux (coll-seq) ;current seq on coll, revisits head if exhausted
                 (lazy-seq 
                   (cons (first coll-seq)
                         (cycle-aux (or (next coll-seq) head-seq))))))
        (cycle-aux s)))
    (list)))

(declaim (ftype (function (t &key (:nreps (or null integer)) (:nvals (or null integer)))
                          (values cl:list &optional)) mcycle))
(defun mcycle (coll &key nreps nvals)
  "An eager version of CYCLE returning a CL:LIST of either:

- NREPS repetitions of all items in COLL, 
- NVALS values drawn from repetitions items of COLL (as if you'd done
  (MTAKE (CYCLE COLL)) - but without any lazyness.

NREPS and NVALS are mutually exclusive.

If COLL is empty, returns NIL.
If NREPS or NVALS is <= zero, returns NIL.

When specifying NREPS, make sure COLL is not an infinite sequence.
Infinite COLL sequences are permitted with NVALS.

See also: CYCLE, MTAKE

TBD: This seems like a dubious M function, but for now it's here for completeness."
  (when (and nreps nvals)
    (error "NREPS and NVALS are mutually exclusive."))
  (unless (or nreps nvals)
    (error "Specify one of NREPS or NVALS"))
  (if (or (and nreps (<= nreps 0))
          (and nvals (<= nvals 0)))
      nil
      (when-let (head-seq (seq coll))
        (serapeum:with-collector (result)
          (if nreps
              (loop for n from 0 below nreps
                    do (loop for s = head-seq then (next s)
                             while s
                             do (result (first s))))
              (let ((n 0))
                (loop while (< n nvals)
                      do (loop while (< n nvals)
                               for s = head-seq then (next s)
                               while s
                               do (result (first s))
                                  (incf n)))))))))

(defn-ish interleave
  "([] [c1] [c1 c2] [c1 c2 & colls])
Returns a lazy seq of the first item in each coll, then the second etc.
E.g. (interleave [:a :b :c] [1 2 3]) => (:a 1 :b 2 :c 3)

Interleave stops when any coll runs out of values.
E.g. (interleave [:a :b :c] [1 2]) => (:a 1 :b 2)

Returns an empty list if no colls are supplied, 
otherwise the result is lazy sequence."
    args                                ;&rest of interleave
  (() (list))
  ((c1) (lazy-seq c1))
  ((c1 c2)
   (lazy-seq
     (let ((s1 (seq c1)) 
           (s2 (seq c2)))
       (when (and s1 s2)
         (cons (first s1) 
               (cons (first s2) 
                     (interleave (rest s1) (rest s2))))))))
  ((c1 c2 & colls)
   (declare (ignore c1 c2 colls)) ; use 'args' instead
   (lazy-seq 
     ;; Unfortunate doing the list* since we're just reconstituting
     ;; a list that was dismantled to get us to this arity in
     ;; the first place. Would be nice if `defn-ish` gave us access
     ;; to the original list.
     (let ((seqs (cl:mapcar #'seq args)))
       (when (every? #'identity seqs)
         (concat (map #'first seqs) 
                 ;; I _think_ this is okay, removing the lazy map for a cl:mapcar
                 ;; on the `rest` calls. Or is it?  Have I lost _essential_ lazyness?
                 ;; CLojure's apply would call seq on 'args' which woul dbe the (map ...)
                 ;; lazyseq, and seq realizes it.  I think we're good.
                 #+(OR)
                 (clj-apply #'interleave (map #'rest seqs))                 
                 (apply #'interleave (cl:mapcar #'rest seqs))))))))

(defn-ish minterleave
  "([] [c1] [c1 c2] [c1 c2 & colls])
An eager version of INTERLEAVE that returns CL:LISTs.
Further optimized for two cl:list inputs."
  args                                  ;allow use of pre-arity &rest args
  ;; TBD: accept an optional sequence type for return value? (cl:list or cl:vector)?
  (() nil)
  ((c1) (if (cl:listp c1) c1 (convert 'cl:list c1)))
  ((c1 c2)
   (if (and (listp c1) (listp c2))
       (loop for x1 in c1
             for y1 in c2
             collect x1
             collect y1)
       (loop with no-val = #1=':no-val
             with iter1 = (iterator c1 no-val)
             with iter2 = (iterator c2 no-val)
             as v1 = (funcall iter1)
             as v2 = (funcall iter2)
             until (or (eq v1 no-val)
                       (eq v2 no-val))
             collect v1
             collect v2)))
  ((c1 c2 & colls)
   (declare (ignore c1 c2 colls))       ;using 'args'
   (let* ((buffer (make-array (cl:length args)))
          (no-val #1#)
          (iterators (mapcar (lambda (coll) (iterator coll no-val)) args)))
     (serapeum:with-collector (result)
       (loop as continue? = (loop for i from 0
                                  for iterator in iterators
                                  as val = (funcall iterator)
                                  do (if (eq val no-val)
                                         (return nil)
                                         (setf (svref buffer i) val))
                                  finally (return t))
             while continue?
             do (loop for item across buffer
                      do (result item)))))))

(defun interpose (sep &optional (coll nil coll-p))
  "([sep] [sep coll])
Returns a lazy seq of the elements of COLL separated by SEP.
Returns a stateful transducer when no collection is provided.

E.g. (interpose \", \" [\"a\" \"b\" \"c\"]) => \"a, b, c\"

See also INTERLEAVE, MINTERPOSE."

  (if coll-p
      (drop 1 (interleave (repeat sep) coll))
      (stateful-transducer (rf result input (started))
        (if started
            (let ((result (funcall rf result sep)))
              (if (reduced? result)
                  result
                  (funcall rf result input)))
            (progn
             (setf started t)
             (funcall rf result input))))))
  
(defun minterpose (sep coll)
  "An eager version of INTERPOSE that returns a CL:LIST."
  (loop with doit = nil
        for s = (seq coll) then (next s)
        while s
        if doit
          collect sep
        else 
          do (setf doit t)
        collect (first s)))


;; Note, mtake-last is in seq-apis2.lisp (uses doseq).
(defun take-last (n coll)
  "([n coll])
Returns a non-lazy seq of the last N items in COLL.

Returns NIL if N <= 0 or COLL is empty.

May be linear time depending on the collection type.

See also: MTAKE-LAST, LAST, BUTLAST, CL:BUTLAST, DROP-LAST,
and SUBVEC (for vectors)."
  ;; Using Clojure's technique here, but we could
  ;; be a lot less seq-consing for vectors
  (loop for s = (seq coll) then (next s)
        for lead = (seq (drop n coll)) then (next lead)
        while lead
        finally (return s)))

(defn-ish mbutlast
  "([coll] [result-type coll])
Like CLJ-COLL::BUTLAST, only it returns a CL:LIST(default) or CL:VECTOR instead of
a seq.  NIL is still returned for cases where the result would be empty.

If RESULT-TYPE is not specified, the result type will be a CL:VECTOR.
If the RESULT-TYPE (a symbol) is specified, it may be either CL:LIST or CL:VECTOR.

If COLL and the result type are both CL:VECTOR, the result is displaced
to the original vector.

See also: CLJ-COLL:BUTLAST, CL:BUTLAST
DROP-LAST - which lets you lazily drop more than one
MDROP-LAST - which lets you eagerly drop more than one"
  ;; If I'd implemented DROP-LAST before BUTLAST, this might just have called
  ;; (MDROP-LAST 1 coll). Though MBUTLAST is presently a tiny bit more efficient
  ;; for dropping only one, as there's no mapping or secondary nthnext going on.
  ((coll) (mbutlast 'cl:list coll))
  ((result-type coll)
   (ecase result-type
     (cl:list (if (cl:listp coll) 
                (return-from mbutlast (cl:butlast coll))
                (serapeum:with-collector (result)
                  (do* ((s (seq coll))
                        (n (next s)))
                       ((null n))
                    (result (first s))
                    (setf s n n (next n))))))
     (cl:vector   
      (let ((n (count coll)))
        (if (<= n 1)
            nil
            (let ((n (1- n)))
              (if (cl:vectorp coll)
                  (make-array n :displaced-to coll)
                  ;; *PERFORMANCE* todo: use non-consing traversal
                  ;; where we can, e.g. doseq, run!, cl-map
                  (do* ((result (make-array n))
                        (i 0 (1+ i))
                        (s (seq coll))
                        (n (next s)))
                       ((null n) result)
                    (declare (simple-vector result))
                    (setf (svref result i) (first s))
                    (setf s n n (next n)))))))))))

(defun butlast (coll)
  "([coll])
Return a non-lazy seq of all but the last item in COLL.

For any collection type other than cl:vector this will require linear time.

Returns nil if there's only one element in COLL (per seq semantics).

See also: CL:BUTLAST, MBUTLAST, 
and DROP-LAST - a lazy version of BUTLAST."
  ;; Clojure note: they call NEXT twice for every element.  We do not.

  ;; We could allocate a persistent vector and use that, 
  ;; but as the user has no access to the vector since we're returning a seq
  ;; we may as well use a more efficient CL vector.
  ;; We avoid using cl:butlast or returning a cl:list-based seq
  ;; because those are just conses and we'd like to hide the
  ;; result a bit behind a seq that isn't a cl:cons
  (seq (mbutlast 'cl:vector coll))
  #+(OR)
  (let ((n (count coll)))
    (if (<= n 1)
        nil
        (let ((n (1- n)))
          (if (cl:vectorp coll)
              (seq (make-array n :displaced-to coll))
              (do* ((result (make-array n))
                    (i 0 (1+ i))
                    (s (seq coll))
                    (n (next s)))
                   ((null n) (seq result))
                (declare (simple-vector result))
                (setf (svref result i) (first s))
                (setf s n n (next n))))))))

(defn-ish drop-last
  "([coll] [n coll])
Return a lazy sequence of all but the last n (default 1) items in coll.

See also:
BUTLAST - which is a non-lazy linear time version of DROP-LAST.
MDROP-LAST"
  ((coll)
   (drop-last 1 coll))
  ((n coll)
   ;; Clever Clojure.
   ;; Though it seems worth mentioning that we're building new collections
   ;; from scratch, when we could subtract from a persistent collection
   ;; and it might be much more efficient.  Worth a performance test
   ;; sometime where we remove a handful of entries from a big collection.
   (map (lambda (x _) (declare (ignore _)) x) coll (drop n coll))))

(defun mdrop-last (&rest args)
  "([coll] [n-or-result-type coll] [result-type n coll]

An eager version of DROP-LAST that returns a CL:LIST(default) or CL:VECTOR with an
optional RESULT-TYPE parameter. The result may share structure with COLL.

If specified, RESULT-TYPE, a symbol, should be first, followed
by N (if specified), and finally COLL.

If all elements would be dropped, returns an empty collection compatible with
RESULT-TYPE, i.e. NIL for CL:LIST, and #() for CL:VECTOR.

Use caution that you don't call this with infinite or excessively large sequences 
as it materalizes collections and nothing is lazy.

Returns COLL if N<=0 and COLL is already of the desired result type,
otherwise COLL is converted to the desired result type.

See also BUTLAST, MBUTLAST."
  ;; Beware NIL as a COLL, not to be confused with RESULT-TYPE
  (let* ((result-type-p (and (> (cl:length args) 1) (symbolp (cl:first args))))
         (result-type (if result-type-p (cl:first args) 'cl:list))
         (args (if result-type-p (cdr args) args))
         (n-p (integerp (cl:first args)))
         (n (if n-p (cl:first args) 1))
         (args (if n-p (cdr args) args))
         (coll (cl:first args)))
    (ecase result-type
      (cl:vector
       (if (empty? coll)
           #()
           (if (cl:vectorp coll)
               ;; share structure
               (if (<= n 0)
                   coll                 ;nothing is dropped
                   (let ((size (cl:length coll)))
                     (if (>= n size)
                         #()
                         (make-array (- size n) :displaced-to coll))))
               ;; Convert opaque coll to vector
               (let ((size (count coll)))
                 (if (>= n size)
                     #()                ;everything dropped
                     (loop with n-actual = (- size (max n 0))
                           with result = (make-array n-actual)
                           for i from 0 below n-actual
                           for s = (seq coll) then (next s)
                           do (setf (svref result i) (first s))
                           finally (return result)))))))
      (cl:list
       (if (empty? coll)
           nil
           (if (<= n 0)                 ;nothing is dropped
               (if (cl:listp coll)
                   coll
                   (convert 'cl:list coll))
               (loop for s1 = (seq coll) then (next s1)
                     for s2 = (nthnext coll n) then (next s2)
                     while s2
                     collect (first s1))))))))

(defn-ish reductions 
  "([f coll] [f init coll])
Returns a lazy seq of the intermediate values of the reduction (as
per REDUCE) of COLL by F, starting with INIT.

In other words, it's just like you called REDUCE, only instead
of returning the final result REDUCTIONS returns the result of
applying F at each step, as a lazy sequence.

E.g. (reductions #'+ '(1 1 1 1)) => (1 2 3 4)

If INIT is reduced, returns a non-lazy list of the unreduced value."
  ((f coll)
   (lazy-seq
     (if-let (s (seq coll))
       (reductions f (first s) (rest s)) ;trampoline to init arity
       (list (funcall f)))))
  ((f init coll)
   (if (reduced? init)
       (list (deref init))
       (cons init
             (lazy-seq
               (when-let (s (seq coll))
                 (reductions f (funcall f init (first s)) (rest s))))))))

(defun tree-seq (branch? children root)
  "Returns a lazy sequence of the nodes in a tree via a depth-first walk.
BRANCH? must be a function of one arg that returns true if passed a node
that can have children (but may not). CHILDREN must be a function of one
arg that returns a sequence of the CHILDREN. CHILDREN will only be called on
nodes for which BRANCH? returns true. ROOT is the root node of the tree.

Tip: SEQ is useful for the CHILDREN function."
  (labels ((walk (node)
             (lazy-seq
               (cons node
                     (when (funcall branch? node)
                       (mapcat #'walk (funcall children node)))))))
    (walk root)))

(defun mtree-seq (branch? children root)
  "An eager version of TREE-SEQ that returns a CL:LIST of nodes in the ROOT tree
via a depth-first walk.

Note that MTREE-SEQ doesn't convert the returned nodes (or node children) to be
CL:LIST types, they'll be EQ to whatever was passed in and/or returned by the
CHILDREN function.

See also: TREE-SEQ"
  (loop with nodes-to-traverse = (cl:list root)
        while nodes-to-traverse
        as node = (cl:first nodes-to-traverse)
        collect node
        do (setf nodes-to-traverse (cdr nodes-to-traverse))
        when (funcall branch? node)
          do (setf nodes-to-traverse
                   (mconcat 'cl:list (funcall children node) nodes-to-traverse))))

(defun flatten (coll)
  "Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat lazy sequence.
  (flatten nil) returns an empty sequence.

See also: MFLATTEN"
  (filter (complement #'sequential?)
          ;; rest to filter out 'coll'
          (rest (tree-seq #'sequential? #'seq coll))))

(defun mflatten (coll)
  "An eager cl:list returning version of FLATTEN."
  (mfilter (complement #'sequential?)
           (cl:rest (mtree-seq #'sequential? #'seq coll))))
