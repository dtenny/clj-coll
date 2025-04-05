(in-package :clj-coll)

;;;;
;;;; Equality / comparison
;;;;

(defun class-slots-equiv? (obj1 obj2 slot-name)
  "Compare slot values and/or boundness of two same-class standard-object objects.
The declarations of standard-object vs structure-object are important for slot-value
optimizations in sbcl, or so it seems."
  (declare (standard-object obj1 obj2) (symbol slot-name)
           (optimize (speed 3) (safety 1)))
  (if (slot-boundp obj1 slot-name)
      (if (slot-boundp obj2 slot-name)
          (equiv? (slot-value obj1 slot-name) (slot-value obj2 slot-name))
          nil)                          ;obj1 bound when obj2 isn't
      (slot-boundp obj2 slot-name)))      ;obj1 not bound, only equiv? if obj2 isn't either

(defun structure-slots-equiv? (obj1 obj2 slot-name)
  "Compare slot values and/or boundness of two same-class structure-object objects."
  (declare (structure-object obj1 obj2) (symbol slot-name)
           (optimize (speed 3) (safety 1)))
  (if (slot-boundp obj1 slot-name)
      (if (slot-boundp obj2 slot-name)
          (equiv? (slot-value obj1 slot-name) (slot-value obj2 slot-name))
          nil)                          ;obj1 bound when obj2 isn't
      (slot-boundp obj2 slot-name)))      ;obj1 not bound, only equiv? if obj2 isn't either

(defvar *compare-objects-by-value* nil
  "If true, comparisons of STANDARD-OBJECT and STRUCTURE-OBJECT, i.e. instances of 
DEFCLASS and DEFSTRUCT (respectively), will use a slot value based equality test, where
for two instances to be `equal?` they must be of the same type, and all slots must have the
same value and slot boundness property. By default objects are compared with EQ.
Note that this does not affect or apply to key comparisons on hashtable & map lookups.")

(defvar *standard-object-equality-fn* nil
  "If true, this should be a function designator use for comparing two standard-object defclass
instances.  Note that any definition of CLJ-COLL::EQUIV? methods on more specific instance
types will bypass this function. CLJ-COLL doesn't define more specific methods.")

(defvar *structure-object-equality-fn* nil
  "If true, this should be a function designator use for comparing two structure-object defstruct
instances.  Note that any definition of CLJ-COLL::EQUIV? methods on more specific instance
types will bypass this function. CLJ-COLL doesn't define more specific methods.")

(defun equiv-seq? (sequential-coll seq)
  "Compare some collection having sequential (ordered) semantics with a seq.
Return true if they're equiv?, false if they're not.

Behave as if seq were called on sequential-coll so we treat empty
collections as if they were null seqs.  Do not accept lazyseqs,
only seqs on lazyseqs if that's the case."
  (when (eq 'lazyseq (type-of seq))
    (error "Lazyseq ~s passed where (seq lazyseq) should have been passed." seq))
  (cond 
    ((empty? sequential-coll) (null seq))
    ((null seq) nil)                    ;because coll isn't empty
    (t (run! (lambda (xval)             ;coll is x, seq is y
               (unless seq
                 (return-from equiv-seq? nil)) ;ran out of Y's
               (unless (equiv? xval (first seq))
                 (return-from equiv-seq? nil))
               (setf seq (next seq)))
             sequential-coll)
       ;; Examined all coll elements and they compared okay, did we run out of seq vals?
       (if seq 
           nil                          ;more seq vals remain
           t))))                        ;seq and coll had same length

(defun equiv-seqs? (seq1 seq2)
  "Compare two seqs, return true if all elements are `equiv?`.
CAUTION: An infinite loop will occur if both seqs are infinite."
  (loop for s1 = seq1 then (next s1)
        for s2 = seq2 then (next s2)
        while (and s1 s2 (equiv? (first s1) (first s2)))
        finally (return (and (null s1) (null s2)))))

(defun fset-ordered-equiv? (coll1 coll2)
  "Compare two fset:seq objects by element value with CLJ-COLL semantics.
FSET:EQUAL? won't do this. It's more akin to cl:equal than clj-coll::equiv?.
Return true if the collections are equiv.

This is probably prohibitively expensive to use in large collections that are very similar
and of equal size.  We may need to replace fset sooner than later if we can't pass our own
equivalence function to it."
  (declare (fset:seq coll1 coll2))
  (or (eq coll1 coll2)
      (and (= (fset:size coll1) (fset:size coll2))
           (progn
             (fset:do-seq (val1 coll1 :index index)
               ;; This LOOKUP works because it isn't doing FSET:EQUAL?, it's just an index lookup
               (unless (equiv? (fset:lookup coll2 index) val1)
                 (return-from fset-ordered-equiv? nil)))
             t))))
               
(defun fset-set-equiv? (coll1 coll2)
  "Compare two unordered FSET Sets using EQUIV? logic that
FSET:EQUAL? doesn't do.  Return true if they're EQUIV? false if they are not.

**** WARNING ****   O(N^2) worst case for similar sets of equal size."
  (declare (fset:set coll1 coll2))
  (or (eq coll1 coll2)
      (and (= (fset:size coll1) (fset:size coll2))
           ;; We can't use FSET:CONTAINS? or FSET:LOOKUP because they rely on FSET:COMPARE and hash
           ;; Really need an EQUIV? capable hash table, even if it's mutable-only to speed this up. 
           ;; XOR is not fastest solution because it's going to check all values in both lists.
           ;; but I figure it might be implemented more efficiently than I can do without an
           ;; EQUIV? powered hashtable.  *FINISH* *TODO*
           (let ((coll1 (fset:convert 'cl:list coll1))
                 (coll2 (fset:convert 'cl:list coll2)))
             (null (cl:nset-exclusive-or coll1 coll2 :test #'equiv?))))))

(declaim (inline atom-cdr-p))
(defgeneric equiv? (x y)                ;not a public function in Clojure, not exported here. Should export?
  (:documentation
   "Determine if two entities are equivalent, by comparing entity content if necessary.
Return T if they are equivalent, NIL if they are not.

Does pairwise comparisions on behalf of EQUAL?, see EQUAL? docs for more details.

Does not handle circular lists.
")
  ;; Other notes:
  ;; FSet does not treat different collection types with same leaf elements as equal.
  ;; i.e. (fset:equal? '(1 2) (vector 1 2)) => nil
  ;; And as such can't be relied upon for comparing collections of collections either.

  ;; Clojure does not treat empty maps as equivalent to other empty sequence types.
  ;; Clojure does not have dotted pairs, we do. fset:compare deals with them.
  ;; Clojure does not compare maps to vectors of pseudo-map-entries, i.e. (= {:a 1} [[:a 1]]) => false. Yay.
  ;; Clojure does not compare sets to anything other than sets.

  ;; Could maybe call EQUAL here, except for FSET types we want its comparison, for now.
  (:method (x y) (fset:equal? x y))
  ;; Null for O(1) length checks on potential lists.
  ;; Since we have explicit NULL checks, do not use LIST types
  ;; which differ from CONS only because they allow NIL
  (:method ((x null) (y t)) (eq x y))   ;only NIL = NIL
  (:method ((x t) (y null)) (eq x y))
  ;; CL:List and CL:VECTOR interaction
  (:method ((x cl:vector) (y cl:cons)) (equiv? y x))
  (:method ((x cl:cons) (y cl:vector))
    (if (atom-cdr-p x)
        nil            ;dotted pair doesn't compare with vector
        (and (cl:= (cl:list-length x) (cl:length y))
             (loop for xl in x
                   for yl across y
                   unless (equiv? xl yl)
                     do (return-from equiv? nil)
                   finally (return t)))))
  (:method ((x cl:cons) (y cl:cons))
    (or (eq x y)
        ;; LOOP will stop traversing a list upon finding an atom cdr. Yay.
        (loop for xc on x
              for yc on y
              do 
                 (unless (equiv? (car xc) (car yc))
                   (return-from equiv? nil))
                 (let ((xatom? (atom (cdr xc)))
                       (yatom? (atom (cdr yc))))
                   (cond ((and xatom? yatom?)
                          (return-from equiv? (equiv? (cdr xc) (cdr yc)))) ; compare atoms
                         ((or xatom? yatom?) 
                          (return-from equiv? nil)) ;cons cdrs differ
                         ;; Otherwise fall through loop to next cons
                         ))))
    t)                                  ;no mismatches in lists
  (:method ((x cl:vector) (y cl:vector))
    (or (eq x y)
        (and (cl:= (cl:length x) (cl:length y))
             (loop for xl across x
                   for yl across y
                   unless (equiv? xl yl)
                     do (return-from equiv? nil)
                   finally (return t)))))
  ;; CL:List and FSET:SEQ interaction
  (:method ((x cl:cons) (y fset:seq))
    (if (atom-cdr-p x)
        nil                             ;dotted pairs don't compare to seqs
        (fset:do-seq (val y)
          (unless (and (cl:consp x)
                       (equiv? (car x) val))
            (return-from equiv? nil))
          (setf x (cdr x))))
    (endp x))
  (:method ((x fset:seq) (y cl:cons)) (equiv? y x))
  ;; CL:LIST and IPERSISTENTLIST interaction
  (:method ((x cl:cons) (y ipersistentlist))
    (if (atom-cdr-p x)
        nil                             ;dotted pairs don't compare to seqs
        (loop for xcons on x
              for pcons = y then (next pcons)
              while pcons               ;while xcons is implicit in this loop
              unless (equiv? (car xcons) (first pcons))
                do (return-from equiv? nil)
              finally (return (and (null xcons)
                                   (and pcons
                                        (null (next pcons))))))))
  (:method ((x ipersistentlist) (y cl:cons)) (equiv? y x))
  ;; CL:VECTOR and IPERSISTENTLIST
  (:method ((x cl:vector) (y ipersistentlist))
    (let ((n-x (cl:length x))
          (n-y (count y)))
      (and (= n-x n-y)
           (or (= 0 n-x n-y)
               (loop for idx from 0 below n-x
                     for s = (seq y) then (next s)
                     while s
                     unless (equiv? (aref x idx) (first s))
                       do (return-from equiv? nil)
                     finally(return (and (= idx n-x)
                                         s (null (next s)))))))))
  (:method ((x ipersistentlist) (y cl:vector)) (equiv? y x))
  ;; CL:VECTOR and FSET:SEQ
  (:method ((x cl:vector) (y fset:seq))
    (let ((idx 0)
          (n (cl:length x)))
      (declare (fixnum idx n))
      (and (cl:= n (fset:size y))
           (or (cl:= n 0)
               (progn
                 (fset:do-seq (val y)
                   (unless (equiv? (aref x idx) val)
                     (return-from equiv? nil))
                   (incf idx))
                 (cl:= idx n))))))
  (:method ((x fset:seq) (y cl:vector)) (equiv? y x))
  ;; Hash tables. FSET:COMPARE doesn't compare CL:HASH-TABLE types, just FSET:MAP types.
  ;; EQUALP will, but doesn't know how to compare CLJ-COLL types in them.
  ;; We do not care about hash table tests or hash functions for purposes of EQUIV.
  ;; *TBD*: should we?
  (:method ((x cl:hash-table) (y cl:hash-table))
    (or (eq x y)
        (and (cl:= (cl:hash-table-count x) (cl:hash-table-count y))
             (progn
               (maphash (lambda (k v)
                          (unless (equiv? v (gethash k y #1='#:eof))
                            (return-from equiv? nil)))
                        x)
               t))))
  (:method ((x cl:hash-table) (y fset:map))
    (and (cl:= (cl:hash-table-count x) (fset:size y))
         (fset:do-map (k v y t)
           (unless (equiv? v (gethash k x #2='#:eof))
             (return-from equiv? nil)))))
  (:method ((x fset:map) (y cl:hash-table)) (equiv? y x))

  ;; Comparing fset maps to fset maps, there's things it won't compare.
  ;; for example, it can't compare cl:list values with persistentlist values
  ;; and it won't compare cl:hash-tables to each other with anything other than EQ.
  (:method ((x fset:map) (y fset:map))
    ;; FSet maps are ordered, so co-iterating solution is easy, hopefully
    ;; minimally consing, and avoids O(n^2) lookups.
    (cond 
      ((eq x y) t)
      ((/= (fset:size x) (fset:size y)) nil)
      (t (loop with xi = (fset:iterator x)
               with yi = (fset:iterator y)
               until (or (funcall xi :done?) (funcall yi :done?))
               do (multiple-value-bind (xk xv) (funcall xi :get)
                    (multiple-value-bind (yk yv) (funcall yi :get)
                      (unless (and (equal? xk yk)
                                   (equal? xv yv))
                        (return-from equiv? nil))))
               finally (return (and (funcall xi :done?) (funcall yi :done?)))))))

  ;; We're doing our own element-by-element comparisons because FSET:EQUAL? isn't
  ;; going to use CLJ-COLL::EQUIV? semantics or let us pass in EQUIV? where we want
  ;; to.  For example, we'd like (EQUIV? #{ (:A 1) } #{ #[ :A 1 ] }) to be true.
  ;; This could be prohibitively slow or consing on large and similar fset
  ;; collections of equal size.  I've done the expensive SET thing if only because
  ;; sets are used a lot in the unit tests to compare all manner of things with
  ;; EQUIV? semantics.
  (:method ((x ipersistentlist) (y ipersistentlist)) 
    (or (eq x y)
        (and (cl:= (count x) (count y))
             (equiv-seqs? x y))))
  (:method ((x fset:seq) (y fset:seq)) (fset-ordered-equiv? x y))
  (:method ((x fset:set) (y fset:set)) (fset-set-equiv? x y))

  ;; PersistentList vs fset collections, as FSET:EQUAL? doesn't understand PersistentList
  (:method ((x ipersistentlist) (y fset:collection)) (equiv-seqs? x y))
  (:method ((x fset:collection) (y ipersistentlist)) (equiv-seqs? x y))

  ;; Seqs, lazyseqs. Try to compare against collections when we can vs seqs on the collection
  ;; which would be much slower.  Of course for lazyseqs we must do this.
  ;; Seqs vs unordered collections will return false.
  ;;
  ;; Note that (equal? [] lazyseq-of-no-vals) => true
  ;; but       (equal? [] nil) => false

  ;; Turn Lazyseqs into seqs or nil.
  ;; Unfortunately this does not work as I'd like, for example
  ;; (equiv? fset:seq lazyseq) doesn't invoke the (x (y lazyseq)) method
  ;; it invokes (structure-object structure-object) method.
  (:method ((x lazyseq) (y lazyseq)) (equiv? (seq x) (seq y)))
  ;;(:method ((x lazyseq) y) (equiv? y (seq x)))
  ;;(:method (x (y lazyseq)) (equiv? x (seq y)))
  (:method ((x seq) (y lazyseq)) (or (eq x y) (equiv? x (seq y))))
  (:method ((x seq) (y seq)) (or (eq x y) (equiv-seqs? x y)))

  ;; Compare ordered collections to seqs
  (:method ((x lazyseq) (y fset:seq)) (equiv-seq? y (seq x)))
  (:method ((x lazyseq) (y IPersistentList)) (equiv-seq? y (seq x)))
  (:method ((x lazyseq) (y cl:sequence)) (equiv-seq? y (seq x)))
  (:method ((x fset:seq) (y lazyseq)) (equiv-seq? x (seq y)))
  (:method ((x IPersistentList) (y lazyseq)) (equiv-seq? x (seq y)))
  (:method ((x cl:sequence) (y lazyseq)) (equiv-seq? x (seq y)))

  (:method ((x seq) (y fset:seq)) (equiv-seq? y x))
  (:method ((x seq) (y IPersistentList)) (equiv-seq? y x))
  (:method ((x seq) (y cl:sequence)) (equiv-seq? y x))
  (:method ((x fset:seq) (y seq)) (equiv-seq? x y))
  (:method ((x IPersistentList) (y seq)) (equiv-seq? x y))
  (:method ((x cl:sequence) (y seq)) (equiv-seq? x y))

  ;; Structs & classes, which fset:compare will not do without augmentation we want to avoid
  ;; The default is a slow slot-by-slot comparison. You can adjust this by either
  ;; providing a function for clj-coll::*instance-equality-function*, or providing
  ;; more specialized methods for the unexported clj-coll::equiv? generic function.
  ;; See README.md for more details.
  (:method ((x cl:standard-object) (y cl:standard-object))
    (if *standard-object-equality-fn*
        (funcall *standard-object-equality-fn* x y)
        (or (eq x y)
            (and *compare-objects-by-value*
                 (let ((xc (cl:class-of x))
                       (yc (cl:class-of y)))
                   (and (eq xc yc)
                        (loop with slots = (c2mop:class-slots xc)
                              for slot-definition in slots
                              as slot-name = (c2mop:slot-definition-name slot-definition)
                              do (unless (class-slots-equiv? x y slot-name)
                                   (return-from equiv? nil))
                              finally (return t))))))))
  ;; Identical to standard-object except for type specifiers to optimize SLOT-VALUE
  ;; and user settable variables to specialize the test.
  (:method ((x cl:structure-object) (y cl:structure-object))
    (if *structure-object-equality-fn*
        (funcall *structure-object-equality-fn* x y)
        (or (eq x y)
            (and *compare-objects-by-value*
                 (let ((xc (cl:class-of x))
                       (yc (cl:class-of y)))
                   (and (eq xc yc)
                        (loop with slots = (c2mop:class-slots xc)
                              for slot-definition in slots
                              as slot-name = (c2mop:slot-definition-name slot-definition)
                              do (unless (structure-slots-equiv? x y slot-name)
                                   (return-from equiv? nil))
                              finally (return t)))))))))
(declaim (notinline atom-cdr-p))


(defun equal? (x &optional (y nil y-p) &rest more)
  "([x] [x y] [x y & more])

This function strives for _most_ compatibility with Clojure's `=` predicate, however
Clojure treats NIL differently than Common Lisp in some cases, and in these cases we
give preference to Common Lisp's semantics. In particularly NIL _is_ equivalent to
an empty list in CL, but not in Clojure.

Thus

    CLJ: (= () nil) => false
    CL:  (equal? () nil) => T

And transitively from there, thus (equal? NIL #()) => T, since lists and vectors with
same length and content are considered equal."
  (cond ((not y-p) t)                   ;(x)
        ((null more) (equiv? x y))      ;(x y)
        (t                              ;(x y ...)
         (if (equiv? x y)
             (loop with result = t
                   for  z in more
                   while result
                   do ;; Prefer x be a non-list for repeated comparisons for length performance
                      (when (and (cl:listp x) (not (cl:listp y)))
                        (setf x y))
                      (setf result (equiv? x z) y z)
                   finally (return result))
             nil))))
