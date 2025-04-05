(in-package :cl-user)

(defpackage :clj-coll-test
  (:use :cl :clj-coll :clj-arrows :fiveam)
  (:shadowing-import-from :fiveam :run!)
  (:shadowing-import-from :clj-coll 
   :assoc :butlast :cons :count :find :first :get #:intersection :map :merge :nth 
   :last :list :list* :listp :pop :reduce :remove :replace :rest :reverse :second :sequence
   :set :some :union :vector :vectorp)
  (:export :run-tests)
  (:documentation "Tests for the :clj-coll package."))

(in-package :clj-coll-test)

;;;; This is the one and only and huge test suite for clj-coll.
;;;; Note that most (but not all) test assertions
;;;; here were made by carefully examining how clojure would behave
;;;; and asserting that we got the same result.
;;;; So if a test breaks, the assertion is probably still valid
;;;; and it's the code implementing the behavior that broke.
;;;; Vetted against *clojure-version* {:major 1, :minor 12, :incremental 0, :qualifier nil}

(def-suite test-suite :description ":clj-coll tests")
(in-suite test-suite)

(named-readtables:in-readtable clj-coll:readtable)

;;
;; Test utilities
;;

(defun maphash-mapentry (f hashtable)
  "Like maphash, except that F receives only ONE argument
being a pseudo MapEntry in the form a list with key / value pairs.
If you don't need a fresh list for each map entry, consider
using plain MAPHASH instead."
  (maphash (lambda (k v) (funcall f (cl:list k v))) hashtable))

(defun mapf (f coll) ;*FINISH* *TBD*: this, and the (test mapf...) below, can probably be replaced by mapv or something now that we're bootstrapped
  "Iterate by calling F on each element of COLL for side effects.
If COLL is a map, returns a map entry consistent with the coll
(a mutable list of key & value for mutable maps, an immutable vector
for immutable maps).

In all cases F is called with one argument.
MAPF returns COLL."
  (etypecase coll
    (fset:map
     (fset:do-map (key value coll)
       (funcall f (vector key value))))
    (cl:hash-table
     (maphash-mapentry f coll))
    (cl:sequence
     (cl:map nil f coll))
    (fset:collection
     (fset:do-seq (value coll)
       (funcall f value)))))

(defun set= (s1 s2)
  "Compare two collections as sets."
  ;; Fset:compare won't compare #[ #[a 1]] to '((a 1))
  ;; Apply crude and limited fix.
  (labels ((to-list (coll)
             (let (result)
               (mapf (lambda (x) 
                                 (if (collp x)
                                     (push (to-list x) result)
                                     (push x result)))
                               coll)
               (nreverse result))))
    (eq :equal (fset:compare (clj-coll::convert 'fset:set (to-list s1))
                             (clj-coll::convert 'fset:set (to-list s2)))))
  #+NIL (null (set-exclusive-or s1 s2)))

;; *FINISH* Delete me, should be able to use equal? now.
(defun coll= (c1 c2)
  "Compare two ordered collections."
  (eq :equal (fset:compare (vec c1) (vec c2))))

(defparameter *similar-sequences*
  (cl:list (vector 1 2 3) (vec '(1 2 3)) (cl-vector 1 2 3) (cl-vec '(1 2 3)) (cl:list 1 2 3)))
(defparameter *overlapping-sequences*
  (cl:list (vector 1) (vec '(1 2)) (cl-vector 1 2 3) (cl-vec '(1 2 3 4)) (cl:list 1 2 3 4 5)))

(defstruct test-struct a b c)
(defclass  test-class () 
  ((a :initarg :a :accessor test-class-a)
   (b :initarg :b :reader test-class-read-b)
   (c :initarg :c :writer test-class-write-c)))
(defclass  test-class-2 (test-class)
  ((d :initarg :d :accessor test-class-2-d)
   (e :initarg :e :reader test-class-2-read-e)))

(defun nums (n)
  "Generates a lazy sequence of N nums.
Note that a lazy seq of zero nums still returns a lazyseq, as the use of N hasn't
been evaluated in an unrealized lazy seq.  That's a feature, not a bug,
this is consistent with Clojure."
  (lazy-seq (if (< n 1) nil (cons n (nums (1- n))))))

(defun false (x)
  "Always return false (NIL) given some thing X."
  (declare (ignore x))
  nil)

(defun true (x)
  "Always return true (T) given some thing X."
  (declare (ignore x))
  t)

;;
;; Tests
;;

(test equal?

  ;; Note: test hash table key equivalence on non-scalar keys is tested in 
  ;; the hash-table-key-equality test, which is different from comparing
  ;; two different hashtables for equality (tested here, with lots of other things).

  (flet ((doit2 (positive x y)
           ;;(format t "~%doit2 positive=~s, x=~s, y=~s~%" positive x y)
           (cond (positive
                  (is (equal? x y))
                  (is (equal? y x)))
                 (t
                  (is (not (equal? x y)))
                  (is (not (equal? y x))))))
         (doit3 (positive x y z)
           (cond (positive
                  (is (equal? x y z))
                  (is (equal? y z x))
                  (is (equal? z y x)))
                 (t
                  (is (not (equal? x y z)))
                  (is (not (equal? y z x)))
                  (is (not (equal? z y x)))))))
    (doit2 t  "a" "a")                  ;T=>"IS", NIL=>"IS NOT"
    (doit2 nil "a" "A")
    (doit2 nil 'a :a)
    ;; Only NIL = NIL
    (doit2 nil '() #())
    (doit2 nil nil #())
    (doit2 nil '(1) nil)
    (doit2 nil (list) nil)
    (doit2 nil (queue) nil)
    (doit2 nil #(1) nil)
    (doit2 nil (vector 1) nil)
    (doit2 nil (cl-hash-map 1) nil)
    (doit2 nil (hash-map 1) nil)
    ;; Empty checks for things other than list
    ;; Note that ordered and unordered things don't match either
    (doit2 t #() (vector))
    (doit2 t (cl-hash-map) (hash-map))
    (doit2 t #{} #{})
    (doit2 nil #{} (vector))
    (doit2 nil #{} #())
    (doit2 nil #{} (hash-map))
    (doit2 nil #{} (cl-hash-map))
    (doit2 nil [] (cl-hash-map))
    (doit2 nil [] (hash-map))
    (doit2 t nil nil)
    (doit2 nil nil #())
    (doit2 nil nil #{})
    (doit2 nil nil [])
    ;; Dotted pairs
    (doit2 t '(1 . 2) (cl:cons 1 2))
    (doit2 nil '(1 . 2) (cl:cons 1 3))
    (doit2 nil (cl:cons 1 2) (cl:list 1 2))
    (doit2 nil (cl:cons 1 2) nil)
    ;; CL:LIST / PERSISTENTLIST / PersistentQueue
    (doit2 t (list :a 1) (cl:list :a 1))
    (doit2 t (queue :a 1) (cl:list :a 1))
    (doit2 nil (list :a 1) (cl:list :a 2))
    (doit2 nil (queue :a 1) (cl:list :a 2))
    ;; CL:LIST / CL:VECTOR
    (doit2 t '(1) '(1))
    (doit2 t '(1) (cl:list 1))
    (doit2 t #(1) '(1))
    (doit2 t #(1) (cl-vector 1))
    (doit2 nil '(1) '(0))
    (doit2 nil '(1) #(0))
    (doit2 nil '(1 2) #(1))
    (doit2 nil #(1 2) '(1))
    ;; CL:LIST / immutable vector
    (doit2 nil '(1) (vector 0))
    (doit2 nil #(1 2) (vector 1))
    (doit2 nil (vector 1 2) #(1))
    (doit2 nil (vector 1 2) '(1))
    (doit2 t [] (list))
    ;; CL:VECTOR / immutable list
    (doit2 t (list 1 2) [1 2])
    (doit2 nil (list 1) [1 2])
    ;; CL:VECTOR / immutable vector
    (doit2 t #() (vector))
    (doit2 t #(1) (vector 1))
    (doit2 t (cl-vector 1 2) (vector 1 2))
    (doit2 nil (cl-vector 1) (vector 1 2))
    (doit2 nil (cl-vector) (vector 1 2))
    ;; CL:VECTOR / immutable list
    (doit2 t #() (list))
    (doit2 t #(1 2 3) (list 1 2 3))
    (doit2 nil #(1 2) (list 1 2 3))
    (doit2 nil #(1 2 3) (list 1 2))
    (doit2 t #(1) (list 1))
    (doit2 nil #() (list 1))
    (doit2 nil #(1) (list))
    ;; immutable list / immutable list
    (doit2 t (list) (list))
    (doit2 t (list 1) (list 1))
    (doit2 t (list 2) (list 2))
    (doit2 t (list 1 2) (list 1 2))
    (doit2 nil (list) (list 1))
    (doit2 nil (list) (list 1 2))
    (doit2 nil (list 1) (list 2))
    (doit2 nil (list 1 2 4) (list 1 2 3))
    (doit2 nil (list 1 2 3 4) (list 1 2 3))
    ;; persistentlist/persistenqueue / fset collections
    (doit2 t [1 2] (list 1 2))
    (doit2 t [1 2] (queue 1 2))
    (doit2 nil [1] (list 1 2))
    (doit2 nil [1] (queue 1 2))
    (doit2 t {:a 1} (list (list :a 1)))
    (doit2 t {:a 1} (list (queue :a 1)))
    (doit2 t (list 1) #{1})
    (doit2 t (queue 1) #{1})
    (doit2 nil (list 2) #{1})
    (doit2 nil (queue 2) #{1})
    ;; SET/SET comparisons - relies on custom/expensive FSET-SET-EQUIV? logic
    (doit2 t #{(list :a 1)} #{(cl:list :a 1)})    
    (doit2 nil #{(list :a 1)} #{(cl:list :a 2)})    
    ;; Lazyseqs & immutable lists
    (doit2 t (nums 0) (list))
    (doit2 t (nums 1) (list 1))
    (doit2 t (nums 2) (list 2 1))
    (doit2 nil (nums 1) (list 0))
    (doit2 nil (nums 0) (list 1))
    (doit2 nil (nums 2) (list 1))
    (doit2 nil (nums 1) (list 1 2))
    ;; Lazyseqs & mutable lists (other lazyseq tests in lazyseq unit)
    (doit2 t (nums 1) '(1))
    (doit2 t (nums 2) '(2 1))
    (doit2 nil (nums 1) '(0))
    (doit2 nil (nums 0) '(1))
    (doit2 nil (nums 2) '(1))
    (doit2 nil (nums 1) '(1 2))
    ;; 3-way
    (doit3 t '(1 2) #(1 2) (vector 1 2))
    (doit3 nil '(1) #(1 2) (vector 1 2 3))
    (doit3 nil '() #() (vector))
    (doit3 t '(2 1 0) (cl-vector 2 1 0) '(2 1 0))
    ;; Nested objects
    (doit2 t (cl:list (cl:list 1 2) (cl:list 3 4)) (cl:list (vector 1 2) (cl-vector 3 4))) 
    (doit2 nil (cl:list (cl:list 1 2) (cl:cons 3 4)) (cl:list (vector 1 2) (cl-vector 3 4)))
    (doit2 nil (cl:list (cl:list 1 2) (cl:cons 3 4)) (cl:list (vector 1 2) (vector 3 4)))
    (doit2 nil (cl:list (cl:list 1 2) (cl:cons 3 4)) (cl:list (vector 1 2) (cl:list 3 4)))
    (doit3 t 
           (cl:list (cl:list 1 2 3) (vector 3 4 5) (cl-vector 3 4 5)) 
           (cl:list (cl:list 1 2 3) (vector 3 4 5) (cl-vector 3 4 5))
           (cl:list (cl:list 1 2 3) (cl-vector 3 4 5) (vector 3 4 5)))
    (doit3 nil
           (cl:list (cl:list 1 2 3 4) (vector 3 4 5) (cl-vector 3 4 5)) 
           (cl:list (cl:list 1 2 3) (vector 3 4 5) (cl-vector 3 4 5))
           (cl:list (cl:list 1 2 3) (cl-vector 3 4 5) (vector 3 4 5)))
    (doit3 nil
           (cl:list (cl:list 1 2 3) (vector 3 4 5 6) (cl-vector 3 4 5)) 
           (cl:list (cl:list 1 2 3) (vector 3 4 5) (cl-vector 3 4 5 6))
           (cl:list (cl:list 1 2 3) (cl:list 2 3 4 5) (cl-vector 3 4 5)))

    ;; hashtables/maps
    (doit2 t {:a 1 :b 2} {:b 2 :a 1})
    (doit2 t (cl-hash-map :a 1 :b 2) (cl-hash-map :b 2 :a 1))
    (doit2 t {:a 1 :b 2} (cl-hash-map :a 1 :b 2))
    (doit2 nil {:a 1} {:a 1 :b 2})
    (doit2 nil (cl-hash-map :a 1) (cl-hash-map :a 1 :b 2))
    (doit2 nil {:a 3} {:a 1})
    (doit2 nil (cl-hash-map :a 3) (cl-hash-map :a 1))
    (doit2 nil {:a 1} (cl-hash-map :a 1 :b 2))
    (doit2 nil {:a 1} (vector (vector :a 1)))
    (doit2 nil {:a 1} '((:a . 1)))
    (doit2 nil {:a 1} '((:a 1)))
    ;; Some nested structures
    (doit2 t {:a 1 :b {:c '(1 2 3)}} (cl-hash-map :a 1 :b (cl-hash-map :c #(1 2 3))))
    (doit2 nil {:a 1 :b {:c '(1 2 3)}} (cl-hash-map :a 1 :b (cl-hash-map :c #(1 2 4))))
    ;; Map comparisons that FSet won't do.
    (doit2 t {:a '(1 2) :b 2} {:a (list 1 2) :b 2}) ; comparing cl:list to clj-col::list
    (doit2 nil {:a '(1 2) :b 2} {:a (list 1 2 3) :b 2}) ; comparing cl:list to clj-col::list
    (doit2 t {:a (cl-hash-map :b 2) :b 2}           ;fsetmap with cl-map values
           (hash-map :a (cl-hash-map :b 2) :b 2))
    (doit2 nil {:a (cl-hash-map :b 2) :b 2}           ;fsetmap with cl-map values
           (hash-map :a (cl-hash-map :b 3) :b 2))
    (doit2 t {:a (cl-hash-map :b 2) :b 2} ;fsetmap to cl:hash-table with cl-map values
           (cl-hash-map :a (cl-hash-map :b 2) :b 2))
    (doit2 t {:a (cl-hash-map :d 4 :b 2) :b 2} ;fsetmap to cl:hash-table with cl-map values
           (cl-hash-map :a (cl-hash-map :b 2 :d 4) :b 2))
    (doit2 nil {:a (cl-hash-map :b 2) :b 2} ;fsetmap to cl:hash-table with cl-map values
           (cl-hash-map :a (cl-hash-map :b 3) :b 2))
    (doit2 nil {:a (cl-hash-map :b 2) :b 2} ;fsetmap to cl:hash-table with cl-map values
           (cl-hash-map :a (cl-hash-map :b 2 :c 3) :b 2))

    ;; Classes/Structs
    ;; ABCL fails on the code for structure-slots as of ABCL 1.9.2 so I've disabled this test.
    ;; I haven't separated out the tests for CLOS instance tests, they may or may not work.
    #+ABCL
    (format t "~%CLJ-COLL structure equivalence fails on ABCL and tests have been disabled.
CLOS instance equivalence has not been separately tested yet.~%")
    #-ABCL
    (let* ((x1 (cl:list (make-test-struct :a :a :b (cl:list :b) :c (vector :c))
                     (make-instance 'test-class :a (hash-map :a 1) :b #(:b 2) :c "c")
                     (make-instance 'test-class-2
                                    :a "a" :b "b" :c (vector :c)
                                    :d (vector :d) :e (cl-hash-map :e 5 :f 6))))
           ;; equal?, but not no eq on first list element
           (x2 (cl:list* (make-test-struct :a :a :b (cl:list :b) :c (vector :c)) (cl:cdr x1)))
           ;; Different :B value
           (x3 (cl:list* (make-test-struct :a :a :b (cl:list :d) :c (vector :c)) (cl:cdr x1)))
           ;; Same content as x1 except that one is a class not a struct.
           (x4 (cl:list* (make-instance 'test-class :a :a :b (cl:list :b) :c (vector :c))))
           )

      (is (eq *compare-objects-by-value* nil)) ;testing default value assumption

      (let ((*compare-objects-by-value* t))
        (doit2 t x1 x1)
        (doit2 t x1 x2)                 ;equivalent comparing slots
        (doit2 nil x1 x3)
        (doit3 nil x1 x2 x3)
        (doit3 nil x1 x2 x4)) ; structure objects do not compare to standard-objects

      (let ((*compare-objects-by-value* nil)) ;the default
        (doit2 t x1 x1)
        (doit2 nil x1 x2)               ;must be eq
        (doit2 nil x1 x3)
        (doit3 nil x1 x2 x3)
        (doit3 nil x1 x2 x4)) ; structure objects do not compare to standard-objects

      ;; Equality comparing only slots a and c
      (flet ((ac= (s1 s2) (equal? (slot-value s1 'a) (slot-value s2 'a))))
        ;; okay for structs to differ on slot B, but not classes
        (let ((*structure-object-equality-fn* #'ac=))
          (is (equal? (make-test-struct :a 1 :b (cl:list :b) :c (vector :c))
                      (make-test-struct :a 1 :b 'not-b :c (vector :c))))
          (is (not (equal? (make-instance 'test-class :a 1 :b (cl:list :b) :c (vector :c))
                           (make-instance 'test-class-2 :a 1 :b 'not-b :c (vector :c))))))
        ;; okay for class instances to differ on slot b, but not structs
        (let ((*standard-object-equality-fn* #'ac=))
          (is (not (equal? (make-test-struct :a 1 :b (cl:list :b) :c (vector :c))
                           (make-test-struct :a 1 :b 'not-b :c (vector :c)))))
          (is (equal? (make-instance 'test-class :a 1 :b (cl:list :b) :c (vector :c))
                      (make-instance 'test-class-2 :a 1 :b 'not-b :c (vector :c))))))))

  ;; This is correct, the NIL is not equivalent to the empty lazyseq in Clojure.
  (is (null (equal? (seq []) (doall (map #'identity (seq []))))))
  ;; non-lazy seq compares to lazyseq, clojure compatible
  (is (equal? (seq [1 2 3]) (doall (map #'identity (seq [1 2 3])))))
  )

(test hash-table-key-equality
  (let* ((map-key {:a 1})
         (vec-key [2])
         (im {map-key 1 vec-key 2})
         (stock-mm (cl-hash-map map-key 1 vec-key 2))
         (mm (alexandria:copy-hash-table stock-mm)))
    ;; FSET maps can deal with broader equality tests.
    (is (equal? {{:a 1} 3 (vector 2) 2} ;nice non-eq broad equivalance
                (assoc im {:a 1} 3)))
    ;; Welcome to the land of disappointment. CL:EQUAL (or EQUALP) can't understand
    ;; our desire for `{:a 1}` to be equal as keys, and we can't portably specify
    ;; `CLJ-COLL:EQUAL?` as with CL:HASH-TABLE, we'd have to use some other
    ;; hash table implementation.  For now, use CLJ-COLL:HASH-MAP if you
    ;; want broader HT key equality semantics.
    (let ((new-key {:a 1}))
      ;; These are redundant, for illustrative purposes
      (is (not (equal? {{:a 1} 3 (vector 2) 2} ; that worked with hash-map above
                       (assoc mm {:a 1} 3))))   ; but isn't going to work with CL:HASHTABLE
      (setq mm (alexandria:copy-hash-table stock-mm))
      (is (equal? (cl-hash-map map-key 1 vec-key 2 new-key 3) 
                  (assoc mm new-key 3))))

    ;; Can FSET do key equality tests on CL:HASHTABLE and CL:VECTOR types in FSET:MAPs?  No
    ;; FSet:equal won't do this, but equal? will. FSet:equal tests cl:hash-tables with EQ.
    (is (equal? (hash-map (cl-hash-map :a 1) (cl-vector 2))
                (hash-map (cl-hash-map :a 1) (cl-vector 2))))
    
    ;; So the moral of the story is: CL types as keys in FSET maps must be EQUAL.
    ;; Fset types as keys in Fset maps must be FSET:EQUAL?.  
    ;; So let it be written, so let it be done.
    ;; See README.md for hashtable key equality notes on how these restructions 
    ;; may be overcome.
    ))

(test coll? ;&collp
  (let* ((immutable-colls (cl:list (list 1) [1] {:a 1} #{1} (nums 1)))
         (mutable-colls (cl:list '(1) #(1) (cl-hash-map :a 1)))
         (seqs (mconcat 'cl:list 
                        (cl-map 'cl:list #'seq immutable-colls)
                        (cl-map 'cl:list #'seq mutable-colls))))
    (loop for coll in immutable-colls
          do (is (coll? coll))
             (is (collp coll)))
    (loop for coll in mutable-colls
          do (is (not (coll? coll)))
             (is (collp coll)))
    (loop for coll in seqs
          do (is (or (cl:consp coll) (coll? coll))) ;(coll? <cl:cons>) is nil for now as explained in docstring.
             (is (collp coll)))))

(test collector
  ;; Type symbol tests
  ;; Sequential
  (loop for symbol in '(cl:list cl:vector clj-coll::persistentlist fset:seq
                        clj-coll::seq clj-coll::lazyseq)
        as c = (clj-coll::make-collector symbol)
        do (clj-coll::collect c 1)
           (clj-coll::collect c 2)
           (is (equal? '(1 2) (clj-coll::grab c)))
           (case symbol
             ((clj-coll::seq clj-coll::lazyseq)
              (is (typep (clj-coll::grab c) 'clj-coll::persistentlist)))
             (t (is (typep (clj-coll::grab c) symbol))))
           (is (-> c clj-coll::reset clj-coll::grab empty?)))
  ;; Set
  (let ((c (clj-coll::make-collector 'fset:set)))
    (clj-coll::collect c 1)
    (clj-coll::collect c 2)
    (is (equal? #{1 2} (clj-coll::grab c)))
    (is (set? (clj-coll::grab c)))
    (is (-> c clj-coll::reset clj-coll::grab empty?)))

  ;; Hashtable/map
  (loop for symbol in '(cl:hash-table fset:map)
        as c = (clj-coll::make-collector symbol)
        do (clj-coll::collect c '(:a 1))
           (clj-coll::collect-kv c :b 2)
           (is (equal? {:a 1 :b 2} (clj-coll::grab c)))
           (is (-> c clj-coll::reset clj-coll::grab empty?)))

  ;; PREPEND for lists
  (loop for prepend in '(nil t)
        as result = (if prepend '(2 1) '(1 2))
        do
           (loop for symbol in '(cl:list clj-coll::persistentlist
                                 clj-coll::seq clj-coll::lazyseq)
                 as c = (clj-coll::make-collector symbol :prepend prepend)
                 do (clj-coll::collect c 1)
                    (clj-coll::collect c 2)
                    (is (equal? result (clj-coll::grab c)))
                    (clj-coll::reset c)
                    (is (empty? (clj-coll::grab c)))
                    (clj-coll::collect c 1)
                    (clj-coll::collect c 2)
                    (is (equal? result (clj-coll::grab c)))))

  ;; Collection type determined from objects
  (loop for object in (cl:list () (cl-vector) (list) [] (seq []) (nums 0))
        as c = (clj-coll::make-collector object)
        do (clj-coll::collect c 1)
           (clj-coll::collect c 2)
           (is (equal? '(1 2) (clj-coll::grab c)))
           ;; Caution comparing array types.
           ;; (cl:vector t <rank>) and different ranks won't compare as identical types
           ;; (subtypep '(cl:vector t) '(cl:vector t)) => T,T
           ;; (subtypep '(cl:vector t 1) '(cl:vector t 8)) => NIL,T
           ;; (subtypep '(cl:vector t 1) '(cl:vector t *)) => T,T ;because * is "unspecified"
           (typecase object
             (null (is (typep (clj-coll::grab c) 'cl:list))) ;NIL treated as empty list.
             (cl:vector (is (typep (clj-coll::grab c) 'cl:vector)))
             ((or clj-coll::seq clj-coll::lazyseq clj-coll::ipersistentlist) ;incl *EMPTY-LIST*
              (is (typep (clj-coll::grab c) 'clj-coll::ipersistentlist)))
             (t (is (typep (clj-coll::grab c) (type-of object)))))
           (is (-> c clj-coll::reset clj-coll::grab empty?)))
  ;; Set
  (let ((c (clj-coll::make-collector #{})))
    (clj-coll::collect c 1)
    (clj-coll::collect c 2)
    (is (equal? #{1 2} (clj-coll::grab c)))
    (is (set? (clj-coll::grab c)))
    (is (-> c clj-coll::reset clj-coll::grab empty?)))

  ;; Hashtable/map
  (loop for object in (cl:list (cl-hash-map) (hash-map))
        as c = (clj-coll::make-collector object)
        do (clj-coll::collect c '(:a 1))
           (clj-coll::collect-kv c :b 2)
           (is (equal? {:a 1 :b 2} (clj-coll::grab c)))
           (is (typep (clj-coll::grab c) (type-of object)))
           (is (-> c clj-coll::reset clj-coll::grab empty?)))
  )

(test copy
  (flet ((doit (c) 
           (let ((r (clj-coll::copy c)))
             (is (equal? c r))
             (if (or (coll? c) (and (atom c) (seq? c)) (null c))
                 (is (eq c r))
                 (is (not (eq c r)))))))
    (doit (cl:list 1 2))
    (doit (cl-vector 1 2))
    (doit (cl-hash-map :a 1 :b 2))
    (doit (seq (cl:list 1 2)))
    (doit (seq (cl-vector 1 2)))
    (doit (seq (cl-hash-map :a 1 :b 2)))
    (doit (list 1 2))
    (doit (vector 1 2))
    (doit (hash-map :a 1 :b 2))
    (doit (nums 0))
    (doit (seq (list 1 2)))
    (doit (seq (vector 1 2)))
    (doit (seq (hash-map :a 1 :b 2)))
    (doit (seq (nums 0))))

  ;; CL:VECTOR and CL:HASHTABLE types should preserve content and attributes
  ;; _except_ for unused capacity.
  (let* ((v (make-array 10 :initial-element 0 :adjustable t :fill-pointer 4))
         (c (clj-coll::copy v)))
    (is (= (array-dimension v 0) 10))
    (is (= (array-dimension c 0) 4)))

  ;; Trying to use conservative and multi-implementation-workable assertions
  ;; on hash table size.
  (let ((ht (make-hash-table :test 'equal :size 100)))
    (assoc ht :a 1 :b 2 :c 3 :d 4)
    (is (= 4 (count ht)))
    (is (>= (cl:hash-table-size ht) 100)) ;may fail on some platforms, tbd
    (let ((c (clj-coll::copy ht)))
      (is (= 4 (count c)))
      (is (< (cl:hash-table-size c) 100))))
  )

(test iterator
  (loop for coll in (cl:list '(1 2 3) (list 1 2 3) (queue 1 2 3) 
                             #(1 2 3) (vector 1 2 3))
        do (loop with iter = (clj-coll::iterator coll 'eof)
                 for i from 1 to 4
                 as val = (funcall iter)
                 do (if (= i 4)
                        (is (eq val 'eof))
                        (is (= val i)))))
  (loop for coll in (cl:list #{1 2 3} (nums 3) (seq [1 2 3])
                             {:a 1 :b 2 :c 3} 
                             (cl-hash-map :a 1 :b 2 :c 3))
        do (loop with iter = (clj-coll::iterator coll 'eof)
                 for i from 1
                 as val = (funcall iter)
                 until (eq val 'eof)
                 finally (is (= i 4)))))
(test mapf
  (flet ((doit (coll) 
           (let (result)
             (mapf (lambda (x) (push x result)) coll)
             (nreverse result))))
    (is (equalp '(1 2 3) (doit '(1 2 3))))
    (is (equalp '(1 2 3) (doit #(1 2 3))))
    (is (equalp '(1 2 3) (doit (vector 1 2 3))))
    (is (set= '((:a 1) (:b 2)) (doit (cl-hash-map :a 1 :b 2))))
    (is (set= '((:a 1) (:b 2)) (doit (hash-map :a 1 :b 2))))
    ))

;; defn-ish tests, is a top level defun
(clj-coll::defn-ish add
  "add one or two numbers"
  (() 0)
  ((a) a)
  ((a b) (declare (fixnum a b)) (+ a b)))

(clj-coll::defn-ish add2or3
  ((a b) (+ a b))
  ((a b & c) (apply #'+ a b c)))

(clj-coll::defn-ish add2or3-w-args
    args
  ((a b) (+ a b))
  ((a b & c)
   (declare (ignorable a b c))
   (+ (apply #'+ args))))

;; With docstring and &rest varname
(clj-coll::defn-ish add-n-doc-and-vars
    "adding" args
  ((a b) (+ a b))
  ((a b & c)
   (+ (apply #'+ a b c) (apply #'+ args))))
    

(test defn-ish
  (is (= 0 (add)))
  (is (= 2 (add 2)))
  (is (= 5 (add 2 3)))
  (is (= 5 (add2or3 2 3)))
  (is (= 10 (add2or3 1 2 3 4)))
  (is (= 10 (add2or3-w-args 1 2 3 4)))
  (is (equal "adding" (documentation 'add-n-doc-and-vars 'function)))
  (is (= 20 (add-n-doc-and-vars 1 2 3 4)))
)

(test mconcat
  (is (null (mconcat)))
  (is (equalp #() (mconcat 'cl:simple-vector)))
  (is (equal "abc" (mconcat 'cl:string "ab" "c")))
  (is (equalp #(#\a #\b #\c) (mconcat 'cl:vector "ab" "c")))
  (is (equalp #(1 2 3 4 5) (mconcat 'cl:vector [1 2] [3] #(4) '(5))))

  (loop for colla in (cl:list '(1 2 3) #(1 2 3) (list 1 2 3) [1 2 3])
        do
        (loop for collb in (cl:list '(4 5 6) #(4 5 6) (list 4 5 6) [4 5 6])
              do (is (equalp '(1 2 3 4 5 6) (mconcat 'cl:list colla collb)))
                 (is (equalp '(1 2 3 4 5 6) (mconcat colla collb)))
                 (is (equalp #(1 2 3 4 5 6) (mconcat 'cl:simple-vector colla collb)))
                 (is (equalp #(1 2 3 4 5 6) (mconcat 'cl:vector colla collb)))
                 (is (equalp #(1 2 3 4 5 6 7) 
                             (let ((v (mconcat 'cl:vector colla collb)))
                               (vector-push-extend 7 v)
                               v)))))

  (is (equalp '(1 2 3 4 5 6 2 1) (mconcat '(1 2) () [3 4] #(5) #() (seq (list 6)) (nums 2))))

  (let* ((l '(1 2 3))
         (r (mconcat l)))
    (is (not (eq r l)))
    (is (equalp r l))))


(test concat
  (is (equal? [1 2] (concat [1 2])))
  (is (equal? [2 1] (concat (nums 2))))
  (is (equal? [1 2 3 4] (concat [1 2] [3 4])))
  (is (equal? (list 2 1 3 4) (concat (nums 2) #(3 4))))
  (is (equal? [2 1 3 4 5 6] (concat (nums 2) #(3 4) [5 6])))
  (is (equal? [2 1 3 4 5 6 7 8] (concat (nums 2) #(3 4) [5 6] '(7 8))))

  (is (equal? [2 1 3 4] (concat (nums 2) #(3 4) [])))
  (is (equal? [2 1 5 6] (concat (nums 2) #() [5 6])))
  (is (equal? [3 4 5 6] (concat (nums 0) #(3 4) [5 6])))
  (is (equal? [] (concat (nums 0) #() [])))
  (is (equal? [] (concat (nums 0) #())))
  (is (equal? [] (concat (nums 0)))))

(test doseq
  (let (r) (doseq (x '(1 2 3)) (push x r)) (is (equalp '(3 2 1) r)))
  (let (r) (doseq (x '(1 2 3) :when (odd? x)) (push x r)) (is (equalp '(3 1) r)))
  ;; Invalid keywords
  (signals error (macroexpand '(doseq (x '(1 2 3) :until (= x 1)))))
  ;; Unsupported > 1 loop sequence (for now)
  (signals error (macroexpand '(doseq (x '(1 2 3) y '(2 3 4)) (print x) (print y))))
  ;;  No errors, just checking validity of our prior two 'signals' assertions
  (finishes (macroexpand '(doseq (x '(1 2 3)) (print x))))

  ;; Also orders :while before :when for cl:loop generation
  (let (r) (doseq (x '(1 2 3) :when (odd? x) :while (< x 3)) (push x r)) (is (equalp '(1) r)))
  ;; multiple tests
  (let ((r 0))
    (labels ((sidenums (n) (lazy-seq (incf r) (if (< n 1) nil (cons n (sidenums (1- n)))))))
      ;; no tests, no body
      (doseq (x (sidenums 3)))          ;side effects fire
      (is (= 4 r))
      (setf r 0)
      (doseq (x (sidenums 3) :while (< x 0))) ;one lazyseq node must be realized for first x tested
      (is (= 1 r))))

  ;; Various collection types
  (loop for input in (cl:list '(1 2) #(1 2) (list 1 2) [1 2])
        as r = nil
        do (doseq (x input) (push x r)) 
           (is (equal '(2 1) r)))
  (is (equal? #{1 2}
              (let (r) (doseq (x #{1 2}) (push x r)) (set r))))
  (is (equal? #{'(:a 1) '(:b 2)}
              (let (r) (doseq (x (cl-hash-map :a 1 :b 2)) (push x r)) (set r))))
  (is (equal? #{[:a 1] [:b 2]}
              (let (r) (doseq (x {:a 1 :b 2}) (push x r)) (set r)))))


(test mapv
  ;; single-sequence mapv, always vector returns
  ;; Really testing mapv1 here.

  ;; No Hashtables
  (loop for seq in *similar-sequences*
        do (is (coll= (vector 2 3 4) (mapv #'1+ seq))))
  (loop with expected = (vector 2)
        for seq in *overlapping-sequences*
        do (is (coll= expected (mapv #'1+ seq)))
           (setq expected (conj expected (1+ (last expected)))))
  ;; Sets
  (is (equal? #{1 2 3 4 5} (set (mapv #'cl:identity #{5 1 4 2 3}))))
  ;; Hashtables/maps
  (is (set= (vector (vector :a 1) (vector :b 2) (vector :c 3))
            (mapv #'identity (hash-map :a 1 :b 2 :c 3))))
  (is (set= (vector (vector :a 1) (vector :b 2) (vector :c 3))
            (mapv #'identity (cl-hash-map :a 1 :b 2 :c 3))))
  (is (coll= '(t t t) (mapv #'consp (cl-hash-map :a 1 :b 2 :c 3))))
  ;; Multi-sequence mapv
  (loop for seq in *similar-sequences*
        do (is (coll= (vector 2 4 6) (mapv #'+ seq seq))))
  (loop for seq in *similar-sequences*
        do (is (coll= (vector 3 6 9) (mapv #'+ seq seq seq))))
  ;; Mismatched lengths
  (is (coll= (vector 5) (apply #'mapv #'+ *overlapping-sequences*)))
  (is (coll= (vector 4 8) (apply #'mapv #'+ (cl:reverse (cdr *overlapping-sequences*)))))

  ;; cl-map with non-vector return types
  (is (coll= (cl-vector 4 8) (apply #'cl-map 'cl:vector #'+ (cdr *overlapping-sequences*))))
  (is (coll= (cl:list 4 8) (apply #'cl-map 'cl:list #'+ (cdr *overlapping-sequences*))))
  (is (equal? #{0 1 2 3} (cl-map 'fset:set #'1- #{2 4 3 1})))
  (is (every? (lambda (v) (< 3 v 13))
              (mapv #'+ #{1 2 3} #(3 2 1) '(2 3 1) #{2 3 1})))

  ;; mapv on seqs using iterators (2+ colls required)
  (is (equal? [[1 2 3]
               [4 5 2]
               [7 8 1]]
               ;; vec, seq, lazyseq
              (mapv #'vector [1 4 7] (seq #(2 5 8)) (nums 3))))
  (is (equal? [[1 2 3]
               [4 5 2]
               [7 8 1]]
               ;; seq, cl:vector, cl:list
              (mapv #'vector [1 4 7] (seq #(2 5 8)) '(3 2 1))))
  (is (equal? [[1 2 3]
               [4 5 2]
               [7 8 1]]
               ;; seq on cl:list, ipersistentlist, seq on persistent list
              (mapv #'vector (seq '(1 4 7)) (list 2 5 8) (seq (list 3 2 1)))))
  (is (equal? [[1 2]
               [4 5]
               [7 8]]
               ;; seq on persistent cons, seq cons
              (mapv #'vector 
                    (cons 1 (cons 4 (cons 7 nil)))
                    (seq (cons 2 (cons 5 (cons 8 nil)))))))
  )

(test cl-map
  ;; Mostly tested indirectly via MAPV
  (is (null (cl-map nil #'identity [1 2 3]))))

(test run
  (flet ((doit (coll)
           (serapeum:with-collector (c)
             (clj-coll:run! (lambda (x) (c x)) coll))))
    (is (equal? '(1 2 3) (doit '(1 2 3))))
    (is (equal? '(1 2 3) (doit #(1 2 3))))
    (is (equal? '(1 2 3) (doit [1 2 3])))
    (is (equal? #{1 2 3} (set (doit #{1 2 3}))))
    (is (equal? '(3 2 1) (doit (nums 3))))
    (let ((r (doit (cl-hash-map :a 1 :b 2))))
      (is (consp (first r)))
      (is (equal? #{'(:a 1) '(:b 2)} (set r))))
    (let ((r (doit (hash-map :a 1 :b 2))))
      (is (vector? (first r)))
      (is (equal? #{[:a 1] [:b 2]} (set r))))))

(test reduce-kv
  ;; for vector/map/hash-table/cl:vector
  (flet ((adder (r k v) (declare (ignore k)) (+ r v)))
    (is (= 6 (reduce-kv #'adder 0 #(1 2 3))))
    (is (= 6 (reduce-kv #'adder 0 (vector 1 2 3))))
    (is (= 6 (reduce-kv #'adder 0 (cl-vector 1 2 3))))
    (is (= 6 (reduce-kv #'adder 0 (hash-map :a 1 :b 2 :c 3))))
    (is (= 6 (reduce-kv #'adder 0 (cl-hash-map :a 1 :b 2 :c 3)))))
  (flet ((adder (r k v) (declare (ignore v)) (+ r k)))
    (is (= 3 (reduce-kv #'adder 0 #(1 2 3))))
    (is (= 3 (reduce-kv #'adder 0 (vector 1 2 3))))
    (is (= 3 (reduce-kv #'adder 0 (cl-vector 1 2 3))))
    (is (= 6 (reduce-kv #'adder 0 (hash-map 1 :a 2 :b 3 :c))))
    (is (= 6 (reduce-kv #'adder 0 (cl-hash-map 1 :a 2 :b 3 :c))))))

(test set-construction
  ;; and syntax, and some equality
  (let ((s1 (hash-set 1 2 3 1 2))
        (s2 (set '(1 2 3 1 2)))
        )
    (is (= (count s1) 3))
    (is (= (count s2) 3))
    (is (equal? s1))                    ;one-arg equal?
    (is (equal? s1 s2))
    (is (not (equal? s1 (hash-set 1 2))))
    (is (not (equal? s1 (hash-set 1 2 4))))
    (is (empty? (hash-set)))
    (is (empty? (set '())))
    (is (set? (set nil)))
    (is (equal? s1 s2 (set [1 2 3]) (set #(3 2 1))))
    ;; Test correct convertion of mapentries to vector types or lists
    (is (equal? #{ [ :A 1 ] [ :B 2 ] } (set {:a 1 :b 2})))
    (is (equal? #{ [ :A 1 ] [ :B 2 ] } (set (cl-hash-map :a 1 :b 2))))
    ;; Some set syntax testing
    (is (equal? s1 #{2 3 1}))
    (is (equal? #{} (set nil)))
    ;; Test our missing open brace error
    ;; I don't know why this is necessary, it only fails if I execute
    ;; the tests from a package other than CLJ-COLL-TEST.
    (let ((*readtable* (named-readtables:find-readtable 'readtable)))
      #+(OR)
      (format t "*readtable* ~s, *package* ~s, close-brace macro character: ~s~%"
              *readtable* *package*
              (get-macro-character clj-coll::+close-brace+ *readtable*))
      (signals error (read-from-string "}")))
    ;; Test our creation of a dispatching match char if we need it
    (let ((rt (copy-readtable *readtable*)))
      (set-macro-character #\# nil t rt)
      (enable-set-syntax rt)
      (let ((*readtable* rt))
        (is (equalp '(HASH-SET) (read-from-string "#{}"))))))

  ;; NIL input
  (let ((s (set nil)))
    (is (set? s))
    (is (empty? s)))

  ;; [lazy]seq inputs
  (let ((s (set (nums 3))))             ;lazyseq in
    (is (set? s))
    (is (= 3 (count s)))
    (is (equal? #{3 1 2} s))
    (is (equal? s (set (seq (nums 3)))))) ;seq in
  (let ((s (set (nums 0))))
    (is (empty? s))
    (is (set? s)))
  )

(test sets
  "Set tests aside from construction"
  (let ((s (set (list 1 2 3))))
    (is (= 3 (count s)))
    (is (set? s))
    (is (equal? (set '(1 2 3)) s))
    (is (equal? (set '(1 2 3)) (set (mapv #'identity s))))
    ;; seq on sets
    (is (equal? (set '(1 2 3)) (set (mapv #'identity (seq s)))))
    (let* ((vals #{0 1 2 3})
           (s (rest (seq vals))))
      (is (= 3 (count s)))
      (is (reduce (lambda (r v) 
                    (and r (contains? vals v)))
                  s)))
    ))

(test get
  (is (= 2 (get [1 2] 1)))
  (is (= 1 (get #(1 2) 0)))
  (is (null (get #(1 2) 2)))
  (is (= 1 (get {:a 1} :a)))
  (is (= 3 (get {:a 1} :b 3)))
  (is (null (get 1 1)))
  (is (null (get '(1 2 3) 1)))
  ;; Out of range returns not-found
  (is (null (get [0 1] 3)))
  (is (null (get #(0 1) 3)))
  ;; sets
  (is (= 2 (get #{1 2} 2)))
  (is (null (get #{1 2} 3)))
  (is (= 4 (get #{1 2} 3 4)))
  )

(test get-in
  (is (= 1 (get-in {:a 1} [:a])))
  (is (= 3 (get-in {:a 1 :b {:c 3}} [:b :c])))
  (is (= 4 (get-in {:a 1} #(:b) 4)))
  (is (null (get-in [1 2] [2])))
  ;; Can go more than one off the end, unlike assoc-in
  (is (null (get-in [1 2] [3])))
  (is (eq :foo (get-in [1 2] [4] :foo)))
  ;; Unsupported type, clojure quirks
  (is (null (get-in (nums 3) [1])))
  )

(test map-construction
  "Hash table/map creation"

  (let ((m {})
        (m2 (zipmap () ())))
    (is (typep m +immutable-map-type+))
    (is (empty? m))
    (is (typep m2 +immutable-map-type+))
    (is (empty? m2)))

  (let* ((clj-coll:*DEFAULT-HASHMAP-CONSTRUCTOR* 'serapeum:dict)
         (m {})
         (m2 (zipmap () ())))
    (is (typep m 'cl:hash-table))
    (is (typep m2 'cl:hash-table))
    (is (empty? m))
    (is (empty? m2)))

  (flet ((testmap (m)
           (is (null (set-difference '(:a :b :c) (clj-coll::convert 'cl:list (keys m)))))
           (is (eql 1 (get m :a)))
           (is (eql 2 (get m :b)))
           (is (eql 3 (get m :c)))
           (is (eql 5 (-> m (get :d) (get :e))))))
    (let ((m {:a 1 :b 2 :c 3 :d {:e 5}})
          (m2 (zipmap '(:a :b :c :d) (cl:list 1 2 3 {:e 5}))))
      (is (typep m +immutable-map-type+))
      (is (typep m2 +immutable-map-type+))
      (testmap m)
      (testmap m2))
    (let* ((clj-coll:*DEFAULT-HASHMAP-CONSTRUCTOR* 'serapeum:dict)
           (m {:a 1 :b 2 :c 3 :d {:e 5}})
           (m2 (zipmap '(:a :b :c :d) (cl:list 1 2 3 {:e 5}))))
      (is (typep m 'cl:hash-table))
      (is (typep m2 'cl:hash-table))
      (testmap m)
      (testmap m2)))

  ;; Some seqs for keys and vals
  (loop for keys in (cl:list '(:a :b :c) #(:a :b :c)
                             (list :a :b :c) (vector :a :b :c))
        for vals in (cl:list '(1 2 3) #(1 2 3) 
                             (list 1 2 3) (vector 1 2 3))
        do (let* ((*default-hashmap-constructor* 'cl-hash-map)
                 (ht (zipmap keys vals)))
             (is (cl:hash-table-p ht))
             (is (equal? {:a 1 :b 2 :c 3} ht)))
           (let ((map (zipmap keys vals)))
             (is (map? map))
             (is (equal? {:a 1 :b 2 :c 3} map))))
  
  ;; Check for mismatch number of keys and values
  (signals error (zipmap '(:a :b) '(1)))

  ;; Check for sets (or other unsequential inputs) being unsupported.
  (signals error (zipmap #{:a :b} '(1 2)))
  )

(test vector-construction-and-type-predicates
  ;; vector, vec, cl-vector, cl-vec
  ;; for all the different inputs, cl:list, cl:vector, fset:seq, fset:set, AND maps?
  ;; 
  ;; coll? coll-p vector? vector-p 

  ;; Simple inputs
  (loop for v in (cl:list (vector 1 2 3) (vec '(1 2 3)) (cl-vector 1 2 3) (cl-vec '(1 2 3)))
        do (is (coll= '(1 2 3) v))
           ;; The next 3 are more to test `coll=` than vectors.
           (is (not (coll= '(4 1 2 3) v)))
           (is (not (coll= '(1 2 3 4) v)))
           (is (not (coll= '(1 2) v)))
           (is (= 3 (count v)))
           (if (cl:vectorp v)
               (is (not (vector? v)))
               (is (vector? v)))
           (is (vectorp v))
           (if (cl:vectorp v)
               (is (not (coll? v)))
               (is (coll? v)))
           (is (collp v)))

  ;; Strings are atomic as inputs to vectors
  (loop for v in (cl:list (vector "abc") (vec '("abc")) (cl-vector "abc") (cl-vec '("abc")))
        do (is (coll= '("abc") v))
           (is (= 1 (count v)))
           (if (cl:vectorp v)
               (is (not (vector? v)))
               (is (vector? v)))
           (is (vectorp v))
           (if (cl:vectorp v)
               (is (not (coll? v)))
               (is (coll? v)))
           (is (collp v)))
  

  ;; Different vec/cl-vec input collection types, list inputs done above
  (loop for v in (cl:list (vec #(1 2 3)) (cl-vec #(1 2 3))
                       (vec (vector 1 2 3)) (cl-vec (vector 1 2 3))
                       (vec (cl-vector 1 2 3)) (cl-vec (cl-vector 1 2 3)))
        do (is (coll= '(1 2 3) v))
           (is (= 3 (count v)))
           (if (cl:vectorp v)
               (is (not (vector? v)))
               (is (vector? v)))
           (is (vectorp v))
           (if (cl:vectorp v)
               (is (not (coll? v)))
               (is (coll? v)))
           (is (collp v)))

  ;; Hashtable inputs
  (loop for v in (cl:list (vec (hash-map :a 1 :b 2 :c 3)) (vec (cl-hash-map :a 1 :b 2 :c 3))
                       (cl-vec (hash-map :a 1 :b 2 :c 3)) (cl-vec (cl-hash-map :a 1 :b 2 :c 3)))
        for i from 0
;; Two problems
;; 1. #[ #[ :A 1 ] #[ :B 2 ] #[ :C 3 ] ] not set= ((:A 1) (:B 2) (:C 3))

        do (is (set= '((:a 1) (:b 2) (:c 3)) v))
           (is (= 3 (count v)))
           (if (cl:vectorp v)
               (is (not (vector? v)))
               (is (vector? v)))
           (is (vectorp v))
           (if (cl:vectorp v)
               (is (not (coll? v)))
               (is (coll? v)))
           (is (collp v)))

  ;; Set inputs
  (is (equal? (set [1 2 3]) (set (vector 3 1 2))))

  ;; [lazy]seq inputs
  (let ((v1 (vec (nums 0)))
        (v2 (cl-vec (nums 0))))
    (is (empty? v1))
    (is (empty? v2))
    (is (= 0 (count v1)))
    (is (= 0 (count v2)))
    (is (vector? v1))
    (is (cl:vectorp v2)))

  (let ((v1 (vec (nums 3)))             ;lazyseq in
        (v2 (cl-vec (nums 3))))
    (is (vector? v1))
    (is (cl:vectorp v2))
    (is (= 3 (count v1)))
    (is (= 3 (count v2)))
    (is (fset:equal? [3 2 1] v1))
    (is (equal? [3 2 1] v2))                        ;fset doesn't compare cl-vectors
    (is (fset:equal? [3 2 1] (vec (seq (nums 3))))) ;seq in
    (is (equal? [3 2 1] (cl-vec (seq (nums 3))))))
  )

(test first
  (is (null (first nil)))
  (is (eql 1 (first '(1 2 3))))
  (is (eql 2 (first #(2 3 4))))
  (is (eql 3 (first (vector 3 4 5))))
  (is (or (equal '(:a 1) (first (cl-hash-map :a 1 :b 2)))
          (equal '(:b 2) (first (cl-hash-map :a 1 :b 2)))))
  (is (or (equal? (vector :a 1) (first (hash-map :a 1 :b 2)))
          (equal? (vector :b 2) (first (hash-map :a 1 :b 2)))))
  ;; Sets, assume no ordering
  (let* ((vals '(3 1 2 9))
         (s (set vals))
         (first (first s))
         (second (second s)))
    (is (member first vals))
    (is (member second vals))
    (is (/= first second)))
  (is (cl:null (first #{})))
  (is (= 1 (first #{1})))

  ;; Correct mapentry decoding
  (is (cl:listp (first (cl-hash-map :a 1))))
  (is (cl:listp (first (seq (cl-hash-map :a 1)))))
  (is (vector? (first (hash-map :a 1))))
  (is (vector? (first (seq (hash-map :a 1)))))

  ;; On sequential & set seqs
  (loop for coll in (cl:list '(1 2) #(1 2) (list 1 2) (vector 1 2) (hash-set 1 2) (nums 2))
        as result = (first (seq coll))
        do (cond ((clj-coll::lazyseq-p coll)
                  (is (= result 2)))
                 ((set? coll)
                  (is (or (= result 1) (= result 2))))
                 (t (is (= result 1)))))
  ;; On map seqs
  (loop for coll in (cl:list (cl-hash-map :a 1 :b 2) {:a 1 :b 2})
        as result = (first (seq coll))
        do (is (or (equal? [:a 1] result)
                   (equal? [:b 2] result))))
  )

(test map-entry?
  (is (map-entry? [:a 1]))
  (is (map-entry? (list :a 1)))
  (is (map-entry? '(:a 1)))
  (is (map-entry? #(:a 1)))
  (is (not (map-entry? (cl:cons :a 1))))
  (is (not (map-entry? [:a])))
  (is (not (map-entry? '(:a 1 :b)))))

(test rest
  (is (eq (list) (rest nil)))
  (is (null (first (list))))
  (is (null (first nil)))
  (is (eq (list) (rest (cons 1 nil))))
  (let ((l (rest (list 1))))
    (is (eq (list) l))
    (is (empty? l))
    (is (null (seq l))))

  (flet ((doit (coll)
           ;;(format t "coll=~s, type=~s~%" coll (type-of coll))
           (is (= 1 (first coll)))
           (is (= 2 (first (rest coll))))
           (is (equal? '(1 2) coll))
           (is (null (-> coll rest rest first)))
           (is (eq (list) (-> coll rest rest)))))
    (doit '(1 2)) 
    (doit (seq '(1 2)))
    (doit #(1 2))
    (doit (list 1 2))
    (doit (seq (list 1 2)))
    (doit (cons 1 (cons 2 nil)))
    (doit (cons 1 (seq (cons 2 nil))))
    (doit [1 2])
    (doit (seq [1 2]))
  ))

(test nthrest
  (is (equal? '(1 2 3) (nthrest '(1 2 3) 0)))
  (is (equal? '(2 3) (nthrest '(1 2 3) 1)))
  (is (equal? '(3) (nthrest '(1 2 3) 2)))
  (is (equal? (list) (nthrest '(1 2 3) 3)))
  (is (equal? '(2 1) (nthrest (nums 2) 0)))
  (is (equal? '(1) (nthrest (nums 2) 1)))
  (is (equal? (list) (nthrest (nums 2) 2)))
  (is (equal? '(2) (nthrest [1 2] 1)))
  (is (equal? (list) (nthrest [1 2] 2)))
  ;; a negative N
  (let ((l (list 1 2 3))
        (v [1 2 3]))
    (is (eq l (nthrest l -1)))
    (is (eq v (nthrest v -100)))
    (is (equal? [2 3] (nthrest v 1))))
  )

(test nthnext
  (is (equal? '(3 2 1) (nthnext (nums 3) 0)))
  (is (equal? '(2 1) (nthnext (nums 3) 1)))
  (is (equal? '(1) (nthnext (nums 3) 2)))
  (is (null (nthnext (nums 3) 3)))
  ;; vs nthrest which is the same except for the null case
  (let ((r (nthrest (nums 3) 3)))
    (is (empty? r))
    (is (list? r))))

(test range
  (is (equal? (list) (range 0 0 0)))
  (is (equal? '(0 0 0) (take 3 (range 0 1 0))))
  (is (equal? '(1 2 3) (range 1 4)))
  (is (equal "(list 3 3.5 4.0)"    ;caution for floating point number representations
             (with-output-to-string (s) 
               (princ (take 3 (range 3 6 0.5)) s))))
  (is (equal? '(10 9 8 7 6 5 4 3 2 1 0 -1 -2 -3 -4 -5 -6 -7 -8 -9)
              (range 10 -10 -1)))
  (is (equal? '(-5 -4 -3 -2 -1 0 1 2 3 4) (range -5 5)))
  (is (equal? '(-1 0 1) (take 3 (range -1 2 1))))
  (is (= 100 (count (take 100 (range)))))
  (is (= -99 (last (take 100 (range -1 -100 -1))))))

(test mrange
  (signals error (mrange 0 0 0))
  (is (equal nil (mrange 0 0 1)))
  (is (equalp #() (mrange 'cl:vector 0 0 1)))
  (is (equal '(1 2 3) (mrange 1 4)))
  (is (equal '(1 2 3) (mrange 'cl:list 1 4)))
  (is (equal "(list 3 3.5 4.0)"    ;caution for floating point number representations
             (with-output-to-string (s) 
               (princ (take 3 (mrange 3 6 0.5)) s))))
  (is (equal '(10 9 8 7 6 5 4 3 2 1 0 -1 -2 -3 -4 -5 -6 -7 -8 -9)
              (mrange 10 -10 -1)))
  (is (equal '(-5 -4 -3 -2 -1 0 1 2 3 4) (mrange -5 5)))
  (is (equalp #(-5 -4 -3 -2 -1 0 1 2 3 4) (mrange 'cl:vector -5 5)))
  (is (equal '(-1 0 1) (mtake 3 (mrange -1 2 1))))
  (is (= -99 (last (take 100 (mrange -1 -100 -1)))))
  (is (= -99 (last (mtake 100 (mrange 'cl:vector -1 -100 -1)))))
  )

(test mpartition
  ;; Pretty much copying partition tests, only the equality predicate is
  ;; equalp instead of equal?
  (is (equalp '((0 1 2) (3 4 5) (6 7 8)) (mpartition 3 (range 9))))
  (is (equalp '((0 1 2) (3 4 5)) (mpartition 3 (range 8))))
  (is (equalp '((0 1 2) (2 3 4) (4 5 6)) (mpartition 3 2 (range 7))))
  (is (equalp '((0 1) (1 2) (2 3) (3 4)) (mpartition 2 1 (range 5))))
  ;; With PAD
  (is (equalp '((0 1 2) (3 4 5) (6 7 99)) (mpartition 3 3 '(99) (range 8))))
  ;; Not all partition size if pad used
  (is (equalp '((0 1 2) (3 4 5) (6 99)) (mpartition 3 3 '(99) (range 7))))
  (is (equalp '((0 1 2 3) (6 7 8 9) (12 13 14 15) (18 19 "a" "b"))
              (mpartition 4 6 ["a" "b" "c" "d"] (range 20)))) ; doesn't use all
  (is (equalp NIL (mpartition 5 [1 2 3 4])))
  (is (equalp NIL (mpartition 5 5 [1 2 3 4])))
  ;; empty pads
  (is (equalp '((1 2 3 4)) (mpartition 5 5 nil [1 2 3 4]))) 
  (is (equalp '((1 2 3 4)) (mpartition 5 5 (list) [1 2 3 4])))
  ;; Zero partition size is invalid
  (signals error (mpartition 0 [1 2 3]))
  ;; Zero step is invalid
  (signals error (mpartition 2 0 '(1 2 3 4)))
  ;; Using of empty pad to drop periodic elements
  (is (equalp '((1 2 3) (5 6 7) (9)) (mpartition 3 4 nil (range 1 10))))
  ;; strings
  (is (equalp '(( #\a #\b #\c)) (mpartition 3 "abcde")))
  )

(test partition
  (is (equal? '((0 1 2) (3 4 5) (6 7 8)) (partition 3 (range 9))))
  (is (equal? '((0 1 2) (3 4 5)) (partition 3 (range 8))))
  (is (equal? '((0 1 2) (2 3 4) (4 5 6)) (partition 3 2 (range 7))))
  (is (equal? '((0 1) (1 2) (2 3) (3 4)) (partition 2 1 (range 5))))
  ;; With PAD
  (is (equal? '((0 1 2) (3 4 5) (6 7 99)) (partition 3 3 '(99) (range 8))))
  ;; Not all partition size if pad used
  (is (equal? '((0 1 2) (3 4 5) (6 99)) (partition 3 3 '(99) (range 7))))
  (is (equal? '((0 1 2 3) (6 7 8 9) (12 13 14 15) (18 19 "a" "b"))
              (partition 4 6 ["a" "b" "c" "d"] (range 20)))) ; doesn't use all
  (is (equal? (list) (partition 5 [1 2 3 4])))
  (is (equal? (list) (partition 5 5 [1 2 3 4])))
  ;; empty pads
  (is (equal? (list (list 1 2 3 4)) (partition 5 5 nil [1 2 3 4]))) 
  (is (equal? (list (list 1 2 3 4)) (partition 5 5 (list) [1 2 3 4])))
  ;; Zero partition size results in infinite sequence
  (is (equal? (list [] [] []) (take 3 (partition 0 [1 2 3]))))
  ;; Zero step size results in infinite sequence
  (is (equal? (list [1] [1] [1]) (take 3 (partition 1 0 [1 2 3]))))
  ;; Using of empty pad to drop periodic elements
  (is (equal? '((1 2 3) (5 6 7) (9)) (partition 3 4 nil (range 1 10))))
  ;; strings
  (is (equal? (list (list #\a #\b #\c)) (partition 3 "abcde")))
  )

(test partitionv
  (flet ((eqvec (expected result)
           (and (equal? expected result)
                (if (empty? result)
                    t
                    (every? #'vector? result)))))
    (is (eqvec '((0 1 2) (3 4 5) (6 7 8)) (partitionv 3 (range 9))))
    (is (eqvec '((0 1 2) (3 4 5)) (partitionv 3 (range 8))))
    (is (eqvec '((0 1 2) (2 3 4) (4 5 6)) (partitionv 3 2 (range 7))))
    (is (eqvec '((0 1) (1 2) (2 3) (3 4)) (partitionv 2 1 (range 5))))
    ;; With PAD
    (is (eqvec '((0 1 2) (3 4 5) (6 7 99)) (partitionv 3 3 '(99) (range 8))))
    ;; Not all partitionv size if pad used
    (is (eqvec '((0 1 2) (3 4 5) (6 99)) (partitionv 3 3 '(99) (range 7))))
    (is (eqvec '((0 1 2 3) (6 7 8 9) (12 13 14 15) (18 19 "a" "b"))
               (partitionv 4 6 ["a" "b" "c" "d"] (range 20)))) ; doesn't use all
    (is (eqvec (list) (partitionv 5 [1 2 3 4])))
    (is (eqvec (list) (partitionv 5 5 [1 2 3 4])))
    ;; empty pads
    (is (eqvec (list (list 1 2 3 4)) (partitionv 5 5 nil [1 2 3 4]))) 
    (is (eqvec (list (list 1 2 3 4)) (partitionv 5 5 (list) [1 2 3 4])))
    ;; Zero partitionv size results in infinite sequence
    (is (eqvec (list [] [] []) (take 3 (partitionv 0 [1 2 3]))))
    ;; Zero step size results in infinite sequence
    (is (eqvec (list [1] [1] [1]) (take 3 (partitionv 1 0 [1 2 3]))))
    ;; Using of empty pad to drop periodic elements
    (is (eqvec '((1 2 3) (5 6 7) (9)) (partitionv 3 4 nil (range 1 10))))
    ;; strings
    (is (eqvec (list (list #\a #\b #\c)) (partitionv 3 "abcde")))
    ))

(test partition-all ; & mpartition-all
  (is (equal? '((0 1 2) (3 4 5) (6 7 8)) (partition-all 3 (range 9))))
  (is (equal '((0 1 2) (3 4 5) (6 7 8)) (mpartition-all 3 (range 9))))
  (is (equal? '((0 1 2) (3 4 5) (6 7)) (partition-all 3 (range 8))))
  (is (equal '((0 1 2) (3 4 5) (6 7)) (mpartition-all 3 (range 8))))
  (is (equal? '((0 1 2) (2 3 4) (4 5 6) (6)) (partition-all 3 2 (range 7))))
  (is (equal '((0 1 2) (2 3 4) (4 5 6) (6)) (mpartition-all 3 2 (range 7))))
  ;; Zero partition size results in infinite sequence
  (is (equal? (list [] [] []) (take 3 (partition-all 0 [1 2 3]))))
  (signals error (mpartition-all 0 [1 2 3]))
  ;; Zero step size results in infinite sequence
  (is (equal? (list [1] [1] [1]) (take 3 (partition-all 1 0 [1 2 3]))))
  (signals error (mpartition-all 1 0 [1 2 3]))
  ;; strings
  (is (equal? (list (list #\a #\b #\c) (list #\d #\e)) (partition-all 3 "abcde")))
  (is (equal '((#\a #\b #\c) (#\d #\e)) (mpartition-all 3 "abcde")))
  ;; mpartition-all types
  (is (equalp #(#(0 1) #(3 4) #(6)) 
              (mpartition-all 2 3 (range 7) :result-type 'cl:vector :partition-type 'cl:vector)))
  ;; Transducer
  (let ((r (transduce (partition-all 2) #'conj (range 7))))
    (is (equal? [[0 1] [2 3] [4 5] [6]] r))
    (is (vector? r))
    (is (vector? (first r))))
  (is (equal? [[0] [1] [2] [3] [4] [5] [6]]
              (transduce (partition-all 1) #'conj (range 7))))
  (is (equal? [[0 1 2 3 4 5 6]]
              (transduce (partition-all 0) #'conj (range 7))))
  (is (equal? [[0]] (transduce (comp (take 1) (partition-all 3) (take 1))
                               #'conj [] (range 15))))
  (signals error (transduce (partition-all -1) #'conj (range 7)))
  ;; There is no way to make PARTITION-ALL tranducer create mutable partitions.
  (let ((r (transduce (partition-all 2) #'cl-conj (cl-vector) (range 7))))
    (is (equal? (cl-vector [ 0 1 ] [ 2 3 ] [ 4 5 ] [ 6 ]) r))
    (is (cl:vectorp r))
    (is (vector? (first r))))           ;sigh
  ;; Cue the MPARTITION-ALL transducer
  (let ((r (transduce (mpartition-all 2 :partition-type 'cl:vector) #'cl-conj (cl-vector) (range 7))))
    (is (equalp #(#(0 1) #(2 3) #(4 5) #(6)) r))
    (is (cl:vectorp r))
    (is (cl:vectorp (first r))))        ;yay
  ;; Testing multiple uses of transducers
  (let* ((t1 (partition-all 2))
         (r1a (transduce t1 #'conj (range 7)))
         (r1b (transduce t1 #'cl-conj (cl-vector) (range 7)))
         (t2 (partition-all 2))
         (r2a (transduce t2 #'conj (range 7)))
         (r2b (transduce t2 #'cl-conj (cl-vector) (range 7))))
    (is (equal? (cl-vector [ 0 1 ] [ 2 3 ] [ 4 5 ] [ 6 ]) r1a r1b))
    (is (equal? (cl-vector [ 0 1 ] [ 2 3 ] [ 4 5 ] [ 6 ]) r2a r2b)))
  )

(test partitionv-all
  (flet ((eqvec (expected result)
           (and (equal? expected result)
                (if (empty? result)
                    t
                    (every? #'vector? result)))))
    (is (eqvec '((0 1 2) (3 4 5) (6 7 8)) (partitionv-all 3 (range 9))))
    (is (eqvec '((0 1 2) (3 4 5) (6 7)) (partitionv-all 3 (range 8))))
    (is (eqvec '((0 1 2) (2 3 4) (4 5 6) (6)) (partitionv-all 3 2 (range 7))))
    ;; Zero partition size results in infinite sequence
    (is (eqvec (list [] [] []) (take 3 (partitionv-all 0 [1 2 3]))))
    ;; Zero step size results in infinite sequence
    (is (eqvec (list [1] [1] [1]) (take 3 (partitionv-all 1 0 [1 2 3]))))
    ;; strings
    (is (eqvec (list (list #\a #\b #\c) (list #\d #\e)) (partitionv-all 3 "abcde")))
    ;; Transducer
    (let ((r (transduce (partitionv-all 2) #'conj (range 7))))
      (is (eqvec [[0 1] [2 3] [4 5] [6]] r))
      (is (vector? r)))
    (is (eqvec [[0] [1] [2] [3] [4] [5] [6]]
               (transduce (partitionv-all 1) #'conj (range 7))))
    (is (eqvec [[0 1 2 3 4 5 6]]
               (transduce (partitionv-all 0) #'conj (range 7))))
    (signals error (transduce (partitionv-all -1) #'conj (range 7)))
    ;; There is no way to make PARTITIONV-ALL tranducer create mutable partitions.
    (let ((r (transduce (partitionv-all 2) #'cl-conj (cl-vector) (range 7))))
      (is (equal? (cl-vector [ 0 1 ] [ 2 3 ] [ 4 5 ] [ 6 ]) r))
      (is (cl:vectorp r))
      (is (vector? (first r))))         ;sigh
    ;; Skipped multiple use of transducer test since partitionv-all's transducer
    ;; is === partition-all's transducer, and that's already tested.
    ))

(test mpartition-by
  (loop for coll in (cl:list '(1 3 5 6 7 9 12 13)
                             #(1 3 5 6 7 9 12 13)
                             (list 1 3 5 6 7 9 12 13)
                             [1 3 5 6 7 9 12 13]
                             #{1 3 5 6 7 9 12 13})
        as result1 = (mpartition-by #'odd? coll)
        as result2 = (mpartition-by #'odd? (seq coll))
        ;; The set/coll test works here because fset:set is ordered
        ;; prepare for failure if you change set implementation.
        do (is (cl:listp result1))
           (is (cl:listp (first result1)))
           (is (equalp '((1 3 5) (6) (7 9) (12) (13)) result1))
           (is (equalp result1 result2)))

  (let ((r (mpartition-by (comp #'odd? #'second) (seq {:a 1 :b 2 :c 3}))))
    (is (equal? '(([:A 1]) ([:B 2]) ([:C 3])))) ;MapEntries as inputs still persistent
    (is (cl:listp r))
    (is (cl:listp (first r))))
  (let ((r (mpartition-by (comp #'odd? #'second) (seq (cl-hash-map :a 1 :b 2 :c 3)))))
    (is (equal? '(((:A 1)) ((:B 2)) ((:C 3)))))
    (is (cl:listp r))
    (is (cl:listp (first r)))
    (is (cl:listp (cl:caar r)))) ; what, no FIIRST in Clojure?  ;-)
  )

(test partition-by
  (loop for coll in (cl:list (list 1 3 5 6 7 9 12 13)
                             [1 3 5 6 7 9 12 13]
                             '(1 3 5 6 7 9 12 13)
                             #(1 3 5 6 7 9 12 13))
        do
           (let ((r (partition-by #'odd? (list 1 3 5 6 7 9 12 13))))
             (is (equal? (list '(1 3 5) '(6) '(7 9) '(12) '(13)) r))
             (is (coll? (first r)))     ;immutable
             (is (coll? (second r))))) 
  ;; Sets 
  (let* ((coll #{1 3 5 6 7 9 12 13})
         (result (partition-by #'odd? coll))
         (result (reduce (lambda (r partition)
                           (fset:union r (set partition)))
                         #{} result)))
    (is (fset:equal? result (set coll))))

  ;; List of maps
  (let ((result (partition-by (alexandria:compose #'odd? #'count)
                              (list {:a 1} {:b 2} {:c 3 :d 4})))
        ;; => (({:a 1} {:b 2}) ({:c 3, :d 4}))
        (p1 (set (cl:list {:a 1} {:b 2})))
        (p2 (cl:list {:c 3 :d 4})))
    (is (= 2 (count result)))
    (is (or (equal? p1 (set (first result)))
            (equal? p1 (set (second result)))))
    (is (or (equal? p2 (first result))
            (equal? p2 (second result)))))
  ;; Seqs on maps
  (is (equal? (list [[:a 1]] [[:b 2]] [[:c 3]]) 
              (partition-by (comp #'odd? #'second) (seq {:a 1 :b 2 :c 3}))))

  ;; Edge cases
  (let ((coll [2 4 6 8]))
    (is (equal? (list coll) (partition-by #'odd? coll))))
  (is (empty? (partition-by #'odd? [])))
  (is (equal? [[1]] (partition-by #'odd? [1])))
  (is (equal? [[2]] (partition-by #'odd? [2])))

  ;; partition-by as a transducer, and, importantly, with a completion step
  ;; also use of transducer twice
  (let* ((xform (partition-by #'odd?))
         (result2 (transduce xform #'conj [] '(1 2 3 3 4 5 5 6 7)))
         (result (transduce xform #'conj [] (list 1 2 3 4))))
    (is (vector? result))
    (is (vector? (first result)))
    (is (equal? [[1] [2] [3] [4]] result))
    (is (equal? [[1] [2] [3 3] [4] [5 5] [6] [7]] result2))
    (is (vector? (first result2))))

  ;; sequential/set results
  ;; All Clojure's partitions are vectors 
  ;; inter-partition order depends on the result collection type.
  ;; intra-partition order is alwayus the order of the input.
  (loop with input = '(1 3 4 6 7)
        with expected = '((1 3) (4 6) (7))
        with xform = (partition-by #'odd?)
        for init in (cl:list nil (cl-vector) (list) (vector) (hash-set))
        ;; NIL will result in a persistent list.
        for result-type in '(clj-coll::persistentlist cl:vector clj-coll::persistentlist
                             fset:seq fset:set)
        for expected-result in (cl:list (reverse expected) expected (reverse expected)
                                        expected (set expected))
        as result = (transduce xform #'conj init input)
        do ;;(format t "~%input=~s result=~s expected-result=~s~%" input result expected-result)
           (is (typep result result-type))
           (is (typep (first result) 'fset:seq))
           (is (equal? expected-result result)))

  ;; I can't think of any valid way to conj partitions into a hashtable.
  ;; Am I missing something? Meanwhile, no test here for hashtables as transducer
  ;; targets, i.e. (transduce xform #'conj <somehashtable/map> input)

  ;; Transducer Edge cases - with conj and cl-conj
  (let ((r (transduce (partition-by #'odd?) #'conj [])))
    (is (empty? r))
    (is (vector? r)))
  (let ((r (transduce (partition-by #'odd?) #'cl-conj (cl-vector))))
    (is (empty? r))
    (is (cl:vectorp r)))
  (let ((r (transduce (partition-by #'odd?) #'conj [2])))
    (is (vector? r))
    (is (vector? (first r)))
    (is (equal? [[2]] r)))
  (let ((r (transduce (partition-by #'odd?) #'cl-conj (cl-vector 2))))
    (is (cl:vectorp r))
    (is (vector? (first r)))
    (is (equal? #((2)) r)))
  (let ((r (transduce (partition-by #'odd?) #'conj (list) [])))
    (is (empty? r))
    (is (list? r)))
  (let ((r (transduce (partition-by #'odd?) #'cl-conj nil (cl-vector))))
    (is (empty? r))
    (is (cl:listp r)))
  ;; Note that the takes are basically no-ops, what really gets tested here
  ;; is the completion step of partition-by.
  (is (equal? [[:a]] (transduce (comp (partition-by #'keyword?) (take 1)) #'conj [] [:a])))
  (is (equal? [[:a]] (sequence (comp (partition-by #'keyword?) (take 1)) [:a])))
  ;; [:a] => partition-by => ([:a]) => take => ([:a]) => partition-by (([:a])) => take (([:a]))
  (is (equal? [[[:a]]] (sequence (comp (partition-by #'keyword?) (take 1) 
                                       (partition-by #'keyword?) (take 1)) [:a]))))

(test second
  (is (null (second nil)))
  (is (eql 2 (second '(1 2 3))))
  (is (eql 3 (second #(2 3 4))))
  (is (eql 4 (second (vector 3 4 5))))
  (let* ((m (cl-hash-map :a 1 :b 2))
         (x (first m))
         (y (second m)))
    (is (or (equal '(:a 1) x)
            (equal '(:b 2) x)))
    (is (or (equal '(:a 1) y)
            (equal '(:b 2) y)))
    (is (not (equal x y))))
  (let* ((m (hash-map :a 1 :b 2))
         (x (first m)) (ex (vector :a 1))
         (y (second m)) (ey (vector :b 2)))
    (is (or (coll= x ex) (coll= x ey)))
    (is (or (coll= y ex) (coll= y ey)))
    (is (not (coll= x y))))

  ;; On sequential & set seqs
  (loop for coll in (cl:list '(1 2) #(1 2) (list 1 2) (vector 1 2) (hash-set 1 2) (nums 2))
        as result = (second (seq coll))
        do (cond ((clj-coll::lazyseq-p coll)
                  (is (= result 1)))
                 ((set? coll)
                  (is (or (= result 1) (= result 2))))
                 (t (is (= result 2)))))
  ;; On map seqs
  (loop for coll in (cl:list (cl-hash-map :a 1 :b 2) {:a 1 :b 2})
        as s = (seq coll)
        as result = (second s)
        do (is (or (equal? [:a 1] result)
                   (equal? [:b 2] result)))
           (is (not (equal? result (first s)))))
  )

(test merge 
  (is (equal? {:a 1 :b 2} (merge {:a 1} {:b 2})))
  (is (equal? {:a 1 :b 2 :c 4} (merge {:a 1} {:b 1} {:c 4} {:b 2 :c 4})))
  ;; target as conj target
  (is (equal? #{1 2} (merge #{} 1 2)))
  (is (equal? [1 2 3] (merge [1 2] 3)))
  (is (equal? [1 2 {:a 1 :b 2}] (merge [1 2] {:a 1 :b 2})))
  (is (equal? {:a 1 :b 2 1 2} (merge {:a 1 :b 2} [1 2])))
  ;; Merging from seqs
  ;; *FINISH* after some reduce work, then define merge in terms of reduce
  ;; But must stop conj from calling merge-aux
  ;; Must also load reduce.lisp before implementation of merge (which can maybe
  ;; move to APIs.lisp? )
  )

(test conj                              ;& cl-conj
  (let ((r (conj nil nil)))             ;=> immutable (nil)
    (is (list? r))
    (is (= 1 (count r)))
    (is (null (first r))))
  (let ((r (cl-conj nil nil)))             ;=> mutable (nil)
    (is (cl:listp r))
    (is (= 1 (count r)))
    (is (null (first r))))
  (let ((r (conj (list) (list))))       ;=> (()) (persistentlist in persistentlist)
    (is (list? r))
    (is (= 1 (count r)))
    (let ((f (first r)))
      (is (list? f))
      (is (= 0 (count f)))))
  (let ((r (cl-conj (list) (list))))       ;=> (()) (persistentlist in persistentlist)
    (is (list? r))                      ;still an immutable list, because that's what we passed in
    (is (= 1 (count r)))
    (let ((f (first r)))
      (is (list? f))
      (is (= 0 (count f)))))

  (let ((v (vector 1 20)))
    (is (eq v (conj v))))
  (let ((v (vector 1 20)))
    (is (eq v (cl-conj v))))
  (let ((v #(1 20)))
    (is (eq v (cl-conj v))))
  (let ((v (-> (conj)                   ;an immutable vector/seq
               (conj 1)
               (conj 2))))
    (is (vector? v))
    (is (coll= (vector 1 2) v)))
  (let ((v (-> (cl-conj)                ;a mutable vector/seq
               (cl-conj 1)
               (cl-conj 2))))
    (is (cl:vectorp v))
    (is (equalp #(1 2) v)))
  (let ((v (-> nil (conj 1) (conj 2))))
    (is (list? v))                      ;persistentlist
    (is (equal? '(2 1) v)))
  (let ((v (-> nil (cl-conj 1) (cl-conj 2))))
    (is (cl:listp v))                   ;mutable cl:list
    (is (equalp '(2 1) v)))
  ;; conj
  (let ((v (-> {} (conj (vector :a 1)) (conj (vector :b 2)))))
    (is (map? v))
    (is (eq :equal (fset:compare {:a 1 :b 2} v)))
    (is (equal? {:a 1 :b 2 :c 3}
                (conj v '(:c 3))))
    ;; Validate types of logical mapentries we accept
    ;; Clojure only permits vectors but not 2 element persistent lists.
    (loop for mapentry 
            in (cl:list (cl:cons :c 3)  ;no dotted pairs
                        (cl:list :d 4)
                        (cl:list :e 5)
                        (cl-vector :f 6)
                        (vector :g 7))
          for valid? in (cl:list nil t t t t)
          if valid?
            do (is (= 3 (count (conj v mapentry))))
          else
            do (signals error (conj v mapentry))))
  ;; cl-conj
  (let ((v (-> (cl-hash-map) (cl-conj (vector :a 1)) (cl-conj (vector :b 2)))))
    (is (cl:hash-table-p v))
    (is (equalp (cl-hash-map :a 1 :b 2) v))
    (is (equalp (cl-hash-map :a 1 :b 2 :c 3)
                (cl-conj v '(:c 3))))   ;*MUTATES* v, not like the `conj` case above
    (loop for mapentry 
            in (cl:list (cl:cons :c -3)  ;no dotted pairs
                        (cl:list :d 4)
                        (cl:list :e 5)
                        (cl-vector :f 6)
                        (vector :g 7))
          for count from 3
          for valid? in (cl:list nil t t t t)
          if valid?
            do (is (= count (count (cl-conj v mapentry))))
          else
            do (signals error (cl-conj v mapentry))))
  ;; sets
  (let ((s (-> #{} (conj 1) (conj 2) (conj 1))))
    (is (equal? s #{1 2})))
  (let ((s (-> #{} (cl-conj 1) (cl-conj 2) (cl-conj 1))))
    (is (equal? s #{1 2})))
  )

(defmacro local-fixture ((start finish) &body body)
  "Execute BODY after first executing the form in START, executing FINISH when BODY exits.
Return value N/A"
  `(unwind-protect
        (progn
          ,start
          ,@body)
     ,finish))

(test map-printing
  (print "MAP-PRINTING test disabled - use it if you're changing map printing code.")
  (is (eq t t)))
#+(OR) 
;; map-printing test disabled, it worked on a single SBCL release, but it's unordered
;; and implementation dependennt.
(test map-printing
  ;; *FINISH*: need a ON/OFF of the print syntax after all as a test fixture.
  ;; Might need some kind of 'printing-enabled?' predicate. For now, brute force.
  ;; Either way, want to restore what was in effect before the test for tidyness.
  (local-fixture ((enable-map-printing)
                  'todo #+NIL                 ;re-enable when enable/disable push/pop printing state
                  (disable-map-printing))
    (let ((m (serapeum:dict :a 1 :b 2 :c 3 :d 4 
                            :e (serapeum:dict :f 6 :g 7 :h 8 :i 9) 
                            :j 10 :k 11 :l 12 
                            :m (serapeum:dict :n 13 :o 14 
                                              :p (serapeum:dict :q 15 :r 16 :s 18 :t 19 :u 20))))
          (m2 {:a 1 :b 2 :c 3 :d 4 
               :e {:f 6 :g 7 :h 8 :i 9} 
               :j 10 :k 11 :l 12 
               :m {:n 13 :o 14 
                   :p {:q 15 :r 16 :s 18 :t 19 :u 20}}})
          (*print-pretty* t))
      (is (equal "
(CL-HASH-MAP :A 1 :B 2 :C 3 :D 4 :E (CL-HASH-MAP :F 6 :G 7 :H 8 :I 9)  :J 10
 :K 11 :L 12
 :M (CL-HASH-MAP :N 13 :O 14 :P (CL-HASH-MAP :Q 15 :R 16 :S 18 :T 19 :U 20) ) )  "
                 (with-output-to-string (stream)
                   (print m stream))))
      (is (equal "
{:A 1 :B 2 :C 3 :D 4 :E {:F 6 :G 7 :H 8 :I 9}  :J 10 :K 11 :L 12
 :M {:N 13 :O 14 :P {:Q 15 :R 16 :S 18 :T 19 :U 20} } }  "
                 (with-output-to-string (stream)
                   (print m2 stream)))))

    (let ((m (serapeum:dict :a 1 :b 2 :c '(c1 c2 c3) :d 4 
                            :e (serapeum:dict :f 6 :g 7 :h 8 :i 9) 
                            :j 10 :k 11 :l 12 
                            :m (serapeum:dict :n 13 :o 14 
                                              :p (serapeum:dict :q 15 :r 16 
                                                                :s #(s1 s2 s3) :t 19 :u 20))))
          (m2 {:a 1 :b 2 :c '(c1 c2 c3) :d 4 
               :e {:f 6 :g 7 :h 8 :i 9} 
               :j 10 :k 11 :l 12 
               :m {:n 13 :o 14 
                   :p {:q 15 :r 16 
                       :s #(s1 s2 s3) :t 19 :u 20}}})
          (*print-pretty* t))
      (is (equal "
(CL-HASH-MAP :A 1 :B 2 :C (C1 C2 C3) :D 4 :E (CL-HASH-MAP :F 6 :G 7 :H 8 :I 9)
 :J 10 :K 11 :L 12
 :M (CL-HASH-MAP :N 13 :O 14
     :P (CL-HASH-MAP :Q 15 :R 16 :S #(S1 S2 S3) :T 19 :U 20) ) )  "
                 (with-output-to-string (stream)
                   (print m stream))))
      (is (equal "
{:A 1 :B 2 :C (C1 C2 C3) :D 4 :E {:F 6 :G 7 :H 8 :I 9}  :J 10 :K 11 :L 12
 :M {:N 13 :O 14 :P {:Q 15 :R 16 :S #(S1 S2 S3) :T 19 :U 20} } }  "
                 (with-output-to-string (stream)
                   (print m2 stream))))

      ;; Test *print-length* observance
      (let ((*print-length* 3)
            (*print-pretty* t))
        (is (equal "
(CL-HASH-MAP :A 1 :B 2 :C (C1 C2 C3) ...)  "
                   (with-output-to-string (stream)
                     (print m stream))))
        (is (equal "
{:A 1 :B 2 :C (C1 C2 C3) ...}  "
                   (with-output-to-string (stream)
                     (print m2 stream))))))

    ;; Test *print-right-margin*
    (is (equal "
(CL-HASH-MAP :A 1 :B 2
 :C #(1 2 3 4 5 56 6)
 :D (CL-HASH-MAP :Z 26
     :Y 25) ) "
               (with-output-to-string (*standard-output*)
                 (let ((*print-right-margin* 30))
                   (pprint (cl-hash-map :a 1 :b 2 :c #(1 2 3 4 5 56 6)
                                        :d (cl-hash-map :z 26 :y 25)))))))
    (is (equal "
{:A 1 :B 2
 :C #(1 2 3 4 5 56 6)
 :D {:Y 25 :Z 26} } "
               (with-output-to-string (*standard-output*)
                 (let ((*print-right-margin* 30))
                   (pprint {:a 1 :b 2 :c #(1 2 3 4 5 56 6)
                            :d {:z 26 :y 25}})))))
    ))

(test vector-printing
  (local-fixture ((enable-vector-printing) 
                  'todo #+NIL                 ;re-enable when enable/disable push/pop printing state
                  (disable-vector-printing))
    (let* ((v1 (cl-vector 1 2 (cl-vector 3 4)))
           (v2 (vec v1))
           (v3 [1 2 [3 4]])
           (*print-pretty* t))
      (is (equal "#(1 2 #(3 4))" 
                 (with-output-to-string (stream)
                   (prin1 v1 stream))))
      (is (equal "[1 2 #(3 4)]" 
                 (with-output-to-string (stream)
                   (prin1 v2 stream))))
      (is (equal "[1 2 [3 4]]" 
                 (with-output-to-string (stream)
                   (prin1 v3 stream))))
      (let ((*print-right-margin* 4))
        (is (equal "[1
 2
 [3
  4]]"
                   (with-output-to-string (stream)
                     (prin1 v3 stream)))))
      (let ((*print-length* 2))
        (is (equal  "[1 2 ...]" 
                    (with-output-to-string (stream)
                      (prin1 v3 stream)))))
      )))

(test set-printing
  (local-fixture ((enable-set-printing)
                  'todo #+NIL                 ;re-enable when enable/disable push/pop printing state
                  (disable-set-printing))
    (flet ((str (set)
             (with-output-to-string (stream)
               (let ((*print-pretty* t))
                 (prin1 set stream)))))
      (is (equal "#{1 2 3}" (str #{1 2 3})))
      (is (equal "#{1 2 3 ...}" 
                 (let ((*print-length* 3))
                   (str #{1 2 3 4}))))
      #+CCL
      ;; cuz it's weird and I have other things to do right now.
      (print "SET-PRINTING *print-right-margin* test disabled.")
      #+SBCL
      (is (equal "#{1 2 3
  4 5 6}"
                 (let ((*print-right-margin* 8))
                   (str #{1 2 3 4 5 6})))))))
                 

(test persistentlist-printing
  ;; Implemented as generic seq printing
  (flet ((str (x)
           (with-output-to-string (stream)
             (let ((*print-pretty* t))
               (prin1 x stream)))))
    (is (equal "(list 1 2 3)" (str (list 1 2 3))))
    (is (equal "(list 1 2 3 ...)" 
               (let ((*print-length* 3))
                 (str (list 1 2 3 4)))))
    #+CCL
    (print "PERSISTENTLIST-PRINTING *print-right-margin* test disabled.")
    #+SBCL
    (is (equal "(list 1 2 3
      4 5 6
      7 8 9
      10)"
               (let ((*print-right-margin* 12))
                 (str (list 1 2 3 4 5 6 7 8 9 10)))))))
                 
(test seq-printing
  ;; seqs print as persistent list
  ;; enabled by default via print-object methods on seqs
  (flet ((str (x)
           (with-output-to-string (stream)
             (let ((*print-pretty* t))
               (prin1 x stream)))))
    (is (equal "(list 3 2 1)" (str (nums 3))))
    (is (equal "(list 3 2 1)" (str (seq (nums 3)))))
    (is (equal "(list 1 2 3)" (str (cons 1 [2 3]))))
    (is (equal "(list 1 2 3)" (str (seq (cons 1 [2 3])))))
    (is (equal "(list 1 2 3)" (str (seq [1 2 3]))))))

(test assoc
  ;; Stuff that wasn't tested elsewhere.
  (is (equal? [0 1 3] (assoc [0 1] 2 3)))
  (signals error (assoc (vector 0 1) 3 4))
  (signals error (assoc (cl-vector 0 1) 3 4))
  (is (fset:equal? {:a 1 :b 2} (assoc nil :a 1 :b 2)))
  (is (equal? [1 2 3 4 5] (assoc [1 2 3] 3 4 4 5)))
  (is (equalp #(1 2 3 4 5) (assoc (cl-vector 1 2 3) 3 4 4 5)))
  (is (equalp #(1 2 3 4 5) (assoc (make-array 3 :initial-contents '(1 2 3)) 3 4 4 5)))
  (loop with v = #() 
        for x from 0 below 10
        do (setf v (assoc v x x))
           (is (equal? (range (1+ x)) v))))

(test assoc-in
  (signals error (assoc-in '(1 2 3) [0] :a)) ; cl:list is not associative
  (signals error (assoc-in (list 1 2 3) [0] :a)) ;persistenlist is not associative
  (signals error (assoc-in (hash-set 1 2 3) [0] :a)) ;hash-set is not associative
  (is (fset:equal? {:a {:b 1}} (assoc-in nil [:a :b] 1)))
  (is (equalp (cl-hash-map :a (cl-hash-map :b 1)) (assoc-in (cl-hash-map) #(:a :b) 1)))
  (is (equal? {:a [0 1] :b [4 3]}
              (assoc-in {:a [0 1] :b [2 3]} [:b 0] 4)))
  (is (equal? {nil {:k :v}} (assoc-in {} [] {:k :v})))
  (is (= 4 (get-in (assoc-in {:a 1 :b {:c 3}} [:b :c] 4) [:b :c])))

  ;; Past array bounds (by one is okay, but two is not, Clojure behavior)
  (is (equal? {:A [0 1 3]} (assoc-in {:a [0 1]} [:a 2] 3)))
  (signals error (assoc-in {:a [0 1]} [:a 3] 4))

  (is (equal? [[1 1 1] [1 0 1] [1 1 1]]
              (assoc-in [[1 1 1]
                         [1 1 1]
                         [1 1 1]] [1 1] 0))))

(test update-in
  (signals error (update-in '(1 2 3) [0] (constantly :a))) ; cl:list is not associative
  (signals error (update-in (list 1 2 3) [0] (constantly :a))) ;persistenlist is not associative
  (signals error (update-in (hash-set 1 2 3) [0] (constantly :a))) ;hash-set is not associative
  (is (fset:equal? {:a {:b 1}} (update-in nil [:a :b] (constantly 1))))
  (is (equalp (cl-hash-map :a (cl-hash-map :b 1)) 
              (update-in (cl-hash-map) #(:a :b) (constantly 1))))
  (is (equal? {:a [0 1] :b [4 3]}
              (update-in {:a [0 1] :b [2 3]} [:b 0] #'+ 2)))
  (is (equal? {nil {:k :v}} (update-in {} [] (constantly {:k :v}))))
  (is (= 4 (get-in (update-in {:a 1 :b {:c 3}} [:b :c] #'+ 1) [:b :c])))

  ;; Past array bounds (by one is okay, but two is not, Clojure behavior)
  (is (equal? {:A [0 1 3]} (update-in {:a [0 1]} [:a 2] (constantly 3))))
  (signals error (update-in {:a [0 1]} [:a 3] (constantly 4)))

  (is (equal? [[1 1 1] [1 -1 1] [1 1 1]]
              (update-in [[1 1 1]
                          [1 1 1]
                          [1 1 1]] [1 1] #'- 1 1))))

(test update
  ;; FSet:Seq
  (is (equal? [2 1] (update [0 1] 0 #'+ 2)))
  (is (equal? [2 2] (update [2 1] 1 #'inc)))
  (is (equal? [2 1 3] (update [2 1 ] 2 (constantly 3))))
  (signals error (update [2 1 ] 3 (constantly 3)))
  ;; CL:Vector
  (is (equalp (cl-vector 2 1) (update (cl-vector 0 1) 0 #'+ 2)))
  (is (equalp (cl-vector 2 2) (update (cl-vector 2 1) 1 #'inc)))
  (is (equalp (cl-vector 2 1 3) (update (cl-vector 2 1) 2 (constantly 3))))
  (signals error (update (cl-vector 2 1) 3 (constantly 3)))
  (let* ((v (cl-vector))
         (r (update v 0 (constantly 6))))
    (is (eq v r))
    (is (equalp #(6) v)))
  ;; FSet:map
  (is (equal? {:a 2} (update {} :a (constantly 2))))
  (is (equal? {:a 2 :b 1} (update {:a 2} :b (constantly 1))))
  (is (equal? {:a 2 :b 2} (update {:a 2 :b 1} :b #'inc )))
  ;; CL:hash-table
  (is (equalp (cl-hash-map :a 2) (update (cl-hash-map) :a (constantly 2))))
  (is (equalp (cl-hash-map :a 2 :b 1) (update (cl-hash-map :a 2) :b (constantly 1))))
  (is (equalp (cl-hash-map :a 2 :b 2) (update (cl-hash-map :a 2 :b 1) :b #'inc )))
  (let* ((ht (cl-hash-map :a 1))
         (r (update ht :b (constantly 2))))
    (is (eq ht r))
    (is (equalp (cl-hash-map :a 1 :b 2) r))))
  
(test subvec
  (let ((mv (cl-vector 1 2 3 4))
        (iv (vector 1 2 3 4)))
    (is (eq mv (subvec mv 0)))
    (is (eq iv (subvec iv 0)))
    (is (equalp #() (subvec mv 0 0)))
    (is (equal? [] (subvec iv 0 0)))
    (is (equalp #(1) (subvec mv 0 1)))
    (is (equal? [1] (subvec iv 0 1)))
    (is (equalp #(2 3 4) (subvec mv 1)))
    (is (equal? #(2 3 4) (subvec iv 1)))
    (is (equalp #(4) (subvec mv 3 4)))
    (is (equal? #(4) (subvec iv 3 4)))
    (is (equalp #() (subvec mv 4 4)))
    (is (equal? #() (subvec iv 4 4)))
    (signals error (subvec mv 5 4))
    (signals error (subvec iv 5 4))))

(test fnil
  (flet ((f1 (x &rest args) (cl:list* x args))
         (f2 (x y &rest args) (cl:list* x y args))
         (f3 (x y z &rest args) (cl:list* x y z args)))
    (let ((sf1 (fnil #'f1 1))
          (sf2 (fnil #'f2 1 2))
          (sf3 (fnil #'f3 1 2 3)))
      (is (equalp '(1) (funcall sf1 1)))
      (is (equalp '(1) (funcall sf1 nil)))
      (is (equalp '(1 2) (funcall sf1 1 2)))
      (is (equalp '(1 nil) (funcall sf1 1 nil)))
      (is (equalp '(1 2) (funcall sf2 1 nil)))
      (is (equalp '(1 2 3) (funcall sf2 nil nil 3)))
      (is (equalp '(1 2 3) (funcall sf3 nil nil nil)))
      (is (equalp '(1 2 3 4 5) (funcall sf3 nil nil 3 4 5))))))

(test map-assoc/dissoc/count/keys ;and vals
  (let ((im (hash-map :a 1 :b 2 :c 3))
        (mm (serapeum:dict :a 1 :b 2 :c 3)))
    (is (= 3 (count im)))
    (is (= 3 (count mm)))
    (is (equal? (set '(:a :b :c)) (set (keys im))))
    (is (equal? (set (keys im)) (set (keys mm))))
    (is (equal? (set '(1 2 3)) (set (vals im))))
    (is (equal? (set (vals im)) (set (vals mm))))
    ;; Keys :a :b :c 
    (let ((im2 (dissoc im :c))
          (mm2 (dissoc mm :c)))
      (is (seq? (keys im2)))
      (is (seq? (keys mm2)))
      (is (equal? (set '(:a :b)) (set (keys im2))))
      (is (equal? (set (keys im2)) (set (keys mm2))))
      (is (equal? (set '(1 2)) (set (vals im2))))
      (is (equal? (set (vals im2)) (set (vals mm2))))
      (is (not (eq im im2)))
      (is (eq mm2 mm))
      (is (not (get im2 :c)))
      (is (not (get mm2 :c)))
      (is (not (get im2 :d)))
      (is (not (get mm2 :d)))
      (setf im im2))
    ;; keys :a :b
    (let ((im2 (dissoc im :a :b :d))
          (mm2 (dissoc mm :a :b :d)))
      (is (null (keys im2)))
      (is (null (keys mm2)))
      (is (null (vals im2)))
      (is (null (vals mm2)))
      (is (empty? im2))
      (is (= 0 (count im2)))
      (is (empty? mm2))
      (is (= 0 (count mm2)))
      (setf im im2))
    ;; No keys
    (let ((im2 (assoc im :e 5 :f 6))
          (mm2 (assoc mm :e 5 :f 6)))
      (is (= 2 (count im2)))
      (is (= 2 (count mm2)))
      (is (equal? (set '(:e :f)) (set (keys im2))))
      (is (equal? (set (keys im2)) (set (keys mm2))))
      (is (eql 5 (get im2 :e)))
      (is (eql 5 (get mm2 :e)))
      (is (eql 6 (get im2 :f)))
      (is (eql 6 (get mm2 :f)))
      (setf im im2))
    ;; Keys :e :f remain
    ))

(test vector-assoc/dissoc/count/keys ;& vals
  ;; Note that dissoc doesn't support vectors in Clojure.
  (let ((v (vector 1 2))
        (mv (cl-vector 1 2)))
    (is (= 2 (count v) (count mv)))
    (setf v (assoc v 0 :a)) (assoc mv 0 :a)
    (is (equal? #(:a 2) v mv))
    (setf v (assoc v 1 :b)) (assoc mv 1 :b)
    (is (equal? #(:a :b) v mv))
    (setf v (assoc v 2 :c)) (assoc mv 2 :c)
    (is (equal? #(:a :b :c) v mv))
    (is (= 3 (count v) (count mv)))
    (is (= 0 (count []) (count #())))

    (signals error (keys v))
    (signals error (keys mv))
    (signals error (vals v))
    (signals error (vals mv))
    (signals error (dissoc v 0))
    (signals error (dissoc mv 0))
    ))

(test disj                              ;set key removal
  (is (null (disj nil)))
  (is (null (disj nil 1)))
  (let ((s #{1}))
    (is (eq s (disj s)))
    (is (fset:equal? #{} (disj s 1))))
  (is (equal? #{1 2} (disj #{4 3 2 1 0} 0 3 4))))

(test union                             ;set union
  (is (null (union)))
  (is (null (union nil)))
  (is (null (union nil nil)))
  (signals error (union #{1} [2]))      ;vectors explicitly rejected
  (let ((s #{1}))
    (is (eq s (union s)))
    (is (eq s (union s nil)))
    (is (fset:equal? #{1 2} (union s #{2}))))
  (is (equal? #{1 2 3 4} (union nil #{1} #{2 3} #{4} )))
  (let ((r (union nil '(1) '(2 3) '(4))))
    (is (cl:listp r))
    (is (equal? (set '(1 2 3 4)) (set r))))
  ;; fset:equal? sees non-matching set entries
  (is (= 2 (count (union (hash-set {:a 1}) (hash-set (cl-hash-map :a 1))))))
  ;; clj-coll:equal? sees matching set entries
  ;; Can't use equal/fset-equal on fset:map here against a cl map, fset only does EQ for cl:hash-table
  (is (= 1 (count (union (cl:list {:a 1}) (cl:list (cl-hash-map :a 1))))))
  )

(test difference                        ;set difference
  ;; Would have to muffle the warning for test code compilation
  ;; Shinmera's :parachute test suite has a `fail-compile` directive
  ;; for this sort of thing.
  ;;(signals error (difference))          ;one arg required
  (is (null (difference nil)))
  (is (null (difference nil nil)))
  (let ((s #{1 2 3}))
    (is (eq s (difference s)))
    (is (eq s (difference s nil)))
    (is (fset:equal? #{1} (difference s #{2} #{3}))))
  (is (equal? #{4} (difference #{1 2 3 4} #{1} #{2 3})))
  (is (equal '(4) (difference '(1 2 3 4) '(1) '(2 3))))
  ;; fset:equal? sees non-matching set entries
  (is (not (empty? (difference (hash-set {:a 1}) (hash-set (cl-hash-map :a 1))))))
  ;; clj-coll:equal? sees matching set entries
  (is (null (difference (cl:list {:a 1}) (cl:list (cl-hash-map :a 1)))))
  ;; vectors and other mixed datatypes rejected
  (signals error (difference #{1} [1]))
  (signals error (difference #{1} '(1))))

(test intersection                        ;set intersection
  ;; Would have to muffle the warning for test code compilation
  ;;(signals error (#'intersection)) ;one arg required
  (is (null (intersection nil)))
  (is (null (intersection nil nil)))
  (is (null (intersection nil nil #{1})))
  (is (null (intersection nil nil '(1))))
  (is (null (intersection #{1} nil)))
  (is (null (intersection '(1) nil)))
  (let ((s #{1 2 3}))
    (is (eq s (intersection s)))
    (is (fset:equal? #{1} (intersection s #{3 1} #{2 1})))
    (is (empty? (intersection s #{4}))))
  (is (equal? #{4} (intersection #{1 2 3 4} #{1 3 4} #{1 2 4} #{4})))
  (is (equal '(4) (intersection '(1 2 3 4) '(1 3 4) '(1 2 4) '(4))))
  ;; fset:equal? sees non-matching set entries
  (is (= 0 (count (intersection (hash-set {:a 1}) (hash-set (cl-hash-map :a 1))))))
  ;; clj-coll:equal? sees matching set entries
  (is (= 1 (count (intersection (cl:list {:a 1}) (cl:list (cl-hash-map :a 1))))))
  ;; vectors and other mixed datatypes rejected
  (signals error (intersection #{1} [1]))
  (signals error (intersection #{1} '(1))))

(test subset?
  (is (eq t (subset? nil nil)))
  (is (eq t (subset? nil #{})))
  (is (eq t (subset? #{} nil)))
  (is (eq t (subset? nil #{1})))
  (is (eq t (subset? nil '(1))))
  ;; vectors and other mixed datatypes rejected
  (signals error (subset? #{1} [1]))
  (signals error (subset? #{1} '(1)))
  (signals error (subset? [1] #{1}))
  (is (eq t (subset? #{1} #{1 2})))
  (is (eq t (subset? '(1) '(1 2))))
  (is (null (subset? #{3} #{1 2})))
  (is (null (subset? '(3) '(1 2))))
  (is (null (subset? #{1 2 3} #{1 2})))
  (is (null (subset? '(1 2 3) '(1 2))))
  ;; fset:equal? sees non-matching set entries
  (is (null (subset? (hash-set {:a 1}) (hash-set (cl-hash-map :a 1)))))
  ;; clj-coll:equal? sees matching set entries
  (is (subset? (cl:list {:a 1}) (cl:list (cl-hash-map :a 1)))))

(test superset?
  (is (eq t (superset? nil nil)))
  (is (eq t (superset? nil #{})))
  (is (eq t (superset? #{} nil)))
  (is (eq t (superset? #{1} nil)))
  (is (null (superset? nil #{1})))
  (is (null (superset? nil '(1))))
  ;; vectors and other mixed datatypes rejected
  (signals error (superset? #{1} [1]))
  (signals error (superset? [1] #{1}))
  (signals error (superset? [1] '(1)))
  (is (eq t (superset? #{1 2} #{1})))
  (is (eq t (superset? '(1 2) '(1))))
  (is (eq t (superset? #{1 2} #{1 2})))
  (is (eq t (superset? '(1 2) '(1 2))))
  (is (null (superset? #{1 2} #{3})))
  (is (null (superset? '(1 2) '(3))))
  (is (null (superset? #{1 2} #{1 2 3})))
  (is (null (superset? '(1 2) '(1 2 3))))
  ;; fset:equal? sees non-matching set entries
  (is (null (superset? (hash-set {:a 1}) (hash-set (cl-hash-map :a 1)))))
  ;; clj-coll:equal? sees matching set entries
  (is (superset? (cl:list {:a 1}) (cl:list (cl-hash-map :a 1)))))

(test contains?
  (is (contains? #(4 5 6) 1))           ;stupid clojure semantics
  (is (contains? [4 5 6] 1))
  (is (not (contains? #(3 2) 2)))
  (is (not (contains? [3 2] 2)))
  (is (contains? {:a 1 :b 2} :b))
  (is (contains? {:a 1 :b 2} :a))
  (is (contains? (cl-hash-map :a 1 :b 2) :b))
  (is (contains? (cl-hash-map :a 1 :b 2) :a))
  (is (not (contains? {:a 1 :b 2} :c)))
  (is (not (contains? (cl-hash-map :a 1 :b 2) :c)))
  (is (contains? #{1 2} 1))
  (is (contains? #{1 2} 2))
  (is (not (contains? #{1 2} :a)))

  ;; Key equivalence quirks for hash-tables/maps/sets
  ;; No, we can't specify CLJ-COLL:EQUAL? for FSET map/set key tests 
  ;; (at the time of this writing).
  (is (not (contains? (hash-set :a '(:b 2)) [:b 2]))) ; fset won't do list/seq equivalence
  (is (contains? (hash-set :a '(:b 2)) '(:b 2)))
  
  (is (contains? (hash-map (vector :a 1) (vector :b 2)) (vector :a 1))) ;fset can use fset objs as keys with structural comparison
  (is (not (contains? (hash-map (vector :a 1) (vector :b 2)) (cl-vector :a 1)))) ; but it can't do the same for CL objs as keys

  (is (not (contains? (cl-hash-map (cl-vector :a 1) (cl-vector :b 2)) (cl-vector :a 1)))) ;cl-hash-map creates an EQUAL CL:HASH-TABLE
  (let ((ht (make-hash-table :test 'equalp)))
    (setf (gethash (cl-vector :a 1) ht) (cl-vector :b 2)) ;could have `assoc`ed this...
    (is (contains? ht (cl-vector :a 1))))) ;EQUALP will allow it, but of course destroys string case sensitivity

(test last
  (flet ((doit (coll expected)
           (is (equal? expected (last coll)))
           (cond ((> (count coll) 2)
                  (is (not (equal? (first coll) (second coll) (last coll)))))
                 ((> (count coll) 1)
                  (is (not (equal? (first coll) (second coll))))
                  (is (equal? (second coll) (last coll))))
                 ((= (count coll) 1)
                  (is (equal? (first coll) (last coll))))))
         (mdoit (contents)
           (let* ((m1 (apply #'hash-map contents))
                  (m2 (apply #'cl-hash-map contents))
                  (clj-coll::*map-entry-comparison-hacks-enabled* t)
                  (valid (set m2)))
             (loop for coll in (cl:list m1 m2)
                   do
                      (cond ((> (count coll) 2)
                             (is (contains? valid (first coll)))
                             (is (contains? valid (second coll)))
                             (is (contains? valid (last coll)))
                             (is (not (equal? (first coll) (second coll) (last coll)))))
                            ((> (count coll) 1)
                             (is (contains? valid (first coll)))
                             (is (contains? valid (second coll)))
                             (is (contains? valid (last coll)))
                             (is (not (equal? (first coll) (second coll))))
                             (is (equal? (second coll) (last coll))))
                            ((= (count coll) 1)
                             (is (contains? valid (first coll)))
                             (is (not (contains? valid (second coll))))
                             (is (contains? valid (last coll)))
                             (is (equal? (first coll) (last coll)))))))))
    (doit #{} nil)
    (doit #{1} 1)
    (doit [3 2 1] 1)
    (doit #(3 2 1) 1)
    (doit '(3 2 1) 1)
    (mdoit '(:a 1))
    (mdoit '(:a 1 :b 2))
    (mdoit '(:a 1 :b 2 :c 3))
    )

  ;; Seqs
  (is (= 1 (last (nums 3))))
  (is (null (last (nums 0))))
  (is (null (last (seq []))))
  (is (= 2 (last (seq [1 2])))))

(test reduce2
  ;; Most additional tests of reduce2 are via the reduce test
  ;; on collections with 2+ items
  (flet ((doit (coll &optional init) ; comparing to cl:reduce
           (if init 
               (is (= (cl:reduce '+ coll :initial-value init)
                      (clj-coll::reduce2 #'+ coll init)))
               (is (= (cl:reduce '+ coll)
                      (clj-coll::reduce2 #'+ coll))))))
    (doit '(1 2))
    (doit '(1 2 3))
    (doit '(1 2 3) 4)
    (doit #(1 2))
    (doit #(1 2 3))
    (doit #(1 2 3) 4)))

(test nil?
  (is (null (nil? (list))))
  (is (nil? ())))

(test some?
  (is (some? t))
  (is (some? (list)))
  (is (not (some? nil))))

(test reduce
  "REDUCE without reduced"

  ;; Simple but edge cases to get us started
  (flet ((f (&rest args) args))
    (is (equal? nil (reduce #'f [])))       ;=> f()
    (is (equal? 1   (reduce #'f [1])))      ;=> 1
    (is (equal? '(1 2) (reduce #'f [1 2]))) ;f(1 2)

    (is (equal? (reduce #'f 3 [])))              ;=> 3
    (is (equal? '(3 1) (reduce #'f 3 [1])))      ;=> f(i 1)
    (is (equal? '((3 1) 2)(reduce #'f 3 [1 2]))));=> f( f(i 1) 2)

  ;; No initial values
  (loop for initial-value in '(100 nil)
        do (loop for coll-type in  '(cl:list cl:vector fset:seq fset:set)
                 do (loop with inputs = (cl:list #(1 2 3 4 5) #() #(1) #(1 2))
                          with answers = '(15 0 1 3)
                          for input in inputs
                          for answer in answers
                          as coll = (clj-coll::convert coll-type input)
                          as result = (if initial-value 
                                          (reduce #'+ initial-value coll)
                                          (reduce #'+ coll))
                          do (is (= (if initial-value (+ answer initial-value) answer)
                                    result)))))
  (is (equal? [] (reduce #'conj {})))
  (is (equal? [:a 1] (reduce #'conj {:a 1})))
  (is (equal? [:a 1 [:b 2]] (reduce #'conj {:a 1 :b 2})))

  ;; with initial values
  (flet ((hm (&rest args) (apply #'cl-hash-map args))
         (equal-set? (answer result)
           (if (set? answer)
               (equal? answer (set result))
               (equal? answer result))))
    (let* ((inputs 
             ;; We rely on knowledge that hash-table inputs start at index 20 in this list.
             ;; and that list-conj-answers from index 6-10 are where they are
             (cl:list () '(1) '(1 2) '(1 2 3) '(1 2 3 4) ;lists
                      #() #(1) #(1 2) #(1 2 3) #(1 2 3 4) ;cl-vectors
                      [] [1] [1 2] [1 2 3] [1 2 3 4]      ;clj-coll vectors
                      #{} #{1} #{1 2} #{1 2 3} #{1 2 3 4} ;sets
                      {} {:a 1} {:a 1 :b 2} {:a 1 :b 2 :c 3} {:a 1 :b 2 :c 3 :d 4} ;immutable
                      (hm) (hm :a 1) (hm :a 1 :b 2) (hm :a 1 :b 2 :c 3) (hm :a 1 :b 2 :c 3 :d 4))) ;mut
           (list-conj-answers ;(reduce #'conj () input)  (noting that '()' is NIL in CL, no an empty persistent list
             (cl:list
              () (list 1) (list 2 1) (list 3 2 1) (list 4 3 2 1) ;persistentlist results for (conj nil <x>)
              () '(1) '(2 1) '(3 2 1) '(4 3 2 1)                 ;cl-vector
              () '(1) '(2 1) '(3 2 1) '(4 3 2 1)                 ;clj-coll vector
              nil #{1} #{2 1} #{3 2 1} #{4 3 2 1} ;set input - compare as sets due to ordering
              nil #{[:a 1]} #{[:b 2] [:a 1]} #{[:c 3] [:b 2] [:a 1]} #{[:d 4] [:c 3] [:b 2] [:a 1]} ;maps - unordered
              ;; (EQUIV? #{ (:A 1) } #{ #[ :A 1 ] }) => nil until we get sets that take CLJ-COLL::EQUAL?, ah well.
              ;; That's why these are sets of lists intead of sets of vectors.
              nil #{'(:a 1)} #{'(:b 2) '(:a 1)} #{'(:c 3) '(:b 2) '(:a 1)} #{'(:d 4) '(:c 3) '(:b 2) '(:a 1)})) ;cl ht - unordered
           (plist-conj-answers          ;(reduce #'conj (list) input)
             (cl:list
              (list) (list 1) (list 2 1) (list 3 2 1) (list 4 3 2 1) ;persistentlist results for (conj nil <x>)
              (list) (list 1) (list 2 1) (list 3 2 1) (list 4 3 2 1) ;cl-vector
              (list) (list 1) (list 2 1) (list 3 2 1) (list 4 3 2 1) ;clj-coll vector
              (list) #{1} #{2 1} #{3 2 1} #{4 3 2 1} ;set input - compare as sets due to ordering
              (list) #{[:a 1]} #{[:b 2] [:a 1]} #{[:c 3] [:b 2] [:a 1]} #{[:d 4] [:c 3] [:b 2] [:a 1]} ;maps - unordered
              (list) #{(list :a 1)} #{(list :b 2) (list :a 1)} #{(list :c 3) (list :b 2) (list :a 1)} #{(list :d 4) (list :c 3) (list :b 2) (list :a 1)})) ;cl ht - unordered
           (cl-vector-conj-answers      ;(reduce #'conj (cl-vector) input)
             (cl:list
              #() #(1) #(1 2) #(1 2 3) #(1 2 3 4) ;cl list input
              #() #(1) #(1 2) #(1 2 3) #(1 2 3 4) ;cl vector input
              #() #(1) #(1 2) #(1 2 3) #(1 2 3 4) ;cl-coll vector input
              #() #{1} #{2 1} #{3 2 1} #{4 3 2 1} ;set input - compare as sets due to ordering
              #() #{[:a 1]} #{[:b 2] [:a 1]} #{[:c 3] [:b 2] [:a 1]} #{[:d 4] [:c 3] [:b 2] [:a 1]} ;maps - unordered
              #() #{'(:a 1)} #{'(:b 2) '(:a 1)} #{'(:c 3) '(:b 2) '(:a 1)} #{'(:d 4) '(:c 3) '(:b 2) '(:a 1)}))
           (vector-conj-answers         ;(reduce #'conj (cl-vector) input)
             (mapcar (lambda (coll)
                       (if (cl:vectorp coll) coll (clj-coll::convert 'fset:set coll)))
                     cl-vector-conj-answers))
           (set-conj-answers            ;(reduce #'conj #{} input)
             (cl:list
              #{} #{1} #{1 2} #{1 2 3} #{1 2 3 4} ;cl list input
              #{} #{1} #{1 2} #{1 2 3} #{1 2 3 4} ;cl vector input
              #{} #{1} #{1 2} #{1 2 3} #{1 2 3 4} ;cl-coll vector input
              #{} #{1} #{2 1} #{3 2 1} #{4 3 2 1} ;set input - compare as sets due to ordering
              #{} #{[:a 1]} #{[:b 2] [:a 1]} #{[:c 3] [:b 2] [:a 1]} #{[:d 4] [:c 3] [:b 2] [:a 1]} ;maps - unordered
              #{} #{'(:a 1)} #{'(:b 2) '(:a 1)} #{'(:c 3) '(:b 2) '(:a 1)} #{'(:d 4) '(:c 3) '(:b 2) '(:a 1)}))
           (cl-hash-map-conj-answers 
             (mapcar (lambda (coll) (clj-coll::convert 'cl:hash-table coll))
                     (subseq set-conj-answers 20)))
           (hash-map-conj-answers 
             (mapcar (lambda (coll) (clj-coll::convert 'fset:map coll))
                     (subseq set-conj-answers 20))))
      ;; cl:list target conj
      (loop for input in (cdr inputs) ;skip NIL which would result in persistent result
            for answer in (cl:subseq list-conj-answers 6 10)
            as result = (reduce #'conj (cl:list 0) input)
            do ;;(format t "(reduce #'conj (cl:list 0) ~s) => ~s, expected ~s as ~s~%" 
               ;;        input result answer (cl:append answer (cl:list 0)))
               (is (equal-set? (cl:append answer (cl:list 0)) result))
               (is (cl:listp result)))
      ;; nil target conj (which becomes persistent list)
      (loop for input in inputs
            for answer in list-conj-answers
            as result = (reduce #'conj () input)
            do ;;(format t "(reduce #'conj () ~s)~% => ~s, expected ~s~%" input result answer)
               (is (equal-set? answer result))
               (is (or (null result) (list? result))))
      ;; persistent list target conj
      (loop for input in inputs
            for answer in plist-conj-answers
            as result = (reduce #'conj (list) input)
            do ;;(format t "(reduce #'conj (list) ~s)~% => ~s, expected ~s~%" input result answer)
               (is (equal-set? answer result))
               (is (list? result)))
      ;; cl-vector conj
      (loop for input in inputs
            for answer in cl-vector-conj-answers
            as result = (reduce #'conj (cl-vector) input)
            do ;;(format t "(reduce #'conj (cl-vector) ~s)~% => ~s, expected ~s~%" input result answer)
               (is (equal-set? answer result))
               (is (cl:vectorp result)))
      ;; vector conj
      (loop for input in inputs
            for answer in vector-conj-answers
            as result = (reduce #'conj (vector) input)
            do ;;(format t "(reduce #'conj (vector) ~s)~% => ~s, expected ~s~%" input result answer)
               (is (equal-set? answer result))
               (is (vector? result)))
      ;; set conj
      (loop for input in inputs
            for answer in set-conj-answers
            as result = (reduce #'conj (hash-set) input)
            do (is (equal-set? answer result))
               (is (set? result)))
      ;; cl-hash-table conj
      (loop for input in (subseq inputs 20)
            for answer in cl-hash-map-conj-answers
            as result = (reduce #'conj (cl-hash-map) input)
            do (is (equal-set? answer result))
               (is (cl:hash-table-p result)))
      ;; hash-map conj
      (loop for input in (subseq inputs 20)
            for answer in hash-map-conj-answers
            as result = (reduce #'conj (hash-map) input)
            do (is (equal-set? answer result))
               (is (map? result))))

    ;; reduce2 on mapseq inputs (seqs on maps having 2+ values)
    (is (equal? {:a 1 :b 2 :c 3 :d 4}
                (reduce #'conj {} (seq {:a 1 :b 2 :c 3 :d 4}))))
    (is (equal? {:a 1 :b 2 :c 3 :d 4}
                (reduce #'conj {} (seq (cl-hash-map :a 1 :b 2 :c 3 :d 4)))))

    ;; set test checking duplicate removal and non-empty initial value
    (is (equal? #{1 2 3 4 5} (reduce #'conj #{1 2 3 4} '(1 2 3 4 5)))))

  ;; reduce/reduce2 on seqs for all but ht/map
  ;; *FINISH*: when we finish missing seqs
  (loop for input in (cl:list 
                      '(1 2 3)          ;(seq '(1 2 3))
                      #(1 2 3)          ;(seq #(1 2 3))
                      [1 2 3] (seq [1 2 3])
                      (list 1 2 3) (seq (list 1 2 3))
                      (cons 3 (cons 2 (cons 1 nil))) (seq (cons 3 (cons 2 (cons 1 nil))))
                      #{1 2 3}          ;(seq #{1 2 3})
                      )
        do (is (= 6 (reduce #'+ input))))
  ;; maps
  (loop for input in (cl:list
                      (cl-hash-map 10 1 20 2 30 3)
                      ;;(seq (cl-hash-map 10 1 20 2 30 3)) *FINISH*
                      (hash-map 10 1 20 2 30 3)
                      ;;(seq (hash-map 1 1 20 2 30 3)) *FINISH*
                      )
        do (is (= 66 (reduce (lambda (r kv) (+ r (first kv) (second kv))) 0 input))))
  )                                     ;reduce

(test reduced
  "REDUCED behavior in REDUCE, along with other reduce-related operations"

  ;; reduced?, reduced, deref 
  ;; If init is is a reduced, result is reduced
  (is (reduced? (reduce #'identity (reduced 1) ())))
  (is (= 1 (deref (reduce #'identity (reduced 1) ()))))
  ;; If only val is reduced, result is reduced
  (is (reduced? (reduce #'identity (cl:list (reduced 1)))))
  (is (= 1 (deref (reduce #'identity (cl:list (reduced 1))))))

  (let ((input '(0 1 2 3 4 5 6 7 8 9)))
    (is (= 45 (reduce #'+ input)))
    (let (last)
      (is (eql :big (reduce (lambda (r v) (setf last v)  (if (< v 5) (+ r v) (reduced :big))) input)))
      (is (= last 5))))
  (flet ((re-some (pred coll)
           (reduce (lambda (_ v) 
                     (declare (ignore _))
                     (alexandria:when-let (x (funcall pred v)) (reduced x)))
                   coll)))
    (is (eq t (re-some #'oddp '(0 2 4 5 6 8))))
    (is (not (re-some #'oddp '(0 2 4)))))

  ;; unreduced
  (is (eq :foo (unreduced :foo)))
  (is (eq :foo (unreduced (reduced :foo))))

  ;; ensure-reduced
  (let ((r (reduced :foo))
        (nr :foo))
    (is (eq r (ensure-reduced r)))
    (is (reduced? (ensure-reduced nr))))
  )


(test nth
  ;; CL:LIST
  (is (= 1 (nth '(1 2 3) 0)))
  (is (= 2 (nth '(1 2) 1)))
  (is (= 3 (nth '(1 2) 2 3)))
  (signals error (nth '(1 2) 3))
  ;; PersistentList
  (is (= 1 (nth (list 1 2 3) 0)))
  (is (= 2 (nth (list 1 2) 1)))
  (is (= 3 (nth (list 1 2) 2 3)))
  (signals error (nth (list 1 2) 3))
  ;; CL:VECTOR
  (is (= 1 (nth #(1 2 3) 0)))
  (is (= 2 (nth (cl-vector 1 2) 1)))
  (is (= 3 (nth #(1 2) 2 3)))
  (signals error (nth (cl-vector 1 2) 3))
  ;; FSET:SEQ
  (is (= 1 (nth [1 2 3] 0)))
  (is (= 2 (nth (vector 1 2) 1)))
  (is (= 3 (nth [1 2] 2 3)))
  (signals error (nth (vector 1 2) 3))
  ;; Sets, maps, nope, 
  (signals error (nth (hash-set 1 2) 0))
  (signals error (nth (hash-map 1 2) 0))
  ;; Strings - yes
  (is (eql #\c (nth "abc" 2)))
  ;; lazyseqs, seqs
  (is (= 2 (nth (nums 3) 1)))
  (is (= 4 (nth (nums 3) 3 4)))
  (signals error (nth (nums 3) 3))
  (is (= 2 (nth (seq (nums 3)) 1)))
  (is (= 4 (nth (seq (nums 3)) 3 4)))
  (signals error (seq (nth (nums 3) 3)))
  (is (= 1 (nth (seq [1 2 3]) 0)))
  (is (= 4 (nth (seq [1 2 3]) 3 4)))
  (signals error (seq (nth [1 2 3] 3)))
  )

(test empty
  (is (eq nil (empty '(1 2 3))))
  (is (fset:equal? [] (empty [1 2 3])))
  (is (fset:equal? #{} (empty #{1 2 3})))
  (is (fset:equal? {} (empty {:a 1})))
  (is (equal? [] (empty [1])))
  (is (equal? #{} (empty #{1})))
  (is (equal? {} (empty {1 2})))
  (is (equalp (cl-hash-map) (empty (cl-hash-map :a 1))))
  (is (equalp (cl-vector) (empty (cl-vector :a 1))))
  (is (null (empty 1)))
  (is (null (empty "abc")))             ;no empty string
  )

(test not-empty
  (flet ((isnotempty (coll) (is (eq coll (not-empty coll))))
         (isempty (coll) (is (eq nil (not-empty coll)))))
    (isnotempty "abc")
    (isnotempty #(1 2 3))
    (isnotempty [1 2 3])
    (isnotempty '(1 2))
    (isnotempty #{1})
    (isnotempty {:a 1})
    (isnotempty (cl-hash-map :a 1))
    (isempty "")
    (isempty #())
    (isempty [])
    (isempty '())
    (isempty #{})
    (isempty {})
    (isempty (cl-hash-map))))

(test doall
  "DOALL, and implicitly, DORUN"
  ;; doall on lazy seqs
  (let ((calls 0))                          ;will be 1+ elts in lazyseq
    (labels ((sidenums (n)
               (lazy-seq 
                 (incf calls)
                 ;;(format t "sidenums ~d~%" n)
                 (if (< n 1)
                     nil
                     (cons n (sidenums (1- n)))))))
      (let* ((ls (sidenums 0))
             (r (doall ls)))
        (is (clj-coll::lazyseq-p ls))
        (is (realized? ls))
        (is (eq ls r))
        (is (= 1 calls)))
      (setf calls 0)
      (let* ((ls (sidenums 1))
             (r (doall ls)))
        (is (realized? ls))
        (is (eq ls r))
        (is (= 2 calls)))
      ;; with N=3 specified to doall
      (setf calls 0)
      (let* ((ls (sidenums 5))
             (r (doall 3 ls)))
        (is (realized? ls))
        (is (eq ls r))
        (is (= 4 calls))
        ;; Because DOALL isn't what you think. DOALL N doesn't cause it to return N values.
        ;; It just does N nexts. So you're not getting '(5 4 3) back. Not here, not in clojure.
        (is (equal? '(5 4 3 2 1) r)))
      ))

  ;;*FINISH* doall on other seq types like arrayseq, including (seq lazyseq)
  )

(test lazyseqs
  ;; lazyseq will accept cl:cons and clj-coll:cons from 
  ;; thunks, test both.  Note that other seqs (e.g. arrayseq) tested elsewhere
  (loop 
    for consfun in (cl:list #'cl:cons #'clj-coll:cons)
    do
       (labels ((foo (n)
                  (lazy-seq (if (< n 1)
                                nil
                                (funcall consfun n (foo (1- n)))))))
         (is (seqable? nil))
         (is (null (seq nil)))
         (is (null (next nil)))
         ;; Empty lazyseq
         (let ((ls (foo 0)))                 ;ls == lazyseq
           (is (clj-coll::lazyseq-p ls))     ;(instance clojure.lang.LazySeq ls)
           (is (eq CLJ-COLL::*EMPTY-LIST* (empty ls)))
           (is (seq? ls))
           (is (seqable? ls))
           (is (sequential? ls))
           (is (not (counted? ls)))
           (is (not (realized? ls)))
           (is (null (first ls)))
           (is (null (second ls)))
           (is (realized? ls))
           (is (= 0 (count ls)))
           (is (null (seq ls)))
           (is (null (next ls))))
         ;; Lazyseq of 1 element
         (let ((ls (foo 1)))
           (is (clj-coll::lazyseq-p ls)) ;(instance clojure.lang.LazySeq ls)
           (is (seq? ls))
           (is (not (realized? ls)))
           (is (= 1 (first ls)))
           (is (null (second ls)))
           (is (realized? ls))
           (is (= 1 (count ls)))
           (is (null (next ls)))
           (let ((s (seq ls)))
             (is (not (clj-coll::lazyseq-p s)))
             (is (seq? s))
             (is (eq CLJ-COLL::*EMPTY-LIST* (empty ls)))
             (is (seqable? s))
             (is (sequential? s))
             (is (not (counted? s)))
             ;; realized? is for lazy-seqs, not regular seqs
             (signals error (realized? s))
             (is (= 1 (count s)))
             (is (= 1 (first s)))
             (is (null (second s)))
             (is (null (next s)))))
         ;; Lazyseq of 2 elements, and some equality
         (let* ((ls (foo 2))
                (s (seq ls)))
           (is (= 2 (count ls) (count s)))
           (is (= 1 (second ls)))      ;specifically testing second on lazyseq/seqval
           (is (= 1 (second s)))
           (is (equal? '(2 1) ls))      ;clojure '=' will do this
           (is (equal? [2 1] s)))

         ;; *FINISH* map/mapv/reduce/reducekv?
         ;; test (reduce + (take 3 (map inc (rep 5)))) on lazy-seq rep
         ))

  ;; Conj on empty lazy seqs
  ;; ... with no Xs
  (let ((c (conj (nums 0))))
    (is (clj-coll::lazyseq-p c))        ;returns coll
    (is (empty? c)))
  ;; ... with 1 x
  (let ((c (conj (nums 0) 1)))
    ;; Because 5AM lies, saying this case broke when it was really the next...
    ;; (when you 'v' to expand source in the sbcl debugger on test failure with `(debug! 'lazyseqs)`
    ;; (format t "~%HEY!  ~s, ~s~%" c (list? c))
    (is (list? c))                      ;persistentlist
    (is (equal? '(1) c)))
  ;; ... with 2 xs
  (let ((c (conj (nums 0) 1 2)))
    ;;(format t "~%HEY!  ~s, ~s~%" c (list? c))
    (is (list? c))                      ;persistentlist
    (is (equal? '(2 1) c)))

  ;; Conj on non lazy seqs
  ;; ... with no Xs
  (let ((c (conj (nums 1))))
    (is (clj-coll::lazyseq-p c))        ;returns coll
    (is (equal? #(1) c)))
  ;; ... with 1 x
  (let ((c (conj (nums 1) 2)))
    (is (cons? c))                      ;and matches Clojure behavior.
    (is (equal? '(2 1) c)))
  ;; ... with 2 xs
  (let ((c (conj (nums 1) 2 3)))
    (is (cons? c))
    (is (equal? '(3 2 1) c)))

  ;; Conj onto seq on lazyseq
  ;; ... with empty seq & no xs
  (let ((c (conj (seq (nums 0)))))
    (is (null c)))        ;returns coll
  ;; ... with empty seq & one x
  (let ((c (conj (seq (nums 0)) 1)))
    (is (list? c))
    (is (equal? #(1) c)))
  ;; ... with 1 x
  (let ((c (conj (seq (nums 1)) 2)))
    (is (cons? c))                      ;and matches Clojure behavior.
    (is (equal? '(2 1) c)))
  ;; ... with 2 xs
  (let ((c (conj (seq (nums 1)) 2 3)))
    (is (cons? c))
    (is (equal? '(3 2 1) c)))

  ;; *FINISH* conj onto other lazyseqs e.g. listseq(?), arrayseq, mapseq(?)
  )

(test deeperseqs
  "lazyseqs returning seqs and/or lazyseqs"
  (labels ((nums (n)
             (lazy-seq (if (< n 1)
                           nil
                           (cl:cons n (nums (1- n))))))
           (key (s) (intern (string-upcase (string s)) :keyword))
           (keywords (n)
             (lazy-seq (if (< n 1)
                           nil
                           (cons (key (cl:elt "ABCDEFGHI" (1- n))) (keywords (1- n))))))
           ;; lazyseq of keywords lazyseqs
           (numsfn (consfn n)
             (lazy-seq (if (< n 1)
                           nil
                           (funcall consfn (keywords n) (numsfn consfn (1- n)))))))
    (is (equal? [2 1] (nums 2)))
    (is (equal? #(2 1) (nums 2)))
    (is (equal? '(2 1) (nums 2)))
    (is (equal? (nums 2) [2 1] ))
    (is (equal? (nums 2) #(2 1)))
    (is (equal? (nums 2) '(2 1)))
    (is (equal? [:b :a] (keywords 2)))
    (is (equal? (nums 2) (nums 2)))
    (is (not (equal? (nums 2) (nums 1))))
    (is (equal? (nums 1) (nums 1)))
    (is (equal? (nums 0) (nums 0)))
    ;; lazyseqs returning lazyseqs 
    (is (equal? [[:b :a] [:a]] (numsfn #'cl:cons 2)))
    (is (equal? [[:b :a] [:a]] (numsfn #'cons 2)))
    (is (equal? [[:a]] (numsfn #'cl:cons 1)))
    (is (equal? [[:a]] (numsfn #'cons 1)))
    (is (equal? [] (numsfn #'cl:cons 0)))
    (is (equal? [] (numsfn #'cons 0)))
    ))

(test reversible?
  (is (not (reversible? "abc")))
  (is (reversible? [1 2 3]))
  (is (reversible? #(1 2 3)))
  (loop for coll in (cl:list '(1 2) (list 1 2) nil 
                             {:a 1} (cl-hash-map :a 1) #{1 2}
                             (nums 3) (seq [1 2]))
        do (is (not (reversible? coll)))))

(test rseq
  (is (null (rseq [])))
  (is (null (rseq #())))
  (is (= 3 (count (rseq [1 2 3]))))
  (is (= 3 (count (rseq #(1 2 3)))))
  (is (= 2 (count (next (rseq [1 2 3])))))
  (is (= 1 (count (rseq [1]))))
  
  (loop for coll in (cl:list '(1 2) (list 1 2) nil (list)
                             {:a 1} (cl-hash-map :a 1) #{1 2}
                             (nums 3) (seq [1 2]))
        do (signals error (rseq coll)))
  (is (equal? '(3 2 1) (rseq [1 2 3])))
  (is (equal? '(3 2 1) (rseq #(1 2 3))))
  (is (equal? '(1) (rseq #(1))))
  (is (equal? '(1) (rseq [1])))
  (is (clj-coll::arrayrseq-p (rseq #(1))))
  (is (clj-coll::arrayrseq-p (rseq [1])))
  ;; Test reduce with init combos
  (loop for coll in (cl:list #(1 2 3) [1 2 3])
        as s = (rseq coll)
        do (reduce (lambda (r v)
                     (is (= v (1- r)))
                     v)
                   4 s)
           (reduce (lambda (r v)
                     (format t "~%r=~s v=~s~%" r v))
                   s))
  ;; Test run! with optimized arrayseq/arrayreq iteration
  (let (r)
    (clj-coll:run! (lambda (x) (push x r)) (rseq #(1 2 3)))
    (is (equalp '(1 2 3) r))
    (setf r nil)
    (clj-coll:run! (lambda (x) (push x r)) (rseq [1 2 3]))
    (is (equalp '(1 2 3) r))
    (setf r nil)

    (clj-coll:run! (lambda (x) (push x r)) (next (rseq #(1 2 3))))
    (is (equalp '(1 2) r))
    (setf r nil)
    (clj-coll:run! (lambda (x) (push x r)) (next (rseq [1 2 3])))
    (is (equalp '(1 2) r))
    (setf r nil))
  )

(test arrayseq
  (let ((arr1 [1 2 3])
        (arr2 #(1 2 3)))
    (loop for arr in (cl:list arr1 arr2)
          as  seq = (seq arr)
          do (is (clj-coll::arrayseq-p seq))
             (is (seq? seq))
             (is (eq clj-coll::*EMPTY-LIST* (empty seq)))
             (is (sequential? arr))
             (is (sequential? seq))
             (is (counted? arr))
             (is (counted? seq))
             (is (seqable? arr))
             (is (seqable? seq))
             (is (equal? '(1 2 3) arr))
             (is (equal? '(1 2 3) seq))
             (is (= 3 (count arr)))
             (is (= 3 (count seq)))
             (is (= 6 (reduce #'+ arr)))
             (is (= 6 (reduce #'+ seq)))
             (is (= 6 (reduce-kv (lambda (r k v) (declare (ignore k)) (+ r v)) 0 arr)))
             ;; reduce-kv does not work for arrayseqs
             (signals error (reduce-kv (lambda (&rest r) (declare (ignore r))) 0 seq))))

  ;; test for detection/error about modified cl:vector having fewer elements than the seq implies
  (let* ((v (cl-vector 1 2 3))
         (vs (-> v seq next next)))
    (vector-pop v)
    (signals error (reduce #'conj vs)))

  ;; Test run! with optimized arrayseq/arrayreq iteration
  (let (r)
    (clj-coll:run! (lambda (x) (push x r)) (seq #(1 2 3)))
    (is (equalp '(3 2 1) r))
    (setf r nil)
    (clj-coll:run! (lambda (x) (push x r)) (seq [1 2 3]))
    (is (equalp '(3 2 1) r))
    (setf r nil)

    (clj-coll:run! (lambda (x) (push x r)) (next (seq #(1 2 3))))
    (is (equalp '(3 2) r))
    (setf r nil)
    (clj-coll:run! (lambda (x) (push x r)) (next (seq [1 2 3])))
    (is (equalp '(3 2) r))
    (setf r nil)))

(test cons
  "CLJ-COLL:CONS and some PersistentList tests"
  (let ((c (cons 1 [2])))
    (is (clj-coll::cons? c))
    (is (= 1 (first c)))
    (is (= 2 (second c)))
    (is (equal? '(1 2) c))
    (let ((s (seq c)))
      (is (eq s c)))                    ;Cons implements ISeq in clojure

    (is (equal? '(1 2 3) (cons 1 [2 3]))))

  (let ((c (cons 1 nil)))
    (is (clj-coll::persistentlist-p c)) ;same a Clojure
    (is (equal? '(1) c))
    (is (equal? '(1) (seq c)))
    (let ((c2 (cons 2 c)))
      (is (clj-coll::cons? c2))
      (is (equal? '(2 1) c2))
      (is (equal? '(2 1) (seq c2)))))

  ;; count
  (is (= 2 (count (cons 2 (cons 1 nil)))))
  (is (= 3 (count (cons 1 [2 3]))))
  (is (= 3 (count (cons 1 (list 2 3)))))

  ;; mapv on cons
  (is (equal? [3 2 1] (mapv #'identity (cons 3 (cons 2 (cons 1 nil))))))
  (is (equal? [3 2 1] (mapv #'identity (seq (cons 3 (cons 2 (cons 1 nil)))))))
  (is (equal? '(3 2 1) (mapv #'identity (cons 3 (cons 2 (cons 1 nil))))))

  ;; conj on cons
  (is (equal? [3 1 2] (mapv #'identity (conj (cons 1 [2]) 3))))

  ;; (cons x seq) (cons x lazyseq)
  (loop
    for consfn in (cl:list #'cl:cons #'cons)
    do (labels ((nums (n)
                  (lazy-seq (if (< n 1)
                                nil
                                (funcall consfn n (nums (1- n)))))))
         (is (equal? '(3 2 1) (cons 3 (nums 2))))
         (is (equal? '(3 2 1) (cons 3 (seq (nums 2)))))
         (is (equal? '(3 2 1) (seq (cons 3 (nums 2)))))
         ))

  ;; reduce 
  (is (= 1 (reduce #'+ (cons 1 nil))))
  (is (= 6 (reduce #'+ (cons 1 [2 3]))))
  (is (= 6 (reduce #'+ (cons 2 (cons 1 (list 3))))))
  )

(test persistentlist
  ;; List fn and cl:list equality
  (let ((l (list 1 2 3)))
    (is (list? l))                      ;persistent list
    (is (counted? l))
    (is (sequential? l))
    (is (= 3 (count l)))
    (is (not (equal? '(1 2 3 4) l)))
    (is (not (equal? l '(1 2 3 4))))
    (is (equal? '(1 2 3) l))
    (is (equal? l '(1 2 3)))
    ;; cl:vector equality
    ;; When first tested, I hadn't implemented a direct equiv? binding for 
    ;; cl:vector + persistentlist, but equiv? used the clj-coll:seq cl:vector combo.
    ;; However we try to avoid seqs when we can. Only.. I don't see a cl:vector combo.
    ;; HOW????  (trace :methods t clj-coll::equiv?)
    ;; However there's a seq+ cl:sequence combo, why didn't it work for lists?
    (is (equal? #(1 2 3) l))
    (is (equal? l #(1 2 3))))

  ;; Conses, lists, empty lists.
  (is (list? (list)))
  (is (cons? (cons 1 (list 2 3))))
  (is (cons? (cons 1 (list))))
  (is (list? (cons 1 nil)))

  (is (fset:equal? [1 2 3] (mapv #'identity (list 1 2 3))))
  (is (fset:equal? [1 2 3] (mapv #'identity (seq (list 1 2 3)))))
  )

(test persistentqueue
  (is (queue? (queue)))
  (is (seq? (queue))) ; remember, seq? doesn't mean it's a 'seq'
  (is (eq *EMPTY-QUEUE* (queue)))
  (is (empty? (queue)))
  (is (equal? (list) (queue)))
  (is (equal? '(1 2 3) (queue 1 2 3)))
  (is (not (equal? '(1 2 3) (queue 1 2))))
  (is (equal? [1 2 3] (mapv #'identity (queue 1 2 3))))
  (is (equal? [2 4 6] (mapv #'+ (queue 1 2 3) (queue 1 2 3))))
  (is (equal? [2 4 6] (map #'+ (queue 1 2 3) (queue 1 2 3))))
  (let ((q (queue 1 2 3)))
    ;; A queue is a list, but a list is not a queue
    (is (queue? q))
    (is (list? q))
    (is (not (queue? (list 1 2 3))))
    (is (equal? (conj *EMPTY-QUEUE* 1 2 3) q))
    (is (counted? q))
    (is (sequential? q))
    (is (= 3 (count q)))
    (is (equal? '(1 2 3) q))
    (is (equal? q '(1 2 3)))
    (is (not (equal? '(1 2) q)))
    (is (equal? '(4 1 2 3) (cons 4 q)))
    (is (equal? '(1 2 3 4) (conj q 4)))
    (is (equal? [1 2 3] q))
    (is (equal? q [1 2 3]))
    (is (equal "<-(1 2 3)-<" (princ-to-string q)))
    ;; Pop
    (let ((r (pop (queue))))
      ;; Though not all empty queues will be EQ 
      ;; depends how they're created.
      (is (eq r *EMPTY-QUEUE*)))
    (let ((r (pop q)))
      (is (queue? r))
      (is (equal? '(2 3) r)))
    (let ((r (rest q)))
      ;; It may print like a list, but it `rest` _does_ yield a queueseq as it should.
      (is (clj-coll::queueseq-p r))
      (is (equal? '(2 3) r)))
    ;; Peek
    (loop for i in '( 1 2 3 nil)
          for q = (queue 1 2 3) then (pop q)
          for j = (peek q)
          while j
          do (is (= i j))
          finally (is (and (null i) (null j))))
    ;; iteration
    (is (equal? '(1 2 3) (reduce #'conj [] (queue 1 2 3))))
    (is (equal? [2 4 6] (map #'+ (queue 1 2 3) (queue 1 2 3))))
    (is (equal? [2 4 6] (mapv #'+ (queue 1 2 3) (queue 1 2 3))))
    (is (equal? [2 3 4] (mapv #'inc (queue 1 2 3))))
    (is (equal? [2 3 4] (map #'inc (queue 1 2 3))))
    (let ((result))
      (clj-coll:run! (lambda (x) (push x result)) (queue 1 2 3))
      (is (equal '(3 2 1) result)))
    (let (result)
      (doseq (x (queue 1 2 3))
        (push x result))
      (is (equal '(3 2 1) result)))
    ))

(test list*
  (let ((r (list* (list 1))))
    (is (list? r))
    (is (equal? '(1) r)))
  (let ((r (list* 1 (list 2))))
    (is (cons? r))
    (is (equal? '(1 2) r)))
  (let ((r (list* 1 [2 3])))
    (is (cons? r))
    (is (equal? '(1 2 3) r)))
  (let ((r (list* 1 2 [3])))
    (is (cons? r))
    (is (equal? '(1 2 3) r)))
  (let ((r (list* 1 2 #{3})))
    (is (cons? r))
    (is (equal? '(1 2 3) r)))
  (is (seq? (list* #{1})))
  (is (null (list* #{})))
  (signals error (list* 1))
  (signals error (list* 1 2)))

(test mfilter
  (loop with input = '(1 2 3 4 5)
        with odds = '(1 3 5)
        for coll in (cl:list input (cl-vec input) (apply #'list input) (vec input) (set input))
        as result = (mfilter #'oddp coll)
        do (if (set? input)
               (is (equal? (set odds) (set result)))
               (is (equalp odds result))))
  (is (null (mfilter #'oddp '(2 4 6))))
  (is (null (mfilter #'oddp NIL)))
  (is (null (mfilter #'oddp [])))
  (is (null (mfilter #'oddp (nums 0)))))

(test filter
  "Filter and Filterv"
  (let ((ls (filter #'oddp '(1 2 3 4 5))))
    (is (clj-coll::lazyseq-p ls))
    (is (equal? '(1 3 5) ) ls))
  (is (fset:equal? [2] (transduce (filter #'evenp) #'conj [1 2 3])))

  (is (equalp #(1 3) (filterv #'oddp '(1 2 3 4))))
  (is (equal? [1 3] (filterv #'oddp (list 1 2 3 4))))
  (is (equal? [1 3] (filterv #'oddp [1 2 3 4])))
  (is (equal? [3 1] (filterv #'oddp (nums 4))))
  )

(test bounded-count
  (is (= 0 (bounded-count 3 (nums 0))))
  (is (= 1 (bounded-count 3 (nums 1))))
  (is (= 2 (bounded-count 3 (nums 2))))
  (is (= 2 (bounded-count 2 (nums 4))))
  (is (= 3 (bounded-count 4 (nums 3))))
  (is (= 1 (bounded-count 3 (nums 1))))
  (is (= 4 (bounded-count 2 #(1 2 3 4))))
  (is (= 4 (bounded-count 2 (seq #(1 2 3 4))))) ;clojure compatible, btw, for seqs on vectors
  (is (= 4 (bounded-count 2 (seq [1 2 3 4])))) ;clojure compatible, btw, for seqs on vectors
  (is (= 2 (bounded-count 2 (cl:list 1 2 3 4))))
  (is (= 2 (bounded-count 1 {:a 1 :b 2})))
  (is (= 2 (bounded-count 1 (cl-hash-map :a 1 :b 2))))
  (is (= 2 (bounded-count 1 (seq {:a 1 :b 2})))) ;also clojure compatible
  (is (= 2 (bounded-count 1 (seq (cl-hash-map :a 1 :b 2)))))
  )
  

(test count-while
  (loop for coll in (cl:list () #() (list) [] #{} {} (cl-hash-map) (nums 0) (seq (list)))
        do (is (= 0 (count-while #'true coll))))
  (loop for coll in (cl:list '(1) #(1) (list 1) [1] #{1} {:a 1} (cl-hash-map :a 1) (nums 1) (Seq (list 1)))
        do (is (= 0 (count-while #'false coll)))
           (is (= 1 (count-while #'true coll))))
  (loop for coll in (cl:list '(1 1 2) #(1 1 2) (list 1 1 2) [1 1 2])
        do (is (= 2 (count-while #'oddp coll))))
  (loop for coll in (cl:list '(1 nil 2) [1 nil 2] (seq [1 nil 2]))
        do (is (= 1 (count-while #'identity coll)))))


(test mdrop
  ;; N>0, possible eq returns
  (let ((v #(1 2 3))
        (l '(1 2 3)))
    (is (eq v (mdrop 'cl:vector 0 v)))
    (is (eq l (mdrop 'cl:list -1 l)))
    (let ((r (mdrop 'cl:vector 1 v)))
      (is (array-displacement r))
      (is (equalp #(2 3) r))))
    
  ;; For various sequential immutable inputs
  (loop for coll in (cl:list (list 1 2) [1 2] (seq [1 2]))
        do (is (equalp #(2) (mdrop 'cl:vector 1 coll)))
           (is (equal '(2) (mdrop 1 coll))))
  ;; N > (count coll)
  (loop for coll in (cl:list '(1 2) #(1 2) (list 1 2) [1 2] #{1 2} (nums 2) (seq [1 2]))
        do (is (null (mdrop 2 coll))) 
           (is (equalp #() (mdrop 'cl:vector 2 coll)))
           (is (not (null (mdrop 1 coll)))))
  ;; HT/MAP
  (loop for coll in (cl:list (cl-hash-map :a 1 :b 2 :c 3) {:a 1 :b 2 :c 3})
        as result = (mdrop 'cl:vector 2 coll)
        do (is (= 1 (count result)))
           (is (cl:vectorp result))
           (let* ((entry (first result))
                  (k (first entry))
                  (v (second entry)))
             (is (member k '(:a :b :c)))
             (is (< 0 v 4))))
  ;; Strings
  (is (equal '(#\d) (mdrop 3 "abcd")))

  ;; Content content checking
  (is (equal '(3 4) (mdrop 2 '(1 2 3 4))))
  (is (equalp #(3 4) (mdrop :best 2 #(1 2 3 4))))
  (is (equal '(3 4) (mdrop :best 2 '(1 2 3 4))))
  (is (equalp #(3 4) (mdrop :best 2 [1 2 3 4])))
  (is (equal '(3 4) (mdrop 2 #(1 2 3 4))))
  (is (equal '(4) (mdrop 3 [1 2 3 4])))
  (is (null (mdrop 3 [4]))))

(test mdrop-while
  ;; Data sharing checks
  (let ((coll '(1 2 3)))
    (is (eq coll (mdrop-while #'false coll)))
    (is (eq coll (mdrop-while 'cl:list 'false coll)))
    (is (eq (cdr coll) (mdrop-while #'(lambda (x) (< x 2)) coll))))
  (let ((coll #(1 2 3)))
    (is (eq coll (mdrop-while 'cl:vector 'false coll)))
    (is (array-displacement (mdrop-while 'cl:vector #'(lambda (x) (< x 2)) coll)))
    (is (equalp #(2 3) (mdrop-while 'cl:vector #'(lambda (x) (< x 2)) coll))))
  ;; Returns NIL/#() if all items are matched by PRED (or COLL is empty).
  (is (null (mdrop-while #'identity '(1 2 3))))
  (is (null (mdrop-while #'false [])))
  (is (equalp #() (mdrop-while 'cl:vector #'identity '(1 2 3))))
  (is (equalp #() (mdrop-while 'cl:vector #'false [])))
  ;; Normal stuff
  (loop for coll in (cl:list '(1 2 3) #(1 2 3) [1 2 3] (list 1 2 3) (seq [1 2 3]))
        do ;; list results
           (is (equal '(2 3) (mdrop-while #'oddp coll)))
           (is (equal '(3) (mdrop-while (lambda (x) (< x 3)) coll)))
           (is (equal '(1 2 3) (mdrop-while #'false coll)))
           (is (null (mdrop-while #'true coll)))
           ;; vector results
           (is (equalp #(2 3) (mdrop-while 'cl:vector #'oddp coll)))
           (is (equalp #(3) (mdrop-while 'cl:vector (lambda (x) (< x 3)) coll)))
           (is (equalp #(1 2 3) (mdrop-while 'cl:vector #'false coll)))
           (is (equalp #() (mdrop-while 'cl:vector #'true coll))))
  ;; seqs/realization
  (is (equalp #(2 1) (mdrop-while 'cl:vector #'oddp (nums 3))))
  (is (equal '(2 1) (mdrop-while #'oddp (seq (nums 3)))))
  (is (equalp #(2 1) (mdrop-while 'cl:vector #'oddp (seq (list 3 2 1)))))
  ;; maps
  (flet ((h-oddp (mapentry)
           (oddp (second mapentry)))
         (valid (s)
           (is (< 0 (count s) 3))
           (or (cl:find :a s :key #'first)
               (cl:find :b s :key #'first))))
    (let* ((m1 (hash-map :a 1 :b 2))
           (m2 (cl-hash-map :a 1 :b 2))
           (r1 (mdrop-while #'h-oddp m1))
           (r2 (mdrop-while #'h-oddp m2)))
      (is (valid r1))
      (is (valid r2)))))

(test drop
  (let ((r (drop 1 nil)))
    (is (clj-coll::lazyseq-p r))
    (is (empty? r)))
  ;; Verifying that drop doesn't traverse lazy sequences beyond N elements.
  ;; Behaves like clojure.
  (let ((count 0))
    (labels ((loudnums (n) (lazy-seq (incf count) (if (< n 1) nil (cons n (loudnums (- n 1)))))))
      (let ((x (+ (first (drop 2 (loudnums 4))) 100)))
        (is (= 102 x))
        (is (= 3 count)))

      ;; Transducers are eager though (Clojure excerpt here):
      ;; (defn loudnums [n] (lazy-seq (println "loudnums n=" n) (if (< n 1) nil (cons n (loudnums (- n 1))))))
      ;; (let [r (transduce (drop 1) conj (loudnums 3))]
      ;;   (println (type r))
      ;;   (println (first r)))
      ;; loudnums n= 3
      ;; loudnums n= 2
      ;; loudnums n= 1
      ;; loudnums n= 0
      ;; clojure.lang.PersistentVector
      ;; 2      

      ;; We behave the same as clojure re: eager transducers.
      (setf count 0)
      (let ((r (transduce (drop 1) #'conj  (loudnums 3))))
        (is (= 4 count))            ;instead of 2 if we short-circuited in transducer
        (is (fset:seq? r))
        (is (= 2 (first r)))
        (is (equal? [2 1] r)))))

  ;; String test
  (is (equal? [#\b #\c] (transduce (drop 1) #'conj  "abc")))

  ;; Edge cases
  (is (empty? (drop 0 [])))
  (is (empty? (drop 1 [1])))
  (is (empty? (drop 1 (seq [1]))))
  (is (equal? '(3 2 1) (drop 2 (cons 5 '(4 3 2 1))))))

(test drop-while
  (is (equal? '(6 7 8) (drop-while #'oddp '(1 3 5 6 7 8))))
  (is (clj-coll::lazyseq-p (drop-while #'oddp '(1 3 5 6))))
  (let ((r (drop-while #'oddp '(1 3 5))))
    (is (clj-coll::lazyseq-p r))
    (is (empty? r)))
  (is (empty? (transduce (drop-while #'oddp) #'conj '(1 3 5))))
  (is (equal? [6] (transduce (drop-while #'oddp) #'conj '(1 3 5 6))))
  (is (equal? [6 7] (transduce (drop-while #'oddp) #'conj '(1 3 5 6 7)))))

(test mtake
  (loop for coll in (cl:list '(1 2 3) #(1 2 3) (list 1 2 3) [1 2 3]
                             (seq '(1 2 3)) (seq #(1 2 3));fset set is ordered
                             (seq (list 1 2 3)) (seq [1 2 3]))
        as take0 = (mtake 0 coll)
        as take1 = (mtake 1 coll)
        as take2 = (mtake 2 coll)
        as take3 = (mtake 3 coll)
        as take4 = (mtake 'cl:list 4 coll)
        as take0v = (mtake 'cl:vector 0 coll)
        as take1v = (mtake 'cl:vector 1 coll)
        as take2v = (mtake 'cl:vector 2 coll)
        as take3v = (mtake 'cl:vector 3 coll)
        as take4v = (mtake 'cl:vector 4 coll)
        do (is (null take0))
           (is (equal '(1) take1))
           (is (equal '(1 2) take2))
           (is (equal '(1 2 3) take3))
           (is (equal '(1 2 3) take4))
           (is (equalp #() take0v))
           (is (equalp #(1) take1v))
           (is (equalp #(1 2) take2v))
           (is (equalp #(1 2 3) take3v))
           (is (equalp #(1 2 3) take4v))))

(test mtake-while
  (loop for coll in (cl:list '(1 1 2) #(1 1 2) (list 1 1 2) [1 1 2] (seq #(1 1 2)))
        do (is (null (mtake-while #'false coll)))
           (is (equalp #() (mtake-while 'cl:vector #'false coll)))
           (is (equal '(1 1 2) (mtake-while #'true coll)))
           (is (equal '(1 1 2) (mtake-while 'cl:list #'true coll)))
           (is (equalp #(1 1 2) (mtake-while 'cl:vector #'true coll))))
  (let* ((v #(1 1 3 4))
         (r1 (mtake-while 'cl:vector #'oddp v))
         (r2 (mtake-while 'cl:vector #'true v)))
    (is (eq v (array-displacement r1)))
    (is (eq v r2))                      ;match whole vector
    (is (equalp #(1 1 3) r1)))

  ;; Knows that set is ordered
  (is (equalp '(1) (mtake-while #'oddp #{1 1 2})))

  (is (null (mtake-while #'true nil)))
  (is (null (mtake-while 'cl:list #'true nil)))
  (is (equalp #() (mtake-while 'cl:vector #'true [])))
  (is (null (mtake-while 'cl:list #'true #())))
  (is (null (mtake-while #'true {}))))

(test take
  (is (equal? (list 1 2 3) (take 3 '(1 2 3 4 5))))
  (is (equal? (list 1 ) (take 1 '(1 2 3 4 5))))
  (is (equal? (list ) (take 0 '(1 2 3 4 5))))
  (is (equal? (list 1 2) (take 3 '(1 2))))
  (is (fset:equal? [1 2] (into [] (take 2) '(1 2 3))))
  (is (fset:equal? [1 2 3] (into [] (take 4) '(1 2 3))))
  (is (fset:equal? [1] (into [] (take 1) '(1 2 3))))
  (is (fset:equal? [] (into [] (take 0) '(1 2 3))))
  (is (equal? (list 2 1) (into nil (take 2) '(1 2 3))))
  (is (fset:equal? #{1 2} (into #{} (take 2) '(1 2 3))))
  (is (equalp '(2 1 1) (into (cl:list 1) (take 2) '( 1 2 3))))
  (is (equal? [:a] (transduce (take 1) #'conj [:a])))
  (is (equal? [:a] (transduce (comp (take 1) (take 1)) #'conj [:a])))
  (is (equal? [:a] (transduce (comp (take 1) (take 1) (take 1)) #'conj [:a]))))

(test take-while
  (is (empty? (take-while #'oddp [2 4 6])))
  (is (empty? (take-while #'oddp [])))
  (is (equal? (list 1 3 5) (take-while #'oddp '(1 3 5 6))))
  (is (equal? [1 3 5] (transduce (take-while #'oddp) #'conj '(1 3 5 6))))
  (is (empty? (transduce (take-while #'oddp) #'conj '(2 4 6))))
  (is (empty? (transduce (take-while #'oddp) #'conj (list))))
  (is (empty? (transduce (take-while #'oddp) #'conj NIL)))
  (let ((r (transduce (take-while #'oddp) #'conj #{} '(1 3 5 6))))
    (is (set? r))
    (is (equal? #{1 3 5} r))))

(test into
  "Test INTO and NINTO (indirectly)"
  ;; (into)
  (is (equal? [] (into)))
  ;; (into nil)
  (is (null (into nil)))
  (let ((r (into nil '(1 2 3))))
    (is (list? r))
    (is (equal? (list 3 2 1) r)))
  ;; (into v)
  (let ((v (vector 4 5)))
    (is (fset:equal? [4 5] (into v)))
    (is (eq v (into v))))
  (let* ((v (cl-vector 4 5))
         (v2 (into v)))                 ;returns v
    (is (eq v v2))
    (is (equalp v #(4 5))))
  ;; mutable vectors
  (let* ((v (cl-vector 1 2))
         (v2 (cl-vector 3 4))
         (v3 (into v v2)))
    (is (equalp #(1 2 3 4) v3))
    (is (not (eq v v3))))               ;makes a copy...
  ;; immutable vectors
  (let* ((v (vector 1 2))
         (v2 (vector 3 4))
         (v3 (into v v2)))
    (is (equal? #(1 2 3 4) v3))
    (is (vector? v3))
    (is (not (eq v v3))))
  ;; List targets (CL or Immutable) reverse order, and list target vs all source types
  (is (equalp '(1) (into '(1) nil)))
  (let ((result  (into '() '(1 2 3))))
    (is (list? result))                  ;persistentlist, courtesy of (conj nil ...)
    (is (equal? '(3 2 1) result))) 
  (is (equalp '(3 2 1 4) (into '(4) #(1 2 3))))
  (is (equalp '((:a 1) 3 2 1 4) (into '(4) (cl:list 1 2 3 '(:a 1)))))
  (is (equal? (cl:list [:a 1] 4) (into '(4) (hash-map :a 1))))
  (let* ((l (cl:list 1))
         (r (into l '(2 3))))
    (is (cl:listp r))
    (is (not (eq l r)))
    (loop for c on r
          do (is (not (eq c l))))) ;last cdr comparison, no list sharing for now
  ;; PersistentList targets
  (let ((r (into (list) '(1 2 3))))
    (is (list? r))
    (is (equal? '(3 2 1) r)))
  (let* ((l (list 1))
         (r (into l (list 2 3))))
    (is (list? r))
    (is (not (eq l r)))
    (is (equal? '(3 2 1) r)))
  ;; CL Vector targets
  (is (equalp #() (into (cl-vector) nil)))
  (is (equalp #(1 2 3) (into (cl-vector) '(1 2 3))))
  (is (equalp #(1 1 2 3) (into (cl-vector 1) '(1 2 3))))
  (is (equalp #(1 1 2 3) (into (cl-vector 1) #(1 2 3))))
  (let ((v (into (cl-vector 1) (cl-hash-map :a 1 :b 2))))
    (is (= 1 (first v)))
    (is (= 3 (count v)))
    (is (cl:find '(:a 1) v :test #'equalp))
    (is (cl:find '(:b 2) v :test #'equalp)))
  ;; Hash-table targets
  (is (equalp (cl-hash-map) (into (cl-hash-map) nil)))
  (is (equalp (cl-hash-map :a 1) (into (cl-hash-map) '((:a 1)))))
  (is (equalp (cl-hash-map :a 1) (into (cl-hash-map) (cl-hash-map :a 1))))
  (is (equalp (cl-hash-map :a 1 :b 2) (into (cl-hash-map :a 1) (cl-hash-map :b 2))))
  (is (equalp (cl-hash-map :a 3 :b 2) (into (cl-hash-map :a 1) #((:a 3) (:b 2)))))
  ;; Set target
  (is (equal? #{1 2 3} (into #{} [3 2 1])))
  ;; Hashmap target
  (is (equal? {{:a 1} {:b 2} :c 3 :d 4}
              (into {:d 4} (hash-map (hash-map :a 1) (hash-map :b 2) :c 3))))
  ;; lazyseq target
  (let ((r (into (nums 0) '(1 2 3))))
    (is (list? r))
    (is (equal? '(3 2 1) r)))
  (let ((r (into (nums 1) '(2 3))))
    (is (clj-coll::cons? r))
    (is (equal? '(3 2 1) r)))
  ;; Other seq target
  (let ((r (into (seq (nums 1)) '(2 3))))
    (is (clj-coll::cons? r))
    (is (equal? '(3 2 1) r)))
  (let ((r (into (seq [1]) '(2 3))))
    (is (clj-coll::cons? r))
    (is (equal? '(3 2 1) r)))

  ;; XFORM case
  ;; *FINISH* transducer support - need some kind of lazy seq?   Shoot
#|
  (is (equalp #(2 3 4) (into #() (map #'inc) #(1 2 3))))
  (is (equalp #(1 3 5) (into #() (filter #'oddp) #(1 2 3 4 5))))
|#
  )

(test cat
  "Testing cat, which always returns a transducer."
  (is (equal? [] (transduce #'cat #'conj #())))
  (is (equal? [1 2 3 4 5 6] (transduce #'cat #'conj (cl:list #(1 2) #(3 4) #(5 6))))))

(test distinct 
  ;; With COLL
  (is (equal? (list 1 2 3) (distinct '(1 2 3 1))))
  (is (equal? (list 1 2 3) (distinct '(1 1 2 3 2 1))))
  (is (equal? (list 1) (distinct '(1))))
  (is (equal? (list) (distinct '())))
  (let ((s (distinct '(1 1 2 3))))
    (is (clj-coll::lazyseq-p s))
    (is (cons? (next s))))
  ;; Transducer
  (is (equal? (list 1 2 3) (transduce (distinct) #'conj '(1 2 3 1))))
  (is (equal? (list 1) (transduce (distinct) #'conj '(1 1))))
  (is (equal? (list 1) (transduce (distinct) #'conj '(1))))
  (is (equal? (list) (transduce (distinct) #'conj '())))
  )

(test mdistinct  ; indirectly a decent test of clj-coll::convert
  (loop for coll in (cl:list (cl:list nil 1 2 3 1 nil) 
                             (list nil 1 2 3 1 nil)
                             (seq (cl:list nil 1 2 3 1 nil)) 
                             (seq (list nil 1 2 3 1 nil))
                             (seq (cl-vector nil 1 2 3 1 nil))
                             (seq (vector nil 1 2 3 1 nil))
                             (cl-vector nil 1 2 3 1 nil)
                             (vector nil 1 2 3 1 nil))
        as list-result = (mdistinct coll)
        as vec-result = (mdistinct 'cl:vector coll)
        do (is (cl:listp list-result))
           (is (cl:vectorp vec-result))
           (is (= 4 (count list-result)))
           (is (= 4 (count vec-result)))
           (is (equal (cl:list 2 3 1 nil) list-result))
           (is (equalp (cl-vector 2 3 1 nil) vec-result)))
  ;; Some non-cl-friendly coll members
  (let ((r (mdistinct [[] 1 2 3 1 [] #{} #{} {} {}])))
    (is (cl:listp r))
    (is (equal? r (cl:list 2 3 1 [] #{} {}))))

  ;; Hashtable/map results We deliberately use values for which FSet won't handle
  ;; EQUAL? the way we'd like. CL and FSet don't know equality between fset:seq and ipersistentlist
  (loop with keya = [:a]
        with keyb = (list :a)
        for coll in (cl:list (cl-hash-map :a 1 keya 2 keyb 2)
                             (seq (cl-hash-map :a 1 keya 2 keyb 2))
                             (hash-map :a 1 keya 2 keyb 2)
                             (seq (hash-map :a 1 keya 2 keyb 2)))
        as result = (mdistinct 'cl:list coll)
        do (is (= 3 (count coll)))      ;broken key equality or seq
           (is (cl:listp result))
           (is (= 2 (count result))))
  ;; Persistent cons + lazyseq
  (is (equal '(3 2 1) (mdistinct (cons 1 (nums 3)))))
  ;; sets
  (is (equal? #{[] 1 2 3} (set (mdistinct #{1 2 3 []}))))
  ;; strings
  (is (equal '(#\b #\c #\a) (mdistinct "abca")))
  )

(test some
  (flet ((pred (x) (if (oddp x) x nil)))
    (is (= 3 (some #'pred (nums 3))))
    (is (= 1 (some #'pred (nums 2))))
    (is (= 1 (some #'pred '(2 4 6 8 1))))
    (is (= 1 (some #'pred '(1))))
    (is (null (some #'pred '())))
    (is (null (some #'pred []))))
  (is (equal? [:a 3] (some (lambda (x)
                             (when (eq :a (first x)) x))
                           {:b 1 :c 2 :a 3})))
  (is (null (some (lambda (x)
                    (when (eq :a (first x)) x))
                  {:b 1 :c 2}))))

(test distinct?
  (is (distinct 1))
  (is (distinct? [] 1 2))
  (is (not (distinct? [] 1 2 []))))

(test every?                            ;and not-every?
  (is (every? #'oddp '(1 3 5))) 
  (is (not-every? #'oddp '(1 2 ))) 
  (is (not-every? #'oddp '(2)))
  (is (every? #'oddp NIL))
  (is (not (not-every? #'oddp NIL)))
  )

(test any?
  ;; Snarky clojure comment suppressed.  Or was it?
  (is (any? 1))
  (is (any? nil)))

(test not-any?
  (is (not-any? #'oddp '(2 4 6)))
  (is (null (not-any? #'oddp '(2 3 4)))))

(test peek
  (is (= 1 (peek '(1 2 3))))
  (is (= 1 (peek (list 1 2 3))))
  (is (= 3 (peek #(1 2 3))))
  (is (= 3 (peek (vector 1 2 3))))
  (is (null (peek nil)))
  (is (null (peek (list))))
  (signals error (peek (nums 3)))
  (signals error (peek (seq [1 2 3])))
  (signals error (peek #{1 2}))
  (signals error (peek #{}))
  )

(test pop
  (let* ((c '(1 2 3))
         (r (pop c)))
    (is (eq r (cdr c)))
    (is (= 2 (first r))))
  (let* ((c (cl-vector 1 2 3))
         (r (pop c)))
    (is (eq r c))
    (is (equalp #(1 2) r)))
  (let* ((c (list 1 2 3))
         (r (pop c)))
    (is (eq r (rest c)))
    (is (= 2 (first r))))
  (let* ((c (vector 1 2 3))
         (r (pop c)))
    (is (not (eq r c)))
    (is (equal? #(1 2) r)))
  ;; Because they're empty
  (loop for coll in (cl:list nil (cl-vector) (vector) (list))
        do (signals error (pop coll)))
  ;; Because they're not lists or vectors, or not fill-pointer vectors
  (loop for coll in (cl:list #{1} {:a 1} (cons 2 (cons 1 nil)) 
                             (make-array 2 :initial-contents '(1 2)))
        do (signals error (pop coll))))
    

(test index-of
  (loop for coll in (cl:list '(1 2 1) #(1 2 1) (vector 1 2 1) (list 1 2 1))
        do (is (= 0 (index-of coll 1)))
           (is (= 1 (index-of coll 2)))
           (is (= -1 (index-of coll 3))))
  (loop for coll in (cl:list '(nil) #(nil) (vector nil) (list nil))
        do (is (= 0 (index-of coll nil))))
  (loop for coll in (cl:list () #() (vector) (list))
        do (is (= -1 (index-of coll 1))))
  (signals error (index-of #{1} 1))
  (signals error (index-of {:a 1} :a))
  )

(test last-index-of
  (loop for coll in (cl:list '(1 2 1) (cl-vector 1 2 1) (vector 1 2 1) (list 1 2 1))
        do (is (= 2 (last-index-of coll 1)))
           (is (= 1 (last-index-of coll 2)))
           (is (= -1 (last-index-of coll 3))))
  (loop for coll in (cl:list '(nil) #(nil) (vector nil) (list nil))
        do (is (= 0 (last-index-of coll nil))))
  (loop for coll in (cl:list () #() (vector) (list))
        do (is (= -1 (last-index-of coll 1))))
  (signals error (last-index-of #{1} 1))
  (signals error (last-index-of {:a 1} :a))
  )

(test map
  (let ((r (map #'cl:list '(1 2 3) (vector 4 5 6))))
    (is (clj-coll::lazyseq-p r))
    (is (cl:listp (first r)))
    (is (equal? (list '(1 4) '(2 5) '(3 6)) r)))
  (is (equal? [[:a 1]] (map #'identity {:a 1})))
  (is (equal? [[1 4 7] [2 5 8]] (map #'vector [1 2 3] #(4 5 6) '(7 8))))
  (let ((r (map #'identity [])))
    (is (clj-coll::lazyseq-p r))
    (is (empty? r)))

  ;; Transducer
  (is (equal? '(4 3 2) (transduce (map #'inc) #'conj (nums 3))))
  (is (equal? (list) (transduce (map #'inc) #'conj ())))

  ;; The fourth arity taking 2+ inputs, the only clojure API
  ;; that can use this arity is `sequence`, otherwise you need
  ;; a custom transducer.

  ;; To read this, note that the map of + goes first,
  ;; we then filter the result of plus.
  ;; So   (+ 0 5 10) => 15, which is filtered
  ;; then (+ 1 6 11) => 18, which is retained, and so on.
  (is (equal? '(18 24)
               (sequence (comp (map #'+) (filter #'even?))
                         (range 5) (range 5 10) (range 10 15))))
  )

(test mmap
  (is (null (mmap #'+ nil #() [] (list) (nums 0))))
  (is (null (mmap #'+)))
  (is (equalp #() (mmap 'cl:vector 'cl:list [1] ())))
  (is (equalp #() (mmap 'cl:vector 'cl:list)))
  (is (equal '(4 8) (mmap #'+ '(1 2) #(1 2 3) [1 2 3] (list 1 2))))
  (is (equalp #(4 8) (mmap 'cl:vector #'+ '(1 2) #(1 2 3) [1 2 3] (list 1 2))))
  (is (equal '(4) (mmap #'+ '(1 2) #(1 2 3) [1 2 3] (list 1))))
  (is (equalp #(4) (mmap 'cl:vector #'+ '(1 2) #(1 2 3) [1 2 3] (list 1))))
  (loop for coll in (cl:list '(1 2 3) #(1 2 3) (list 1 2 3) (vector 1 2 3))
        as result = (mmap 'cl:list coll)
        do (is (equal '((1) (2) (3)) result)))
  (let* ((colls (cl:list '(1 2 3) #(1 2 3) (list 1 2 3) (vector 1 2 3) #{1 2 3}))
         (seqs (cl:append (cl:mapcar #'seq colls) (cl:list (nums 3))))
         (all  (cl:append colls seqs)))
    (is (= 30 (apply #'+ (apply #'mmap #'+ colls))))
    (is (= 36 (apply #'+ (apply #'mmap #'+ seqs))))
    (is (= 66 (apply #'+ (apply #'mmap #'+ all))))
    (is (= 30 (clj-apply #'+ (apply #'mmap 'cl:vector #'+ colls))))
    (is (= 36 (clj-apply #'+ (apply #'mmap 'cl:vector #'+ seqs))))
    (is (= 66 (clj-apply #'+ (apply #'mmap 'cl:vector #'+ all))))))
                             
(defparameter *xf-test*
  ;; Always deletes odd numbers.
  ;; Always duplicates even numbers N-1 times.
  (lambda (rf)
    (lambda (&rest args)
      (case (cl:length args)
        (0 (funcall rf))
        (1 (funcall rf (cl:first args)))
        (2 (let ((result (cl:first args))
                 (input  (cl:second args)))
             (cond ((cl:oddp input))
                   ((cl:evenp input)
                    (loop repeat (- input 1)
                          do (setf result (funcall rf result input))))
                   (t (error "unsupported input: ~s" input)))
             result))
        (t (let ((result (cl:first args))
                 (input  (cl:second args))
                 (inputs (cl:cddr args)))
             (cond ((cl:oddp input))
                   ((cl:evenp input)
                    (loop repeat (- input 1)
                          do (setf result (funcall rf result (cl:list* input inputs)))))
                   (t (error "unsupported input: ~s" input)))
             result)))))
  "A transducer/SEQUENCE compatible xform (function) which has
arities for an arbitrary number of inputs. Always suppresses odd inputs and
always duplicates even inputs > 0.")

(defparameter *xf-completion-inputs-to-add* 0
  "For *xf-test-multi-input-stateful* completion step, a value from 0 to 3 inclusive")

(defparameter *xf-test-multi-input-stateful*
  ;; Always inserts a duplicate record, can accept multiple inputs
  ;; As this is for SEQUENCE testing, 'result' will be a serapeum queue.
  (lambda (rf)
    (lambda (&rest args)
      (case (cl:length args)
        (0 (funcall rf))
        ;; Completion step. Result is an empty serapeum queue and we're going to
        ;; add some records for completion step testing.
        (1 (let ((result (cl:first args)))
             (ecase *xf-completion-inputs-to-add*
               (0 (funcall rf result :zero))
               (1 (funcall rf result :one))
               (2 (loop repeat 2 
                        do (setf result (funcall rf result :two))
                        finally (return result)))
               (3 (loop repeat 3
                        do (setf result (funcall rf result :three))
                        finally (return result))))))
        (2 (let ((result (cl:first args))
                 (input  (cl:second args)))
             (-<> (funcall rf result (cl:list 1 input))
                  (funcall rf <> (cl:list 2 input)))))
        (t (let ((result (cl:first args))
                 (input  (cl:second args))
                 (inputs (cl:cddr args)))
             (-<> (funcall rf result (cl:list* 1 input inputs))
                  (funcall rf <> (cl:list* 2 input inputs))))))))
  "transform for SEQUENCE testing, with a reducing function operating on a serapeum:queue
as the 'result' in xform steps")

(test sequence
  (is (equal? (list) (sequence nil)))
  ;; (sequence coll)
  (let* ((s (nums 3))
         (r (sequence s)))
    (is (eq s r)))
  (let* ((s #(1 2 3))
         (r (sequence s)))
    (is (not (eq s r)))
    (is (seq? r))
    (is (clj-coll::arrayseq-p r))
    (is (equal? '(1 2 3) r)))
  (let* ((s (list 1 2 3))
         (r (sequence s)))
    (is (eq s r)))
  (let* ((s [1 2 3])
         (r (sequence s)))
    (is (not (eq s r)))
    (is (seq? r))
    (is (clj-coll::arrayseq-p r))
    (is (equal? '(1 2 3) r)))
  ;; Turn strings into sequences
  (let* ((s "abc")
         (r (sequence s)))
    (is (equal? (list #\a #\b #\c) r)))

  ;; (sequence xform coll)
  (is (empty? (sequence (filter #'oddp) '(0))))
  (is (equal? [1] (sequence (filter #'oddp) '(1))))
  (is (equal? [1] (sequence (filter #'oddp) '(0 1))))
  (is (equal? (list 1 3 5 7 9)
              (sequence (filter #'oddp) (range 1 10))))
  (is (equal? (list 1 3 5 7 9)
              (sequence (comp (filter #'oddp) (take 5)) (range 20))))

  ;; Custom xform always deletes odd numbers, dups even numbers N-1 times
  ;; One input
  (let ((ls (sequence *xf-test* '(1 1 2 1 4 1 5))))
    (is (clj-coll::lazyseq-p ls))
    (is (equal? '(2 4 4 4) ls)))
  (let ((ls (sequence *xf-test* '(2 2 1 3 4 1 5 2))))
    (is (clj-coll::lazyseq-p ls))
    (is (equal? '(2 2 4 4 4 2) ls)))
  ;; Two inputs
  (let ((ls (sequence *xf-test* '(2 1 3 4) '(:a :b :c :d))))
    (is (clj-coll::lazyseq-p ls))
    (is (equal? '((2 :a) (4 :d) (4 :d) (4 :d)) ls)))
  ;; Three inputs, one a different length
  (let ((ls (sequence *xf-test* '(2 1 3 4 5 6) '(:a :b :c :d) '(1 2 3 4 5))))
    (is (clj-coll::lazyseq-p ls))
    (is (equal? '((2 :a 1) (4 :d 4) (4 :d 4) (4 :d 4)) ls)))
  ;; Three inputs, edge case
  (let ((ls (sequence *xf-test* '(1) [] (list))))
    (is (clj-coll::lazyseq-p ls))
    (is (equal? (list) ls)))
  ;; Test the SEQUENCE xform driver to make sure it calls the xform 
  ;; completion arity
  (is (equal? '(1 2 3 2 1) (sequence (dedupe) '(1 2 3 2 2 1 1))))
  ;; Custom xform to test multiple inputs (like map) and xform completion (like dedupe)
  ;; There is no Clojure native xform that tests this corner.
  ;; 0, 1, 2, 3 inputs
  (loop for n-inputs from 1 below 4
        as coll = (mapv #'identity (range n-inputs))
        as colls = (mrepeat n-inputs coll)
        as *xf-completion-inputs-to-add* = n-inputs
        as result = (apply #'sequence *xf-test-multi-input-stateful* colls)
        do
        (ecase n-inputs
          (1 (is (equal? '((1 0) (2 0) :one) result)))
          (2 (is (equal? '((1 0 0) (2 0 0) (1 1 1) (2 1 1) :two :two) result)))
          (3 (is (equal? '((1 0 0 0) (2 0 0 0)
                           (1 1 1 1) (2 1 1 1)
                           (1 2 2 2) (2 2 2 2)
                           :three :three :three) result))))))

(test select-keys
  ;; On maps
  (let* ((m (cl-hash-map :a 1 :b 2))
         (r (select-keys m #(:a :c))))
    (is (not (eq m r)))
    (is (equalp r (cl-hash-map :a 1))))
  (is (equalp {:c 3 :b 2} (select-keys {:a 1 :b 2 :c 3} [:c :d :b])))
  (is (equalp {} (select-keys {:a 1} [])))
  ;; On vectors
  (is (equal? {0 1 2 3} (select-keys [1 2 3] [0 0 2 4])))
  (is (equalp (cl-hash-map 0 1 2 3) (select-keys #(1 2 3) [0 0 2 4])))
  )

(test index
  (let* ((bad-data #{{:name 'betsy :weight 1000}
                     ;; mixing up mutable & immutable, don't do that
                     (cl-hash-map :name 'jake :weight 756)
                     {:name 'shyq :weight 1000}})
         (good-data (set (map (lambda (m) (into {} m)) bad-data)))
         (r (index good-data [:weight]))
         (expected {{:weight 756}  #{{:name 'jake :weight 756}} 
                    {:weight 1000} #{{:name 'shyq :weight 1000} 
                                     {:name 'betsy :weight 1000}}}))
    (signals error (index bad-data [:weight]))
    (is (map? r))
    ;; Right now this is immutable, perhaps someday it will be mutable if we can
    ;; iron out CL:hash-tables and FSet:maps and structural equivalence.
    (-> r (get {:weight 756}) first map? is)
    (is (fset:equal? expected r))
    (is (= 2 (count (get r {:weight 1000}))))

    ;; When keys aren't found in the data, we get a map with an empty-map key
    ;; and all values.  Who dreamed up this thing?  Anyway, clojure compatible.
    (let ((expected {{} good-data}))
      (flet ((tst (r) 
               (is (equal? r expected))
               (is (set? (-> r vals first)))))
        (tst (index good-data [:foo]))
        (tst (index good-data []))))
    (is (fset:equal? {} (index {} [])))
    (is (fset:equal? {} (index {} [:bar])))
    ;; Maps as rels
    (is (fset:equal? {} (index {} [])))
    (is (fset:equal? {} (index {} [:bar])))
    (is (fset:equal? {{} #{[:b 2] [:a 1]}} (index {:a 1 :b 2} [:a])))
    ;; Lists as rels - with duplicate map
    (is (equal? {{:a 1} #{{:a 1 :b 2} {:a 1 :c 2}}}
                (index (list {:a 1 :b 2} {:a 1 :b 2} {:a 1 :c 2}) [:a])))

    ;; This example https://clojuredocs.org/clojure.set/index#example-5aca6714e4b045c27b7fac3a
    ;; doing a join, minus the poison vectors passed to union.
    (let* ((ds1 #{{:id 1 :name "name1"}
                  {:id 2 :name "name2"}
                  {:id 3 :name "name3"}})
           (ds2 #{{:id 2 :address "addr2"}
                  {:id 3 :address "addr3"}
                  {:id 4 :address "addr4"}})
           (ds-both (union ds2 ds1)))
      (is (equal? (hash-set {:id 1 :name "name1"}
                            {:id 4 :address "addr4"}
                            {:id 3 :name "name3" :address "addr3"}
                            {:id 2 :address "addr2" :name "name2"})
                  (into #{} (map (lambda (m) (reduce #'merge m)) 
                                 (vals (index ds-both [:id])))))))))

(test frequencies
  (is (equal? {} (frequencies nil)))
  (is (map? (frequencies nil)))
  (is (equal? {:a 2 :b 1} (frequencies [:a :b :a])))
  (is (equalp (cl-hash-map :a 2 :b 1) (frequencies #(:a :b :a)))))

(test group-by
  (let ((r (group-by #'identity nil)))
    (is (map? r))
    (is (empty? r)))
  (let ((r (group-by #'identity '(:a :b nil :a))))
    (is (cl:hash-table-p r))
    (is (equalp (cl-hash-map :a '(:a :a) :b '(:b) nil '(nil)) r)))
  (let ((r (group-by #'identity [:a :b nil :a])))
    (is (map? r))
    (is (equal? {:a [:a :a] :b [:b] nil [nil]} r))
    (is (vector? (get r :a))))
  (flet ((add-10 (x) (+ x 10)))
    (is (equal? {11 [1 1] 12 [2 2]} (group-by #'add-10 [1 2 1 2])))
    (is (equalp (cl-hash-map 11 '(1 1) 12 '(2 2)) (group-by #'add-10 #(1 2 1 2))))))

(test merge-with
  (is (equal? {:a '(1 2) :b 2} (merge-with #'list {:a 1} {:a 2 :b 2})))
  (is (equalp (cl-hash-map :a '(1 2) :b 2) (merge-with #'cl:list (cl-hash-map :a 1) {:a 2 :b 2})))
  ;; With a nod to clojuredocs.org
  (is (equal? {"Lisp" ["Common Lisp" "Clojure" "Scheme"]
               "ML" ["Caml" "Objective Caml" "Standard ML"]}
              (merge-with #'into
	                  {"Lisp" ["Common Lisp" "Clojure"] "ML" ["Caml" "Objective Caml"]}
	                  {"Lisp" ["Scheme"] "ML" ["Standard ML"]})))
  (is (equal? {:c -100 :a 10 :b 100}
              (merge-with #'+ {:a 1  :b 2} {:a 9  :b 98 :c 0} {:c -100})))
  (is (equalp (cl-hash-map :c -100 :a 10 :b 100)
              (merge-with #'+ (cl-hash-map :a 1  :b 2) {:a 9  :b 98 :c 0} (cl-hash-map :c -100))))
  (is (equal? {:c #{1 2 3} :a #{1 2 3 7 8} :b #{4 5 6}}
              (merge-with #'union
                          {:a #{1 2 3} :b #{4 5 6}}
                          {:a #{2 3 7 8} :c #{1 2 3}}))))

(test rename-keys
  (is (fset:equal? {:b 1} (rename-keys {:a 1} {:a :b})))
  ;; Once again, the clojure.org dogs are wildly out of date (says this won't work)
  (is (fset:equal? {{:name :nickname} {:name "john"}}
                   (rename-keys {:user {:name "john"}}
                                {:user {:name :nickname}}))) 
  ;; Keys which are not present are ignored
  (is (fset:equal? {:a 1} (rename-keys {:a 1} {:b 2})))
  ;; double key collision, the equalp test is also testing mutable-in mutable-out
  (is (fset:equal? {:b 1 :a 2} (rename-keys {:a 1 :b 2}  {:a :b :b :a})))
  (is (equalp (cl-hash-map :b 1 :a 2) (rename-keys (cl-hash-map :a 1 :b 2)  (cl-hash-map :a :b :b :a)))))

(test rename
  (is (equal? #{{:aa 1 :bb 2} {:aa 11 :bb 22}} (rename #{{:a 1 :b 2} {:a 11 :b 22}} {:a :aa :b :bb})))
  (let ((*default-hashmap-constructor* 'cl-hash-map))
    ;; Not a set result because input is mutable
    (is (= 2 (count (intersection (cl:list {:aa 1 :bb 2} {:aa 11 :bb 22}) 
                                  (rename (cl-vector {:a 1 :b 2} {:a 11 :b 22}) {:a :aa :b :bb})))))
    ;; Garbage from mixed mutable/immutable maps/set combinations, until we straighten out EQUAL?.
    (let ((r (rename (cl:list (hash-map :a 1 :b 2) (hash-map :a 11 :b 22)) {:a :aa :b :bb})))
      (is (not (coll? r)))
      (is (coll? (first r))))
    ;; Disallowed (remember, syntax maps here are mutable in this sexp)
    (signals error (rename #{{:a 1 :b 2} {:a 11 :b 22}} {:a :aa :b :bb}))))

(test map-invert
  (is (equal? {1 :a 2 :b} (map-invert {:a 1 :b 2})))
  (is (equalp (cl-hash-map 1 :a 2 :b) (map-invert (cl-hash-map :a 1 :b 2))))
  (let ((r (map-invert nil)))
    (is (map? r))
    (is (equal? {} r)))
  (let ((r (map-invert {})))
    (is (map? r))
    (is (equal? {} r)))
  (let ((r (map-invert (cl-hash-map))))
    (is (cl:hash-table-p r))
    (is (equalp (cl-hash-map) r)))
  ;; Key collision
  (let ((r (map-invert {:a 1 :b 2 :c 1})))
    (is (= 2 (count r)))
    (is (eq :b (get r 2)))
    (is (or (eq :a (get r 1))
            (eq :c (get r 1))))))

(test update-keys
  (flet ((add-10 (x) (+ 10 x)))
    (is (equal? {11 :a 12 :b} (update-keys {1 :a 2 :b} #'add-10)))
    (is (equalp (cl-hash-map 11 :a 12 :b) (update-keys (cl-hash-map 1 :a 2 :b) #'add-10)))
    (let ((r (update-keys nil 'identity)))
      (is (map? r))
      (is (equal? {} r)))
    (let ((r (update-keys {} 'identity)))
      (is (map? r))
      (is (equal? {} r)))
    (let ((r (update-keys (cl-hash-map) 'identity)))
      (is (cl:hash-table-p r))
      (is (equalp (cl-hash-map) r))))
  ;; Key collision
  (let ((r  (update-keys {:a 1 :b 2 :c 3}
                         (lambda (k) (if (eq :c k) :a k)))))
    (is (= 2 (count r)))
    (is (= 2 (get r :b)))
    (is (or (= 1 (get r :a))
            (= 3 (get r :a))))))

(test update-vals
  (is (equal? {:a 2 :b 3} (update-vals {:a 1 :b 2} #'inc)))
  (is (equalp (cl-hash-map :a 2 :b 3) (update-vals (cl-hash-map :a 1 :b 2) #'inc)))
  (let ((r (update-vals nil 'identity)))
    (is (map? r))
    (is (equal? {} r)))
  (let ((r (update-vals {} 'identity)))
    (is (map? r))
    (is (equal? {} r)))
  (let ((r (update-vals (cl-hash-map) 'identity)))
    (is (cl:hash-table-p r))
    (is (equalp (cl-hash-map) r))))

(test find
  ;; Maps
  (is (null (find {:a 1} :b)))
  (is (null (find (cl-hash-map :a 1) :b)))
  (is (cl:listp (find (cl-hash-map :a 1) :a)))
  (is (vector? (find (hash-map :a 1) :a)))
  (is (equalp '(:a 1) (find (cl-hash-map :a 1) :a)))
  (is (equal? [:a 1] (find (hash-map :a 1) :a)))
  ;; Vectors
  (is (null (find [] 0)))
  (is (null (find [] 1)))
  (is (null (find #() 0)))
  (is (null (find #() 1)))
  (is (null (find [:a :b] :a))) ; non-integer keys to find on vectors => nil
  (is (cl:listp (find #(:a) 0)))
  (is (vector? (find [:a] 0)))
  (is (equalp '(0 :a) (find #(:a) 0)))
  (is (equal? [0 :a] (find [:a] 0)))
  (is (null (find #(1 2 3) 4294967296))) ;unlike clojure
  (signals error (find '(:a :b) 0))
  )

(test replace
  ;; The following two RMAP expressions are equivalent
  (is (equal? [:zero :one 2 3 :zero]
              (replace {0 :zero 1 :one} [0 1 2 3 0])))
  (is (equal? [:zero :one 2 3 :zero]
              (replace [:zero :one]  [0 1 2 3 0])))
  ;; Only vectors or *lazy* seqs as results, oy.
  ;; Map input means that we must match a mapentry in rmap
  (let ((r (replace {:a "aaa" [:b 2] 2} {:a 1 :b 2 :c 3})))
    (is (clj-coll::lazyseq-p r))
    (is (equal? [[:a 1] 2 [:c 3]] r)))

  ;; Transducers
  (is (equal? [-1 1 :b 2 -3 3]
              (transduce (replace {:a -1 :c -3}) #'conj [:a 1 :b 2 :c 3])))
  (is (equal? [:zero 1 2 :three]
              (transduce (replace [:zero 1 2 :three]) #'conj [0 1 2 3]))))

(test mreplace
  (is (null (mreplace [] nil)))
  ;; Mutates input cl:vectors AND cl:lists
  ;; Vector replacement map
  (let* ((l (cl:list 1 2 3))
         (v (cl-vector 1 2 3))
         (vmap [0 :one :two])
         (rl (mreplace vmap l))
         (rv (mreplace vmap v)))
    (is (eq rl l))
    (is (equalp '(:one :two 3) rl))
    (is (eq v rv))
    (is (equalp #(:one :two 3) rv))
    (is (equal? rl rv (replace vmap [1 2 3])))) ;replace, not mreplace, as a sanity check
  ;; Map replacement map
  (let* ((l (cl:list 1 2 3))
         (v (cl-vector 1 2 3))
         (rmap {0 0 1 :one 2 :two})
         (rl (mreplace rmap l))
         (rv (mreplace rmap v)))
    (is (eq rl l))
    (is (equalp '(:one :two 3) rl))
    (is (eq v rv))
    (is (equalp #(:one :two 3) rv))
    (is (equal? rl rv (replace rmap [1 2 3])))) ;deliberate replace
  ;; always returns cl:vector or cl:list
  ;; and test various seqs as input
  (loop with colls = (cl:list (list 1 2 3) [1 2 3])
        with seqs = (cl:mapcar 'seq colls)
        with all = (mconcat colls seqs) ; a cl:list
        with rmap = (cl-hash-map 0 0 1 :one 2 :two)
        for coll in all
        as result = (mreplace rmap coll)
        do (is (not (eq coll result)))
           ;; list? x not useful because we have seqs that are not list?
           (if (vector? coll)
               (progn
                 (is (cl:vectorp result))
                 (is (equalp #(:one :two 3) result)))
               (progn
                 (is (cl:listp result))
                 (is (equal '(:one :two 3) result)))))

  ;; Lazyseq
  (let* ((rmap {0 0 1 :one 2 :two})
         (r (mreplace rmap (nums 3))))
    (is (cl:listp r))
    (is (equalp '(3 :two :one) r)))

  ;; Empty vectors return vectors
  (let* ((v1 (cl-vector))
         (r1 (mreplace [] v1))
         (r2 (mreplace [] [])))
    (is (eq v1 r1))
    (is (empty? r1))
    (is (empty? r2))
    (is (cl:vectorp r2)))
  )

(test coll-fun
  ;; That original intent was that APIs use COLL-FUN internally. However
  ;; in case we have cold feet or otherwise havein't implemented that, we do explicit
  ;; COLL-FUN testing here.
  (is (equal? '(1 2) (filter (coll-fun #{1 2 3}) '(1 2 4))))
  (is (equal? '(:a :b) (filter (coll-fun {:a 1 :b 2}) #{:a :b :c})))
  (is (equal? '(1 2 :fred) (map (coll-fun {:a 1 :b 2} :fred) [:a :b :c])))
  (is (equal? '(1 2) (filter (coll-fun 'identity) '(1 2))))
  (is (equal? (list {:a 1} {:a 3}) (filter (coll-fun :a) [{:a 1} {:b 2} {:a 3}])))
  (is (equal? '(1 2) (filter (coll-fun (lambda (x) x)) '(1 2)))))

(test juxt ; and mjuxt
  (let ((r (funcall (juxt #'first #'second) [[:a 1]])))
    (is (vector? r))
    (is (equal? [[:a 1] nil] r)))
  (let ((r (funcall (mjuxt #'first #'second) [[:a 1]])))
    (is (cl:listp r))
    (is (equal? (cl:list [:a 1] nil) r))))

(test partial
  (is (= 30 (funcall (partial #'* 10) 3))))

(test every-pred
  (is (funcall (every-pred #'evenp #'plusp) 2 4 6))
  (is (not (funcall (every-pred #'evenp #'plusp) 2 4 5)))
  (is (funcall (every-pred (constantly nil))))
  (is (equal? '("abc" "def") 
              (filterv (every-pred #'stringp (lambda (s) (= 3 (cl:length s)))
                                   (lambda (s) 
                                     (unless (stringp s)
                                       (error "predicates were not evaluated in proper order"))
                                     t))
                       ["abc" "a" "ab" :d 1 "def" "d"]))))

(test some-fn
  (flet ((plusval (x) (and (plusp x) x))
         (oddval (x) (and (oddp x) x)))
    ;; The first pred is tried on all vals before the second pred,
    ;; so plusval selects 1 before oddval can select -1
    (is (= 1 (funcall (some-fn #'plusval #'oddval) -1 0 1)))
    (is (= -5 (funcall (some-fn #'plusval #'oddval) -2 -4 -5))))
  (is (not (funcall (some-fn (constantly t))))))

(test repeatedly ;& mrepeatedly
  (let ((x 0))
    (flet ((side () (incf x)))
      (is (empty? (repeatedly 0 #'side)))
      (let ((r (take 10 (repeatedly #'side))))
        (is (= 10 (count (distinct r))))
        (is (equal? (range 1 11) r)))
      (let ((r (repeatedly 10 #'side)))
        (is (= 10 (count (distinct r))))
        (is (equal? (range 12 22) r)))
      (is (equal nil (mrepeatedly 0 #'side)))
      (is (equal '(22 23 24) (mrepeatedly 3 #'side))))))

(test repeat ;& mrepeat
  (is (empty? (repeat 0 :a)))
  (is (equal? (list :a) (repeat 1 :a)))
  (is (equal? (list :a :a) (repeat 2 :a)))
  (is (= 10 (count (take 10 (repeat :b)))))
  (is (eql :b (first (repeat :b))))
  (is (null (mrepeat 0 :a)))
  (is (equal '(:a) (mrepeat 1 :a)))
  (is (equal '(:a :a) (mrepeat 2 :a))))

(test iterate
  (let ((fib (map #'first (iterate (lambda (pair)
                                     (let ((x (first pair))
                                           (y (second pair)))
                                       [y (+ x y)]))
                            [0 1]))))
    (is (equal? '(0 1 1 2 3 5 8 13 21 34) (take 10 fib)))))

(defun resolve-test-file (file)
  (asdf:system-relative-pathname
   "clj-coll"
   (concatenate 'string "test-files/"
                file)))

(test slurp                             ; & line-seq - to share test file
  (let* ((file (resolve-test-file "simple-text.txt"))
         (content (slurp file)))
    ;; verify slurp's result
    (with-input-from-string (stream content)
      (loop for i from 1
            as line = (read-line stream nil nil)
            while line
            do (if (< i 6)
                   (is (equal (format nil "line ~d" i) line))
                   (is (equal (format nil "~:R line" i) line)))
            finally (is (= 11 i))))
    ;; verify line-seq's result
    (with-open-file (stream file :direction :input)
      (loop with ls = (line-seq stream)
            for i from 1
            for s = (seq ls) then (next s)
            as line = (first s)
            while s
            do (if (< i 6)
                   (is (equal (format nil "line ~d" i) line))
                   (is (equal (format nil "~:R line" i) line)))
            finally (is (= 11 i))))))

(test keep ;& mkeep
  (let ((r (keep #'oddp nil)))
    (is (clj-coll::lazyseq-p r))
    (is (empty? r)))
  (flet ((oddval (x) (when (oddp x) x)))
    (is (equal? '(1 3 5) (keep #'oddval (range 1 7))))
    (is (equal '(1 3 5) (mkeep #'oddval (range 1 7))))
    (is (equal? '(1 3 5) (transduce (keep #'oddval) 'conj [] (range 1 7))))))

(test keep-indexed ;& mkeep-indexed
  (flet ((posidx (i x)
           (when (pos? x)
             i)))
    (is (equal? [1 3 4] 
                (keep-indexed #'posidx [-3 29 -7 45 3 -8])))
    (is (equal '(1 3 4)
                (mkeep-indexed #'posidx [-3 29 -7 45 3 -8])))
    (is (empty? (keep-indexed #'posidx [-1])))
    (is (null (mkeep-indexed #'posidx [-1])))
    (is (equal? [1 3 4] 
                (transduce (keep-indexed #'posidx) 
                           'conj [] [-3 29 -7 45 3 -8]))))
  (flet ((oddidx (i x) (when (oddp i) x)))
    ;; treats any coll as a seq
    (loop for coll in (cl:list '(1 2 3) [1 2 3] (list 1 2 3) #{1 2 3} 
                               (queue 1 2 3) #(1 2 3) {:a 1 :b 2 :c 3}
                               (cl-hash-map :a 1 :b 2 :c 3)
                               (nums 3) (seq (nums 30)))
          do (is (not (empty? (keep-indexed #'oddidx coll))))
             (is (not (empty? (mkeep-indexed #'oddidx coll)))))))

(test map-indexed
  (let ((r (map-indexed #'cl:list [:a :b :c])))
    (is (clj-coll::lazyseq-p r))
    (is (equal? '((0 :a) (1 :b) (2 :c)) r)))
  (is (empty? (map-indexed 'cl:list [])))
  (is (equal? [[0 #\a] [1 #\b]] (map-indexed #'vector "ab"))) ; works for strings
  ;; transducer
  (is (equalp (cl-vector '(0 :a) '(1 :b)) (into (cl-vector) (map-indexed #'cl:list) [:a :b])))
  )

(test remove ;& mremove
  (is (clj-coll::lazyseq-p (remove #'oddp [])))
  (is (null (mremove #'oddp [])))
  (is (equal? [0 2 4] (remove #'oddp (range 5))))
  (is (equal '(0 2 4) (mremove #'oddp (range 5))))
  (is (equal (cl:reverse '(0 2 4)) (transduce (remove #'oddp) #'cl-conj nil (range 5)))))

(test take-nth ;& mtake-nth
  (is (empty? (take-nth 2 [])))
  (is (null (mtake-nth 2 [])))
  (is (equal? [0] (take-nth 2 (range 1))))
  (is (equal '(0) (mtake-nth 2 (range 1))))
  (is (equal? [0 2 4] (take-nth 2 (range 6))))
  (is (equal '(0 2 4) (mtake-nth 2 (range 6))))
  (is (equal? [0 3 6 9] (take-nth 3 (range 10))))
  (is (equal '(0 3 6 9) (mtake-nth 3 (range 10))))
  (is (equal? [1 2 3] (take-nth 1 '(1 2 3))))
  (is (equal  '(1 2 3) (mtake-nth 1 '(1 2 3))))
  (signals error (mtake-nth 0 '(1 2)))
  (signals error (mtake-nth -1 '(1 2)))
  (is (equal? [0 0 0 0] (take 4 (take-nth 0 (range 2)))))
  (is (equal? '(0 1 2) (transduce (take-nth 1) #'conj [] (range 3))))
  (is (equal? '(0 2 4) (transduce (take-nth 2) #'conj [] (range 6))))
  (is (equal? '(0 3 6 9) (transduce (take-nth 3) #'conj [] (range 10))))
  (signals error (transduce (take-nth 0) #'conj [] (range 6)))
  (signals error (transduce (take-nth -1) #'conj [] (range 6)))
  )


(test dedupe ; &mdedupe
  (is (equal? '(1 2 3 2 1) (dedupe '(1 2 3 2 2 1 1))))
  (is (equal? '(#\A #\b #\c) (dedupe "Abc")))
  (is (equal? '(1 2 1 2 1 2) (dedupe [1 2 2 1 1 1 2 1 2 2])))
  (is (equal? (list 1/2) (dedupe [1/2 2/4])))
  (is (equal? (list 2 3) (sequence (comp (map #'inc) (dedupe)) [1 1 2 2])))
  ;; mdedupe as list
  (signals error (mdedupe 'foo '(1 2)))
  (is (null (mdedupe [])))
  (is (equal '(1 2 3 2 1) (mdedupe '(1 2 3 2 2 1 1))))
  (is (equal '(#\A #\b #\c) (mdedupe "Abc")))
  (is (equal '(1 2 1 2 1 2) (mdedupe 'cl:list [1 2 2 1 1 1 2 1 2 2])))
  ;; mdedupe as vector
  (is (equalp (cl-vector) (mdedupe 'cl:vector [])))
  (is (equalp (cl-vector 1 2 3 2 1) (mdedupe 'cl:vector '(1 2 3 2 2 1 1))))
  (is (equalp (cl-vector #\A #\b #\c) (mdedupe 'cl:vector "Abc")))
  (is (equalp (cl-vector 1 2 1 2 1 2) (mdedupe 'cl:vector [1 2 2 1 1 1 2 1 2 2])))
  )

(test random-sample ;& rand (implicitly) & mrandom-sample
  ;; *TBD*: we may need more lax tolerances on the assertions here due to 
  ;; random number effects
  (is (= 100 (count (random-sample 1 (range 100)))))
  (is (> 100 (count (random-sample .9 (range 100)))))
  (is (< 400 (count (transduce (random-sample .5) #'conj [] (range 1000))) 600))
  ;; mrandom-sample
  (is (= 100 (count (mrandom-sample 1 (range 100)))))
  (is (> 100 (count (mrandom-sample .9 (range 100))))))

(test rand-nth
  (let* ((r (range 100))
         (r2 (repeatedly 100 (lambda () (rand-nth r))))
         (r3 (distinct r2)))
    (is (> (count r3) 50))
    (is (every? (lambda (x)  (<= 0 x 99)) r3)))
  (is (null (rand-nth nil)))
  (signals error (rand-nth []))
  (is (< 0 (rand-nth (nums 3)) 4)))

(test clj-apply
  (is (equal '(1 2 3 4) (clj-apply #'cl:list 1 2 '(3 4))))
  (is (equal '(1 2 3 4) (clj-apply #'cl:list 1 2 [3 4]))))

(test mapcat ;& mmapcat
  (flet ((add1 (&rest nums) (mapv #'1+ nums)))
    (let ((r (mapcat #'add1 [1 2 3] [4 5 6])))
      (is (equal? (list 2 5 3 6 4 7) r))
      (is (clj-coll::lazyseq-p r))))
  (flet ((num-to-trio (n) [(1- n) n (1+ n)]))
    (is (equal? '(0 1 2 10 11 12 20 21 22)
                (clj-apply #'concat (map #'num-to-trio [1 11 21]))
                (mapcat #'num-to-trio [1 11 21]))))
  ;; mmapcat
  (flet ((add1 (&rest nums) (mapv #'1+ nums)))
    (let ((r (mmapcat #'add1 [1 2 3] [4 5 6])))
      (is (cl:listp r))
      (is (equal '(2 5 3 6 4 7) r)))
    (let ((r (mmapcat 'cl:list #'add1 [1 2 3] [4 5 6])))
      (is (cl:listp r))
      (is (equal '(2 5 3 6 4 7) r)))
    (let ((r (mmapcat 'cl:vector #'add1 [1 2 3] [4 5 6])))
      (is (cl:vectorp r))
      (is (equalp #(2 5 3 6 4 7) r)))
    (is (null (mmapcat #'add1)))
    (is (null (mmapcat 'cl:list #'add1)))
    (is (equalp #() (mmapcat 'cl:vector #'add1)))
    (is (null (mmapcat #'add1 nil)))
    (is (equalp #() (mmapcat 'cl:vector #'add1 nil)))
    (is (null (mmapcat 'cl:list #'add1 nil)))
    (is (null (mmapcat #'add1 nil [])))
    (is (null (mmapcat #'add1 [] [])))
    (is (null (mmapcat #'add1 [] []))))
  (flet ((num-to-trio (n) [(1- n) n (1+ n)]))
    (is (equal '(0 1 2 10 11 12 20 21 22)
               (mmapcat #'num-to-trio [1 11 21])))
    (is (equalp #(0 1 2 10 11 12 20 21 22)
               (mmapcat 'cl:vector #'num-to-trio [1 11 21]))))
  ;; Transducer
  (is (equal? [10 5 5 16 7 7 13]
              (into []
                    (mapcat (lambda (x) 
                              (if (<= 0 x 9)
                                  (list x x)
                                  (list x))))
                    (list 10 5 16 7 13)))))

(test lazy-cat
  (let ((ls (lazy-cat '(1) #(2) (list 3) [4] {:a 1} (cl-hash-map :b 2))))
    (is (clj-coll::lazyseq-p ls))
    (is (equal? (cl:list 1 2 3 4 [:a 1] '(:b 2)) ls)))
  (is (equal? '(1 2 3 4 5) (lazy-cat '(1 2) [3 4] #(5))))
  (let ((ls (lazy-cat)))
    (is (clj-coll::lazyseq-p ls))
    (is (empty? ls)))
  (let ((ls (lazy-cat [])))
    (is (clj-coll::lazyseq-p ls))
    (is (empty? ls))))

(test cycle ;& mcycle
  (is (empty? (cycle [])))
  (is (empty? (take 10 (cycle []))))
  (is (equal? [1 1 1] (take 3 (cycle [1]))))
  (is (equal? [1 2 3 1 2 3 1] (take 7 (cycle [1 2 3]))))
  ;; mcycle - nreps
  (is (null (mcycle [1 2 3] :nreps 0)))
  (is (null (mcycle [1 2] :nreps 0)))
  (is (null (mcycle [] :nreps 1)))
  (is (equal '(1 2 3 1 2 3) (mcycle [1 2 3] :nreps 2)))
  (is (equal '(1 2 3 1 2 3 1) (mtake 7 (mcycle [1 2 3] :nreps 3))))
  ;; mcycle - nvals
  (is (equal '(1 2 3 1 2 3 1) (mcycle [1 2 3] :nvals 7)))
  (is (null (mcycle [1 2 3] :nvals 0)))
  (is (null (mcycle [1 2] :nvals 0)))
  (is (null (mcycle [] :nvals 1)))
  (is (equal '(1 1 1) (mcycle (repeat 1) :nvals 3))) ;infinite sequence
  (signals error (mcycle [] :nvals 1 :nreps 1))
  (signals error (mcycle [])))

(test interleave ;& minterleave
  (is (equal? '(:a 1 :b 2 :c 3) (interleave [:a :b :c] [1 2 3])))
  (is (equal? '(:a 1 :b 2)  (interleave [:a :b :c] [1 2])))
  (let ((r (interleave [1 2 3])))
    (is (clj-coll::lazyseq-p r))
    (is (equal? '(1 2 3) r)))
  (let ((r (interleave nil)))
    (is (clj-coll::lazyseq-p r))
    (is (empty? r)))
  ;; minterleave
  (is (equal '(:a 1 :b 2 :c 3) (minterleave [:a :b :c] [1 2 3])))
  (is (equal '(:a 1 :b 2 :c 3) (minterleave '(:a :b :c) '(1 2 3))))
  (is (equal '(:a 1 :b 2)  (minterleave [:a :b :c] [1 2])))
  (is (equal '(:a 1 :b 2)  (minterleave '(:a :b :c) '(1 2))))
  (is (equal '(:a 1 :b 2)  (minterleave '(:a :b :c) [1 2])))
  (is (equal '(1 :a 2 :b)  (minterleave [1 2] [:a :b :c])))
  (is (equal '(1 :a -1)  (minterleave [1 2] [:a :b :c] '(-1))))
  (is (equal '(1 2 3) (minterleave [1 2 3])))
  (let* ((l '(1 2 3))
         (r (minterleave l)))
    (is (eq l r)))
  (is (null (minterleave nil)))
  (is (null (minterleave nil nil)))
  )

(test interpose ; & minterpose
  (let ((r (interpose "a" nil)))
    (is (clj-coll::lazyseq-p r))
    (is (empty? r)))
  (is (equal? '(#\a "x" #\b "x" #\c) (interpose "x" "abc")))
  (is (equal? '("a" "x" "b") (interpose "x" ["a" "b"])))
  (is (equal? '(:a) (interpose 1 [:a])))
  ;; transducer
  (is (equal? [1 0 2 0 3 0 4] (into [] (interpose 0) (concat [1 2] [3 4]))))
  (is (equal? [1] (into [] (interpose 0) [1])))
  (is (equal? [] (into [] (interpose 0) [])))
  ;; minterpose
  (is (null (minterpose "a" nil)))
  (is (equal '(#\a "x" #\b "x" #\c) (minterpose "x" "abc")))
  (is (equal '("a" "x" "b") (minterpose "x" ["a" "b"])))
  (is (equal '(:a) (minterpose 1 [:a])))
  )

(test take-last ;& mtake-last
  (is (null (take-last 4 [])))
  (is (null (take-last 4 nil)))
  (is (null (take-last 0 [1])))
  (is (null (take-last -1 [1])))
  (is (equal? '(1 2) (take-last 3 [1 2])))
  (is (equal? '(1 2 3) (take-last 3 [1 2 3])))
  (is (equal? '(2 3) (take-last 2 [1 2 3])))
  (is (cons? (take-last 2 (nums 3))))
  ;; mtake-last
  (signals error (mtake-last 'frob 3 '(1 2 3)))
  (is (null (mtake-last 4 [])))
  (is (null (mtake-last 4 nil)))
  (is (null (mtake-last 'cl:list 4 [])))
  (is (equalp #() (mtake-last 'cl:vector 4 [])))
  (is (null (mtake-last 4 nil)))
  (is (null (mtake-last 'cl:list 4 nil)))
  (is (equalp #() (mtake-last 'cl:vector 4 nil)))
  (is (null (mtake-last 0 [1])))
  (is (null (mtake-last 'cl:list 0 [1])))
  (is (null (mtake-last -1 [1])))
  (is (null (mtake-last 'cl:list -1 [1])))
  (is (equalp #() (mtake-last 'cl:vector -1 [1])))
  ;; Shared lists
  (let ((c '(1 2 3)))
    (is (eq c (mtake-last 3 c)))
    (is (eq (cdr c) (mtake-last 2 c)))
    (is (eq (cddr c) (mtake-last 1 c)))
    (is (null (mtake-last 0 c)))
    (is (eq c (mtake-last 'cl:list 3 c)))
    (is (eq (cdr c) (mtake-last 'cl:list 2 c)))
    (is (eq (cddr c) (mtake-last 'cl:list 1 c)))
    (is (null (mtake-last 'cl:list 0 c))))

  (flet ((shared-equalp (expected actual)
           (is (equalp expected actual ))
           (is (array-displacement actual))))
    (is (equalp #(1 2) (mtake-last 'cl:vector 3 [1 2])))
    (is (equal '(1 2) (mtake-last 'cl:list 3 [1 2])))
    (is (equal '(1 2) (mtake-last 'cl:list 3 '(1 2))))
    (is (equalp #(1 2) (mtake-last 'cl:vector 3 [1 2])))
    (is (equalp #(1 2) (mtake-last 'cl:vector 3 #(1 2))))
    (is (shared-equalp #(1 2) (mtake-last 'cl:vector 3 #(1 2))))
    (is (shared-equalp #(1 2) (mtake-last 'cl:vector 2 #(1 2))))
    (is (shared-equalp #(2) (mtake-last 'cl:vector 1 #(1 2))))
    (is (shared-equalp #(1 2) (mtake-last 'cl:vector 3 #(1 2))))
    (is (shared-equalp #(1 2) (mtake-last 'cl:vector 2 #(1 2))))
    (is (shared-equalp #(2) (mtake-last 'cl:vector 1 #(1 2))))
    (is (equal '(1 2) (mtake-last 'cl:list 3 [1 2])))
    (is (equalp #(1 2) (mtake-last 'cl:vector 3 [1 2])))
    (is (equalp #(1 2) (mtake-last 'cl:vector 3 [1 2])))
    (is (equalp #(1 2 3) (mtake-last 'cl:vector 3 [1 2 3])))
    (is (equalp '(2 3) (mtake-last 2 [1 2 3])))
    (is (equalp #(2 1) (mtake-last 'cl:vector 2 (nums 3))))
    ))

(test drop-last ;& mdrop-last
  ;; drop-last is going to return a lazy seq where butlast might return nil
  ;; Nothing remaining
  (let ((r (drop-last nil)))
    (is (clj-coll::lazyseq-p r))
    (is (equal? (list) r)))
  (is (null (mdrop-last nil)))
  (is (null (mdrop-last 'cl:list 1 nil)))
  (is (null (mdrop-last 1 [])))
  (is (equalp #() (mdrop-last 'cl:vector 1 nil)))
  (let ((r (drop-last [1])))
    (is (clj-coll::lazyseq-p r))
    (is (equal? (list) r)))
  (is (null (mdrop-last [1])))
  (is (null (mdrop-last 'cl:list 1 [1])))
  (is (equalp #() (mdrop-last 'cl:vector 1 [1])))
  (let ((r (drop-last 2 [1])))
    (is (clj-coll::lazyseq-p r))
    (is (equal? (list) r)))
  (is (null (mdrop-last 2 #(1))))
  (is (null (mdrop-last 'cl:list 2 #(1))))
  (is (equalp #() (mdrop-last 'cl:vector 2 #(1))))
  (is (null (mdrop-last 'cl:list 2 '(1))))
  ;; Nothing dropped
  (let ((r (drop-last 0 [1])))
    (is (clj-coll::lazyseq-p r))
    (is (equal? [1] r)))
  (let* ((v #(1))
         (r (mdrop-last 'cl:vector 0 v)))
    (is (eq v r))
    (is (not (eq v (mdrop-last 0 v))))
    (is (equal '(1) (mdrop-last 0 v))))
  (let* ((v '(1))
         (r (mdrop-last 'cl:list 0 v))
         (r2 (mdrop-last 'cl:vector 0 v)))
    (is (eq v r))
    (is (equalp #(1) r2)))
  (is (equalp #(1) (mdrop-last 'cl:vector -1 [1])))
  (is (equalp #(1) (mdrop-last 'cl:vector -1 [1])))
  (is (equal '(1) (mdrop-last 'cl:list -1 [1])))
  ;; some dropped
  (let ((r (drop-last 2 [1 2 3 4])))
    (is (clj-coll::lazyseq-p r))
    (is (equal? [1 2] r)))
  (let* ((v #(1 2 3 4))
         (r (mdrop-last 'cl:vector 2 v)))
    (is (eq (array-displacement r) v))
    (is (equalp #(1 2) r)))
  (let* ((v #(1 2 3 4))
         (r (mdrop-last 'cl:list 2 v)))
    (is (cl:listp r))
    (is (equal '(1 2) r)))
  (is (equalp #(1 2 3) (mdrop-last 'cl:vector 1 '(1 2 3 4))))
  (is (equalp '(1 2 3) (mdrop-last 'cl:list 1 #(1 2 3 4))))
  )

(test butlast ;& mbutlast
  ;; butlast uses mbutlast in vector mode
  (is (null (butlast nil)))
  (is (null (butlast [1])))
  (let ((r (butlast [1 2])))
    (is (clj-coll::arrayseq-p r))
    (is (equal? [1] r)))
  (is (equal? '(1 2 3) (butlast #(1 2 3 4))))
  ;; mbutlast in list mode
  (is (null (mbutlast nil)))
  (is (null (mbutlast [1])))
  (is (null (mbutlast 'cl:list nil)))
  (is (null (mbutlast 'cl:list [1])))
  (let ((r (mbutlast [1 2])))
    (is (cl:listp r))
    (is (equal '(1) r)))
  (let* ((v #(1 2))
         (r (mbutlast 'cl:vector v)))
    (is (cl:vectorp r))
    (is (eq (array-displacement r) v))
    (is (equalp #(1) r)))
  (is (equal '(1 2 3) (mbutlast #(1 2 3 4))))
  )

(test split-at
  (let ((r (split-at 3 (range 7))))
    (is (vector? r))
    (is (clj-coll::lazyseq-p (first r)))
    (is (not (realized? (first r))))
    (is (equal? '((0 1 2) (3 4 5 6)) r)))
  (is (equal? [(list) (list)] (split-at 3 nil))))

(test splitv-at
  (let ((r (splitv-at 3 (range 7))))
    (is (vector? r))
    (is (vector? (first r)))
    (is (clj-coll::lazyseq-p (second r)))
    (is (not (realized? (second r))))
    (is (equal? '((0 1 2) (3 4 5 6)) r)))
  (is (equal? ['() '()] (split-at 3 nil))))

(test msplit-at
  (let ((r (msplit-at 3 (range 7))))
    (is (cl:listp r))
    (is (cl:listp (first r)))
    (is (cl:listp (second r)))
    (is (equal '((0 1 2) (3 4 5 6)) r)))
  (let ((r (msplit-at 3 (range 7) :result-type 'cl:vector)))
    (is (cl:vectorp r))
    (is (cl:listp (first r)))
    (is (cl:listp (second r)))
    (is (equalp #((0 1 2) (3 4 5 6)) r)))
  (let ((r (msplit-at 3 (range 7) :partition-type 'cl:vector :result-type 'cl:vector)))
    (is (cl:vectorp r))
    (is (cl:vectorp (first r)))
    (is (cl:vectorp (second r)))
    (is (equalp #(#(0 1 2) #(3 4 5 6)) r)))
  (is (equal '(nil nil) (msplit-at 3 nil)))
  (is (equalp #(nil nil) (msplit-at 3 nil :result-type 'cl:vector)))
  (is (equalp '(#() #()) (msplit-at 3 nil :partition-type 'cl:vector)))
  (is (equalp #(#() #()) (msplit-at 3 nil :result-type 'cl:vector :partition-type 'cl:vector))))
  
(test split-with
  (let ((r (split-with (lambda (x) (< x 3)) (range 7))))
    (is (vector? r))
    (is (clj-coll::lazyseq-p (first r)))
    (is (clj-coll::lazyseq-p (second r)))
    (is (not (realized? (first r))))
    (is (not (realized? (second r))))
    (is (equal? '((0 1 2) (3 4 5 6)) r)))
  (is (equal? [(list) (list)] (split-at 3 nil))))

(test msplit-with
  (let ((r (msplit-with (lambda (x) (< x 3)) (range 7))))
    (is (cl:listp r))
    (is (cl:listp (first r)))
    (is (cl:listp (second r)))
    (is (equal '((0 1 2) (3 4 5 6)) r)))
  (let ((r (msplit-with (lambda (x) (< x 3)) (range 7) :result-type 'cl:vector)))
    (is (cl:vectorp r))
    (is (cl:listp (first r)))
    (is (cl:listp (second r)))
    (is (equalp #((0 1 2) (3 4 5 6)) r)))
  (let ((r (msplit-with (lambda (x) (< x 3)) (range 7) :result-type 'cl:vector :partition-type 'cl:vector)))
    (is (cl:vectorp r))
    (is (cl:vectorp (first r)))
    (is (cl:vectorp (second r)))
    (is (equalp #(#(0 1 2) #(3 4 5 6)) r)))
  (is (not (vector? (msplit-with #'identity nil))))
  (is (equal '(nil nil) (msplit-with #'identity nil)))
  (is (equalp '(nil nil) (msplit-with #'identity [] :partition-type 'cl:list)))
  (is (equalp '(#() #()) (msplit-with #'identity [] :partition-type 'cl:vector))))
  

(test shuffle
  (loop with expected = #{1 2 3}
        for coll in (cl:list '(1 2 3) #(1 2 3) [1 2 3] (list 1 2 3)
                             (queue 1 2 3) (nums 3) #{1 2 3})
        as r = (shuffle coll)
        do (is (equal? expected (set r))))
  (let* ((v (cl-vector 1 2 3 4 5 6))
         (r (shuffle v)))
    ;; Make sure original vector isn't mutated in cl:vector case
    (is (equalp v #(1 2 3 4 5 6))) ; testing V
    (is (equal? (set v) (set r))))
  (is (vector? (shuffle (cl:list 1 2 3))))
  (is (vector? (shuffle nil))))
        
(test reverse ;& mreverse
  (loop for coll in (cl:list '(1 2 3) (list 1 2 3) #(1 2 3) (vector 1 2 3))
       as r = (reverse coll)
       as ml = (mreverse coll)
       as mv = (mreverse 'cl:vector coll)
       do (is (equal? '(3 2 1) r))
          (is (equal '(3 2 1) ml))
          (is (equalp #(3 2 1) mv))
          (is (list? r))
          (is (cl:listp ml))
          (is (cl:vectorp mv)))
  (loop for coll in (cl:list '(1) '(1 2) '(1 2 3))
        as r = (reverse coll)
        as ml = (mreverse coll)
        as mv = (mreverse 'cl:vector coll)
        as answer = (cl:reverse coll)
        do (is (equal? answer r))
           (is (equal answer ml))
           (is (equalp (coerce answer 'cl:vector) mv)))
  (is (null (mreverse (list))))
  (is (equalp #() (mreverse 'cl:vector (list))))
  (is (null (mreverse nil)))
  (is (equal? (list) (reverse nil))))

(test when-first
  (is (= 1 (when-first (x [1 2 3]) x)))
  (is (= 4 (when-first (x '(1 2 3)) (+ x x x x))))
  (is (null (when-first (x []) x)))
  (is (null (when-first (x nil) x))))

(test max-key
  (is (equal "abcd" (max-key #'count [1 2 3] "abcd" '(1))))
  (is (= 33 (max-key #'identity 1 2 33 3 33)))
  ;; Find key with highest value  in a map
  (is (eq :c (key (clj-apply #'max-key #'val {:a 1 :b 2 :c 3}))))
  (is (eq :c (key (clj-apply #'max-key #'val (cl-hash-map :a 1 :b 2 :c 3))))))

(test min-key
  (is (equal '(1) (min-key #'count [1 2 3] "abcd" '(1))))
  (is (= 1 (min-key #'identity 1 2 33 3 33)))
  ;; Find key with highest value  in a map
  (is (member (key (clj-apply #'min-key #'val {:a 1 :b 2 :c 3 :e 1})) '(:a :e)))
  (is (eq :a (key (clj-apply #'min-key #'val (cl-hash-map :a 1 :b 2 :c 3))))))

(test reductions
  (let ((r (reductions #'+ '(1 1 1 1))))
    (is (clj-coll::lazyseq-p r))
    (is (equal? [1 2 3 4] r)))
  (is (= (reduce #'+ [1 2 3]) 
         (last (reductions #'+ [1 2 3])))))

(defun dump-compare (a b)
  "For sort, return strict < semantics for symbols/strings and/or numbers"
  (let ((a (if (or (symbolp a) (stringp a)) a (princ-to-string a)))
        (b (if (or (symbolp b) (stringp b)) b (princ-to-string b))))
    (string< a b)))

(test tree-seq ;& mtree-seq
  ;; With additional indirect tests via flatten, which uses tree-seq.
  (let ((r (map #'first (tree-seq #'next #'rest '(:A (:B (:D) (:E)) (:C (:F)))))))
    (is (clj-coll::lazyseq-p r))
    (is (equal? '(:A :B :D :E :C :F) r)))
  (let ((r (cl:map 'cl:list #'first (mtree-seq #'next #'rest '(:A (:B (:D) (:E)) (:C (:F)))))))
    (is (cl:listp r))
    (is (equal '(:A :B :D :E :C :F) r)))
  ;; Note for both CLJ-COLL and Clojure, (seq? <vector-type>) => nil/false.
  ;; And thus the behavior of `tree-seq` when used with `seq?` as a branch
  ;; function will differ.
  (loop for coll in (cl:list '(1 2 (3 (4))) (list 1 2 (list 3 (list 4)))
                             #(1 2 #(3 #(4))) [1 2 [3 [4]]])
        do (is (equal? (if (seq? coll)
                           '((1 2 (3 (4))) 1 2 (3 (4)) 3 (4) 4)
                           '((1 2 (3 (4)))))
                       (tree-seq #'seq? #'identity coll))))
  ;; Mtree-seq doesn't convert the nodes that were passed to it from
  ;; whatever they were to cl:list or cl:vector so you can't EQUAL the output unless
  ;; the nodes are CL:LIST inputs.
  (loop for coll in (cl:list '(1 2 (3 (4))) (list 1 2 (list 3 (list 4)))
                             #(1 2 #(3 #(4))) [1 2 [3 [4]]])
        as result = (mtree-seq #'seq? #'identity coll)
        as expected = (if (seq? coll)
                          '((1 2 (3 (4))) 1 2 (3 (4)) 3 (4) 4)
                          '((1 2 (3 (4))))) ;some vector input
        as pred = (if (cl:listp (first result)) #'equal #'equal?)
        do (is (cl:listp result))
           (is (funcall pred expected result)))
  ;; Some map action
  ;; Note, keys and vals on cl-hash-map will vary in order across platforms
  ;; and even successive uses in different versions of the same lisp
  (flet ((keys-and-vals (map)
           ;; Return keys and corresponding in predicable order
           (let* ((keys (cl:sort (mkeys map) #'dump-compare))
                  (vals (cl-map 'cl:list (lambda (k) (get map k)) keys)))
             (values keys vals))))

    (loop for map in (cl:list {:a 1 :b {:c 3 :d 4 :e {:f 6 :g 7}}}
                              (cl-hash-map :a 1 :b (cl-hash-map :c 3 :d 4 :e (cl-hash-map :f 6 :g 7))))
          as result = (tree-seq #'mapp  ;look at children of maps/hash-tables
                                (lambda (map) 
                                  (apply #'interleave 
                                         (multiple-value-list (keys-and-vals map))))
                                map)
          as mresult = (mtree-seq #'mapp ;look at children of maps/hash-tables
                                  (lambda (map) 
                                    (apply #'interleave 
                                           (multiple-value-list (keys-and-vals map))))
                                  map)
          do (is (equal? 
                  (cl:list {:a 1 :b {:c 3 :d 4 :e {:f 6 :g 7}}} 
                           :a 1 :b {:c 3 :d 4 :e {:f 6 :g 7}} 
                           :c 3 :d 4 :e {:f 6 :g 7}  
                           :f 6 :g 7)
                  result mresult)))))

(test flatten ;& mflatten
  (let ((r (flatten [1 [2 [3 4 [5 6]]]])))
    (is (clj-coll::lazyseq-p r))
    (is (equal? (list 1 2 3 4 5 6) r)))
  (is (equal '(1 2 3 4 5 6) (mflatten [1 [2 [3 4 [5 6]]]])))
  (let ((r (flatten nil)))
    (is (clj-coll::lazyseq-p r))
    (is (equal? (list) r)))  
  (is (null (mflatten nil)))
  (is (equal? '(nil 1 x :a "abc") (flatten '(nil 1 x :a "abc"))))
  (is (equal '(nil 1 x :a "abc") (mflatten '(nil 1 x :a "abc"))))
  (is (empty? (flatten "abc")))
  (is (empty? (mflatten "abc")))
  (is (equal? '(#\a #\b #\c) (flatten (seq "abc"))))
  (is (equal '(#\a #\b #\c) (mflatten (seq "abc"))))
  (is (empty? (flatten #{:a :b})))
  (is (empty? (mflatten #{:a :b})))
  (is (equal? (list :A :B :C :D :E 'F :G #{1 :H})
              (flatten [:a :b #(:c :d) '(:e f) (list :g #{:h 1})])))
  ;; Can't EQUAL compare the sets
  (is (equal (cl:butlast (cl:list :A :B :C :D :E 'F :G #{1 :H}))
             (cl:butlast (mflatten [:a :b #(:c :d) '(:e f) (list :g #{:h 1})])))))

(test walk
  (loop for coll in (cl:list '(1 2 3) #(1 2 3) (list 1 2 3) [1 2 3] (queue 1 2 3)
                             nil (seq [1 2 3]) (nums 3) 
                             #{1 2 3} {:a 1 :b 2 :c 3} (cl-hash-map :a 1 :b 2 :c 3))
        as result = (walk #'identity #'identity coll)
        do ;;(format t "~%coll type: ~s, result type: ~s" (type-of coll) (type-of result))
           (is (equal? coll result))
           ;; type check
           (typecase coll 
            ((or clj-coll::seq clj-coll::lazyseq)
             (is (typep result 'clj-coll::lazyseq)))
            (cl:vector  ; coll is (simple-vector 3), result is (cl:vector T 4)
             (is (typep result 'cl:vector)))
            (t (is (equal (type-of coll) (type-of result)))))))

(defun test-walk (walkfn &optional debug)
  "Walk form with WALKFN which should be #'prewalk or #'postwalk.
Returns a vector of stages of the walk to be verified for order.
While the walk result is the same in this case for both
prewalk and postwalk, the returned stages will be reversed."
  (let* ((counter 0)
         (stages (cl-vector))
         (r (flet ((print-touch (x)
                     (incf counter)
                     (when debug (format t "~%~s : ~s => " counter x)))
                   (change-type (x)
                     (let ((new-x (if (vector? x)
                                      (clj-apply #'list x)
                                      (format nil "~s" x))))
                       (when debug (format t "~s" new-x))
                       new-x)))
              (funcall walkfn
               (lambda (x) 
                 (print-touch x)
                 (let ((r (change-type x)))
                   (vector-push-extend r stages)
                   r))
               [:a [:ba :bb] :c]))))
    (is (list? r))
    (is (string? (first r)))
    (is (list? (second r)))
    (is (equal? (list ":A" (list ":BA" ":BB") ":C") r))
    stages))

(test postwalk
  (is (equal?
       [":A" 
       ":BA" 
       ":BB" 
       (list ":BA" ":BB") 
       ":C" 
       (list ":A" (list ":BA" ":BB") ":C")]
       (test-walk #'postwalk)))

  (is (equal?
       [1 2 :a 3
       [:a 3] ; the MapEntry
       {:a 3} ; the Map
       4 5 [5] (list 4 [5])
       [1 2 {:a 3} (list 4 [5])]] ;the root
       (let ((a []))
         (postwalk 
          (lambda (form) 
            ;;(format t "~%a=~s, form=~s, conj=~s" a form (conj a form))
            (setf a (conj a form)) form)
          [1 2 {:a 3} (list 4 [5])])
         a))))

(test prewalk
  (is (equal?
       [(list :A [:BA :BB] :C) 
       ":A" 
       (list :BA :BB) 
       ":BA" 
       ":BB" 
       ":C"]
       (test-walk #'prewalk)))

  (let* ((matrix [[1 2 3]
                  [4 5 6]
                  [7 8 9]])
         (r (prewalk (lambda (x) (if (number? x) (inc x) x)) matrix)))
    (is (equal? [[2 3 4] [5 6 7] [8 9 10]] r))
    (is (every? #'vector? r))))

(test prewalk-replace
  (is (equal '(c (d b))                 ;not equal?, should be a cl:list
             (prewalk-replace {'a 'b} '(c (d a)))))
  (is (equal? [1 2 [1 2] :c]
              (prewalk-replace {:a 1 :b 2} [:a :b [:a :b] :c])))
  (is (equal? [:b {:b :b} (list 3 :c :b)]
              (prewalk-replace {:a :b} [:a {:a :a} (list 3 :c :a)]))))

(test postwalk-replace
  (is (equal? [1 2 [1 2] :c]
              (postwalk-replace {:a 1 :b 2} [:a :b [:a :b] :c])))
  (is (equal? [:b {:b :b} (list 3 :c :b)]
              (postwalk-replace {:a :b} [:a {:a :a} (list 3 :c :a)]))))

(test join
  ;; I have put zero thought into this, tests are mostly from the clojuredocs page.
  ;; Without specific key map
  (is (equal? #{{:b 1 :a 1} 
                {:b 2 :a 1} 
                {:b 1 :a 2} 
                {:b 2 :a 2}}
              (join  #{{:a 1} {:a 2}} #{{:b 1} {:b 2}})))
  (is (equal? #{{:a 5 :b 6} {:a 1 :b 2 :c 3 :d 4}}
              (join [{:a 1 :b 2}] [{:c 3 :d 4} {:a 5 :b 6}]))) 

  (let ((animals #{{:name "betsy" :owner "brian" :kind "cow"}
                   {:name "jake"  :owner "brian" :kind "horse"}
                   {:name "josie" :owner "dawn"  :kind "cow"}})
        (personalities #{{:kind "cow" :personality "stoic"}
                         {:kind "horse" :personality "skittish"}}))
    (is (equal? #{{:owner "dawn"  :name "josie" :kind "cow"   :personality "stoic"}
                  {:owner "brian" :name "betsy" :kind "cow"   :personality "stoic"}
                  {:owner "brian" :name "jake"  :kind "horse" :personality "skittish"}}
                (join animals personalities)))
    ;; with cl:hash-tables.  Index is going to complain about mutable content in sets.
    (signals error (equal? #{{:owner "dawn"  :name "josie" :kind "cow"   :personality "stoic"}
                             {:owner "brian" :name "betsy" :kind "cow"   :personality "stoic"}
                             {:owner "brian" :name "jake"  :kind "horse" :personality "skittish"}}
                           (join (into #{} (map (lambda (m) (into (cl-hash-map) m)) animals))
                                 (into #{} (map (lambda (m) (into (cl-hash-map) m)) personalities)))))

    ;; With specific key map
    (let ((personalities #{{:species "cow" :personality "stoic"}
                           {:species "horse" :personality "skittish"}}))
      (is (equal? #{{:kind "cow" :owner "dawn" :name "josie" :species "cow" :personality "stoic"}
                    {:kind "horse" :owner "brian" :name "jake" :species "horse" :personality "skittish"}
                    {:kind "cow" :owner "brian" :name "betsy" :species "cow" :personality "stoic"}}
                  (join animals personalities {:kind :species}))))))

(test select
  (let ((r (select #'odd? #{1 2 3})))
    (is (equal? #{1 3} r))
    (is (set? r))))

(test project
  (is (equal? #{} (project nil #(:a))))
  (is (equal? #{{:a 1}} (project #{{:a 1 :b 2}} [:a])))
  (is (equal? #{{:a 1}} (project #{(cl-hash-map :a 1 :b 2)} [:a])))
  (let ((compositions
          #{{:name "Art of the Fugue" :composer "J. S. Bach"}
            {:name "Musical Offering" :composer "J. S. Bach"}
            {:name "Requiem" :composer "Giuseppe Verdi"}
            {:name "Requiem" :composer "W. A. Mozart"}}))
    (let ((r (project compositions [:name])))
      (is (set? r))
      (is (equal? #{{:name "Art of the Fugue"}
                    {:name "Requiem"}
                    {:name "Musical Offering"}}
                  r)))
    (is (equal? #{{:composer "W. A. Mozart"}
                  {:composer "Giuseppe Verdi"}
                  {:composer "J. S. Bach"}}
                (project compositions [:composer])))
    (is (equal? #{{}} (project compositions [:year])))
    (is (equal? #{{}} (project #{{}} [:name])))))

(test halt-when
  (is (eq :anomaly 
          (into [] (comp (filter #'some?) (halt-when (coll-fun #{:anomaly})))
                [1 2 3 :anomaly 4])))
  (is (equal? {:anomaly :oh-no!
               :partial-results [1 2]}
              (into []
                    (halt-when (coll-fun :anomaly) 
                               (lambda (x y) 
                                 (assoc y :partial-results x)))
                    [1 2 {:anomaly :oh-no!} 3 4]))))

(test completing
  (flet ((my-abs (x) (cl:abs x)))
    (is (= 8 (transduce (filter #'odd?) (completing #'+ #'my-abs) 0 (range -5 2))))))

(test keys ;& mkeys
  (is (null (keys {})))
  (is (null (keys (cl-hash-map))))
  (is (null (mkeys {})))
  (is (null (mkeys (cl-hash-map))))
  (signals error (mkeys 'cl:sequence {}))
  (signals error (mkeys []))
  (signals error (mkeys (cl-vector)))
  (is (equal? '(:a) (keys {:a 1})))
  (is (equal? (set '(:a :b)) (set (keys {:a 1 :b 2}))))
  (is (equal '(:a) (mkeys {:a 1})))
  (is (equal '(:a) (mkeys (cl-hash-map :a 1))))
  (flet ((ksort (s) (cl:sort s #'string<)))
    (is (equal '(:a :b) (ksort (mkeys {:a 1 :b 2}))))
    (is (equal '(:a :b) (ksort (mkeys (cl-hash-map :a 1 :b 2)))))
    (is (equalp #(:a) (mkeys 'cl:vector {:a 1})))
    (is (equalp #(:a) (mkeys 'cl:vector (cl-hash-map :a 1))))
    (is (equalp #(:a :b) (ksort (mkeys 'cl:vector {:a 1 :b 2}))))
    (is (equalp #(:a :b) (ksort (mkeys 'cl:vector (cl-hash-map :a 1 :b 2)))))))

(test vals ;& mvals
  (is (null (vals {})))
  (is (null (vals (cl-hash-map))))
  (is (null (mvals {})))
  (is (null (mvals (cl-hash-map))))
  (signals error (mvals 'cl:sequence {}))
  (signals error (mvals []))
  (signals error (mvals (cl-vector)))
  (is (equal? '(1) (vals {:a 1})))
  (is (equal? (set '(1 2)) (set (vals {:a 1 :b 2}))))
  (is (equal '(1) (mvals {:a 1})))
  (is (equal '(1) (mvals (cl-hash-map :a 1))))
  (flet ((ksort (s) (cl:sort s #'<)))
    (is (equal '(1 2) (ksort (mvals {:a 1 :b 2}))))
    (is (equal '(1 2) (ksort (mvals (cl-hash-map :a 1 :b 2)))))
    (is (equalp #(1) (mvals 'cl:vector {:a 1})))
    (is (equalp #(1) (mvals 'cl:vector (cl-hash-map :a 1))))
    (is (equalp #(1 2) (ksort (mvals 'cl:vector {:a 1 :b 2}))))
    (is (equalp #(1 2) (ksort (mvals 'cl:vector (cl-hash-map :a 1 :b 2)))))))

(test mutation-barrier
  (let ((*mutation-considered-harmful* t))
    ;; The direct mutators
    (signals clj-coll::mutation-error (conj (cl-vector) 1))
    (signals clj-coll::mutation-error (conj (cl-hash-map) [:a 1]))
    (signals clj-coll::mutation-error (assoc (cl-hash-map :a 1) :b 2))
    (signals clj-coll::mutation-error (assoc (cl-vector) 0 1))
    (signals error (dissoc (cl-vector 1 2) 0)) ;dissoc not supported on vectors, different error
    (signals clj-coll::mutation-error (dissoc (cl-hash-map :a 1) :a))
    (signals clj-coll::mutation-error (pop (cl-vector 1 2 3)))
    ;; Pop on cl:list is okay, just returns cdr of list
    (is (equal '(2 3) (pop (cl:list 1 2 3))))
    (signals clj-coll::mutation-error (rename-keys (cl-hash-map :a 1) {:a 2}))
    (signals clj-coll::mutation-error (mreplace {:a 2} (cl-vector :a 1)))
    (signals clj-coll::mutation-error (mreplace {:a 2} (cl:list :a 1)))
    ;; The indirect mutators
    ;; <punted for now> ASSOC-IN, MERGE, MERGE-WITH, RENAME, REPLACE, UPDATE, UPDATE-IN
    ))

(test safe-vector-push-extend
  (loop with inputs 
          = (cl:list #(1 2) 
                     (make-array 2 :initial-contents '(1 2))
                     (make-array 2 :fill-pointer 2 :initial-contents '(1 2))
                     ;; will NOT allow vector-push-extend in sbcl
                     (make-array 2 :adjustable t :initial-contents '(1 2))
                     (make-array 2 :fill-pointer 2 :adjustable t :initial-contents '(1 2)))
        for v in inputs
        for realloc-expected in
        #+(OR)(print (cl:mapcar (lambda (v) (not (adjustable-array-p v))) inputs))
        '(t t
          ;; Whether fill-pointered arrays are adjustable varies by implementation
          ;; fill-pointered arrays are implicitly adjustable in SBCL, but not CCL
          ;; If this is T, they have fill pointers without being adjustable.
          ;; If it is NIL, they can't have fillpointers without being adjustable.
          #+(OR CCL ECL) T
          #-(OR CCL ECL) NIL           ;sbcl, FP implies adjustable
          t              ;adjustable arrays are not implicitly fill-pointered in SBCL
          nil)
        as r = (clj-coll::safe-vector-push-extend 3 v)
        do ;;(format t "~%v = ~s, realloc-expected = ~s, r = ~s, (eq v r) = ~s~%" v realloc-expected r (eq v r))
           ;;(describe v)
           (if realloc-expected
               (is (and r (not (eq v r))) (equalp #(1 2 3) r))
               (is (and (null r) (equalp #(1 2 3) v)))))
  (let* ((v (make-array 2 :fill-pointer 0))
         (r (clj-coll::safe-vector-push-extend 3 v)))
    ;; Fill-pointered arrays are adjustable in SBCL
    (is (null r)) ; returns nil if not realloced
    (is (equalp #(3) v))))

#+(OR)
(test realloc
  (flet ((realloc (x &optional (n 1))
           (clj-coll::realloc-if-necessary x n)))
    (loop for v in (cl:list #(1 2) 
                            (make-array 2 :fill-pointer 2 :initial-contents '(1 2))
                            (make-array 2 :adjustable t :initial-contents '(1 2))
                            (make-array 2 :fill-pointer 2 :adjustable t :initial-contents '(1 2))
                            (make-array 2 :fill-pointer 0))
          for realloc-expected in '(t 
                                    nil ; fill-pointered arrays are implicitly adjustable in SBCL
                                    t nil nil)
          as r = (realloc v)
          do ;;(format t "~%v = ~s, realloc-expected = ~s, r = ~s, (eq v r) = ~s~%" v realloc-expected r (eq v r))
             (if realloc-expected
                 (is (not (eq v r)))
                 (is (eq v r)))
             (when (= 2 (cl:length r))
               (is (equalp r #(1 2)))))
    (let* ((v (make-array 3 :fill-pointer 1 :initial-contents '(1 2 3))) ; SBCL requires 3 for initial contents
           (r (realloc v 2)))
      (is (eq v r))
      ;;(is (not (eq v (realloc v 3)))) depends on whether implicitly adjustable because of fill pointer
      )))

(defun run-tests ()
  "Run all :clj-coll tests."
  ;; (debug! 'test-name) if you're debugging a particular broken test.
  (explain! (run 'test-suite)))
