(in-package :clj-coll)

;;;; Pretty printing support for CLJ-COLL types as well
;;;; as related Common Lisp types for which CLJ-COLL provides support.
;;;; See README.md for some printing-related notes.

(defun map-printer (stream map kv-iterator)
;; *FINISH*: use an APIs kv-iterator abstraction from clojure if we can
;; REDUCE-KV I think, for vectors and maps
  "Print MAP (assumed to be some kind of map we know about) 
to STREAM in {key val ...} notation.

Note that we don't print commas between mapentries like Clojure would, it isn't
going to READ in Common Lisp like it would in Clojure (where it's just whitespace).

KV-ITERATOR must be some function to iterate over elements of MAP as if by MAPHASH

Returns N/A."
  ;; Ignoring *print-readably* for now, which is technically our responsibility here.
  (let ((ht?  (cl:hash-table-p map)))
    (pprint-logical-block (stream nil)
      (pprint-newline :fill stream)
      (if ht?
          (princ "(CL-HASH-MAP " stream)
          (princ +open-brace+ stream))
      (let ((elts-printed 0)
            (count (count map)))
        (block nil
          (funcall kv-iterator
                   (lambda (k v) 
                     (unless (= 0 elts-printed) ;no space before first k
                       (princ #\space stream))
                     (when (and *print-length* (= elts-printed *print-length*))
                       (princ "..." stream)
                       (return))
                     ;; Is *print-depth* handled by pprint-logical-block and friends?
                     ;; Or must we implement it here?
                     (prin1 k stream)
                     (princ #\space stream)
                     (prin1 v stream)
                     (incf elts-printed)
                     (unless (= elts-printed count)
                       (pprint-newline :fill stream)))
                   map)))
      (if ht?
          (princ ")" stream)
          (princ +close-brace+ stream))
      (princ #\space stream))))

(defun hash-table-map-printer (stream hash-table)
  "Function for use with SET-PPRINT-DISPATCH to print CL hash-tables.
Return value: N/A"
  (map-printer stream hash-table #'maphash))

(defun immutable-map-printer (stream map)
  "Function for use with SET-PPRINT-DISPATCH to print FSet maps.
Return value: N/A"
  (map-printer stream map 
               (lambda (f map)          ;maphash signature
                 (fset:do-map (k v map) (funcall f k v)))))

(defun enable-map-printing (&optional (priority 0) (table *print-pprint-dispatch*))
  "Enable pretty printing of maps (mutable or immutable) supported by CLJ-COLL
in the specified (or current) pprint-dispatch table.

This function will install a pprint-dispatch function with the specified 
priority in the specified pprint dispatch table. Priority must be of type REAL.

Returns T."
  (set-pprint-dispatch 'cl:hash-table 'hash-table-map-printer priority table)
  (set-pprint-dispatch +immutable-map-type+ 'immutable-map-printer priority table)
  t)

(defun disable-map-printing (&optional (table *print-pprint-dispatch*))
  "Disable pretty printing of maps (mutable or immutable) supported by CLJ-COLL
in the specified (or current) readtable.

This function will remove any pprint-dispatch function for supported CLJ-COLL
map types in TABLE, including dispatch functions which were not installed
by ENABLE-MAP-PRINTING.

Returns NIL."
  (set-pprint-dispatch 'cl:hash-table NIL 0 table)
  (set-pprint-dispatch +immutable-map-type+ NIL 0 table)
  NIL)

#|
(defvar *m* (serapeum:dict :a 1 :b 2 :c 3 :d 4 :e (serapeum:dict :f 6 :g 7 :h 8 :i 9) :j 10 :k 11 :l 12 :m (serapeum:dict :n 13 :o 14 :p (serapeum:dict :q 15 :r 16 :s 18 :t 19 :u 20))))
(defvar *m2* (serapeum:dict :a 1 :b 2 :c '(c1 c2 c3) :d 4 :e (serapeum:dict :f 6 :g 7 :h 8 :i 9) :j 10 :k 11 :l 12 :m (serapeum:dict :n 13 :o 14 :p (serapeum:dict :q 15 :r 16 :s #(s1 s2 s3) :t 19 :u 20))))
;clj
(def m {:a 1 :b 2 :c 3 :d 4 :e {:f 6 :g 7 :h 8 :i 9} :j 10 :k 11 :l 12 :m {:n 13 :o 14 :p {:q 15 :r 16 :s 18 :t 19 :u 20}}})
(swank:inspect-in-emacs *m*)
(enable-map-printing)
(let ((*print-right-margin* 50)) (format t "~s~%" *m*))
|#


;;;
;;; Vector & fset:seq printing
;;;

(defun pprint-vector (stream vector)
  "Pretty print cl:vector and fset:seq types. Return value: N/A"
  (pprint-logical-block (stream nil :prefix "[" :suffix "]")
    (reduce-kv 
     (lambda (_ key val)
       (declare (ignore _))
       (unless (zerop key)
         (format stream " ~:_"))        ;== (pprint-newline :fill)
       (pprint-pop)                     ;for *print-length* logic
       (prin1 val stream))
     0 vector)))

(defun enable-vector-printing (&optional (priority 0) (table *print-pprint-dispatch*))
  "Enable pretty printing of immutable vectors supported by CLJ-COLL
in the specified (or current) pprint-dispatch table so that it uses the Clojure-style syntax.

This function will install a pprint-dispatch function with the specified 
priority in the specified pprint dispatch table. Priority must be of type REAL.

Returns T.

Note that if you don't use ENABLE-VECTOR-PRINTING, you'll
get the FSet printing for FSET:SEQ, which is #[ 1 2 3 ... ]"

  ;; Note that there are two problems (or "features" YMMV) about setting a printer
  ;; for CL:VECTOR types:
  ;; 1. You don't want to print strings like vectors, which
  ;; happens if you specify a CL:VECTOR printer of your own on SBCL (and perhaps
  ;; other lisps, I haven't tested).
  ;; 2. It is useful to have the default #(a b c)
  ;; vector printing (which is how it looks in SBCL at least) so that you can print
  ;; CL vectors readably and know when you see the print representation that it's a
  ;; mutable hash table.
  ;; (set-pprint-dispatch 'cl:vector 'pprint-vector priority table) ;nope.
  (set-pprint-dispatch +immutable-vector-type+ 'pprint-vector priority table)
  t)

(defun disable-vector-printing (&optional (table *print-pprint-dispatch*))
  "Disable pretty printing enabled by ENABLE-VECTOR-PRINTING
in the specified (or current) pprint-dispatch table.

Note that this is NOT guaranteed to restore the *PPRINT-PRINT-DISPATCH*
table to the way it looked before ENABLE-VECTOR-PRINTING is called,
it just clears all dispatch entries of all priorities for those types
diddled by ENABLE-VECTOR-PRINTING.

Returns NIL."
  ;; (set-pprint-dispatch 'cl:vector NIL 0 table) ; nopw
  (set-pprint-dispatch +immutable-vector-type+ NIL 0 table)
  NIL)

#|
(defvar *v* (cl:vector 1 2 3 4 5 6 7 8 9 10))
(defvar *v2* (vec *v*))
(enable-vector-printing)
(let ((*print-right-margin* 10)) (format t "~s~%~s~%" *v* *v2*))
|#


;;;
;;; Set printing. Unclear whether to use dispatch table like we do for vectors/maps
;;;

;; Nope, fset uses the :print-function option to defstruct, this print-object isn't called.
#+NIL
(defmethod print-object ((obj fset:set) stream)
  (pprint-logical-block (stream nil :prefix "#{" :suffix "}")
    (let ((first t))
      (fset:do-set (val obj)
        (if first
            (setf first nil)
            (format stream " ~:_"))       ;== (pprint-newline :fill)
        (pprint-pop)                     ;for *print-length* logic
        (prin1 val stream)))))

(defun pprint-set (stream set)
  (pprint-logical-block (stream nil :prefix "#{" :suffix "}")
    (let ((first t))
      (fset:do-set (val set)
        (if first
            (setf first nil)
            (format stream " ~:_"))       ;== (pprint-newline :fill)
        (pprint-pop)                     ;for *print-length* logic
        (prin1 val stream)))))

(defun enable-set-printing (&optional (priority 0) (table *print-pprint-dispatch*))
  "Enable pretty printing of immutable sets supported by CLJ-COLL
in the specified (or current) pprint-dispatch table so that it uses the Clojure-style syntax.

This function will install a pprint-dispatch function with the specified 
priority in the specified pprint dispatch table. Priority must be of type REAL.

Returns T.

Note that if you don't use ENABLE-VECTOR-PRINTING, you'll
get the FSet defstruct `:print-function` value for FSET:SET which is similar a bit off:
- it's *print-length* syntax is missing a preceding space
- it doesn't observe *print-right-margin*
- it has extraneous leading and trailing space characters

See also: disable-set-printing."
  (set-pprint-dispatch 'fset:set 'pprint-set priority table)
  t)

(defun disable-set-printing (&optional (table *print-pprint-dispatch*))
  "Disable pretty printing enabled by ENABLE-SET-PRINTING
in the specified (or current) pprint-dispatch table.

Note that this is NOT guaranteed to restore the *PPRINT-PRINT-DISPATCH*
table to the way it looked before ENABLE-SET-PRINTING is called,
it just clears all dispatch entries of all priorities for those types
diddled by ENABLE-SET-PRINTING.

Returns NIL."
  ;; Curiously, this let's the original FSET :print-function do its thing. Neat.
  (set-pprint-dispatch 'fset:set NIL 0 table)
  NIL)

#|
(defvar *s* (set '(1 2 3 4 5 6 7 8 9 10)))
(let ((*print-right-margin* 10)) (format t "~s~%" *s*))
(let ((*print-length* 3)) (format t "~s~%" *s*))
|#



;;;
;;; PersistentList
;;;

(defun pprint-seq (obj stream prefix suffix)
  "Print (seq obj) as a PersistentList, since this is how most seqs in Clojure
print.  Note that we  must print these as (list x y z) and not (x y z)
because we want immutable seqs to be printed so that they can be recreated as immutable seqs
if the representation is read.

Returns nil"
  (pprint-logical-block (stream nil :prefix prefix :suffix suffix)
    (loop with first = t
          for s = (seq obj) then (next s)
          while s
          do (if first
                 (setf first nil)
                 (format stream " ~:_"))     ;== (pprint-newline :fill)
             (pprint-pop)               ;for *print-length* logic
             (prin1 (first s) stream))))
  
(alexandria:define-constant +persistent-list-prefix+ "(list "
  :test 'equal :documentation "Prefix used when printing persistent lists,
to disambiguate their printed representation from mutablc CL lists.")
(alexandria:define-constant +persistent-list-suffix+ ")"
  :test 'equal :documentation "Suffix used when printing persistent lists,
to disambiguate their printed representation from mutablc CL lists.")

;; Note that queueseqs don't print like queues, they print like any seq (as a list)
(defmethod print-object ((obj persistentqueue) stream) 
  (pprint-seq obj stream "<-(" ")-<"))

(defmethod print-object ((obj seq) stream) ;Cons, PersistentList, ArraySeq, ArrayRSeq, MapSeq
  (pprint-seq obj stream +persistent-list-prefix+ +persistent-list-suffix+))
(defmethod print-object ((obj ipersistentlist) stream) ;Cons, PersistentList, ArraySeq, MapSeq
  (pprint-seq obj stream +persistent-list-prefix+ +persistent-list-suffix+))

;; If you're debugging things about lazyseqs, printing out the elements of the 
;; seq for their default print representation can wreck your debugging.
;; On the other hand, most of the time you want lazyseqs to print their elements
;; like other seqs.
(defmethod print-object ((obj lazyseq) stream)
  (pprint-seq obj stream +persistent-list-prefix+ +persistent-list-suffix+))
#+NIL
(defmethod print-object ((obj lazyseq) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (if (realized? obj)
        (format stream "Realized, seq = ~s" (lazyseq-seq obj))
        (princ "Unrealized" stream))))

#|
(defparameter *l* (list 1 2 3 4 5 6 7 8 9 10))
(let ((*print-right-margin* 12)) (format t "~s~%" *l*))
(let ((*print-length* 3)) (format t "~s~%" *l*))
|#
