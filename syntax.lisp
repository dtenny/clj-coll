(in-package :clj-coll)

;;;; Read table support for vectors `[]` and maps `{}`.
;;;; All behavior is via named readtables except for specific functions
;;;; the user _may_ call to update *readtable* at the time of invocation.
;;;;
;;;; Note that CLJ-COLL vector syntax differs from lisp's `#()` vector syntax
;;;; in that the reader returns a macro expansion which will 
;;;; evaluate contents of the `[]` delimited tokens.

;;; If you're looking for character syntax information, it's here:
;;; https://www.lispworks.com/documentation/HyperSpec/Body/02_ad.htm
;;; CLHS Section 2.1.4
;;;
;;; The `[]{}` characters are normally "constituent characters", 
;;; which is to say you could use them in symbol names and such, however
;;; they are never used in Common Lisp implementations as constituent characters,
;;; their use is left up to programmers.

;;;
;;; Vector syntax - e.g. [1 2 3]
;;;

(defconstant +open-bracket+ #\[
  "Character that initiates reading of a CLJ-COLL vector
according to the value of *DEFAULT-VECTOR-CONSTRUCTOR*.")

(defconstant +close-bracket+ #\])

(defun clj-vector-reader (stream char)
  "A 'reader macro function' for reading Clojure-style vector syntax, i.e. [1 2 3]."
  (declare (optimize (speed 3) (safety 1)) (ignore char));*FINISH* safety 0
  (let ((list (read-delimited-list +close-bracket+ stream)))
    ;; See `clj-coll-reader` for possible semantic notes of interest.
    ;; (apply *default-vector-constructor* list)     ;for read-time semantics
    `(funcall *default-vector-constructor* ,@list))) ;for load-time semantics

(defun missing-open-bracket (stream char)
  "A reader macro function invoked if the close bracket (#\]) is encountered in the input
stream without having first triggered CLJ-VECTOR-READER on the open bracket (#\[)."
  (declare (ignore stream char))
  (error "No matching ~c for ~c" +open-bracket+ +close-bracket+))

(named-readtables:defreadtable readtable-vector-mixin
  (:macro-char +open-bracket+ 'clj-vector-reader)
  (:macro-char +close-bracket+ 'missing-open-bracket))

(defun enable-vector-syntax (&optional (read-table *readtable*))
  "Enables Clojure vector syntax in READ-TABLE, which defaults to CL:*READTABLE*.

Returns T.

Be careful not to alter CLJ-COLL or other named-readtables you might wish to use later
for their documented purposes."
  (set-macro-character +open-bracket+ 'clj-vector-reader nil read-table)
  (set-macro-character +close-bracket+ 'missing-open-bracket nil read-table))

(defun disable-vector-syntax (&optional (read-table *readtable*))
  "Disables Clojure vector syntax in READ-TABLE, which defaults to CL:*READTABLE*
by setting the macro character functions for open and close brackets to NIL.

Returns NIL.

Be careful not to alter CLJ-COLL or other named-readtables you might wish to use later
for their documented purposes."
  (set-macro-character +open-bracket+ nil nil read-table)
  (set-macro-character +close-bracket+ nil nil read-table))

;;;
;;; Map syntax - e.g. {:a 1 :b 2}
;;;

(defconstant +open-brace+ #\{ 
  "Character that initiates reading of an associative data structure to be created 
according to the value of *DEFAULT-HASHMAP-CONSTRUCTOR*.")

(defconstant +close-brace+ #\})

(defun clj-map-reader (stream char)
  "A 'reader macro function' for reading clojure-style map syntax, i.e. {:a 1 :b 2}"
  (declare (optimize (speed 3) (safety 1)) (ignore char));*FINISH* safety 0
  (let ((list (read-delimited-list +close-brace+ stream)))
    (when (oddp (length list))
      (error "Odd number of elements in hash table/map literal."))
    ;; If we wanted `(1 2 3 {:e 4})` to create a map at read time, this APPLY 
    ;; is what we'd need. See README.md.
    ;; (apply *default-hashmap-constructor* list)     ;for read-time semantics
    `(funcall *default-hashmap-constructor* ,@list))) ;for load-time semantics

(defun missing-open-brace (stream char)
  "A reader macro function invoked if the close brace (#\}) is encountered in the input
stream without having first triggered CLJ-MAP-READER on the open brace (`{`)
or CLJ-SET-READER for the open brace dispatch macro subchar (`#{`)."
  (declare (ignore stream char))
  (error "No matching ~c for ~c" +open-brace+ +close-brace+))

(named-readtables:defreadtable readtable-map-mixin
  "A named readtable that enables Clojure-style map syntax, e.g. {:a 1 :b 2}."
  (:macro-char +open-brace+ 'clj-map-reader)
  (:macro-char +close-brace+ 'missing-open-brace))

(defun enable-map-syntax (&optional (read-table *readtable*))
  "Enables Clojure map syntax (e.g. `{:a 1}`) in READ-TABLE, which defaults to CL:*READTABLE*.

Returns T.

Be careful not to alter CLJ-COLL or other named-readtables you might wish to use later
for their documented purposes."
  (set-macro-character +open-brace+ 'clj-map-reader nil read-table)
  (set-macro-character +close-brace+ 'missing-open-brace nil read-table))

(defun disable-map-syntax (&optional (read-table *readtable*))
  "Disables Clojure map syntax in READ-TABLE, which defaults to CL:*READTABLE*
by setting the macro character functions for open and close braces to NIL.

Returns NIL.

Be careful not to alter CLJ-COLL or other named-readtables you might wish to use later
for their documented purposes."
  (set-macro-character +open-brace+ nil nil read-table)
  (set-macro-character +close-brace+ nil nil read-table))


(defun enable-printing ()
  "Enable CLJ-COLL pretty printing with Clojure syntax for all
set/vector/map types supported by CLJ-COLL."
  (enable-vector-printing)
  (enable-set-printing)
  (enable-map-printing))

(defun disable-printing ()
  "Disable CLJ-COLL pretty printing with Clojure syntasx for all
set/vector/map types supported by CLJ-COLL."
  (disable-vector-printing)
  (disable-set-printing)
  (disable-map-printing))

;;;
;;; Set syntax - e.g. #{1 2 3}
;;;

;;https://www.common-lisp.com/support/documentation/ansicl/subsubse/macrocha.htm
;; infix-parameter would be a number between # and {
(defun clj-set-reader (stream char infix-parameter)
  "A reader macro function for reading Clojure-style set syntax, i.e. #{1 2 3}."
  (declare (optimize (speed 3) (safety 1)) (ignore char infix-parameter));*FINISH* safety 0
  (let ((list (read-delimited-list +close-brace+ stream)))
    `(hash-set ,@list)))
  
(named-readtables:defreadtable readtable-set-mixin
  "A named readtable that enables CLojure-style SET syntax, e.g. #{1 2 3}."
  (:macro-char #\# :dispatch)
  (:dispatch-macro-char #\# +open-brace+ 'clj-set-reader)
  (:macro-char +close-brace+ 'missing-open-brace))

(defun enable-set-syntax (&optional (read-table *readtable*))
  "Enables Clojure set syntax (e.g. `#{1 2 3}`) in READ-TABLE, which defaults to CL:*READTABLE*.

Returns T.

Be careful not to alter CLJ-COLL or other named-readtables you might wish to use later
for their documented purposes."
  ;; Check to see if a dispatching macro function for # is already in the table.
  ;; Create one if necessary
  (unless (get-macro-character #\# read-table)
    ;; We make it non-terminating because that's what would be in the standard readtable,
    ;; though really we don't want Clojure syntax to be permitted in CL identifiers, 
    ;; so maybe this isn't the best decision, but it is status quo.
    (make-dispatch-macro-character #\# t read-table))
  (set-dispatch-macro-character #\# +open-brace+ 'clj-set-reader read-table)
  (set-macro-character +close-brace+ 'missing-open-brace nil read-table))

(defun disable-set-syntax (&optional (read-table *readtable*))
  "Disables Clojure set syntax (e.g. `#{1 2 3}`) in READ-TABLE, which defaults to CL:*READTABLE*
by setting the macro character (or dispatch subchar) functions for open and close braces to NIL.

Returns NIL.

Be careful not to alter CLJ-COLL or other named-readtables you might wish to use later
for their documented purposes."
  (set-dispatch-macro-character #\# +open-brace+ nil read-table)
  (set-macro-character +close-brace+ nil nil read-table))

;;;
;;; One ring to bind them all...
;;

(named-readtables:defreadtable readtable
  "A named readtable that enables Clojure-style syntax for vectors, maps, and sets."
  (:merge :standard readtable-map-mixin readtable-vector-mixin readtable-set-mixin))

