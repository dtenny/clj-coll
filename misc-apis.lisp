(in-package :clj-coll)

;;;; Clojure APIs that have little to do with [lazy]seqs or collections.

(defmacro doc (name)
  "Loosely emulate clojure.repl/doc, providing documentation related to NAME,
an unquoted and possibly package-qualified symbol.

Note: you'll probably trip up the reader if you specify package-qualified symbols 
that don't exist (e.g. 'cl:foo).

Non-existent unqualified symbols are fine (at least in SBCL).

Returns NIL"
  `(progn 
     (describe ',name)
     nil))

(defun dirlist (package-name)
  "Not a clojure capability.  Returns a sorted list of external symbols in `dir`
(really a package) as a list. `package-name` should be a `package-designator`, which
includes `string-designator`.  Note that as CL symbols each symbols has
a package-name associated with it."
  (let (list)
    (do-external-symbols (symbol package-name)
      (cl:push symbol list))
    (cl:sort list #'string< :key #'symbol-name)))

(defun dir (package-name &optional (with-package-name t))
  "Emulate `clojure.repl/dir`.

In Clojure it would print a sorted listing of public vars in a namespace.
In Common Lisp it prints a sorted listing of exported symbols in a package.

`PACKAGE-NAME` should be a 'package-designator', which includes 'string-designator'.

E.g. (dir :clj-coll) => ... not the best example, so many exports :-) ...

If `WITH-PACKAGE-NAME` is true, print the symbols prefixed with the package name
so that they're more useful sitting there in your SLIME interaction and you can 
Meta-. on them.

See also the unexported function `DIRLIST` in this package, 
which is not part of Clojure but seemed worth mentioning.

Returns nil."
  (flet ((printer (sym)
           (if with-package-name
               (cl:format t "~a:~a~%" package-name (cl:string sym))
               (cl:format t "~a~%" (cl:string sym)))))
    (cl:mapc #'printer (dirlist package-name))
    nil))

(defun distinct? (val &rest vals)
  "([x] [x y] [x y & more])
Returns true if no two of the arguments are EQUAL?"
  (let ((s (mdistinct (cl:list* val vals))))
    (= (length s) (+ 1 (length vals)))))

(defun any? (x)
  "([x])
Returns T given any argument, not at all related to `not-any?`.
A bit of clojure.spec baggage."
  (declare (ignore x))
  T)

(declaim (ftype (function (&rest t) (values (function (&rest t)) &optional)) fnil))

(defn-ish fnil
  ;; I'd put this in utils.lisp, using defn-ish can't be done in that module.
  "([f x] [f x y] [f x y z])
Takes a function f and returns a function that calls f
after replacing up to the first 3 arguments if they are nil.

F can take any number of arguments, not just the ones that nil-defaulted."
  ;; Note that SERAPEUM:FNIL does not behave like Clojure (when I tested it).
  ;; CL:  (funcall (serapeum:fnil #'cl:list 1 2) nil nil) => (1 nil)
  ;; CLJ: ((fnil list 1 2) nil nil) => (1 2)
  ;; This FNIL behaves identically to Clojure. (Once again resisting temptation
  ;; to tinker with improvements, like defaulting on more than 3 args :-) )
  ((f x)
   (lambda (x1 &rest args)
     (apply f (or x1 x) args)))
  ((f x y)
   (lambda (x1 y1 &rest args)
     (apply f (or x1 x) (or y1 y) args)))
  ((f x y z)
   (lambda (x1 y1 z1 &rest args)
     (apply f (or x1 x) (or y1 y) (or z1 z) args))))
             
(defgeneric slurp (x &rest opts)
  (:documentation "Return the contents of x as a string. 
X may be a pathname designator or an open input stream. 
If X is a stream it will be closed on exit. (Clojure semantics)
OPTS are key/value pairs as per OPEN, :DIRECTION :INPUT is implicit.")
  (:method ((x stream) &rest opts)
    (if (input-stream-p x)
        (with-open-stream (s x)
          (with-output-to-string (output)
            (uiop/stream:copy-stream-to-stream 
             s output 
             :element-type (cl:getf opts :element-type 'character))))
        (error "~A is not an open input stream." x)))
  (:method ((x t) &rest opts)
    ;; X is hopefully a pathname-designator
    ;; If the file doesn't exist and :if-does-not-exist nil is specified
    ;; return nil (as per OPEN).
    (let ((s (apply #'cl:open x (cl:list* :direction :input opts))))
      (and s (apply #'slurp s opts)))))
