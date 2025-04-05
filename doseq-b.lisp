(in-package :clj-coll)

;;;; doseq-b.lisp has the doseq macro that relies on stuff which has been
;;;; compiled and loaded in doseq-a.lisp.

(defmacro doseq ((&rest exprs) &body body)
  "A subset of Clojure doseq that supports :WHEN and :WHILE tests but not :LET,
with only a single iteration.

E.g. (doseq (x [1 2 3] :when (oddp x)) (print x)) => 1 3 5

You can use multiple :when and :while clauses.
Mostly compatible with Clojure's doseq, except for the use of parens
instead of brackets.

Returns nil."
  (unless (evenp (cl:length exprs))
    (error "An even number of var/seq or test-key/form elements are expected: ~s" exprs))
  ;; Partition exprs by iteration context (i.e. var/coll pair)
  ;; Then sort the keyword driven behaviors within an iteration partition such that
  ;; :WHILE tests (loop termination) precede :WHEN tests (conditional execution)
  (let ((groups (group-doseq-exprs exprs)))
    (unless (= 1 (cl:length groups))
      (error "Only one var/sequence binding is supported right now: ~s" exprs))
    (let* ((var (gensym "doseq-"))      ;both var bound to iterator and end-of-seq flag
           (group (cl:first groups))
           (user-var (-> group cl:first cl:first))
           (seq (-> group cl:first cl:second))
           (tests (apply #'cl:concatenate 'cl:list (cdr group)))
           (body (if body `(:do ,@body))))
      `(loop :with ,var function = (iterator ,seq ',var)
             :as ,user-var = (funcall ,var)
             :until (eq ',var ,user-var)
             ,@tests
             ,@body))))
