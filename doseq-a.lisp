(in-package :clj-coll)

;;;;
;;;; Implementation of DO-SEQ that needs a separate compilation unit
;;;; for the DOSEQ macroexpansion to work.  Relies on things in seq-apis.lisp.
;;;;

(defun sort-doseq-exprs (exprs)
  "Given one doseq expression group (a list of pairs)
starting with the var/seq pair and optionally followed
by one or more :when and :while pairs, sort the input list
such that :while (termination) pairs always precede 
:when (test-condition) pairs.

This is a destructive sort on exprs."
  (cl:sort exprs                        ;destructive
           (lambda (pair1 pair2)
             (let ((sym1 (car pair1))
                   (sym2 (car pair2)))
               ;; (< T :WHILE :WHEN)
               (case sym1
                 (:while (case sym2
                           (:while nil)
                           (:when t)
                           (t nil)))
                 (:when nil)
                 (t (case sym2
                      (:while t)
                      (:when t)
                      (t nil))))))))

(defun group-doseq-exprs (exprs)
  "Given a series of exprs for doseq like [x '(1 2 3) :when (odd? x) ...]
such that each variable/sequence pair is the start of a new group, e.g.:

    ( var-pair1 [test-pairs-on-varpair1 ...] ) [ (varpair2 [test-pairs-on-varpar2 ...] ) ...]

In other words, each variable/sequence pair is the start of a new group.

Sort the resulting groups such that loop termination pairs precent conditional execution pairs
but follow var/seq pairs.

Signals an error if there are invalid keywords/variables in exprs.

Returns a list of pair-lists, e.g.:
(((x (range 9)) (:when (odd? x)) ...)
 ((y ...)))"
  (let ((pairs (mpartition 2 exprs))
        (pair-collector (make-collector 'cl:list)))
    (serapeum:with-collector (pair-groups) ; list of var/seq term/test pairs for one var/seq
      (loop for pair in pairs
            as sym = (car pair)
            as var-p = (non-keyword-symbol-p sym)
            do (unless (symbolp sym)
                 (error "Symbol expected where ~s received in ~s" sym exprs))
               (unless (or var-p
                           (eq sym :when)
                           (eq sym :while))
                 (error "~s is not a supported DOSEQ keyword in ~s" sym exprs))
               (when var-p
                 (let ((group (grab pair-collector)))
                   (when group
                     (pair-groups (sort-doseq-exprs group)) ;destructive
                     (reset pair-collector))))
               (collect pair-collector pair)
            finally
               (when-let (group (grab pair-collector))
                 (pair-groups (sort-doseq-exprs group)))))))

