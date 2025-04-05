(in-package :clj-coll)

;;;; Structures for lazy seqs, persistent collections, etc, that we want to have
;;;; available at compile time in a subsequent compiled/loaded module.
;;;; It worked well on SBCL, but I had to give up on struct-valued constants with CCL.
;;;; +EMPTY-LIST+ is now *EMPTY-LIST*.

(eval-when (:compile-toplevel :load-toplevel :execute) ;so we can use make-emptylist in constant
  (defstruct (Seq
              (:copier nil))
    "The SEQ type is used to identify subtypes as seqs, i.e. logically implementing ISeq
via FIRST, NEXT, CONS. (CLJ-COLL doesn't use the Clojure-internal MORE function).

More specifically, CLJ-COLL restricts types bearing Seq inheritance to those
which are 'seqs' that currently have a value to yield by way of a call to first().
Of a thing is some collection for which `(seq x)` would return null, it should not inherit
Seq.")

  (defstruct (IPersistentList
              (:copier nil)))

  (defstruct (EmptyList
              (:include IPersistentList)
              (:copier nil))
    "Singleton instance of an empty immutable list.")

  (defmethod make-load-form ((e EmptyList) &optional environment)
    (declare (ignore environment e))
    `(make-emptylist))

  (defstruct (PersistentQueue
              (:include IPersistentList)
              (:copier nil))
    "Emulates a Clojure immutable fifo queue modeled after Okasaki Batched Queues
but using a seq (for front) and vector (for rear) instead of two lists."
    (front-seq nil :read-only t)    ;front of queue, seq on persistent vector, or nil
    (rear-vec  nil :read-only t)    ;rear of queue, persistent vector (not a seq)
    (count 0 :type fixnum :read-only t))

  (defmethod make-load-form ((q PersistentQueue) &optional environment)
    (declare (ignore environment))
    `(make-persistentqueue 
      :front-seq ,(persistentqueue-front-seq q)
      :rear-vec ,(persistentqueue-rear-vec q)
      :count ,(persistentqueue-count q)))
  )                                       ;eval-when

(defstruct (PersistentList   ;isa Seq, but struct inheritance single, resolved via GF
            (:include IPersistentList)
            (:copier nil))
  "Emulates a Clojure immutable list. Note that NIL and the empty immutable list are
not the same, as they would be when comparing NIL and an empty CL:LIST."
  (first nil :read-only t)     ;Object or nil
  ;; This is logically readonly, however if we construct lists a certain way
  ;; such as `list` does, we need to set it after creation (but before the user sees it).
  (rest  nil)     ;IPersistentList or nil
  (count 0 :type fixnum :read-only t))

