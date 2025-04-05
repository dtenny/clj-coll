(in-package :clj-coll)

;;;;
;;;; Seq APIs that wanted to use DOSEQ and so are loaded after DOSEQ's implementation.
;;;;

(defn-ish mtake-last
  "([n coll] [result-type n coll])

A more efficient seq-less version of TAKE-LAST.

Returns a CL:LIST(default) or CL:VECTOR of the last N items in COLL.

The optional RESULT-TYPE argument is a symbol used to specify the desired return type
and is one of CL:LIST or CL:VECTOR.

Returns NIL if N <= 0 or COLL is empty when the result-type is CL:LIST.
Returns #() in similar cases for result-type CL:VECTOR (note that this is different
from TAKE-LAST which always returns NIL for empty results).

Result may share structure with COLL.

See also: TAKE-LAST."
  ;; *PERFORMANCE*  
  ;; We're potentially doing O(n) or O(2n) worst-case traversals here for COUNT
  ;; and seq re-traversal for the copy, which is unfortunate.
  ;;
  ;; We could use iterators for much less consing if we could do 'nthnext' and 'count'
  ;; Perhaps it's worth creating another iterator where state is managed sarpately and
  ;; not closed over in the iterator function.
  ((n coll) (mtake-last 'cl:list n coll))
  ((result-type n coll)
   (ecase result-type
     (cl:list
      (if (or (<= n 0) (empty? coll))
          nil
          (if (cl:listp coll)
              (cl:last coll n)          ;structure sharing
              (let* ((size (count coll))
                     (start-index (if (> n size) 0 (- size n))))
                (loop for s = (nthnext coll start-index) then (next s)
                      while s
                      collect (first s))))))
     (cl:vector
      (if (or (<= n 0) (empty? coll))
          #()
          (let* ((size (count coll))
                 (start-index (if (> n size) 0 (- size n)))
                 (n-avail (- size start-index)))
            (if (zerop n-avail)
                #()
                (if (cl:vectorp coll)
                    (make-array n-avail 
                                :displaced-to coll :displaced-index-offset start-index)
                    (loop with result = (make-array n-avail)
                          for i from 0
                          for s = (nthnext coll start-index) then (next s)
                          while s
                          do (setf (svref result i) (first s))
                          finally (return result))))))))))

