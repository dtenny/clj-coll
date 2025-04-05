(in-package :clj-coll)

;;;
;;; Ring Queue
;;;

;; Exports - ring-queue, rq-empty-p rq-full-p rq-count rq-put rq-get

(defstruct (ring-queue 
            (:constructor %make-rq)
            (:conc-name :rq-)
            (:copier nil))
  "A FIFO ring queue, not thread safe"
  (vec (make-array 0) :type simple-vector :read-only t) 
  (capacity 0 :type fixnum :read-only t) ;shortcut for (cl:length vec), 1+ logical size
  ;; If queue is empty, r/w the same.
  ;; If there are values, r/w are never the same.
  (read-idx 0 :type fixnum)             ;current slot to be read
  (write-idx 0 :type fixnum))           ;next slot to be written

(defun ring-queue (n)
  "Create an empty ring queue that can hold at most N elements."
  ;; Need capacity 1+ number of desired elements
  (%make-rq :vec (make-array (1+ n)) :capacity (1+ n)))
                     
(defun rq-empty-p (rq)
  "T if ringqueue is empty, nil otherwise."
  (declare (ring-queue rq)
           (optimize (speed 3) (safety 1) (debug 0)))
  (= (rq-write-idx rq) (rq-read-idx rq)))

(defun rq-count (rq)
  "Return count of elements in ringqueue"
  (declare (ring-queue rq)
           (optimize (speed 3) (safety 1) (debug 0)))
  (let ((w (rq-write-idx rq))
        (r (rq-read-idx rq)))
    (if (= w r)
        0
        (if (> w r)
            (the fixnum (- w r))
            (the fixnum (- (rq-capacity rq) (the fixnum (- r w))))))))

(defun rq-full-p (rq)
  "T if ringqueue is full, NIL otherwise."
  (declare (ring-queue rq)
           (optimize (speed 3) (safety 1) (debug 0)))
  (= (rq-count rq) (1- (rq-capacity rq))))

(declaim (inline rq-inc-idx rq-dec-idx))

(declaim (ftype (function (fixnum fixnum) (values fixnum &optional)) rq-inc-idx))
(defun rq-inc-idx (idx cap)
  "Increment read or write index, wrapping if necessary"
  (declare (fixnum idx cap)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((n (+ idx 1)))
    (declare (fixnum n))
    (if (= n cap)
        0
        n)))

(declaim (ftype (function (fixnum fixnum) (values fixnum &optional)) rq-dec-idx))
(defun rq-dec-idx (idx cap)
  "Decrement read or write index, wrapping if necessary"
  (declare (fixnum idx cap)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((n (- idx 1)))
    (declare (fixnum n))
    (if (< n 0)
        (1- cap)
        n)))

(defun rq-put (rq elt)
  "Insert ELT into ring-queue RQ.
If the queue is full the oldest value is removed.
Returns ELT."
  (declare (ring-queue rq)
           (optimize (speed 3) (safety 1) (debug 0)))
  (let ((w (rq-write-idx rq))
        (r (rq-read-idx rq))
        (c (rq-capacity rq)))
    (setf (svref (rq-vec rq) w) elt)
    (setf w (rq-inc-idx w c) (rq-write-idx rq) w)
    (when (= w r)
      ;; queue full, lose an element
      (setf (rq-read-idx rq) (rq-inc-idx r c)))
    elt))

(defun rq-get (rq &optional default)
  "Get the FIFO/oldest value in the ringqueue,
return DEFAULT if there are no more values."
  (declare (ring-queue rq)
           (optimize (speed 3) (safety 1) (debug 0)))
  (let ((w (rq-write-idx rq))
        (r (rq-read-idx rq)))
    (if (= w r)
        default                         ;empty
        (progn
          (setf (rq-read-idx rq)
                (if (> w r)
                    (1+ r)
                    (rq-inc-idx r (rq-capacity rq))))
          (svref (rq-vec rq) r)))))

(defmethod print-object ((rq ring-queue) stream)
  (let* ((w (rq-write-idx rq))
         (r (rq-read-idx rq))
         (c (rq-capacity rq))
         (v (rq-vec rq))
         (prefix (format nil "#<~a-~d/~d:" (type-of rq) (rq-count rq) (1- c))))
    (pprint-logical-block (stream nil :prefix prefix :suffix ">")
      (flet ((print-element (i)
               (format stream " ~:_")   ;== (pprint-newline :fill)
               (pprint-pop)             ;for *print-length* logic
               (prin1 (svref v i) stream)))
        (cond ((= w r))
              ((> w r)
               (loop for i from r below w do (print-element i)))
              (t                        ;(< w r)
               (loop for i from r below c do (print-element i))
               (loop for i from 0 below w do (print-element i))))))))
             

(defun rq-test ()
  (macrolet ((is (form)
               (let ((v (gensym)))
                 `(let ((,v ,form))
                    (unless ,v
                      (error "Evaluation of ~s yielded ~s" ',form ,v))))))
    (let ((rq (ring-queue 4)))
      (loop for i from 1 to 4 
            do (rq-put rq i)
               (is (= i (rq-count rq))))
      (loop for i from 1 to 4
            do (is (= i (rq-get rq i)))
               (is (= (- 4 i) (rq-count rq))))
      (is (null (rq-get rq)))
      ;; Will lose elt 1
      (loop for i from 1 to 5 
            do (rq-put rq i)
               (is (= (min 4 i) (rq-count rq))))
      (loop for i from 2 to 5
            for c from 3 downto 0
            do (is (= i (rq-get rq i)))
               (is (= c (rq-count rq))))
      (is (null (rq-get rq)))
      (is (rq-empty-p rq))

      ;; Mix it up
      (rq-put rq 1) (rq-put rq 2)
      (is (= 1 (rq-get rq))) 
      (is (= 1 (rq-count rq)))
      (rq-put rq 3) (rq-put rq 4)
      (is (= 2 (rq-get rq)))
      (is (= 2 (rq-count rq)))
      (rq-put rq 5) (rq-put rq 6)
      (is (= 3 (rq-get rq)))
      (is (= 3 (rq-count rq)))
      (rq-put rq 7) (rq-put rq 8) ;4 overwritten
      (is (= 5 (rq-get rq)))
      (is (= 3 (rq-count rq)))
      (rq-put rq 9) (rq-put rq 10) ;6 overwritten
(print rq)
      (is (= 4 (rq-count rq)))
      (is (= 7 (rq-get rq)))
      (is (= 3 (rq-count rq)))
      (rq-put rq 11) (rq-put rq 12) (rq-put rq 13) ;8,9 overwritten
      (is (rq-full-p rq))
      (is (= 4 (rq-count rq)))
      (is (= 10 (rq-get rq)))
      (is (= 3 (rq-count rq)))
      (is (= 11 (rq-get rq)))
      (is (= 2 (rq-count rq)))
      (is (= 12 (rq-get rq)))
      (is (= 1 (rq-count rq)))
      (is (= 13 (rq-get rq)))
      (is (= 0 (rq-count rq)))
      (is (rq-empty-p rq))
      "ringqueue tests pass"
      )))


