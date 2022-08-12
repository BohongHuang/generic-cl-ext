(defpackage generic-cl-ext.collection
  (:use #:generic-cl)
  (:shadow #:foreach #:random)
  (:export #:map #:foreach
           #:zip #:zip-with-index
           #:filter #:filter-not
           #:flatten #:flatmap
           #:fold-left #:fold-right #:scan-left
           #:take #:take-while
           #:drop #:drop-while
           #:index-of #:index-where
           #:last-index-of #:last-index-where
           #:find #:find-if #:find-if-not
           #:find-last #:find-last-if #:find-last-if-not
           #:forall #:exists
           #:sliding #:random))

(in-package #:generic-cl-ext.collection)

(defmethod random ((integer integer) &key)
  (cl:random integer))

(defmethod random ((float float) &key)
  (cl:random float))

(defmethod random ((sequence sequence) &key)
  (elt sequence (random (length sequence))))

(defmethod foreach (function sequence &rest sequences)
  (apply #'map function sequence sequences)
  nil)

(defmethod foreach (function (sequence sequence) &rest sequences)
  (apply #'generic-cl:foreach function (cons sequence sequences)))

(defmethod foreach (function (iterator iterator) &rest iterators)
  (apply #'generic-cl:foreach function (cons iterator iterators)))

(defmethod (setf first) (value sequence)
  #+ecl
  (declare (optimize (speed 2)))
  (setf (elt sequence 0) value))

(defmethod (setf last) (value sequence &optional (n 0))
  #+ecl
  (declare (optimize (speed 2)))
  (setf (elt sequence (cl:- (length sequence) 1 n)) value))

(defstruct (map (:include iterator))
  iterator
  function)

(defmethod at ((iterator map))
  (funcall (map-function iterator) (at (map-iterator iterator))))

(defmethod endp ((iterator map))
  (endp (map-iterator iterator)))

(defmethod advance ((iterator map))
  (advance (map-iterator iterator)))

(defstruct (filter (:include iterator))
  iterator
  function)

(defmethod endp ((iterator filter))
  (let ((iterator (filter-iterator iterator))
        (function (filter-function iterator)))
    (loop :until (or (endp iterator)
                     (funcall function (at iterator)))
          :do (advance iterator))
    (endp iterator)))

(defmethod at ((iterator filter))
  (endp iterator)
  (at (filter-iterator iterator)))

(defmethod advance ((iterator filter))
  (advance (filter-iterator iterator))
  (endp iterator))

(defstruct (zip (:include iterator))
  iterators)

(defmethod endp ((iterator zip))
  (some #'endp (zip-iterators iterator)))

(defmethod advance ((iterator zip))
  (map #'advance (zip-iterators iterator)))

(defmethod at ((iterator zip))
  (map #'at (zip-iterators iterator)))

(defstruct (iterator-of-iterable (:include iterator))
  iterator
  iterable-iterator)

(defun itertaor-of-iterator-init-iterable-iterator (iterator)
  (unless (or (iterator-of-iterable-iterable-iterator iterator)
              (endp (iterator-of-iterable-iterator iterator)))
    (setf (iterator-of-iterable-iterable-iterator iterator)
          (iterator (at (iterator-of-iterable-iterator iterator))))))

(defmethod endp ((iterator iterator-of-iterable))
  (itertaor-of-iterator-init-iterable-iterator iterator)
  (let ((iter1 (iterator-of-iterable-iterator iterator))
        (iter2 (iterator-of-iterable-iterable-iterator iterator)))
    (if (endp iter1)
        t
        (if (endp iter2)
            (progn (advance iter1)
                   (setf (iterator-of-iterable-iterable-iterator iterator) nil)
                   (endp iterator))
            nil))))

(defmethod advance ((iterator iterator-of-iterable))
  (itertaor-of-iterator-init-iterable-iterator iterator)
  (let ((iter1 (iterator-of-iterable-iterator iterator))
        (iter2 (iterator-of-iterable-iterable-iterator iterator)))
    (if (endp iter2)
        (advance iter1)
        (progn (advance iter2)
               (when (endp iter2)
                 (advance iter1)
                 (setf (iterator-of-iterable-iterable-iterator iterator) nil))))))

(defmethod at ((iterator iterator-of-iterable))
  (itertaor-of-iterator-init-iterable-iterator iterator)
  (at (iterator-of-iterable-iterable-iterator iterator)))

(defstruct (take-n (:include iterator))
  iterator
  n
  (index 0))

(defmethod endp ((iterator take-n))
  (or (= (take-n-n iterator) (take-n-index iterator)) (endp (take-n-iterator iterator))))

(defmethod advance ((iterator take-n))
  (advance (take-n-iterator iterator))
  (incf (take-n-index iterator)))

(defmethod at ((iterator take-n))
  (at (take-n-iterator iterator)))

(defstruct (take-while (:include iterator))
  iterator
  function)

(defmethod at ((iterator take-while))
  (at (take-while-iterator iterator)))

(defmethod advance ((iterator take-while))
  (advance (take-while-iterator iterator)))

(defmethod endp ((iterator take-while))
  (or (not (funcall (take-while-function iterator) (at iterator))) (endp (take-while-iterator iterator))))

(defstruct (drop-while (:include iterator))
  iterator
  function)

(defun drop-while-init-iterator (iterator)
  (let ((function (drop-while-function iterator))
        (iterator (drop-while-iterator iterator)))
    (loop :while (and (not (endp iterator)) (funcall function (at iterator)))
          :do (advance iterator))))

(defmethod at ((iterator drop-while))
  (drop-while-init-iterator iterator)
  (at (drop-while-iterator iterator)))

(defmethod advance ((iterator drop-while))
  (drop-while-init-iterator iterator)
  (advance (drop-while-iterator iterator)))

(defmethod endp ((iterator drop-while))
  (drop-while-init-iterator iterator)
  (endp (drop-while-iterator iterator)))

(defstruct (drop-n (:include iterator))
  iterator
  n
  (index 0))

(defun drop-n-init-iterator (self)
  (let ((iterator (drop-n-iterator self)))
    (loop :while (and (not (endp iterator)) (< (drop-n-index self) (drop-n-n self)))
          :do (progn (advance iterator)
                     (incf (drop-n-index self))))))

(defmethod at ((iterator drop-n))
  (drop-n-init-iterator iterator)
  (at (drop-n-iterator iterator)))

(defmethod advance ((iterator drop-n))
  (drop-n-init-iterator iterator)
  (advance (drop-n-iterator iterator)))

(defmethod endp ((iterator drop-n))
  (drop-n-init-iterator iterator)
  (endp (drop-n-iterator iterator)))

(defstruct (sliding (:include iterator))
  iterator
  size
  step
  window)

(defun sliding-init-iterator (self)
  (let ((iterator (sliding-iterator self)))
    (loop :while (and (< (length (sliding-window self)) (sliding-size self))
                      (not (endp iterator)))
          :do (progn (if (sliding-window self)
                         (nconc (sliding-window self) (list (at iterator)))
                         (push (at iterator) (sliding-window self)))
                     (advance iterator)))))

(defmethod at ((iterator sliding))
  (sliding-init-iterator iterator)
  (copy-list (sliding-window iterator)))

(defmethod advance ((iterator sliding))
  (sliding-init-iterator iterator)
  (dotimes (_ (sliding-step iterator))
    (if (sliding-window iterator)
        (pop (sliding-window iterator))
        (unless (endp (sliding-iterator iterator))
          (advance (sliding-iterator iterator))))))

(defmethod endp ((iterator sliding))
  (sliding-init-iterator iterator)
  (< (length (sliding-window iterator)) (sliding-size iterator)))

(defmethod sliding ((sequence sequence) size &optional (step 1))
  (coerce (sliding (iterator sequence) size step)
          (generic-type-of sequence)))

(defmethod sliding ((iterator iterator) size &optional (step 1))
  (make-sliding :iterator iterator :size size :step step))

(defmethod map (function (iterator iterator) &rest iterators)
  (if iterators
      (make-map :iterator (make-zip :iterators (cons iterator iterators))
                :function (lambda (args) (apply function args)))
      (make-map :iterator iterator :function function)))

(defmethod remove-if-not (function (iterator iterator) &rest iterators)
  (if iterators
      (make-filter :iterator (make-zip :iterators (cons iterator iterators))
                   :function (lambda (args) (apply function args)))
      (make-filter :iterator iterator :function function)))

(defmethod remove-if (function (iterator iterator) &rest iterators)
  (apply #'remove-if-not (complement function) iterator iterators))

(defmethod make-iterator ((iterator iterator) start end)
  (if (and (= start 0) (null end))
      iterator
      (map (lambda (elem-with-index)
             (first elem-with-index))
           (remove-if-not (lambda (elem-with-index)
                            (<= (or start 0) (second elem-with-index) (or end most-positive-fixnum)))
                          (make-zip :iterators (list iterator (iterator (range 0))))))))

(defmethod concatenate ((iterator iterator) &rest iterators)
  (if iterators
      (make-iterator-of-iterable :iterator (iterator (cons iterator iterators)))
      iterator))

(defmethod generic-type-of (obj)
  (type-of obj))

(defmethod generic-type-of ((list list))
  'list)

(defmethod generic-type-of ((vector vector))
  'vector)

(defmethod generic-type-of ((iterator iterator))
  'iterator)

(defmethod zip (iterable &rest iterables)
  (if iterables
      (if (cdr iterables)
          (apply #'map (lambda (&rest args) args) iterable iterables)
          (apply #'map (lambda (a b) (cons a b)) iterable iterables))
      iterable))

(defmethod map-to ((type (eql 'iterator)) function &rest iterables)
  (apply #'map
         function
         (iterator (car iterables))
         (mapcar #'iterator (cdr iterables))))

(defmethod coerce ((sequence sequence) type)
  (map-to type #'identity sequence))

(defmethod coerce ((iterator iterator) type) 
  (map-to type #'identity iterator))

(defmethod zip-with-index ((iterator iterator))
  (zip iterator (iterator (range 0))))

(defmethod zip-with-index ((sequence sequence))
  (zip sequence (range 0)))

(defmethod filter (function iterable)
  (remove-if-not function iterable))

(defmethod filter-not (function iterable)
  (remove-if function iterable))

(defmethod flatten ((sequence sequence))
  (map-extend #'identity sequence))

(defmethod flatten ((iterator iterator))
  (make-iterator-of-iterable :iterator iterator))

(defmethod flatmap (function sequence &rest sequences)
  (flatten (apply #'map function sequence sequences)))

(defmethod flatmap (function (sequence sequence) &rest sequences)
  (apply #'map-extend function sequence sequences))

(defmethod fold-left (function init-value (iterator iterator))
  (let ((acc init-value))
    (doiter (iterator iterator)
      (setf acc (funcall function acc (at iterator))))
    acc))

(defmethod fold-left (function init-value (sequence sequence))
  (fold-left function init-value (iterator sequence)))

(defmethod fold-right (function init-value (iterator iterator))
  (labels ((recur (iter)
             (if (endp iter)
                 init-value
                 (let ((value (at iter)))
                   (advance iter)
                   (funcall function value (recur iter))))))
    (recur iterator)))

(defmethod fold-right (function init-value (sequence sequence))
  (fold-right function init-value (iterator sequence)))

(defmethod scan-left (function init-value (iterator iterator))
  (let ((acc init-value))
    (map (lambda (it)
           (setf acc (funcall function acc it)))
         iterator)))

(defmethod scan-left (function init-value (sequence sequence))
  (coerce (scan-left function init-value (iterator sequence))
          (generic-type-of sequence)))

(defmethod take (n (iterator iterator))
  (make-take-n :n n :iterator iterator))

(defmethod take (n (sequence sequence))
  (subseq sequence 0 (min (max n 0) (length sequence))))

(defmethod take-while (function (iterator iterator))
  (make-take-while :function function :iterator iterator))

(defmethod take-while (function (sequence sequence))
  (coerce (take-while function (iterator sequence))
          (generic-type-of sequence)))

(defmethod drop (n (iterator iterator))
  (make-drop-n :n n :iterator iterator))

(defmethod drop (n (sequence sequence))
  (subseq sequence (min (max n 0) (length sequence)) (length sequence)))

(defmethod drop-while (function (iterator iterator))
  (make-drop-while :function function :iterator iterator))

(defmethod drop-while (function (sequence sequence))
  (coerce (drop-while function (iterator sequence))
          (generic-type-of sequence)))

(defmethod index-where (function iterable)
  (let ((index 0))
    (doiter (iterator iterable)
      (if (funcall function (at iterator))
          (return-from index-where index)
          (incf index)))
    -1))

(defmethod index-of (obj iterable)
  (index-where (lambda (x) (= x obj)) iterable))

(defmethod last-index-where (function iterable)
  (let ((index 0)
        (result -1))
    (doiter (iterator iterable)
      (when (funcall function (at iterator))
        (setf result index))
      (incf index))
    result))

(defmethod last-index-of (obj iterable)
  (last-index-where (lambda (x) (= x obj)) iterable))

(defmethod find-last-if (function iterable)
  (last (filter function iterable)))

(defmethod find-last-if-not (function iterable)
  (find-last-if (complement function) iterable))

(defmethod find-last (obj iterable)
  (find-last-if (lambda (x) (= x obj)) iterable))

(defmethod forall (function iterable &rest iterables)
  (apply #'every function iterable iterables))

(defmethod exists (function iterable &rest iterables)
  (apply #'some function iterable iterables))

(defmethod lessp ((iter1 iterator) (iter2 iterator))
  (let ((id-count 0))
    (doiters ((iter1 iter1)
              (iter2 iter2))
      (let ((a (at iter1))
            (b (at iter2)))
        (cond
          ((lessp a b) (return-from lessp id-count))
          ((lessp b a) (return-from lessp nil))))
      (incf id-count))
    (when (and (endp iter1) (not (endp iter2)))
      id-count)))

(defmethod lessp ((seq1 sequence) (seq2 sequence))
  (lessp (iterator seq1) (iterator seq2)))

(defmethod greaterp ((iter1 iterator) (iter2 iterator))
  (lessp iter2 iter1))

(defmethod greaterp ((seq1 sequence) (seq2 sequence))
  (greaterp (iterator seq1) (iterator seq2)))
