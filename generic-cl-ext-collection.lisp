(defpackage :generic-cl-ext.collection
  (:use :generic-cl)
  (:export :collect :collect-to
           :zip :zip-with-index
           :filter :filter-not
           :flatten :flatmap
           :fold-left :scan-left
           :take :take-while :drop :drop-while :index-of :index-where :last-index-where :last-index-of
           :find-last :find-last-if :find-last-if-not))

(in-package :generic-cl-ext.collection)

(declaim (ftype (function (t &optional fixnum fixnum t) iterator) iterator))

(defmethod (setf first) (value sequence)
  (setf (elt sequence 0) value))

(defmethod (setf last) (value sequence &optional (n 0))
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

(defstruct (index-generator (:include iterator))
  (index 0))

(defmethod endp ((iterator index-generator))
  nil)

(defmethod advance ((iterator index-generator))
  (incf (index-generator-index iterator)))

(defmethod at ((iterator index-generator))
  (index-generator-index iterator))

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
                          (make-zip :iterators (list iterator (make-index-generator)))))))

(defmethod concatenate ((iterator iterator) &rest iterators)
  (if iterators
      (make-iterator-of-iterable :iterator (iterator (cons iterator iterators)))
      iterator))

(declaim (inline zip))
(defun zip (iterable1 &optional iterable2 &rest iterables)
  (if iterable2
      (if iterables
          (apply #'map (lambda (&rest args) args) (cons iterable1 (cons iterable2 iterables)))
          (apply #'map (lambda (a b) (cons a b)) (list iterable1 iterable2)))
      iterable1))

(declaim (inline zip-with-index))
(defun zip-with-index (iterable)
  (zip iterable (make-index-generator)))

(declaim (inline collect-to))
(defun collect-to (type &rest iterables)
  (if (subtypep type 'iterator)
      (iterator (apply #'zip iterables))
      (apply #'map-to type #'identity iterables)))

(declaim (inline collect))
(defun collect (&rest iterables)
  (apply #'collect-to 'vector iterables))

(setf (fdefinition 'filter) #'remove-if-not)
(setf (fdefinition 'filter-not) #'remove-if)

(defmethod generic-type-of (obj)
  (type-of obj))

(defmethod generic-type-of ((list list))
  'list)

(defmethod generic-type-of ((vector vector))
  'vector)

(declaim (inline flatten))
(defun flatten (iterable)
  (collect-to (generic-type-of iterable)
              (make-iterator-of-iterable :iterator (iterator iterable))))

(declaim (inline flatmap))
(defun flatmap (&rest map-args)
  (flatten (apply #'map map-args)))

(declaim (inline fold-left))
(defun fold-left (function init-value iterable)
  (let ((acc init-value))
    (doiter (iterator iterable)
      (setf acc (funcall function acc (at iterator))))
    acc))

(declaim (inline scan-left))
(defun scan-left (function init-value iterable)
  (let ((acc init-value))
    (map (lambda (it)
           (setf acc (funcall function acc it)))
         iterable)))

(declaim (inline take))
(defun take (n iterable)
  (collect-to (generic-type-of iterable)
              (make-take-n :n n :iterator (iterator iterable))))

(declaim (inline take-while))
(defun take-while (function iterable)
  (collect-to (generic-type-of iterable)
              (make-take-while :function function :iterator (iterator iterable))))

(declaim (inline drop-while))
(defun drop-while (function iterable)
  (collect-to (generic-type-of iterable)
              (make-drop-while :function function :iterator (iterator iterable))))

(declaim (inline drop))
(defun drop (n iterable)
  (collect-to (generic-type-of iterable)
              (make-drop-n :n n :iterator (iterator iterable))))

(declaim (inline index-where))
(defun index-where (function iterable)
  (let ((index 0))
    (doiter (iterator iterable)
      (if (funcall function (at iterator))
          (return-from index-where index)
          (incf index)))
    -1))

(declaim (inline index-of))
(defun index-of (obj iterable)
  (index-where (lambda (x) (= x obj)) iterable))

(declaim (inline last-index-where))
(defun last-index-where (function iterable)
  (let ((index 0)
        (result -1))
    (doiter (iterator iterable)
      (when (funcall function (at iterator))
        (setf result index))
      (incf index))
    result))

(declaim (inline last-index-of))
(defun last-index-of (obj iterable)
  (last-index-where (lambda (x) (= x obj)) iterable))

(declaim (inline find-last-if))
(defun find-last-if (function iterable)
  (last (filter function iterable)))

(declaim (inline find-last-if-not))
(defun find-last-if-not (function iterable)
  (find-last-if (complement function) iterable))

(declaim (inline find-last))
(defun find-last (obj iterable)
  (find-last-if (lambda (x) (= x obj)) iterable))
