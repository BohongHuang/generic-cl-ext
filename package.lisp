(uiop:define-package generic-cl-ext
  (:mix-reexport #:generic-cl-ext.collection #:generic-cl-ext.trivia #:generic-cl))

(in-package #:generic-cl-ext)

(unexport 'some)
(unexport 'every)
