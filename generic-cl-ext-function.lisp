(defpackage generic-cl-ext.function
  (:use #:generic-cl))

(in-package generic-cl-ext.function)

(defmethod map (map-function (function function) &rest functions)
  (if functions
      (lambda (&rest args)
        (apply map-function
               (nmap (cons function functions)
                     (lambda (function)
                       (apply function args)))))
      (lambda (&rest args)
        (funcall map-function
                 (apply function args)))))
