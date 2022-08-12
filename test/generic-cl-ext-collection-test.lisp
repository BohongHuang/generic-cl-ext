(defpackage :generic-cl-ext.test
  (:use #:generic-cl-ext #:parachute)
  (:export #:run-test))

(in-package :generic-cl-ext.test)

(define-test iterator
  (of-type iterator (iterator (iterator '(1 2 3 4)))))

(define-test concatenate
  (is = #(1 2 3 4) (map-to 'vector #'identity (concatenate (iterator nil) (iterator '(1 2)) (iterator nil) (iterator #(3 4)) (iterator nil)))))

(define-test map
  (is = '(6 8 10 12) (map #'+ '(1 2 3 4) '(5 6 7 8)))
  (is = 0 (length (map #'+ (iterator '(1 2 3 4)) (iterator '()))))
  (is = #(6 8 10) (map-to 'vector #'+ (iterator '(1 2 3 4)) (iterator '(5 6 7)))))

(define-test coerce
  :depends-on (map)
  (is = #(1 2 3 4) (coerce '(1 2 3 4) 'vector))
  (of-type iterator (coerce '(1 2 3 4) 'iterator))
  (is = #(1 2 3 4) (coerce (coerce '(1 2 3 4) 'iterator) 'vector))
  (is = '(1 2 3 4) (coerce (coerce '(1 2 3 4) 'iterator) 'list)))

(define-test flatten
  :depends-on (map coerce)
  (is = '(1 2 3 4) (flatten '(() (1) () (2 3 4) ())))
  (is = '(1 2 3 4) (coerce (flatten (iterator (mapcar #'iterator '(() (1) () (2 3 4) ())))) 'list))
  (is = nil (coerce (flatten (iterator (list (iterator nil)))) 'list))
  (is = nil (coerce (flatten (iterator nil)) 'list))
  (fail  (flatten '(() (1 2 3) 4))))

(define-test flatmap
  :depends-on (flatten)
  (is = '(1 1 2 2 3 3) (flatmap (lambda (x) (list x x)) '(1 2 3)))
  (is = #(1 1 2 1 2 3) (coerce (flatmap (lambda (x) (iterator (loop :for i :from 1 :to x :collect i))) (iterator '(0 1 2 3))) 'vector)))

(define-test fold-left
  :depends-on (coerce)
  (is = 15 (fold-left #'+ 5 '(1 2 3 4))))

(define-test scan-left
  :depends-on (coerce)
  (is = '(6 8 11 15) (scan-left #'+ 5 '(1 2 3 4))))

(define-test take
  :depends-on (coerce)
  (is = #(1 2) (take 2 #(1 2 3 4)))
  (is = #(1 2 3 4) (take 10 #(1 2 3 4)))
  (is = '(1 2 3 4) (coerce (take 10 (iterator #(1 2 3 4))) 'list))
  (is = nil (take 10 nil)))

(define-test take-while
  :depends-on (coerce)
  (is = #(1 2) (take-while (lambda (x) (< x 3)) #(1 2 3 4))))

(define-test drop
  :depends-on (coerce)
  (is = #(3 4) (drop 2 #(1 2 3 4)))
  (is = #(1 2 3 4) (drop 0 #(1 2 3 4)))
  (is = '(1 2 3 4) (coerce (drop 0 (iterator #(1 2 3 4))) 'list))
  (is = nil (drop 10 '(1 2 3 4))))

(define-test drop-while
  :depends-on (coerce)
  (is = #(3 4) (drop-while (lambda (x) (< x 3)) #(1 2 3 4)))
  (is = #(1 2 3 4) (drop-while (lambda (x) (< x 1)) #(1 2 3 4)))
  (is = nil (drop-while (lambda (x) (< x 10)) '(1 2 3 4))))

(define-test zip
  :depends-on (map coerce)
  (is = '(1 2 3 4) (zip '(1 2 3 4)))
  (is = '((1 . 5) (2 . 6) (3 . 7) (4 . 8)) (zip '(1 2 3 4) '(5 6 7 8)))
  (is = '((1 5 9) (2 6 10) (3 7 11) (4 8 12)) (zip '(1 2 3 4) '(5 6 7 8) '(9 10 11 12)))
  (is = '((1 . 5) (2 . 6) (3 . 7) (4 . 8)) (coerce (zip (iterator '(1 2 3 4)) (iterator '(5 6 7 8))) 'list))
  (is = nil (zip '(1 2 3 4) '(5 6 7 8) nil)))

(define-test zip-with-index
  :depends-on (zip)
  (is = '((1 . 0) (2 . 1) (3 . 2) (4 . 3)) (zip-with-index '(1 2 3 4))))

(define-test index-of
  (is = -1 (index-of 0 '(1 2 3 4)))
  (is = 3 (index-of 4 '(1 2 3 4))))

(define-test last-index-of
  (is = -1 (last-index-of 0 '(1 2 3 1)))
  (is = 3 (last-index-of 1 '(1 2 3 1))))

(define-test find-last
  (is = nil (find-last 0 '(1 2 3 1)))
  (is = 1 (find-last 1 '(1 2 3 1))))

(define-test compose
  (is = #(1 2 3 4) (coerce (concatenate
                            (iterator '(1))
                            (map #'car
                                 (take 2
                                       (drop 1
                                             (filter (lambda (x) (< (cdr x) 5))
                                                     (zip-with-index
                                                      (iterator '(1 2 3 4 5)))))))
                            (iterator '(4)))
                           'vector)))

(define-test sliding
  (is = '((1 2) (2 3) (3 4)) (coerce (sliding '(1 2 3 4) 2) 'list))
  (is = '((1 2) (3 4)) (coerce (sliding '(1 2 3 4) 2 2) 'list))
  (is = '((1 2)) (coerce (sliding '(1 2 3 4) 2 3) 'list))
  (is = '((1 2 3 4)) (coerce (sliding '(1 2 3 4) 4 4) 'list))
  (is = '() (coerce (sliding '(1 2 3 4) 5 4) 'list)))

(define-test fold-right
  (is = 18 (fold-right #'+ 8 '(1 2 3 4)))
  (is = 8 (fold-right (lambda (it acc) (declare (ignore it)) acc) 8 '(1 2 3 4)))
  (is = 8 (fold-right #'+ 8 '())))

(defun run-test ()
  (test :generic-cl-ext.test))
