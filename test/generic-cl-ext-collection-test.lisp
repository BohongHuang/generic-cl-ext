(defpackage :generic-cl-ext.test
  (:use :generic-cl :generic-cl-ext :parachute))

(in-package :generic-cl-ext.test)

(define-test iterator
  (of-type iterator (iterator (iterator '(1 2 3 4)))))

(define-test concatenate
  (is = #(1 2 3 4) (map-to 'vector #'identity (concatenate (iterator nil) (iterator '(1 2)) (iterator nil) (iterator #(3 4)) (iterator nil)))))

(define-test map
  (is = '(6 8 10 12) (map #'+ '(1 2 3 4) '(5 6 7 8)))
  (is = 0 (length (map #'+ (iterator '(1 2 3 4)) (iterator '()))))
  (is = #(6 8 10) (map-to 'vector #'+ (iterator '(1 2 3 4)) (iterator '(5 6 7)))))

(define-test collect-to
  :depends-on (map)
  (is = #(1 2 3 4) (collect-to 'vector '(1 2 3 4)))
  (of-type iterator (collect-to 'iterator '(1 2 3 4)))
  (is = #(1 2 3 4) (collect-to 'vector (collect-to 'iterator '(1 2 3 4))))
  (is = '(1 2 3 4) (collect-to 'list (collect-to 'iterator '(1 2 3 4)))))

(define-test flatten
  :depends-on (map collect-to)
  (is = '(1 2 3 4) (flatten '(() (1) () (2 3 4) ())))
  (is = '(1 2 3 4) (collect-to 'list (flatten (iterator (mapcar #'iterator '(() (1) () (2 3 4) ()))))))
  (is = nil (collect-to 'list (flatten (iterator (list (iterator nil))))))
  (is = nil (collect-to 'list (flatten (iterator nil))))
  (fail  (flatten '(() (1 2 3) 4))))

(define-test flatmap
  :depends-on (flatten)
  (is = '(1 1 2 2 3 3) (flatmap (lambda (x) (list x x)) '(1 2 3)))
  (is = #(1 1 2 1 2 3) (collect-to 'vector (flatmap (lambda (x) (iterator (loop :for i :from 1 :to x :collect i))) (iterator '(0 1 2 3))))))

(define-test fold-left
  :depends-on (collect-to)
  (is = 15 (fold-left #'+ 5 '(1 2 3 4))))

(define-test scan-left
  :depends-on (collect-to)
  (is = '(6 8 11 15) (scan-left #'+ 5 '(1 2 3 4))))

(define-test take
  :depends-on (collect-to)
  (is = #(1 2) (take 2 #(1 2 3 4)))
  (is = #(1 2 3 4) (take 10 #(1 2 3 4)))
  (is = '(1 2 3 4) (collect-to 'list (take 10 (iterator #(1 2 3 4)))))
  (is = nil (take 10 nil)))

(define-test take-while
  :depends-on (collect-to)
  (is = #(1 2) (take-while (lambda (x) (< x 3)) #(1 2 3 4))))

(define-test drop
  :depends-on (collect-to)
  (is = #(3 4) (drop 2 #(1 2 3 4)))
  (is = #(1 2 3 4) (drop 0 #(1 2 3 4)))
  (is = '(1 2 3 4) (collect-to 'list (drop 0 (iterator #(1 2 3 4)))))
  (is = nil (drop 10 '(1 2 3 4))))

(define-test drop-while
  :depends-on (collect-to)
  (is = #(3 4) (drop-while (lambda (x) (< x 3)) #(1 2 3 4)))
  (is = #(1 2 3 4) (drop-while (lambda (x) (< x 1)) #(1 2 3 4)))
  (is = nil (drop-while (lambda (x) (< x 10)) '(1 2 3 4))))

(define-test zip
  :depends-on (map collect-to)
  (is = '(1 2 3 4) (zip '(1 2 3 4)))
  (is = '((1 . 5) (2 . 6) (3 . 7) (4 . 8)) (zip '(1 2 3 4) '(5 6 7 8)))
  (is = '((1 5 9) (2 6 10) (3 7 11) (4 8 12)) (zip '(1 2 3 4) '(5 6 7 8) '(9 10 11 12)))
  (is = '((1 . 5) (2 . 6) (3 . 7) (4 . 8)) (collect-to 'list (zip (iterator '(1 2 3 4)) (iterator '(5 6 7 8)))))
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
  (is = #(1 2 3 4) (collect-to 'vector (concatenate
                                        (iterator '(1))
                                        (map #'car
                                             (take 2
                                                   (drop 1
                                                         (filter (lambda (x) (< (cdr x) 5))
                                                                 (zip-with-index
                                                                  (iterator '(1 2 3 4 5)))))))
                                        (iterator '(4))))))

(test *package*)
