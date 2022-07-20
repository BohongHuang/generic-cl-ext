(defpackage :generic-cl-ext.trivia
  (:use :cl :trivia :alexandria)
  (:shadowing-import-from :generic-cl :< :> :<= :>= := :/=))

(in-package :generic-cl-ext.trivia)

(defpattern < (upper-bound)
  "Match when the object is a number less than the given upper bound."
  (with-gensyms (it it2)
    `(guard1 (,it :type real) (realp ,it)
             ,it
             (guard1 ,it2 (< ,it2 ,upper-bound)))))

(defpattern > (lower-bound)
  "Match when the object is a number greater than the given lower bound."
  (with-gensyms (it it2)
    `(guard1 (,it :type real) (realp ,it)
             ,it
             (guard1 ,it2 (< ,lower-bound ,it2)))))

(defpattern <= (upper-bound)
  "Match when the object is a number less than equal to the given upper bound."
  (with-gensyms (it it2)
    `(guard1 (,it :type real) (realp ,it)
             ,it
             (guard1 ,it2 (<= ,it2 ,upper-bound)))))

(defpattern >= (lower-bound)
  "Match when the object is a number greater than equal to the given lower bound."
  (with-gensyms (it it2)
    `(guard1 (,it :type real) (realp ,it)
             ,it
             (guard1 ,it2 (<= ,lower-bound ,it2)))))

(defpattern = (number)
  "Match when the object is a number is = to the given number."
  (with-gensyms (it it2)
    `(guard1 (,it :type number) (numberp ,it)
             ,it
             (guard1 ,it2 (= ,number ,it2)))))

(defpattern /= (number)
  "Match when the object is a number /= to the given number."
  (with-gensyms (it it2)
    `(guard1 (,it :type number) (numberp ,it)
             ,it
             (guard1 ,it2 (/= ,number ,it2)))))
