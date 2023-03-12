#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)

(defpackage #:pixie/tests
  (:use #:cl
        #:rove)
  (:import-from
    #:pixie))
(in-package :pixie/tests)

(defvar *tests-dir*
  (merge-pathnames
    #P"tests/"
    (slot-value
      (asdf:find-system "pixie")
      'asdf/component:absolute-pathname)))

(deftest
  main-test
  (testing "main"
           (ok (equals (pixie:main (3) 3)))))
