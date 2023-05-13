#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)

(defpackage #:pixie/tests
  (:use #:cl
        #:rove)
  (:import-from
    #:pixie)
  (:import-from
    #:nrdl)
  )
(in-package :pixie/tests)

(defvar *tests-dir*
  (merge-pathnames
    #P"tests/"
    (slot-value
      (asdf:find-system "pixie")
      'asdf/component:absolute-pathname)))

(deftest
  help-page-test
    (testing "Help page: typical invocation"
             (ok
               (equals
                 "Usage: pixie <subcommands>

  Subcommands:

    - `dm ls`

To see the help page for a subcommand, run `pixie <subcmd> help.

Given options:

  - `FOO` = `bar`
  - `PLETHORA` = `15`
  - `GARBLED` = `FALSE`

"
			(pixie:help-page
              '((:garbled . nrdl:false)
                (:plethora . 15)
                (:foo . "bar")))))))
