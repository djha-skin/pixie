(defsystem "pixie"
  :version "0.1.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
               "cl-i"
               "arrows"
               "alexandria"
               "nrdl"
               "dexador"
               "quri"
               "cl-ppcre"
               "trivial-features"
               "trivial-package-local-nicknames"
               )
  :components ((:module "src"
                        :components
                        (
                         (:file "client")
                         (:module "clients"
                                  :components
                                  (
                                   (:file "groupme")
                                   ))
                         (:file "main")
                         )))
  :description "Small, mischeivous chat client"
  :build-operation "program-op"
  :build-pathname "pixie"
  :entry-point "pixie:main"
  :in-order-to ((test-op (test-op "pixie/tests"))))

(defsystem "pixie/tests"
  :version "0.1.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
      "pixie"
      "cl-ppcre"
      "rove")

  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for pixie"
  :perform (test-op (op c) (symbol-call :rove :run c)))
