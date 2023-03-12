(defsystem "pixie"
  :version "0.1.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
               "cl-i"
               )
  :components ((:module "src"
          :components
          ((:file "main"))))
  :description "CLI library for Common Lisp. Handles args, envvars, and conf"
  :in-order-to ((test-op (test-op "pixie/tests"))))

(defsystem "pixie/tests"
  :version "0.1.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
      "pixie"
      "rove")

  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for pixie"
  :perform (test-op (op c) (symbol-call :rove :run c)))
