(defsystem generic-cl-ext
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "mit"
  :description "Some useful extensions for generic-cl, such as functional iterator API."
  :homepage "https://github.com/BohongHuang/generic-cl-ext"
  :bug-tracker "https://github.com/BohongHuang/generic-cl-ext/issues"
  :source-control (:git "https://github.com/BohongHuang/generic-cl-ext.git")
  :components ((:file "generic-cl-ext-collection")
               (:file "generic-cl-ext-trivia")
               (:file "generic-cl-ext-function")
               (:file "package"))
  :depends-on (:generic-cl :trivia)
  :in-order-to ((test-op (test-op #:generic-cl-ext/test))))

(defsystem generic-cl-ext/test
  :pathname "./test/"
  :components ((:file "generic-cl-ext-collection-test"))
  :depends-on (:generic-cl-ext :parachute)
  :perform (test-op (op c) (symbol-call :generic-cl-ext.test :run-test)))
