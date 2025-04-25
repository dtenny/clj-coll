(in-package :cl-user)

(defpackage :clj-coll-test-asd
  (:use :cl :asdf))

(in-package :clj-coll-test-asd)

(defsystem :clj-coll-test
  :version "1.0.1"
  :license "Eclipse V1.0"
  :author "Dave Tenny"
  :description "Tests for the :clj-coll package."
  :depends-on (#:clj-coll #:fiveam #:named-readtables :clj-arrows)
  :components ((:file "clj-coll-test")))
