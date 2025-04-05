(in-package :cl-user)

(defpackage :clj-coll-user-asd
  (:use :cl :asdf))

(in-package :clj-coll-user-asd)

(defsystem :clj-coll-user
  :version "1.0.0"
  :license "Eclipse V1.0"
  :author "Dave Tenny"
  :description "CLJ-COLL-USER is a system that defines a CLJ-USER package for playing around 
with Clojure APIs in Common Lisp. It draws on CLJ-COLL, CLJ-ARROWS, and one or two
ALEXANDRIA macros, and sets up read-tables to support #{} {} [] Clojure syntax
(only in the CLJ-USER package)."
  :depends-on (#:alexandria #:clj-coll #:named-readtables :clj-arrows)
  :components ((:file "clj-coll-user")))
