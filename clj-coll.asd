(in-package :cl-user)

(defpackage #:clj-coll-asd
  (:use #:cl #:asdf))

(in-package #:clj-coll-asd)

(defsystem :clj-coll
  :version "1.0.2"
  :license "Eclipse 1.0"
  :author "Dave Tenny"
  :description "Clojure collection and sequence APIs for Common Lisp,
with optional read-table support for Clojure set/map/vector syntax."
  :bug-tracker "https://github.com/dtenny/clj-coll/issues"
  :source-control (:git "https://github.com/dtenny/clj-coll")
  :depends-on (#:uiop #:alexandria #:serapeum #:named-readtables #:clj-arrows 
               #:read-line-crlf #:fset #:bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "ringqueue")
               (:file "utils")
               (:file "seq-structs")
               (:file "seqs")
               (:file "colliter")
               (:file "glue")
               (:file "seq-apis")
               (:file "doseq-a")
               (:file "doseq-b")
               (:file "seq-apis2")
               (:file "equiv")
               (:file "reduce")
               (:file "syntax")
               (:file "print")
               (:file "misc-apis")
               (:file "coll-apis")))
