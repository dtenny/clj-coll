(in-package :cl-user)

(defpackage :clj-user
  (:use :cl :clj-coll :clj-arrows)
  (:import-from #:alexandria #:if-let #:when-let)
  (:shadowing-import-from :clj-coll . #.(clj-coll:cl-symbol-names))
  (:documentation "Define a CLJ-USER package and initialize its read-tables for Clojure
syntax."))

(in-package :clj-user)

(named-readtables:in-readtable clj-coll:readtable) ; so you can use #{} {} []
(enable-printing) ;so persistent structures print somewhat like clojure

