(in-package :cl-user)

(defpackage :clj-user
  (:use :cl :clj-coll :clj-arrows)
  (:import-from #:alexandria #:if-let #:when-let)
  (:shadowing-import-from :clj-coll 
   :assoc :butlast :cons :count :find :first :get #:intersection :map :merge :nth 
   :last :list :list* :listp :pop :reduce :remove :replace :rest :reverse :second :sequence
   :set :some :union :vector :vectorp)
  (:documentation "Define a CLJ-USER package and initialize its read-tables for Clojure
syntax."))

(in-package :clj-user)

(named-readtables:in-readtable clj-coll:readtable) ; so you can use #{} {} []
(enable-printing) ;so persistent structures print somewhat like clojure

