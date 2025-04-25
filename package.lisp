(in-package :cl-user)

(defpackage #:clj-coll
  (:use #:cl #:clj-arrows)
  (:import-from #:alexandria #:if-let #:when-let)

  (:shadow 
   ;; Clojure-styled functions provided by this package that collide with common-lisp names.
   #:assoc
   #:butlast
   #:cons
   #:count
   #:find
   #:first
   #:get
   #:intersection
   #:last
   #:list
   #:list*
   #:listp
   #:map
   #:merge
   #:nth
   #:pop
   #:reduce
   #:remove
   #:replace
   #:rest
   #:reverse
   #:second
   #:sequence
   #:set
   #:some
   #:union
   #:vector
   #:vectorp
   )
 
  (:export
   ;; Miscellaneous
   #:cl-symbol-names            ;to assist with your DEFPACKAGE (or related) activity
   #:non-cl-symbol-names        ;to assist with your DEFPACKAGE (or related) activity
   #:cl-compatible-symbol-names ;to assist with your DEFPACKAGE (or related) activity

   #:+immutable-map-type+
   #:+immutable-set-type+
   #:+immutable-vector-type+
   #:*mutation-considered-harmful*
   #:PersistentList
   #:PersistentQueue
   #:coll-fun                           ;treat your keywords/keyed-colls as functions
   

   ;; Readtable related exports
   #:*default-hashmap-constructor*
   #:*default-vector-constructor*
   #:readtable-mixin
   #:readtable          ;for this: (named-readtables:in-readtable clj-coll:readtable)

   #:enable-map-syntax
   #:enable-set-syntax
   #:enable-vector-syntax
   #:disable-map-syntax
   #:disable-set-syntax
   #:disable-vector-syntax

   ;; Printing related exports
   #:enable-printing                    ;does all of the next three functions
   #:enable-map-printing
   #:disable-map-printing
   #:enable-vector-printing
   #:disable-printing                   ;does all of the next three functions
   #:disable-vector-printing
   #:enable-set-printing
   #:disable-set-printing

   ;; Equality and comparison
   #:*compare-objects-by-value*
   #:*standard-object-equality-fn*
   #:*structure-object-equality-fn*
   #:equal?
   ;; No #:compare for now.

   ;; Miscellaneous for convenience / clojure compatibility
   ;; Note that the following CL function-related functions are compatible with Clojure's:
   ;;   CONSTANTLY, COMPLEMENT, IDENTITY
   #:clj-apply                ;an APPLY that accepts CLJ-COLL collections as last arg
   #:comp
   #:dec
   #:decimal?
   #:dir                                ;clojure.repl/dir - because I keep typing it
   #:doc                                ;clojure.repl/doc - because I keep typing it
   #:double?
   #:even?
   #:every-pred
   #:ffirst
   #:float?
   #:fnext
   #:fnil
   #:inc
   #:integer?
   #:juxt     #:mjuxt        ;immutable vector and mutable list returns, respectively
   #:key                     ;key of a MapEntry
   #:keyword?
   #:max-key
   #:min-key
   #:neg?
   #:nfirst
   #:nil?
   #:nnext
   #:number?
   #:odd?
   #:partial
   #:pos?
   #:rand
   #:rand-int
   #:ratio?
   #:rational?
   #:run!
   #:slurp                              ;Purely for convenience
   #:some?                              ;NOT at all like CL:SOME
   #:some-fn
   #:symbol?                            ;caution: different from cl:symbolp
   #:val                                ;Value of a MapEntry
   #:when-first
   #:zero?

   ;; Map Creation - unsorted.  See also: frequencies, group-by, index
   #:hash-map                           ;immutable map
   #:cl-hash-map                        ;mutable   map
   #:zipmap                             ;observes *default-hashmap-constructor*

   ;; Map Creation - sorted
   ;; sorted-map, sorted-map-by, ordered-map, priority-map, ordering-map, int-map
   ;; *TBD*: supported because FSET:MAP is sorted?  Still need to consider COMPARE.

   ;; Vector Creation
   #:vector                             ;immutable vector from args, shadows CL:VECTOR
   #:cl-vector                          ;mutable vector from args
   #:vec                                ;immutable vector from collection
   #:cl-vec                             ;mutable vector from collection

   ;; Set Creation
   #:hash-set
   #:set                                ;shadows CL:SET

   ;; Persistent List creation
   #:list
   #:list*

   ;; Persistent FIFO queue creation
   #:*EMPTY-QUEUE*           ;clojure.lang.PersistentQueue/EMPTY substitute, or call (queue)
   #:queue                   ;instead of (conj *EMPTY-QUEUE* & xs) - not a clojure API

   ;; COLLECTION APIs, roughly, those which deal with collections and not specifically
   ;; targeting seqs and lazy-seqs for input/ouput. (For those, see 'SEQUENCE APIs' below.)
   #:any?
   #:assoc                   ;shadows CL:ASSOC
   #:assoc-in
   #:bounded-count
   #:cl-conj                 ;== conj, except it creates mutable collections if necessary
   #:conj                    ;mutable in, mutable out, immutable in, immutable out
   #:contains?
   #:count                   ;collection count, shadows CL:COUNT
   #:difference              ;clojure.set/difference
   #:disj                    ;set member removal
   #:dissoc
   #:distinct?               ;true if no two args equal?
   #:empty
   #:empty?                  ;true if empty collection, incl strings
   #:every?
   #:find                    ;shadows CL:FIND
   #:frequencies
   #:get                     ;shadows CL:GET
   #:get-in
   #:group-by
   #:index                   ;clojure.set/index
   #:index-of                ;.indexOf in Clojure
   #:intersection            ;clojure.set/intersection, shadows CL symbol
   #:join                    ;clojure.set/join
   #:map-invert              ;clojure.set/map-invert
   #:merge
   #:merge-with
   #:not-any?
   #:not-empty               ;return coll (incl strings) if not empty
   #:not-every?
   #:last-index-of           ;.lastIndexOf in Clojure
   #:peek
   #:pop
   #:postwalk
   #:postwalk-demo
   #:postwalk-replace
   #:prewalk
   #:prewalk-demo
   #:prewalk-replace
   #:project                 ;clojure.set/project
   #:rand-nth
   #:reduce-kv
   #:rename                  ;clojure.set/rename
   #:rename-keys             ;clojure.set/rename-keys
   #:select                  ;clojure.set/set
   #:select-keys
   #:some                    ;return first true pred result
   #:subset?                 ;clojure.set/subset?
   #:superset?               ;clojure.set/superset?
   #:subvec
   #:union                   ;clojure.set/union
   #:update
   #:update-in
   #:update-keys
   #:update-vals
   #:walk
   ;; Type tests, those ending in 'p' are supersets of clojure and non-standard
   #:coll?                   ;true if immutable collection
   #:collp                   ;true if mutable CL or immutable CLJ-COLL collection
   #:cons?                   ;true if CLJ-COLL::CONS object. NOT in the Clojure API but useful
   #:list?                   ;true if implements IPersistentList
   #:listp                   ;true if implements IPersistentList or CL:LIST, shadows CL:LISTP
   #:map-entry?              ;true if object is a logical MapEntry representation
   #:map?                    ;true if immutable map
   #:mapp                    ;true if mutable CL hash-table or immutable CLJ-COLL map
   #:queue?                  ;true if immutable FIFO queue (non-standard- not Clojure API)
   #:set?                    ;true if immutable set 
   #:string?                 ;true if string, which is mutable in CLJ-coll but not CLojure
   #:vector?                 ;true if immutable vector
   #:vectorp ;true if mutable CL vector or immutable CLJ-COLL vector, shadows CL:VECTORP
   ;; Capability tests
   #:associative?
   #:counted?
   #:reversible?
   #:seq?                    ;read the docstring on it, again and again and again.
   #:seqable?
   #:sequential?

;;; SEQUENCE APIs. Things that are a bit more 'seq' or lazy-seq oriented,
;;; and that might have transducers or M functions.
;;;
;;; First two columns are clojure APIs, remaining are related CLJ-COLL APIs 
;;; Multiple functions on a line indicates functions have similar semantics.
;;;
;;; Non-lazy   Lazy            Eager/Mutable    Destructive   OTHER
;;; Clojure    Clojure         CLJ-COLL         CLJ-COLL      CLJ-COLL
    #:butlast                  #:mbutlast
                                                              #:cl-map
               #:concat        #:mconcat
                                                              #:count-while
               #:cycle         #:mcycle
               #:dedupe        #:mdedupe
               #:distinct      #:mdistinct
    #:doseq
               #:drop          #:mdrop
               #:drop-last     #:mdrop-last
               #:drop-while    #:mdrop-while
    #:filterv  #:filter        #:mfilter
    #:first
               #:flatten       #:mflatten
               #:interleave    #:minterleave
               #:interpose     #:minterpose
    #:into                                      #:ninto
               #:iterate       ;No miterate
               #:keep          #:mkeep
               #:keep-indexed  #:mkeep-indexed
    #:keys                     #:mkeys
    #:last
               #:line-seq      ;No mline-seq
               #:map           #:mmap
               #:mapcat        #:mmapcat
               #:map-indexed   ;No mmap-indexed, use map-indexed transducer
    #:mapv
    #:nth
               #:partition     #:mpartition
               #:partitionv    ;No mpartitionv, consider mpartition or transducer
               #:partitionv-all ;No mpartitionv-all, consider mpartition or transducer
               #:partition-all #:mpartition-all
               #:partition-by  #:mpartition-by
               #:remove        #:mremove
               #:random-sample #:mrandom-sample
               #:range         #:mrange
               #:reductions
               #:repeat        #:mrepeat
               #:repeatedly    #:mrepeatedly
               #:replace       #:mreplace
    #:reverse                  #:mreverse
    #:second
               #:sequence      ;No msequence
    #:shuffle                  #:mshuffle
               #:split-at      #:msplit-at
               #:splitv-at     ;No msplitv-at, consider msplit-at
               #:split-with    #:msplit-with
               #:take          #:mtake
               #:take-nth      #:mtake-nth
    #:take-last                #:mtake-last
               #:take-while    #:mtake-while
               #:tree-seq      #:mtree-seq
    #:vals                     #:mvals

   ;; Reduce machinery, and pure transducers.
   ;; Reduce, reduced, transducers, reduced?, deref, ensure-reduced, unreduced
   #:cat                                ;strictly a transducer
   #:completing
   #:deref
   #:ensure-reduced
   #:halt-when
   #:unreduced
   #:reduce
   #:reduced
   #:reduced?
   #:transduce

   ;; Seq machinery
   #:cons                               ;clj-coll:cons != cl:cons!
   #:doall
   #:dorun
   #:lazy-cat
   #:lazy-seq
   #:next
   #:nthnext
   #:nthrest
   #:realized?
   #:rest
   #:seq
   #:rseq
   )
  (:documentation
   "Clojure-style collection and seq APIs with support for
immutable lists/queues/vectors/sets/maps, mutable Common Lisp lists/vectors/maps,
'seqs' on all those types, and lazy sequences.

See README.md for more information."))
