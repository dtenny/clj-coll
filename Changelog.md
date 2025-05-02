# CLJ-COLL Change log

## [1.0.2] 2025-MAY-02

* Fix cl:hash-table iterators to provide cl:list valued mapentry
  equivalents instead of persistent list pairs.

* `rename` fixes:
  ** Should not accept single maps in place of 'rels'. This fix may break
     existing uses, but such uses probably weren't working as expected
     anyway and it was not a Clojure compatible interaction.
  ** Returns the potentially updated original container if it was mutable
     instead of a new list. This fix may break existing callers relying
     on a new cl:list being allocated, we're gambling that nobody is using
     CLJ-COLL enough in this first month to have relied on this behavior.

* Updated README.md to reflect intentions w.r.t. future API compatibility
  and potentially breaking changes.

## [1.0.1] 2025-APR-25

Add three DEFPACKAGE-related utility functions (with tests and README.md documentaton).

* CL-SYMBOL-NAMES
* NON-CL-SYMBOL-NAMES
* CL-COMPATIBLE-SYMBOL-NAMES

You can use these with various DEFPACKAGE options to easily to
shadow and import symbols.

Also clarified customization of the vector syntax reader.

Ran tests on SBCL, CCL, ECL, ABCL.

## [1.0.0] 2025-APR-05

* Initial release.
