
# ClojureCLR Stuff

This repository contains better-than-nothing-worse-than-anything-else
implementations for ClojureJVM functionality that's missing from
ClojureCLR.

This repository is in no official way connected to Clojure, ClojureCLR
or their authors. It is just an 'apocryphal' conversion of some of
`clojure.contrib` to use the CLR.

Much of the code in here is derived (by trivial means such as renames)
from the `clojure.contrib` code. Files that have the same name as a
file from `clojure.contrib` contain such code. Credit and copyright
(if missing from the apocryphal version) goes to the people mentioned
in the original files copyright notices. All mistakes in the
apocryphal version are mine (mbrezu).

All the added content is licensed under the EPL and CPL, as requested
by usage of the license of the original code.

## Running tests

There is a `test.clj` in the root of this distribution. Running

    (use :reload-all 'test)

will run all the available tests and print a global summary at the
end.

## List of modules

 * `apocryphal.clojure.contrib.future-impl`
   * implements a replacement for the `future` Clojure macro, and
     adapts `pmap` & friends from clojure.core
 * `apocryphal.clojure.contrib.repl-utils`
   * has a working version of `show`, `source` and `expression-info`
     from the 'canonical' `clojure.contrib`.
 * `apocryphal.clojure.contrib.clr-macros`
   * has a `with-disposable` macro that works like `with-open`, but
     has `using` (from C#) semantics (call `.Dispose` instead of
     `.Close`, type hint to `IDisposable`).

## TODO

 * `pprint`
   * `ColumnWriter.clj`
   * `PrettyWriter.clj`
   * `pprint_base`
 * `clojure.contrib.seq-utils` - `seque`
 * `clojure.contrib.str-utils` - add more tests (current tests not
   running, missing .start in JReMatcher)
 * `clojure.contrib.repl-utils` - done properly
