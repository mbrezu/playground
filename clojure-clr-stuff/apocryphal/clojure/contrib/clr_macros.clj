
(ns apocryphal.clojure.contrib.clr-macros)

;; Adaptation of 'with-open' which uses .Dispose instead of .Close
;; and typehints for IDisposable.
(defmacro with-disposable
  "bindings => name init

  Evaluates body in a try expression with names bound to the values
  of the inits, and a finally clause that calls (.close name) on each
  name in reverse order."
  [bindings & body]
  (cond
   (= (count bindings) 0) `(do ~@body)
   (symbol? (bindings 0)) `(let ~(subvec bindings 0 2)
                             (try
                              (with-disposable ~(subvec bindings 2) ~@body)
                              (finally
                               (.Dispose #^IDisposable ~(bindings 0)))))))


