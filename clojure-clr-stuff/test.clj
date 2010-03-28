
(ns test
  (:use clojure.test
        apocryphal.clojure.contrib.test-contrib.str-utils-test
        apocryphal.clojure.contrib.test-contrib.seq-utils-test
        apocryphal.clojure.contrib.test-contrib.repl-utils-test))

(let [test-results [(run-tests 'apocryphal.clojure.contrib.test-contrib.str-utils-test)
                    (run-tests 'apocryphal.clojure.contrib.test-contrib.seq-utils-test)
                    (run-tests 'apocryphal.clojure.contrib.test-contrib.repl-utils-test)]
      add-overwrite (fn [v1 v2] (if (and (number? v1) (number? v2)) (+ v1 v2) v2))]
  (println (apply merge-with add-overwrite test-results)))
