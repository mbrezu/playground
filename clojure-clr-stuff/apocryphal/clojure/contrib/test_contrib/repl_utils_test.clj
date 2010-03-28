(ns apocryphal.clojure.contrib.test-contrib.repl-utils-test
    (:use clojure.test
          apocryphal.clojure.contrib.repl-utils))

(defn test-source-test [] (str "hello"))

(deftest test-source
  (let [expected-source "(defn test-source-test [] (str \"hello\"))"]
    (is (= (get-source 'test-source-test) expected-source))))