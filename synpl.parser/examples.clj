
(ns synpl.examples
  (:use clojure.contrib.pprint)
  (:use synpl.parser))

;; some very simple lexers

(defn letter? [#^Character character]
  (Character/isLetter character))

(defn digit? [#^Character character]
  (and (Character/isLetterOrDigit character)
       (not (Character/isLetter character))))

(defn space? [#^Character character]
  (Character/isWhitespace character))

(def lex-digit (lex-predicate-character digit?))

(def lex-letter (lex-predicate-character letter?))

(def lex-integer (lex-sequence [(lex-optional (lex-alternative [(lex-character \+)
                                                                (lex-character \-)]))
                                lex-digit
                                (lex-many lex-digit)]))

;; use of parse-lexer to make some simple literal parsers

(def parse-integer (parse-lexer :integer "Not an integer." (lex-ws lex-integer)))

(def lex-string-literal
     (lex-ws
      (lex-sequence [(lex-character \")
                     (lex-many
                      (lex-alternative [(lex-string "\\\"")
                                        (lex-predicate-character #(not (= % \")))]))
                     (lex-character \")])))

(def parse-string (parse-lexer :string "Not a string." lex-string-literal))

(defn fast-lex-string-literal [pstate]
  (let [fail-result (struct lex-result (:start pstate) (:start pstate) false pstate)]
    (loop [stream pstate
           state :out]
      (let [character (next-char stream)]
        (cond (= state :out) (cond (= character \") (recur (forward-parse-state stream) :in)
                                   :else fail-result)
              (= state :in) (cond (= character \") (struct lex-result
                                                           (:start pstate)
                                                           (:start stream)
                                                           true
                                                           (forward-parse-state stream))
                                  (= character \\) (recur (forward-parse-state) :esc)
                                  (nil? character) fail-result
                                  :else (recur (forward-parse-state stream) :in))
              (= state :esc) (cond (nil? character) fail-result
                                   :else (recur (forward-parse-state stream) :in)))))))


(def fast-parse-string (parse-lexer :string "Not a string" fast-lex-string-literal))

;; a parser for S-expressions
;; uses lazy-seq to break mutual recursion in the combinators

(declare parse-sexp)

(def parse-id
     (parse-lexer :id
                  "Not an identifier."
                  (lex-ws
                   (lex-sequence [(lex-alternative [lex-letter
                                                    (lex-character \_)])
                                  (lex-many (lex-alternative [lex-letter
                                                              lex-digit
                                                              (lex-character \_)]))]))))

(def parse-open-paren (parse-lexer :open-paren
                                   "Not an open paren."
                                   (lex-ws (lex-character \())))

(def parse-close-paren (parse-lexer :close-paren
                                    "Not a close paren."
                                    (lex-ws (lex-character \)))))

(def parse-quote (parse-lexer :quote
                              "Not a quote."
                              (lex-ws (lex-character \'))))

(defn parse-list-fn []
  (parse-sequence (combiner :list)
                  [parse-open-paren
                   (parse-change-error "Not a list."
                                       (parse-many (combiner :list-items) parse-sexp))
                   parse-close-paren]))

(defn parse-quoted-sexp-fn []
  (parse-sequence (combiner :quoted) [parse-quote parse-sexp]))

(defn parse-sexp-fn []
  (parse-alternative (lazy-seq [parse-string
                                parse-integer
                                parse-id
                                (parse-list-fn)
                                (parse-quoted-sexp-fn)])))

(def parse-sexp (parse-sexp-fn))

