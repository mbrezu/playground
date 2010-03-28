
(ns synpl.synpl
  (:use clojure.contrib.pprint))

(defn enumerate-str [str]
  (map vector str (iterate inc 0)))

(defstruct parse-tree :type :start :end :subnodes)

(defstruct parse-result :result :error :rest)

(defstruct lex-result :lexed-chars :success :rest)

;; A lexer takes an enumerated string as input and returns the number
;; of lexed chars, a boolean result stating if the lex succeeded and
;; the rest of the enumerated string (a lex-result structure).

(defn lex-predicate [predicate]
  (fn [estr]
    (if (empty? estr)
      (struct lex-result 0 false estr)
      (if (predicate ((first estr) 0))
        (struct lex-result 1 true (rest estr))
        (struct lex-result 0 false estr)))))

(defn lex-character [character]
  (lex-predicate #(= % character)))

(defn lex-sequence [lexers]
  (fn [estr]
    (loop [acc-num-lexed-chars 0
           stream estr
           to-use lexers]
      (if (empty? to-use)
        (struct lex-result acc-num-lexed-chars true stream)
        (let [lex ((first to-use) stream)]
          (if (:success lex)
            (recur (+ acc-num-lexed-chars (:lexed-chars lex))
                   (:rest lex)
                   (rest to-use))
            (struct lex-result 0 false estr)))))))

(defn lex-alternative [lexers]
  (fn [estr]
    (loop [to-use lexers]
      (if (empty? to-use)
        (struct lex-result 0 false estr)
        (let [lex ((first to-use) estr)]
          (if (:success lex)
            (struct lex-result (:lexed-chars lex) true (:rest lex))
            (recur (rest to-use))))))))

(defn lex-many [lexer]
  (fn [estr]
    (loop [acc-num-lexed-chars 0
           stream estr]
      (let [lex (lexer stream)]
        (if (:success lex)
          (recur (+ acc-num-lexed-chars (:lexed-chars lex)) (:rest lex))
          (struct lex-result acc-num-lexed-chars true stream))))))

(defn lex-many1 [lexer]
  (fn [estr]
    (let [lex ((lex-many lexer) estr)]
      (if (= 0 (:lexed-chars lex))
        (assoc lex :success false)
        lex))))

(defn lex-optional [lexer]
  (fn [estr]
    (let [lex (lexer estr)]
      (assoc lex :success true))))

(defn parse-lexer [type error lexer]
  (fn [estr]
    (let [lex (lexer estr)]
      (if (:success lex)
        (let [hd (first estr)
              start (hd 1)
              end (+ start (dec (:lexed-chars lex)))
              ptree (struct parse-tree type start end [])]
          (struct parse-result ptree nil (:rest lex)))
        (struct parse-result nil error estr)))))

(defn letter? [#^Character character]
  (Character/isLetter character))

(defn digit? [#^Character character]
  (and (Character/isLetterOrDigit character)
       (not (Character/isLetter character))))

(defn space? [#^Character character]
  (Character/isWhitespace character))

(def lex-digit (lex-predicate digit?))

(def parse-integer (parse-lexer :integer
                                "Not an integer."
                                (lex-sequence [(lex-optional (lex-alternative [(lex-character \+)
                                                                               (lex-character \-)]))
                                               (lex-many1 lex-digit)])))

(def parse-string (parse-lexer :string
                               "Not a string."
                               (lex-sequence [(lex-character \")
                                              (lex-many
                                               (lex-alternative [(lex-sequence [(lex-character \\)
                                                                                (lex-character \")])
                                                                 (lex-predicate #(not (= % \")))]))
                                              (lex-character \")])))

;; (defn make-character-parser [predicate type error]
;;   (fn [estr]
;;     (if (empty? estr)
;;       (struct parse-result nil "End-of-input" estr)
;;       (let [hd (first estr)
;;             rst (rest estr)
;;             hd-chr (hd 0)
;;             hd-pos (hd 1)]
;;         (if (predicate hd-chr)
;;           (struct parse-result
;;                   (struct parse-tree type hd-pos hd-pos [] (str hd-chr))
;;                   nil
;;                   rst)
;;           (struct parse-result
;;                   nil
;;                   error
;;                   estr))))))

;; (defn make-lexer [type error consumer initial-state]
;;   (fn [estr]
;;     (if (empty? estr)
;;       (struct parse-result nil "End-of-input" estr)
;;       (loop [stream estr
;;              state initial-state
;;              start-pos ((first estr) 1)
;;              end-pos start-pos
;;              content []]
;;         (let [[continue? new-state new-content new-stream consumed-chars] (consumer stream state)]
;;           (if (not continue?)
;;             (let [final-content (apply str content)]
;;               (if (= (count final-content) 0)
;;                 (struct parse-result nil error estr)
;;                 (let [parse-tree (struct parse-tree type start-pos (dec end-pos) [] final-content)]
;;                   (struct parse-result parse-tree nil stream))))
;;             (recur new-stream
;;                    new-state
;;                    start-pos
;;                    (+ end-pos consumed-chars)
;;                    (conj content new-content))))))))

;; (defn starts-with [predicates estr]
;;   (if (empty? predicates)
;;     true
;;     (if (empty? estr)
;;       false
;;       (let [hd-str ((first estr) 0)]
;;         (if ((first predicates) hd-str)
;;           (recur (rest predicates) (rest estr))
;;           false)))))

;; (defn- state-predicate-consumer []
;;   (fn [estr state]
;;     (if (or (empty? estr) (empty? state))
;;       [false nil nil nil nil nil]
;;       (let [hd-chr ((first estr) 0)
;;             [hd-pred pred-repeat & rest-preds] state
;;             match (hd-pred hd-chr)]
;;         (cond (and (= pred-repeat :?) (not match)) (recur estr (drop 2 state))
;;               (and (= pred-repeat :?) match) [true (drop 2 state) hd-chr (rest estr) 1]
;;               (and (= pred-repeat :1) (not match)) [false nil nil nil nil nil]
;;               (and (= pred-repeat :1) match) [true (drop 2 state) hd-chr (rest estr) 1]
;;               (and (= pred-repeat :+) (not match)) [false nil nil nil nil nil]
;;               (and (= pred-repeat :+) match) [true
;;                                               (concat [hd-pred :*] (drop 2 state))
;;                                               hd-chr
;;                                               (rest estr)
;;                                               1]
;;               (and (= pred-repeat :*) (not match)) (recur estr (drop 2 state))
;;               (and (= pred-repeat :*) match) [true state hd-chr (rest estr) 1])))))
        
;;         (cond (= pred-repeat :?
;;         (if ((first state) hd-chr)
;;           [true state hd-chr (rest estr) 1]
;;           [false nil nil nil nil])))))

;; (defn- predicate-consumer [predicates]

;; (def integer-consumer (predicate-consumer digit?))

;; (def lex-integer (make-lexer :number "Not an integer." state-predicate-consumer [sign? :? identity digit? :+]))

;; (def space-consumer (predicate-consumer space?))

;; ;; (defn- string-cp [estr state]
;; ;;   (cond (= state nil) [(starts-with "\"" estr) :inside]
;; ;;         (= state :inside) (cond (starts-with "\\\"" estr) [true :inside]
;; ;;                                 (starts-with "\"" estr) [true :outside])
;; ;;         (= state :outside) [false nil]))

;; ;; (defn- string-consumer [estr]
;; ;;   (cond 

;; ;; (def lex-string (make-lexer :number "Not an integer." integer-cp integer-consumer))

;; (def parse-letter (make-character-parser letter? :letter "Not a letter."))

;; (def parse-digit (make-character-parser digit? :digit "Not a digit."))

;; (defn parse-character
;;   ([char]
;;      (make-character-parser #(= % char) :char (str "Not " char ".")))
;;   ([char type]
;;      (make-character-parser #(= % char) type (str "Not " char "."))))

;; (defn extract-position [parse-trees]
;;   (let [starts (filter (fn [pos] pos) (map :start parse-trees))
;;         ends (filter (fn [pos] pos) (map :end parse-trees))]
;;     [(apply min starts) (apply max ends)]))

;; (defn squasher-dropper [new-type]
;;   (fn [parse-trees]
;;     (let [[start end] (extract-position parse-trees)]
;;       (struct parse-tree new-type start end [] ""))))

;; (defn combiner [new-type]
;;   (fn [parse-trees]
;;     (let [[start end] (extract-position parse-trees)
;;           filtered-trees (filter #(not (nil? (:type %))) parse-trees)]
;;       (struct parse-tree new-type start end filtered-trees ""))))

;; (defn content-transformer [new-type transform-fn]
;;   (fn [parse-trees]
;;     (let [[start end] (extract-position parse-trees)
;;           content (apply str (map :content parse-trees))
;;           transformed-content (transform-fn content)]
;;       (struct parse-tree new-type start end [] transformed-content))))

;; (defn squasher [new-type]
;;   (content-transformer new-type identity))

;; (defn parse-change-error [new-error parser]
;;   (fn [estr]
;;     (let [{result :result error :error rst :rest :as bigresult} (parser estr)]
;;       (if (not error)
;;         bigresult
;;         (struct parse-result nil new-error estr)))))

;; (defn parse-sequence [combine-fn parsers]
;;   (fn [estr]
;;     (loop [acc [] to-use parsers stream estr]
;;       (if (empty? to-use)
;;         (struct parse-result (combine-fn acc) nil stream)
;;         (let [{error :error rst :rest result :result} ((first to-use) stream)]
;;           (if (not error)
;;             (recur (conj acc result) (rest to-use) rst)
;;             (struct parse-result nil error estr)))))))

;; (defn parse-alternative [parsers]
;;   (fn [estr]
;;     (loop [to-use parsers]
;;       (if (empty? to-use)
;;         (struct parse-result nil "No alternatives left." estr)
;;         (let [{error :error rst :rest result :result} ((first to-use) estr)]
;;           (if (not error)
;;             (struct parse-result result nil rst)
;;             (recur (rest to-use))))))))

;; (defn parse-while [combine-fn parser-element]
;;   (fn [estr]
;;     (loop [acc [] stream estr]
;;       (let [{error :error result :result rst :rest} (parser-element stream)]
;;         (if error
;;           (if (empty? acc)
;;             (struct parse-result nil error estr)
;;             (struct parse-result (combine-fn acc) nil stream))
;;           (recur (conj acc result) rst))))))

;; (defn parse-consume [parser]
;;   (fn [estr]
;;     (let [{result :result error :error rst :rest :as bigresult} (parser estr)]
;;       (if error
;;         bigresult
;;         (struct parse-result
;;                 (struct parse-tree nil (:start result) (:end result) [] "")
;;                 nil
;;                 rst)))))

;; (defn parse-optional [parser]
;;   (fn [estr]
;;     (let [{result :result error :error rst :rest :as bigresult} (parser estr)]
;;       (if (not error)
;;         bigresult
;;         (struct parse-result
;;                 (struct parse-tree nil nil nil [] "")
;;                 nil
;;                 rst)))))

;; (def parse-quote (parse-character \"))

;; (def parse-non-quote (make-character-parser #(not (= \" %)) :char "Not a non-quote."))

;; (def parse-escaped-quote (parse-sequence (content-transformer :char (fn [_] \"))
;;                                          [(parse-character \\)
;;                                           (parse-character \")]))

;; (def parse-string
;;      (parse-change-error
;;       "Not a string."
;;       (parse-sequence (squasher :string)
;;                       [(parse-consume parse-quote)
;;                        (parse-while (squasher :string)
;;                                     (parse-alternative [parse-escaped-quote parse-non-quote]))
;;                        (parse-consume parse-quote)])))

;; (def parse-integer lex-integer)
;; ;;      (parse-change-error
;; ;;       "Not a number."
;; ;;       (parse-sequence (squasher :number)
;; ;;                       [(parse-optional (parse-alternative [(parse-character \+)
;; ;;                                                            (parse-character \-)]))
;; ;;                        lex-integer])))

;; (def parse-gte (parse-change-error
;;                 "Not gte."
;;                 (parse-sequence (squasher-dropper :gte)
;;                                 [(parse-character \>) (parse-character \=)])))

;; (def parse-eq (parse-change-error
;;                "Not eq."
;;                (parse-sequence (squasher-dropper :eq)
;;                                [(parse-character \=) (parse-character \=)])))

;; (def parse-lte (parse-change-error
;;                 "Not lte."
;;                 (parse-sequence (squasher-dropper :lte)
;;                                 [(parse-character \<) (parse-character \=)])))

;; (def parse-rel-op (parse-alternative [parse-gte parse-eq parse-lte]))

;; (def parse-id
;;      (parse-change-error
;;       "Not an identifier."
;;       (parse-sequence (squasher :id)
;;                       [(parse-alternative [parse-letter
;;                                            (parse-character \_)])
;;                        (parse-optional (parse-while (squasher :id)
;;                                                     (parse-alternative [parse-letter
;;                                                                         parse-digit
;;                                                                         (parse-character \_)])))])))
;; ;; a parser for S-expressions

;; (def lex-whitespace (parse-optional (make-lexer :space "Not space." space-consumer nil)))

;; (defn parse-ws-before [parser]
;;   (parse-sequence (fn [parse-trees] (parse-trees 1))
;;                   [lex-whitespace parser]))

;; (declare parse-sexp)

;; (defn extract-middle [parse-trees]
;;   (let [middle (parse-trees 1)
;;         start (:start (parse-trees 0))
;;         end (:end (parse-trees 2))]
;;     (assoc middle :start start :end end)))

;; (defn parse-list-fn []
;;   (parse-sequence extract-middle
;;                   [(parse-character \()
;;                    (parse-change-error "Not a list."
;;                                        (parse-while (combiner :list) parse-sexp))
;;                    (parse-ws-before (parse-character \)))]))

;; (defn parse-quoted-sexp-fn []
;;   (parse-sequence (combiner :quoted)
;;                   [(parse-ws-before (parse-character \'))
;;                    parse-sexp]))

;; (defn parse-sexp-fn []
;;   (parse-ws-before (parse-alternative (lazy-seq [parse-string
;;                                                  parse-integer
;;                                                  parse-id
;;                                                  (parse-list-fn)
;;                                                  (parse-quoted-sexp-fn)]))))

;; (def parse-sexp (parse-sexp-fn))

;; ;; an arithmetic expression is
;; ;; exp : term + exp
;; ;;     | term - exp
;; ;;     | term
;; ;; term : factor * term
;; ;;      | factor / term
;; ;;      | factor
;; ;; factor : number
;; ;;        | id
;; ;;        | ( exp )

;; ;; translated into synpl-cpl, this is:

;; (defn binary-combiner [parse-trees]
;;   (let [start (:start (parse-trees 0))
;;         end (:end (parse-trees 2))
;;         op (parse-trees 1)
;;         subnodes [(parse-trees 0) (parse-trees 2)]]
;;     (struct parse-tree (op :type) start end subnodes (op :content))))

;; (defn parse-binary-assoc-left [combine-fn parse-term parse-operator]
;;   (fn [estr]
;;     (let [{term1 :result error-term1 :error rst-term1 :rest} (parse-term estr)]
;;       (if error-term1
;;         (struct parse-result nil error-term1 estr)
;;         (loop [term term1 stream rst-term1]
;;           (let [{op :result error-op :error rst-op :rest} (parse-operator stream)]
;;             (if error-op
;;               (struct parse-result term nil stream)
;;               (let [{term2 :result error-term2 :error rst-term2 :rest } (parse-term rst-op)]
;;                 (if error-term2
;;                   (struct parse-result nil error-term2 estr)
;;                   (recur (combine-fn [term op term2]) rst-term2))))))))))

;; (def parse-sum (parse-binary-assoc-left binary-combiner
;;                                         (parse-ws-before parse-integer)
;;                                         (parse-ws-before
;;                                          (parse-alternative [(parse-character \+ :op-plus)
;;                                                              (parse-character \- :op-minus)]))))

;; (defn- dump-parse-tree-impl [parse-tree indent]
;;   (println (str indent
;;                 (:type parse-tree)
;;                 "["
;;                 (:content parse-tree)
;;                 "] ("
;;                 (:start parse-tree)
;;                 ", "
;;                 (:end parse-tree)
;;                 ")"))
;;   (doseq [subnode (:subnodes parse-tree)]
;;     (dump-parse-tree-impl subnode (str indent "  "))))

;; (defn dump-parse-tree [parse-tree]
;;   (dump-parse-tree-impl parse-tree ""))

;; (defn test-it []
;;   (let [test1 "<=aeouh"
;;         {result1 :result} (parse-rel-op (enumerate-str test1))]
;;     (assert (= (result1 :type) :lte))
;;     (assert (= (result1 :start) 0))
;;     (assert (= (result1 :end) 1))
;;     (assert (= (result1 :content) "")))
;;   (let [test2 "\"\\\"str\"outofstr."
;;         {result2 :result} (parse-string (enumerate-str test2))]
;;     (assert (= (result2 :type) :string))
;;     (assert (= (result2 :start) 0))
;;     (assert (= (result2 :end) 6))
;;     (assert (= (result2 :content) "\"str")))
;;   (let [test3 " (a )"
;;         {result3 :result} (parse-sexp (enumerate-str test3))
;;         subnode (nth (result3 :subnodes) 0)]
;;     (assert (= (result3 :type) :list))
;;     (assert (= (result3 :start) 1))
;;     (assert (= (result3 :end) 4))
;;     (assert (= (subnode :type) :id))
;;     (assert (= (subnode :content) "a")))
;;   (let [test4 "1+ 2"
;;         {result4 :result} (parse-sum (enumerate-str test4))
;;         subnode1 (nth (result4 :subnodes) 0)
;;         subnode2 (nth (result4 :subnodes) 1)]
;;     (assert (= (result4 :type) :op-plus))
;;     (assert (= (result4 :start) 0))
;;     (assert (= (result4 :end) 3))
;;     (assert (= (subnode1 :type) :number))
;;     (assert (= (subnode1 :content) "1"))
;;     (assert (= (subnode1 :start) 0))
;;     (assert (= (subnode1 :end) 0))
;;     (assert (= (subnode2 :type) :number))
;;     (assert (= (subnode2 :content) "2"))
;;     (assert (= (subnode2 :start) 3))
;;     (assert (= (subnode2 :end) 3))))

;; (test-it)