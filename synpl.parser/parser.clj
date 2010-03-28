
(ns synpl.parser
  (:use clojure.contrib.pprint))

(defstruct parse-state :string :start)

(defn make-parse-state [string]
  (struct parse-state string 0))

(defn- str-len [string]
  (.length #^String string))

(defn parse-state-empty [pstate]
  (= (str-len (:string pstate))
     (:start pstate)))

(defn forward-parse-state
  ([pstate n]
     (struct parse-state
             (:string pstate)
             (+ n (:start pstate))))
  ([pstate]
     (forward-parse-state pstate 1)))

(defn next-char [pstate]
  (let [start (:start pstate)]
    (if (< start (str-len (:string pstate)))
      (.charAt #^String (:string pstate) start)
      nil)))

(defn prefix
  ([pstate n]
     (let [start (:start pstate)
           len (str-len (:string pstate))
           end-index (+ start n)
           usable-end-index (min end-index len)]
       (.substring #^String (:string pstate) start usable-end-index)))
  ([pstate]
     (prefix pstate 1)))

(defstruct lex-result :start :end :success :parse-state)

(defn lex-eater [eater]
  (fn [pstate]
    (let [chars-to-eat (eater pstate)]
      (let [start (:start pstate)]
        (if (> chars-to-eat 0)
          (let [end (+ start chars-to-eat)
                new-parse-state (forward-parse-state pstate chars-to-eat)]
            (struct lex-result start end true new-parse-state))
          (struct lex-result start start false pstate))))))

(defn lex-string [string]
  (let [n (str-len string)]
    (lex-eater (fn [pstate] (if (= (prefix pstate n) string) n 0)))))

(defn lex-predicate-character [character-predicate]
  (lex-eater (fn [pstate]
               (let [character (next-char pstate)]
                 (if (nil? character)
                   0
                   (if (character-predicate character) 1 0))))))

(defn lex-character [character]
  (lex-predicate-character #(= % character)))

(defstruct parse-tree :type :start :end :subnodes :content)

(defstruct parse-result :parse-tree :error :parse-state :warnings)

(defn lex-sequence [lexers]
  (fn [pstate]
    (loop [acc-end (:start pstate)
           itr-parse-state pstate
           to-use lexers]
      (if (empty? to-use)
        (struct lex-result
                (:start pstate)
                acc-end
                true
                itr-parse-state)
        (let [lex ((first to-use) itr-parse-state)]
          (if (:success lex)
            (recur (:end lex)
                   (:parse-state lex)
                   (rest to-use))
            (let [start (:start pstate)]
              (struct lex-result start start false pstate))))))))

(defn lex-alternative [lexers]
  (fn [pstate]
    (loop [to-use lexers]
      (if (empty? to-use)
        (let [start (:start pstate)]
          (struct lex-result start start false pstate))
        (let [lex ((first to-use) pstate)]
          (if (:success lex)
            lex
            (recur (rest to-use))))))))

(defn lex-many [lexer]
  (fn [pstate]
    (loop [acc-end (:start pstate)
           itr-parse-state pstate]
      (let [lex (lexer itr-parse-state)]
        (if (:success lex)
          (recur (:end lex) (:parse-state lex))
          (struct lex-result (:start pstate) acc-end true itr-parse-state))))))

(defn lex-optional [lexer]
  (fn [pstate]
    (let [lex (lexer pstate)]
      (assoc lex :success true))))

(def lex-whitespace (lex-many (lex-alternative [(lex-character \space)
                                                (lex-character \newline)
                                                (lex-character \tab)])))

(defn lex-consume-first [ws-lexer lexer]
  (fn [pstate]
    (let [lex (ws-lexer pstate)]
      (if (:success lex)
        (lexer (:parse-state lex))
        (lexer pstate)))))

(defn lex-ws [lexer]
  (lex-consume-first lex-whitespace lexer))

(defn parse-lexer [type error lexer]
  (fn [pstate]
    (let [lex (lexer pstate)]
      (if (:success lex)
        (let [start (:start lex)
              end (:end lex)
              content (.substring #^String (:string pstate) start end)
              ptree (struct parse-tree type start end [] content)]
          (struct parse-result ptree nil (:parse-state lex)))
        (struct parse-result nil error pstate)))))

(defn combiner [new-type]
  (fn [parse-trees]
     (let [start (:start (first parse-trees))
           end (:end (last parse-trees))]
       (struct parse-tree new-type start end parse-trees ""))))

(defn parse-change-error [new-error parser]
  (fn [pstate]
    (let [{error :error :as bigresult} (parser pstate)]
      (if (not error)
        bigresult
        (struct parse-result nil new-error pstate)))))

(defn- drop-optional-trees [parse-trees]
  (filter (complement #(nil? (:type %))) parse-trees))

(defn parse-sequence [combine-fn parsers]
  (fn [pstate]
    (loop [acc [] to-use parsers stream pstate]
      (if (empty? to-use)
        (struct parse-result
                (combine-fn (drop-optional-trees acc))
                nil
                stream)
        (let [{error :error rst :parse-state result :parse-tree} ((first to-use) stream)]
          (if (not error)
            (recur (conj acc result) (rest to-use) rst)
            (struct parse-result nil error pstate)))))))

(defn parse-alternative [parsers]
  (fn [pstate]
    (loop [to-use parsers]
      (if (empty? to-use)
        (struct parse-result nil "No alternatives left." pstate)
        (let [{error :error :as bigresult} ((first to-use) pstate)]
          (if (not error)
            bigresult
            (recur (rest to-use))))))))

(defn parse-many [combine-fn parser-element]
  (fn [pstate]
    (loop [acc [] stream pstate]
      (let [{error :error result :parse-tree rst :parse-state} (parser-element stream)]
        (if error
          (let [real-acc (drop-optional-trees acc)]
            (if (empty? real-acc)
              (struct parse-result nil nil pstate)
              (struct parse-result
                      (combine-fn real-acc)
                      nil
                      stream)))
          (recur (conj acc result) rst))))))

(defn parse-optional [parser]
  (fn [pstate]
    (let [{error :error rst :parse-state :as bigresult} (parser pstate)]
      (if (not error)
        bigresult
        (struct parse-result
                (struct parse-tree nil nil nil [] "")
                nil
                rst)))))

(defn- dump-parse-tree-impl [parse-tree indent]
  (println (str indent
                (:type parse-tree)
                "["
                (:content parse-tree)
                "] ("
                (:start parse-tree)
                ", "
                (:end parse-tree)
                ")"))
  (doseq [subnode (:subnodes parse-tree)]
    (dump-parse-tree-impl subnode (str indent "  "))))

(defn dump-parse-tree [parse-tree]
  (dump-parse-tree-impl parse-tree ""))
