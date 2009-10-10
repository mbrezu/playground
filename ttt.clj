
(ns playground.ttt
  (:use clojure.contrib.seq-utils)
  (:use clojure.contrib.pprint))

(def table [[:tic nil :tac]
            [nil :tac nil]
            [:tic :tac :tic]])

(def table2 [[:tac nil :tic]
             [nil :tic :tic]
             [:tac :tac :tic]])

(def table3 [[:tac :tac :tic]
             [:tic :tic :tic]
             [:tac :tac :tic]])

(def table4 [[nil nil nil]
             [nil nil nil]
             [nil nil nil]])

(defn all-same [row]
  (and (first row)
       (if (every? #(= % (first row)) row)
         (first row))))

(defn transpose [table]
  (apply map vector table))

(defn extract-diagonal [table]
  (map (fn [row index] (first (drop index row)))
       table
       (iterate inc 0)))

(defn horizontal-flip [table]
  (map reverse table))

(defn vertical-flip [table]
  (reverse table))

(defn win? [table]
  (or (some all-same table)
      (some all-same (transpose table))
      (-> table extract-diagonal all-same)
      (-> table horizontal-flip extract-diagonal all-same)))

(defn full? [table]
  (not (some nil? (apply concat table))))

(defstruct <move> :row :column)

(defstruct <play> :move :table)

(defn generate-succs [table]
  (apply concat (for [[row-number row ] (indexed table)
                      [column-number el] (indexed row)
                      :when (nil? el)]
                  [(struct <play>
                           (struct <move> row-number column-number)
                           (assoc-in table [row-number column-number] :tic))
                   (struct <play>
                           (struct <move> row-number column-number)
                           (assoc-in table [row-number column-number] :tac))])))

(defn other-player [player]
  (cond (= :tic player) :tac
        (= :tac player) :tic))

(defn neg-nil [value]
  (if (nil? value)
    value
    (- value)))

(defn min-nil [value1 value2]
  (cond (nil? value1) value2
        (nil? value2) value1
        :else (min value1 value2)))

(defn alpha-beta [table player alpha beta]
  (let [win (win? table)]
    (if win
      (cond (= win player) [nil 1]
            :else [nil -1])
      (if (full? table)
        [nil 0]
        (loop [alternatives (generate-succs table)
               iter-alpha alpha
               best-moves (-> alternatives first :move)]
          (let [current-move (-> alternatives first :move)]
            (if (empty? alternatives)
              [best-moves iter-alpha]
              (let [[_ new-alpha-raw] (alpha-beta (-> alternatives first :table)
                                                  (other-player player)
                                                  (- beta)
                                                  (- iter-alpha))
                    new-alpha (- new-alpha-raw)]
                (if (> new-alpha iter-alpha)
                  (if (>= new-alpha beta)
                    [current-move new-alpha]
                    (recur (rest alternatives) new-alpha current-move))
                  (recur (rest alternatives) iter-alpha best-moves))))))))))

(defn choose-move [table player]
  (let [[move score] (alpha-beta table player -1000 1000)]
    move))

(defn apply-move [table player move]
  (assoc-in table [(:row move) (:column move)] player))

(defn empty-table []
  (vec (take 3 (repeat (vec (take 3 (repeat nil)))))))

(defn tictac-xo [item]
  (cond (= item nil) "."
        (= item :tic) "X"
        (= item :tac) "O"))

(defn tictac-toggle [item]
  (cond (= item :tic) :tac
        (= item :tac) :tic))

(defn render-table [table]
  (with-out-str
    (doseq [row table]
      (doseq [item row]
        (print (tictac-xo item)))
      (println))))

(defn move [[table player]]
  (if (or (win? table) (full? table))
    nil
    (let [the-move (choose-move table player)
          new-table (apply-move table player the-move)]
      [new-table (other-player player)])))

(defn print-game []
  (let [lazy-game (iterate move [table4 :tic])
        game (take-while identity lazy-game)]
    (doseq [table game]
      (println (render-table (first table)))
      (println (str "Next move: " (tictac-xo (second table)))))))

(defn parse-table [str-table]
  (let [empty (empty-table)
        translated (map (fn [ch] (cond (= ch \.) nil
                                       (= ch \X) :tic
                                       (= ch \O) :tac))
                        str-table)
        indexed1 (indexed translated)
        indexed2 (for [[idx el] indexed1] [[(quot idx 3) (rem idx 3)] el])]
    (reduce (fn [table item] (assoc-in table
                                       [(-> item first first) (-> item first second)]
                                       (second item)))
            empty
            indexed2)))
