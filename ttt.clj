
;; A toy TicTacToe engine (plays perfectly though).
;;
;; Contains three different algorithms:
;;
;;  * plain minimax
;;
;;  * negamax with alpha-beta pruning (pseudo code taken from Wikipedia)
;;
;;  * minimax with alpha-beta pruning (an attempt to wrap my mind
;;    around the Wikipedia pseudocode.
;;
;; to use, run (time (print-game table5))
;; (or table4 or any table)
;;
;; WARNING: the plain minimax algorithm takes 20 minutes for an empty
;; board.  The alpha-beta pruning variations take 10 seconds for a
;; complete game. Guess that halving the exponent really works, even
;; if I didn't bother to sort variations (that probably doesn't matter
;; for a game as simple as TicTacToe).

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
             [:tic nil :tac]
             [:tac nil :tic]])

(def table5 [[:tac nil nil]
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

(defn generate-succs [table player]
;;  (println ">>> " player)
  (for [[row-number row ] (indexed table)
        [column-number el] (indexed row)
        :when (nil? el)]
    (struct <play>
            (struct <move> row-number column-number)
            (assoc-in table [row-number column-number] player))))

(defn other-player [player]
  (cond (= :tic player) :tac
        :else :tic))

(defn game-over [table player]
  (let [win (win? table)]
    (if win
      (cond (= win player) 1
            :else -1)
      (if (full? table) 0))))

(defn negamax-alpha-beta [table player alpha beta]
  (let [done (game-over table player)]
    (if done
      [nil done]
      (loop [alternatives (generate-succs table player)
             iter-alpha alpha
             best-moves (-> alternatives first :move)]
        (let [current-move (-> alternatives first :move)]
          (if (empty? alternatives)
            [best-moves iter-alpha]
            (let [[_ new-alpha-raw] (negamax-alpha-beta (-> alternatives first :table)
                                                        (other-player player)
                                                        (- beta)
                                                        (- iter-alpha))
                  new-alpha (- new-alpha-raw)]
              (if (> new-alpha iter-alpha)
                (if (>= new-alpha beta)
                  [current-move new-alpha]
                  (recur (rest alternatives) new-alpha current-move))
                (recur (rest alternatives) iter-alpha best-moves)))))))))

(defn minimax [table player orig-player]
  (let [done (game-over table orig-player)
        maximizing (= player orig-player)]
    (if done
      [nil done]
      (let [alternatives (generate-succs table player)
            scores (map second
                        (map #(minimax (:table %)
                                       (other-player player)
                                       orig-player)
                             alternatives))
            scored-moves (map vector (map :move alternatives) scores)
            best-score (apply (if maximizing max min) scores)
            best-moves (map first (filter #(= (second %) best-score) scored-moves))
            best-move (rand-elt best-moves)]
        [best-move best-score]))))

(defn minimax-alpha-beta [table player orig-player alpha beta]
  (let [done (game-over table orig-player)
        maximizing (= player orig-player)]
    (if done
      [nil done]
      (loop [alternatives (generate-succs table player)
             iter-alpha alpha
             iter-beta beta
             best-move (-> alternatives first :move)]
        (if (empty? alternatives)
          (if maximizing
            [best-move iter-alpha]
            [best-move iter-beta])
          (let [[_ score] (minimax-alpha-beta (-> alternatives first :table)
                                              (other-player player)
                                              orig-player
                                              iter-alpha
                                              iter-beta)
                move (-> alternatives first :move)]
            (if maximizing
              (if (> score iter-beta)
                [move score]
                (recur (rest alternatives)
                       (max score iter-alpha)
                       iter-beta
                       (if (> score iter-alpha)
                         move
                         best-move)))
              (if (< score iter-alpha)
                [move score]
                (recur (rest alternatives)
                       iter-alpha
                       (min score iter-beta)
                       (if (< score iter-beta)
                         move
                         best-move))))))))))

(defn choose-move [table player]
;;   (let [[move score] (negamax-alpha-beta table
;;                                          player
;;                                          -1000
;;                                          1000)]
;;   (let [[move score] (minimax table
;;                               player
;;                               player)]
  (let [[move score] (minimax-alpha-beta table
                                         player
                                         player
                                         -1000
                                         1000)]
    move))

(defn apply-move [table player move]
  (assoc-in table [(:row move) (:column move)] player))

(defn empty-table []
  (vec (take 3 (repeat (vec (take 3 (repeat nil)))))))

(defn tictac-xo [item]
  (cond (= item nil) "."
        (= item :tic) "X"
        (= item :tac) "O"))

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

(defn print-game [start-table]
  (let [lazy-game (iterate move [start-table :tic])
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
