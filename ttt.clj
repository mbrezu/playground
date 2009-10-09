
(ns playground.ttt
  (:use clojure.contrib.seq-utils))

(def table [[:tic nil :tac]
            [nil :tac nil]
            [:tic :tac :tic]])

(def table2 [[:tac nil :tic]
             [nil :tic :tic]
             [:tac :tac :tic]])

(def table3 [[:tac :tac :tic]
             [:tic :tic :tic]
             [:tac :tac :tic]])

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

(defn win? [table]
  (or (some all-same table)
      (some all-same (transpose table))
      (-> table extract-diagonal all-same)
      (-> table horizontal-flip extract-diagonal all-same)))

(defn full? [table]
  (not (some nil? (apply concat table))))

(defn generate-succs [table]
  (apply concat (for [[row-number row ] (indexed table)
                      [column-number el] (indexed row)
                      :when (nil? el)]
                  [(assoc-in table [row-number column-number] :tic)
                   (assoc-in table [row-number column-number] :tac)])))
