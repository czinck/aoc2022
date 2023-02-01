(ns aoc-2022.day5
  (:require [clojure.string :as str])
  (:use clojure.tools.trace)
  (:require clojure.set))

(defn readlines
  [filename]
  (clojure.string/split-lines (slurp filename)))

(defn sum
  [l]
  (reduce + l))

(defn parse-int
  [s]
  (Integer/parseInt s))

(defn parse-start
  ([start-lines]
  (let [reversed (reverse start-lines)]
    (parse-start reversed [[] [] [] [] [] [] [] [] []])))
  ([start-lines existing]
  (let [columns (partition 4 4 " " (seq (first start-lines)))]
    (if (not-empty start-lines)
      (parse-start (rest start-lines) (map (fn [g cv] (if (not= (second g) \space) (conj cv (second g)) cv)) columns existing))
      existing))))

(defn parse-moves
  [moves]
  (map (fn [move] (map parse-int (rest (re-matches #"move (\d+) from (\d+) to (\d+)" move)))) moves))


(defn parse-lines
  [lines]
  (let [[start-padded moves] (split-at 10 lines)
        start (drop-last 2 start-padded)]
    [(parse-start start) (parse-moves moves)]))

(defn update-column
  [column grid i t from to]
  (cond 
    (= i to) (concat column (reverse (take-last t (nth grid (- from 1)))))
    (= i from) (drop-last t column)
    :else column))


(defn play-move
  [grid move]
  (let [[t from to] move]
    (map #(update-column %1 grid %2 t from to) grid (range 1 (+ 1 (count grid))))))

(defn part1
  []
  (let [[start moves] (parse-lines (readlines "resources/day5-1.txt"))
        finished (reduce play-move start moves)]
    (map last finished)))


(defn part2
  []
  (let [[start moves] (parse-lines (readlines "resources/day5-1.txt"))
        finished (reduce play-move start moves)]
    (map last finished)))
