(ns aoc-2022.day4
  (:require [clojure.string :as str])
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

(defn expand-range
  [r]
  (let [[l r] (map parse-int (str/split r #"\-"))]
    (range l (+ 1 r))))

(defn expand-line
  [line]
  (let [[l r] (str/split line #",")]
    (map set [(expand-range l) (expand-range r)])))

(defn full-overlap
  [[l r]]
  (or (clojure.set/superset? l r) (clojure.set/superset? r l)))

(defn partial-overlap
  [[l r]]
  (not-empty (clojure.set/intersection l r)))

(defn part1
  []
  (let [groups (map expand-line (readlines "resources/day4-1.txt"))
        overlapping-groups (filter full-overlap groups)]
    (count overlapping-groups)))


(defn part2
  []
  (let [groups (map expand-line (readlines "resources/day4-1.txt"))
        overlapping-groups (filter partial-overlap groups)]
    (count overlapping-groups)))
