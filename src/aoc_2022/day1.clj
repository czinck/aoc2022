(ns aoc-2022.day1
  (:require [clojure.string :as str]))

(defn readlines
  [filename]
  (clojure.string/split-lines (slurp filename)))

(defn sum
  [l]
  (reduce + l))

(defn parse-int
  [s]
  (Integer/parseInt s))

(defn get-calorie-count
  []
  (let [str-items (partition-by str/blank? (readlines "resources/day1-1.txt"))
        items (map #(map parse-int (remove str/blank? %)) str-items)]
    (map sum items)))
(defn part1
  []
  (apply max (get-calorie-count)))

(defn part2
  []
  (sum (take 3 (reverse (sort (get-calorie-count))))))
