(ns aoc-2022.day3
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

(defn split-in-two
  [s]
  (partition (/ (count s) 2) (seq s)))

(defn score-item
  [item]
  (let [ord (int item)]
    (if (>= ord 97) (- ord 96) (- ord 38))))


(defn part1
  []
  (let [bags (readlines "resources/day3-1.txt")
        items (map #(map set %) (map split-in-two bags))
        overlaps (map (fn [item] (clojure.set/intersection (first item) (second item))) items)
        bag-scores (map (fn [bag] (sum (map score-item bag))) overlaps)]
    (sum bag-scores)))

(defn find-overlap
  [group]
  (first (clojure.set/intersection (set (nth group 0)) (set (nth group 1)) (set (nth group 2)))))

(defn part2
  []
  (let [bags (map seq (readlines "resources/day3-1.txt"))
        groups (partition 3 bags)
        badges (map find-overlap groups)]
    (sum (map score-item badges))))

