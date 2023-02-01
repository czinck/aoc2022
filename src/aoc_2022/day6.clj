(ns aoc-2022.day6
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

(defn groups
  [input n]
  (partition n (apply interleave (for [x (range 0 n)] (nthrest input x)))))

(defn part1
  []
  (let [input (slurp "resources/day6-1.txt")]
    (+ 4 (count (take-while (fn [group] (not= (count (set group)) 4)) (groups input 4))))))

(defn part2
  []
  (let [input (slurp "resources/day6-1.txt")]
    (+ 14 (count (take-while (fn [group] (not= (count (set group)) 14)) (groups input 14))))))
