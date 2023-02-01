(ns aoc-2022.day8
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

(defn row
  [grid n]
  (nth grid n))

(defn column
  [grid n]
  (map #(nth % n) grid))

(defn get-data
  []
  (let [lines (readlines "resources/day8-1.txt")]
    (map (fn [line] (map #(parse-int (str %)) line)) lines)))

(defn helper-max
  [l] 
  (if (> (count l) 0)
    (apply max l)
    -1))

(defn score-direction
  [height dir]
  (reduce (fn [c d] (if (>= d height) (reduced (+ 1 c)) (+ 1 c))) 0 dir))

(defn score-visibility
  [grid i j]
  (let [tree-height (nth (row grid i) j)
        [-left -right] (split-at j (row grid i))
        left (reverse -left)
        right (rest -right)
        [-up -down] (split-at i (column grid j))
        up (reverse -up)
        down (rest -down)]
    (* (score-direction tree-height up)
       (score-direction tree-height down)
       (score-direction tree-height left)
       (score-direction tree-height right))))

(defn tree-visible
  [grid i j]
  (let [tree-height (nth (row grid i) j)
        [left -right] (split-at j (row grid i))
        right (rest -right)
        [up -down] (split-at i (column grid j))
        down (rest -down)]
    (or (> tree-height (helper-max left))
         (> tree-height (helper-max right))
         (> tree-height (helper-max up))
         (> tree-height (helper-max down)))))


(defn part1 
  []
  (let [grid (get-data)]
    (count (filter identity 
      (for [i (range (count grid))
            j (range (count (first grid)))]
        (tree-visible grid i j))))))

(defn part2 
  []
  (let [grid (get-data)]
    (apply max
      (for [i (range (count grid))
            j (range (count (first grid)))]
        (score-visibility grid i j)))))

