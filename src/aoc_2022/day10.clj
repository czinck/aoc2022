(ns aoc-2022.day10
  (:require [clojure.string :as str])
  (:use clojure.tools.trace)
  (:require clojure.set))

(defn readlines
  [filename]
  (clojure.string/split-lines (slurp filename)))

(defrecord Loc [r c])

(defn sum
  [l]
  (reduce + l))

(defn parse-int
  [s]
  (Integer/parseInt s))


(defn expand
  [lines]
  (mapcat (fn [l] (if (= l "noop") [l] ["noop" l])) lines))

(defn register-reducer
  [v cycle]
  (if (= cycle "noop") v
    (let [[addx cs] (str/split cycle #"\s+")
          c (parse-int cs)]
      (+ v c))))

(defn signal-strengths
  [registers]
  (map (fn [i r] (* (+ 1 i) r)) (range (count registers)) registers))



(defn part1
  []
  (let [cycles (expand (readlines "resources/day10.txt"))
        registers (reductions register-reducer 1 cycles )
        strengths (signal-strengths registers)]
    (sum (for [i [20 60 100 140 180 220]]
           (nth strengths (- i 1))))))

(defn matches
  [i v]
  (let [r (mod i 40)]
    (cond 
      (= r 0) (or (= v r) (= v (+ r 1)))
      (= r 39) (or (= v (- r 1)) (= v r))
      :else (or (= v (- r 1)) (= v r) (= v (+ r 1))))))

(defn print-screen-line
  [line]
  (str (str/join (map (fn [d] (if d "#" " ")) line)) \newline))

(defn print-screen
  [matches]
  (let [lines (partition 40 matches)]
    (map print-screen-line lines)))

(defn part2
  []
  (let [cycles (expand (readlines "resources/day10.txt"))
        registers (reductions register-reducer 1 cycles )]
    (print-screen (map matches (range (count registers)) registers))))

