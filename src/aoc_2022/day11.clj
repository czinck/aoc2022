(ns aoc-2022.day11
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

(defrecord Monkey [id items operation throw-to])

(deftrace monkey-throw
  [monkey item]
  (let [new-worry (int (/ ((:operation monkey) item) 3))
        to ((:throw-to monkey) new-worry)]
    [new-worry to]))

(defn update-monkey
  [monkey item]
  (Monkey. (:id monkey) (conj (:items monkey) item) (:operation monkey) (:throw-to monkey)))

(defn monkey-turn
  [monkey monkeys]
  (reduce (fn [new-monkeys item] 
            (let [[worry to-monkey-i] (monkey-throw monkey item)
                  to-monkey (monkeys to-monkey-i)]
              (update new-monkeys to-monkey update-monkey worry))) monkeys (:items monkey)))


(defn part1
  []
  (let [start-monkeys-arr [(Monkey. 0 [84 72 58 51] (fn [i] (* i 3)) (fn [i] (if (= (mod i 13) 0) 1 7)))
                       (Monkey. 1 [88 58 58] (fn [i] (+ i 8)) (fn [i] (if (= (mod i 2) 0) 7 5)))
                       (Monkey. 2 [93 82 71 77 83 53 71 89] (fn [i] (* i i)) (fn [i] (if (= (mod i 7) 0) 3 4)))
                       (Monkey. 3 [81 68 65 81 73 77 96] (fn [i] (+ i 2)) (fn [i] (if (= (mod i 17) 0) 4 6)))
                       (Monkey. 4 [75 80 50 73 88] (fn [i] (+ i 3)) (fn [i] (if (= (mod i 5) 0) 6 0)))
                       (Monkey. 5 [59 72 99 87 91 81] (fn [i] (* i 17)) (fn [i] (if (= (mod i 11) 0) 2 3)))
                       (Monkey. 6 [86 69] (fn [i] (+ i 6)) (fn [i] (if (= (mod i 3) 0) 1 0)))
                       (Monkey. 7 [91] (fn [i] (+ i 1)) (fn [i] (if (= (mod i 19) 0) 2 5)))]
        start-monkeys (apply hash-map (mapcat (fn [m] [(:id m) m]) start-monkeys-arr))]
    (monkey-turn (start-monkeys 0) start-monkeys)))

(defn part2 
  []
  )
