(ns aoc-2022.day2
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

; A X rock
; B Y paper
; C Z scissors
(defn win-loss-score
  [hand]
  (let [[opp you] (re-seq #"\w" hand)]
    (cond 
      (= opp "A") (case you
                    "X" 3
                    "Y" 6
                    "Z" 0)
      (= opp "B") (case you
                    "X" 0
                    "Y" 3
                    "Z" 6)
      (= opp "C") (case you
                    "X" 6
                    "Y" 0
                    "Z" 3))))

(defn your-play-score
  [hand]
  (let [[_ you] (re-seq #"\w" hand)]
    (case you
      "X" 1
      "Y" 2
      "Z" 3)))

; x lose
; y draw
; z win
(defn re-calc-play 
  [hand]
  (let [[opp you] (re-seq #"\w" hand)]
    (case opp
      "A" (case you
            "X" "A Z"
            "Y" "A X"
            "Z" "A Y")
      "B" (case you
            "X" "B X"
            "Y" "B Y"
            "Z" "B Z")
      "C" (case you
            "X" "C Y"
            "Y" "C Z"
            "Z" "C X"))))
            


(defn part1
  []
  (let [hands (readlines "resources/day2-1.txt")]
    (sum (map (fn [hand] (+ (win-loss-score hand) (your-play-score hand))) hands))))
  

(defn part2
  []
  (let [hands (map re-calc-play (readlines "resources/day2-1.txt"))]
    (sum (map (fn [hand] (+ (win-loss-score hand) (your-play-score hand))) hands))))
 
