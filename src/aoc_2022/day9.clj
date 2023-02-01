(ns aoc-2022.day9
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


(defn line-splitter
  [line]
  (let [[l r] (str/split line #"\s+")]
    [l (parse-int r)]))

(defn move
  [d]
  (cond 
    (> d 1) 1
    (< d -1) -1
    :else 0))

(defn mag
  [n]
  (/ n (abs n)))

(defn fix-tail
  [hn tc]
  (let [rd (- (:r hn) (:r tc))
        cd (- (:c hn) (:c tc))]
    (cond
      (and (<= (abs rd) 1) (<= (abs cd) 1)) [hn tc]
      (and (or (> (abs rd) 1) (> (abs cd) 1)) (or (= rd 0) (= cd 0))) [hn (Loc. (+ (:r tc) (move rd)) (+ (:c tc) (move cd)))]
      :else [hn (Loc. (+ (:r tc) (mag rd)) (+ (:c tc) (mag cd)))])))
      
      

(defn move-head
  [dir hc tc]
  (case dir
    "R" (fix-tail (Loc. (:r hc) (+ (:c hc) 1)) tc)
    "L" (fix-tail (Loc. (:r hc) (- (:c hc) 1)) tc)
    "U" (fix-tail (Loc. (- (:r hc) 1) (:c hc)) tc)
    "D" (fix-tail (Loc. (+ (:r hc) 1) (:c hc)) tc)))


(defn reduce-moves
  [[hc tc seen] [dir c]]
  (case c 
    0 [hc tc (conj seen tc)]
    1 (let [[-hc -tc] (move-head dir hc tc)]
      [-hc -tc (conj seen -tc)])
    (let [[-hc -tc -seen] (reduce reduce-moves [hc tc seen [dir 1]] (repeat c [dir 1]))]
      [-hc -tc (conj -seen -tc)])))


(defn part1
  []
  (let [moves (map line-splitter (readlines "resources/day9-1.txt"))]
    ;(sort (map (fn [l] [(:r l) (:c l)]) (nth (reduce reduce-moves [(Loc. 0 0) (Loc. 0 0) #{}] moves) 2)))))
    (count (nth (reduce reduce-moves [(Loc. 0 0) (Loc. 0 0) #{}] moves) 2))))


(defn part2 
  [])
