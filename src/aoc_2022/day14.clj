(ns aoc-2022.day14
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


(defn create-initial-grid
  []
  (let [file-contents (slurp "resources/day14-1.txt")
        coords (map (fn [g] [(parse-long (second g)) (parse-long (nth g 2))]) (re-seq #"(\d+),(\d+)" file-contents))
        max-c (apply max (map first coords))
        max-r (apply max (map second coords))]
    (vec (for [r (range (+ max-r 2))]
      (vec (for [c (range (+ max-c 200))]
        0))))))

(defn expand-segment
  [[lc lr] [rc rr]]
  (if (= lc rc) 
    (map (fn [r] [lc r]) (range (min lr rr) (inc (max lr rr))))
    (map (fn [c] [c lr]) (range (min lc rc) (inc (max lc rc))))))


(defn expand-pipe
  [pipe]
  (let [pipe-segments (map (fn [m] [(parse-long (second m)) (parse-long (nth m 2))]) (re-seq #"(\d+),(\d+)" pipe))]
    (mapcat expand-segment pipe-segments (rest pipe-segments))))


(defn add-pipe-to-grid
  [grid pipe]
  (let [expanded-pipe (expand-pipe pipe)]
    (reduce (fn [-grid coord] (assoc-in -grid (reverse coord) 1)) grid expanded-pipe)))


        
(defn build-grid
  [initial-grid lines]
  (reduce add-pipe-to-grid initial-grid lines))

(defn sand-next-move
  [[sandc sandr] grid]
  (cond 
    (= (dec (count grid)) sandr) (reduced nil)
    (zero? (nth (nth grid (inc sandr)) sandc)) [sandc (inc sandr)]
    (zero? (nth (nth grid (inc sandr)) (dec sandc))) [(dec sandc) (inc sandr)]
    (zero? (nth (nth grid (inc sandr)) (inc sandc))) [(inc sandc) (inc sandr)]
    :else (reduced [sandc sandr])))

(defn drop-sand-grain
  [grid i]
  (let [coord (reduce sand-next-move [500 0] (repeat grid))]
    (if (or (nil? coord) (zero? (second coord)))
      (reduced i)
      (assoc-in grid (reverse coord) 2))))


(defn part1
  []
  (let [pipe-lines (readlines "resources/day14-1.txt")
        grid (build-grid (create-initial-grid) pipe-lines)
        sand-count (reduce drop-sand-grain grid (range))]
    sand-count))

(defn part2
  []
  (let [pipe-lines (readlines "resources/day14-1.txt")
        no-floor-grid (build-grid (create-initial-grid) pipe-lines)
        grid (conj no-floor-grid (vec (repeat (count (first no-floor-grid)) 1)))
        sand-count (reduce drop-sand-grain grid (range))]
    sand-count))
