(ns aoc-2022.day15
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


(defn build-grid
  []
  (let [lines (readlines "resources/day15-1.txt")
        parsed (map #(map parse-long (rest (re-find #"^.*x=(-?\d+), y=(-?\d+):.* x=(-?\d+), y=(-?\d+).*$" %))) lines)]
    (apply hash-map (mapcat (fn [[sx sy bx by]] [(list sx sy) (list bx by)]) parsed))))


(defn get-exclusions
  [[sx sy] [bx by]]
  (let [radius (+ (abs (- sx bx)) (abs (- sy by)))]
    (conj (for [nx (range (min (- sx radius) 0) (min (+ sx radius) 4000000))
          ny (range (min (- sy radius) 0) (min (+ sy radius) 4000000))
          :when (<= (+ (abs (- sx nx)) (abs (- sy ny))) radius)]
      (list nx ny)) (list bx by))))

(defn build-exclusion-list
  [grid]
  (set (mapcat (fn [k] (get-exclusions k (grid k) )) (keys grid))))


(defn candidates-for-sensor
  [[sx sy] [bx by]]
  (let [radius (+ (abs (- sx bx)) (abs (- sy by)))]
    (for [nx (range (- sx (inc radius)) (+ sx (inc radius)))
          ny [(- sy (inc (abs (- sx nx)))) 
              (+ sy (inc (abs (- sx nx))))
              (- sy (abs (- sx nx)))
              (+ sy (abs (- sx nx)))]
          :when (and (>= (+ (abs (- sx nx)) (abs (- sy ny))) radius) (>= nx 0) (>= ny 0) (<= nx 4000000) (<= ny 4000000))]
      (list nx ny))))


(defn remove-candidates
  [candidates [sx sy] [bx by]]
  (let [radius (+ (abs (- sx bx)) (abs (- sy by)))]
    (set (filter (fn [[cx cy]] (> (+ (abs (- sx cx)) (abs (- sy cy))) radius)) candidates))))


(defn part1
  []
  #_(let [grid (build-grid) 
        beacons (set (vals grid))
        exclusions-list (build-exclusion-list grid)]
    (count (filter #(= 2000000 (second %)) exclusions-list))))


(defn part2
  []
  (let [grid (build-grid) 
        beacons (set (vals grid))
        candidates (set (mapcat (fn [k] (candidates-for-sensor k (grid k))) (keys grid)))]
    (reduce (fn [c k] (remove-candidates c k (grid k))) candidates (keys grid))))
