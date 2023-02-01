(ns aoc-2022.day21
  (:require [clojure.string :as str])
  (:use clojure.tools.trace)
  (:require clojure.set))

(defn readlines
  [filename]
  (clojure.string/split-lines (slurp filename)))

(defn sum
  [l]
  (reduce + l))

(defn map-row
  [row]
  (let [[monkey function] (rest (re-find #"^(\w+): (.*)$" row))
        function-tokens (str/split function #" ")]
    (cond 
      (= monkey "humn") nil
      (= (count function-tokens) 1) [monkey (repeat (parse-long (first function-tokens)))]
      :else [monkey [(second function-tokens) (first function-tokens) (nth function-tokens 2)]])))


(defn build-tree
  [fname]
  (let [lines (readlines fname)]
    (apply hash-map (mapcat map-row lines))))

(defn parse-operator
  [op]
  (case op
    "+" +
    "-" -
    "*" *
    "/" /))

(defn update-monkey
  [monkeys monkey]
  (cond 
    (int? (monkeys monkey)) monkeys
    :else (let [[op f s] (monkeys monkey)]
            (if (and (int? (monkeys f)) (int? (monkeys s))) 
              (assoc monkeys monkey ((parse-operator op) (monkeys f) (monkeys s)))
              monkeys))))

(defn update-known-monkeys
  [monkeys]
  (loop [-monkeys monkeys]
    (if (int? (monkeys "root"))
      (monkeys "root")
      (recur (reduce update-monkey -monkeys (keys -monkeys))))))


(defn do-operation
  [op l r]
  (cond 
    (and (int? l) (int? r)) (op l r)
    (and (seq? l) (seq? r)) (map op l r)
    (seq? l) (map #(op % r) l)
    :else (map #(op l %) r)))

(defn calc-monkey
  [monkeys monkey cache]
  (cond 
    (= monkey "humn") [cache (range)]
    (or (int? (monkeys monkey)) (seq? (monkeys monkey))) [cache (monkeys monkey)]
    (contains? cache monkey) [cache (cache monkey)]
    :else (let [[op f s] (monkeys monkey)
                [-fcache fv] (calc-monkey monkeys f cache)
                fcache (assoc -fcache f fv)
                [-fscache sv] (calc-monkey monkeys s fcache)
                fscache (assoc -fscache s sv)
                v (do-operation (parse-operator op) fv sv)]
            [(assoc fscache monkey v) v])))
  
(defn root-equal
  [monkeys]
  (let [[_ f s] (monkeys "root") 
        [fcache fv] (calc-monkey monkeys f {})
        [fscache sv] (calc-monkey monkeys s fcache)]
    (first (filter (fn [[l r i]] (= l r)) (map (fn [l r i] [l r i]) fv sv (range))))))


(defn part1
  []
  #_(let [monkeys (build-tree "resources/day21-1.txt")]
    (second (calc-monkey monkeys "root" {}))))

(defn part2
  []
  (let [monkeys (build-tree "resources/day21-1.txt")]
    (root-equal monkeys)))
