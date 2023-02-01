(ns aoc-2022.day7
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

(defn handle-command
  [current-dir path line]
  (let [[_ command args] (str/split line #"\s+")]
    (cond
      (= command "cd") (if (= args "..") [current-dir (pop path)] [current-dir (conj path args)])
      :else [current-dir path])))

(defn handle-dir
  [current-dir path line]
  ; i think this is actuall nothing?
  [current-dir path])

(defn handle-file
  [current-dir path line]
  (let [[size fname] (str/split line #"\s+")]
    [(assoc-in current-dir (conj path fname) (parse-int size)) path]))

(defn update-directory
  [current-dir path line]
  (cond 
    (= (first line) \$) (handle-command current-dir path line)
    (= (take 3 line) '(\d \i \r)) (handle-dir current-dir path line)
    :else (handle-file current-dir path line)))


(defn curry-reducer
  [[current-dir path] line] 
    (update-directory current-dir path line))

#_(defn -walk-directory
  [directory path]
  (for [k (keys directory)
        :let [v (get directory k)]]
    (if (int? v) [path v] (-walk-directory v (conj path k)))))

#_(defn walk-directory
  [directory path]
  (let [walked (-walk-directory directory path)]
    (conj walked [(butlast (first (first walked))) (sum (map (fn [w] (second w)) walked))])))
      

(defn walk-directory
  [directory path]
  (apply concat (for [k (keys directory)
        :let [v (get directory k)]]
    (if (int? v) [path] (concat [path] (walk-directory v (conj path k)))))))

(defn directory-size
  [directory path]
  (let [current (get-in directory path)]
    (if (int? current) 
      current 
      (sum (for [k (keys current)] (directory-size directory (conj path k)))))))
                 

(defn part1
  []
  (let [lines (readlines "resources/day7-1.txt")
        [grid _] (reduce curry-reducer [nil []] lines)
        directories (set (walk-directory grid []))]
    (sum (filter #(< % 100000) (map #(directory-size grid %) directories)))))


(defn part2
  []
  (let [lines (readlines "resources/day7-1.txt")
        [grid _] (reduce curry-reducer [nil []] lines)
        directories (set (walk-directory grid []))
        used-size (directory-size grid ["/"])]
    (apply min (filter #(> % (- 30000000 (- 70000000 used-size))) (map #(directory-size grid %) directories)))))
