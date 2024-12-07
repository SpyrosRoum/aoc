(ns aoc2024.day07
  (:require
   [clojure.string :refer [split split-lines]]))

(def example-input "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(def raw-input (slurp "inputs/day07"))

(defn parse-line
  [line]
  (let [[target-str others-str] (split line #": ")
        target (parse-long target-str)
        others (split others-str #" ")]
    {:target target :values (map parse-long others)}))

(defn parse-equations
  [text]
  (->> (split-lines text)
       (map parse-line)))

(defn concat-nums [x y] (parse-long (format "%d%d" x y)))

(defn solvable?
  ([{target :target nums :values} ops]
   (solvable? {:target target :values (rest nums)} ops (first nums)))
  ([{target :target nums :values} ops tally]
   (if (empty? nums)
     (= tally target)
     (->> ops
          (map (fn [op] #(solvable? {:target target :values (rest nums)} ops (op tally (first nums)))))
          (reduce (fn [_ func] (when (func) (reduced true))) false)))))

(defn part1
  [text]
  (->> text
       parse-equations
       (filter #(solvable? % [+ *]))
       (map :target)
       (reduce +)))

(defn part2
  [text]
  (->> text
       parse-equations
       (filter #(solvable? % [+ * concat-nums]))
       (map :target)
       (reduce +)))

(let [text raw-input]
  (println "Part 1:" (part1 text))
  (println "Part 2:" (part2 text)))
