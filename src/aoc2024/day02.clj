(ns aoc2024.day02
  (:require [clojure.string :refer [split]]))

(def example-text "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(def raw-text (slurp "inputs/day02"))

(defn parse-lists
  [text]
  (as-> text v
      (split v #"\n")
      (map #(split % #" ") v)
      (map (fn [row] (map parse-long row)) v)))

(defn check-report
  [report]
  (let [going-up (< (first report) (second report))]
    (every?
     (fn [[a b]]
       (let [diff (- a b)]
         (and (not= diff 0)
              (<= (abs diff) 3)
              (= (< diff 0) going-up))))
     (partition 2 1 report))))

(defn report-without-idx
  [report idx]
  (concat (take idx report)
          (take-last (dec (- (count report) idx)) report)))

(defn check-report-with-tolerance
  ([report]
   (if (check-report report)
     true
     (check-report-with-tolerance report 0)))
  ([report skip-idx]
   (if (< skip-idx (count report))
     (if (check-report (report-without-idx report skip-idx))
       true
       (recur report (inc skip-idx)))
     false)))

(defn part1
  [text]
  (let [lists (parse-lists text)]
    (->> lists
         (map check-report)
         (filter (fn [r] r))
         count)))

(defn part2
  [text]
  (let [lists (parse-lists text)]
    (->> lists
         (map check-report-with-tolerance)
         (filter (fn [r] r))
         count)))
