(ns aoc2024.day03
  (:require
   [clojure.string :refer [join]]))

(def example-text "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(def example-text2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
(def raw-text (slurp "inputs/day03"))

(def mul-regex #"mul\((\d{1,3}),(\d{1,3})\)")
(def do-dont-regex #"do(?:n't)?\(\)")
(def either-regex (re-pattern (join "|" [mul-regex do-dont-regex])))

(defn part1
  [text]
  (reduce
    +
    0
    (for [[_ a b] (re-seq mul-regex text)]
      (* (parse-long a) (parse-long b)))))

(defn part2
  ([text]
   (part2 (re-seq either-regex text) true 0))
  ([col should-multiply total]
   (if (empty? col)
     total
     (let [[[action a b] & tail] col]
       (if (and should-multiply (re-matches #"mul.*" action))
         (recur tail should-multiply (+ total (* (parse-long a) (parse-long b))))
         (recur tail (= action "do()") total))))))

(part1 raw-text)
(part2 raw-text)
