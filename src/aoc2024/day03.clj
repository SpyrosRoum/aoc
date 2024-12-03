(ns aoc2024.day03
  (:require [clojure.string :refer [join]]
             [clojure.tools.trace :refer [trace]]))

(def example-text "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(def example-text2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(def mul-regex #"mul\((\d{1,3}),(\d{1,3})\)")
(def do-dont-regex #"do(?:n't)?\(\)")
(def either-regex (re-pattern (join "|" [mul-regex do-dont-regex])))

(def raw-text (slurp "inputs/day03"))

(defn part1
  [text]
  (reduce
   +
   0
   (for [[_ a b] (re-seq mul-regex text)]
     (* (parse-long a) (parse-long b))
     )))

(defn part2-multiplier
  ([col] (part2-multiplier col true 0))
  ([col should_multiply total]
   (if (empty? col)
     total
     (let [[[action a b] & tail] col]
       (if should_multiply
         (if (and (some? a) (some? b))
           ;; We have a multiplication, and we should multiply
           (recur tail should_multiply (+ total (* (parse-long a) (parse-long b))))
           ;; We have a do or dont
           (recur tail (= action "do()") total))
         ;; We shouldn't multiply, no need to check what exactly we have
         (recur tail (= action "do()") total))))))


(defn part2
  [text]
  (part2-multiplier (re-seq either-regex text)))
