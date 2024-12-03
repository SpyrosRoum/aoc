(ns aoc2024.day03
  (:require [clojure.string :refer [join]]))

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
  [text]
  (let [should_mul (atom true)]
    (for [[action a b] (re-seq either-regex text)]
      (if (and (some? a) (some? b))
        (if (deref should_mul)
          (let [a (parse-long a)
                b (parse-long b)]
            (* a b)))
        (if (= action "do()")
          (reset! should_mul true)
          (reset! should_mul false))))))

(defn part2
  [text]
  (reduce + (filter number? (part2-multiplier text))))
