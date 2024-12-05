(ns aoc2024.day05
  (:require
   [clojure.string :refer [split split-lines]]
   [clojure.tools.trace :refer [trace]]))

(def example-text
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(def raw-text (slurp "inputs/day05"))

(defn rule-pairs [raw-rules]
  "Split rules to pairs"
  (map #(split % #"\|") (split-lines raw-rules)))

(defn parse-rules [text]
  (let [raw-rules (first (split text #"\n\n"))
        pairs (rule-pairs raw-rules)]
    (loop [[[before after] & rest] pairs
           result {}]
      (if (nil? before)
        result
        (let [existing (get result before #{})]
          (recur rest (assoc result before (conj existing after))))))))

(defn is-ordered? [before->after job]
  (let [page-to-position (first (reduce
                                  (fn [[res idx] page]
                                    [(assoc res page idx) (inc idx)])
                                  [{} 0]
                                  job))]
    (reduce
      (fn [res [before afters]]
        (if-let [before-pos (get page-to-position before nil)]
          (loop [[after & rest] afters
                 is-ok res]
            (if (nil? after)
              is-ok
              (if-let [after-pos (get page-to-position after nil)]
                (if (< after-pos before-pos)
                  (reduced false)
                  (recur rest is-ok))
                (recur rest is-ok))))
          true))
      true
      before->after)))

(defn get-middle [job]
  (let [mid (int (/ (count job) 2))]
    (nth job mid)))

(defn part1 [text]
  (let [[raw-rules raw-updates] (split text #"\n\n")
        rules (parse-rules raw-rules)
        updates (map #(split % #",") (split-lines raw-updates))]
    (->> updates
         (filter (partial is-ordered? rules))
         (map get-middle)
         (map parse-long)
         (reduce +))))

(defn custom-compare [before->after x y]
  "Returns `true` if `x` comes before `y`"
  (some? (some #{y} (get before->after x #{}))))

(defn part2 [text]
  (let [[raw-rules raw-updates] (split text #"\n\n")
        rules (parse-rules raw-rules)
        comp (partial custom-compare rules)
        updates (map #(split % #",") (split-lines raw-updates))]
    (->> updates
         (filter (complement (partial is-ordered? rules)))
         (map #(sort comp %))
         (map get-middle)
         (map parse-long)
         (reduce +))))

(let [text raw-text]
  (println "Part 1:" (part1 text))
  (println "Part 2:" (part2 text)))
