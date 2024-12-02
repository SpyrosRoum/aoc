(ns aoc2024.day01
  (:gen-class))

(def example-text "3   4
4   3
2   5
1   3
3   9
3   3")

(def raw-text (slurp "inputs/day01"))

(defn split-lists
  [text]
  (reduce
   (fn [[list1 list2]
        line]
     (let [[a b] (clojure.string/split line #"   ")]
       [(conj list1 (Integer. a))  (conj list2 (Integer. b)) ]))
   [[] []]
   (clojure.string/split text #"\n")))

;; TODO: Could have used `frequencies` probably
(defn count-occurancies
  [arr n]
  (count (filter #(= n %) arr)))

(defn part1
  [text]
  (let [[list1 list2] (split-lists text)
        sorted-list1 (sort list1)
        sorted-list2 (sort list2)]
    (->> (map (comp abs -) sorted-list1 sorted-list2)
         (reduce + 0))))

(defn part2
  [text]
  (let [[list1 list2] (split-lists text)]
    (def count-occ (memoize (partial count-occurancies list2)))
    (->> list1
         (map count-occ)
         (map * list1)
         (reduce + 0))))
