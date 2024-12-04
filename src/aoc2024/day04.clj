(ns aoc2024.day04
  (:require
   [clojure.string :refer [split split-lines]]))

(def example-text
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")
(def raw-text (slurp "inputs/day04"))

(def xmas-vec (split "XMAS" #""))

(defn normalize-text
  "Return the text as a vector of characters, along with the length of each line"
  [text]
  (let [lines (split-lines text)
        ;; inc to account for the \n that will be in the lines
        line-len (inc (count (first lines)))]
    {:text-coll (->> (split text #"")
                     ;; We want to keep the \n in the final seq so that we can
                     ;; easily search for XMAS without worrying that we go over a line
                     flatten
                     (into []))
     :line-len line-len}))

(defn line-col-to-idx
  [[line col] line-len]
  (+ (* line-len line) col))

(defn get-at
  [coll line-col line-len]
  (let [idx (line-col-to-idx line-col line-len)]
    (nth coll idx nil)))

(defn next-line-col [[line col] line-len]
  "Increment the `col`, if at line edge increment `line` and set `col` to `0`."
  (if (>= (inc col) line-len)
    [(inc line) 0]
    [line (inc col)]))

(defn idx-right-diff-calc [data] (+ (:times data)))
(def idx-left-diff-calc (comp - idx-right-calc))

(defn idx-down-diff-calc [data] (* (:line-len data) (:times data)))
(def idx-up-diff-calc (comp - idx-down-diff-calc))

(defn idx-down-right-diff-calc [data] (+ (idx-down-diff-calc data) (idx-right-diff-calc data)))
(defn idx-down-left-diff-calc [data] (+ (idx-down-diff-calc data) (idx-left-diff-calc data)))

(defn idx-up-right-diff-calc [data] (+ (idx-up-diff-calc data) (idx-right-diff-calc data)))
(defn idx-up-left-diff-calc [data] (+ (idx-up-diff-calc data) (idx-left-diff-calc data)))

(defn check-xmas
  "Check if there is an XMAS starting at the current position. The next position is determined by `next-idx-calc`"
  [coll [line col] starting-idx line-len idx-diff-calc]
  (->> (reduce
         (fn [times target-char]
           (let [target-idx (+ starting-idx (idx-diff-calc {:times times :line-len line-len}))
                 char-at-idx (nth coll target-idx nil)]
             (if (= char-at-idx target-char)
               (inc times)
               (reduced :miss-match))))
         1
         ["M" "A" "S"])
       (not= :miss-match)))

(defn count-xmas
  "Count how many XMASes there are in the collection"
  ([line-len coll] (count-xmas line-len coll [0 0] 0))
  ([line-len coll line-col total]
   (if-let [curr (get-at coll line-col line-len)]
     (if (= curr "X")
       (let [curr-idx (line-col-to-idx line-col line-len)]
         (->> []
              (cons (check-xmas coll line-col curr-idx line-len idx-right-diff-calc))
              (cons (check-xmas coll line-col curr-idx line-len idx-left-diff-calc))

              (cons (check-xmas coll line-col curr-idx line-len idx-up-diff-calc))
              (cons (check-xmas coll line-col curr-idx line-len idx-down-diff-calc))

              (cons (check-xmas coll line-col curr-idx line-len idx-up-right-diff-calc))
              (cons (check-xmas coll line-col curr-idx line-len idx-down-right-diff-calc))

              (cons (check-xmas coll line-col curr-idx line-len idx-up-left-diff-calc))
              (cons (check-xmas coll line-col curr-idx line-len idx-down-left-diff-calc))

              (filter true?)
              count
              (+ total)
              (recur line-len coll (next-line-col line-col line-len))))
       (recur line-len coll (next-line-col line-col line-len) total))
     total)))

(defn part1
  ([text]
   (let [flattened (normalize-text text)]
     (count-xmas (:line-len flattened) (:text-coll flattened)))))

;; === Part 2

(defn check-mas
  "Check if there is a valid MAS with the A in the current position"
  [coll [line col] line-len]
  (and (or (and
             (= (get-at coll [(dec line) (dec col)] line-len) "M")
             (= (get-at coll [(inc line) (inc col)] line-len) "S"))
           (and
             (= (get-at coll [(dec line) (dec col)] line-len) "S")
             (= (get-at coll [(inc line) (inc col)] line-len) "M")))
       (or (and
             (= (get-at coll [(dec line) (inc col)] line-len) "M")
             (= (get-at coll [(inc line) (dec col)] line-len) "S"))
           (and
             (= (get-at coll [(dec line) (inc col)] line-len) "S")
             (= (get-at coll [(inc line) (dec col)] line-len) "M")))))

(defn count-mas
  ([line-len coll] (count-xmas-2 line-len coll [0 0] 0))
  ([line-len coll line-col total]
   (if-let [curr (get-at coll line-col line-len)]
     (if (= curr "A")
       (if (check-mas coll line-col line-len)
         (recur line-len coll (next-line-col line-col line-len) (inc total))
         (recur line-len coll (next-line-col line-col line-len) total))
       (recur line-len coll (next-line-col line-col line-len) total))
     total)))

(defn part2 [text]
  (let [flattened (normalize-text text)]
    (count-mas (:line-len flattened) (:text-coll flattened))))

(let [text raw-text]
  (println "Part1: " (part1 text))
  (println "Part2: " (part2 text)))
